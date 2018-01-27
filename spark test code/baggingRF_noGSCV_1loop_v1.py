__author__ = 'hjin'
__project__ = 'baggingRF'

# Created by hjin on 5/19/2016
"""
This application is to do bagging RF without CV and grid search in ML module

The main function would be used is the map function, the model is RF

"""

import sys
import os
import time
import datetime
from pyspark import SparkConf, SparkContext
from pyspark.sql import SQLContext
from pyspark.sql.functions import *
from pyspark.mllib.linalg import Vectors
import numpy as np
from pyspark.sql.functions import *
from pyspark.sql.types import DoubleType

from pyspark.ml import Pipeline
from pyspark.ml.classification import RandomForestClassifier
from pyspark.ml.feature import StringIndexer, VectorIndexer
from pyspark.ml.evaluation import BinaryClassificationEvaluator, MulticlassClassificationEvaluator

#constants variables
#app_name = sys.argv[1]
app_name = "Jie_test"
path = "s3://emr-rwes-pa-spark-dev-datastore/Hui/shire_test"
data_path = path + "/01_data/data_973"
pos_file = "/dat_hae.csv"
neg_file ="/dat_nonhae.csv"
ts_prop = 0.2
num_simu = 5
ratio = 50
nIter = int(200/ratio)
numTrees = 30
numDepth = 5
seed = 42
par = 10

start_time = time.time()
st = datetime.datetime.fromtimestamp(start_time).strftime('%Y%m%d')
resultDir_s3 = path + "/02_result/" + app_name + st + "/"

#define the functions will be used
#function 1: load the CSV file from S3 bucket and convet to labeledPoint format in RDD
def load_csv_file(sc, data_path, data_file, par, pos_flag=True):
    #reading in data as RDD data
    data1 = sc.textFile(data_path + data_file, par) #in RDD format
    # Skip the header, and re-structure the columns as "label" and "features"
    header = data1.first()
    data2 = data1.filter(lambda x: x != header).map(lambda line: line.split(','))
    if pos_flag == True:
        #column 0 is patient id, column 1 is label, from column 2 to end are features
        data3 = data2.map(lambda line: (line[0], line[1], Vectors.dense(np.asarray(line[2:]).astype(np.float32))))
        # Convert to Spark DataFrame
        data_df = sqlContext.createDataFrame(data3, ['patid', 'label', 'features'])
    else:
        #column 0 is patient id, column 1 is label, last column is the hae_patient_id from, column 2 to end are features
        data3 = data2.map(lambda line: (line[0], line[1], line[-1],
                                        Vectors.dense(np.asarray(line[2:-1]).astype(np.float32))))
        # Convert to Spark DataFrame
        data_df = sqlContext.createDataFrame(data3, ['patid', 'label',
                                                     'hae_patid', 'features'])
    # Convert label to double type
    data_df1 = data_df.withColumn('patid', data_df['patid'].cast('double'))
    return data_df1.withColumn('label', data_df1['label'].cast('double'))

#function 2: output the DataFrame to CSV
def toCSVLine(data):
    return ','.join(str(d) for d in data)

#function 3: split the column in RDD into different columns in dataframe
def Parse(pair):
    return dict(list(pair[0].asDict().items()) + [("iterid", pair[1])])

#function 4: get the predicted probability in Vector
def getitem(i):
    def getitem_(v):
         return v.array.item(i)
    return udf(getitem_, DoubleType())

#function 5: register the DataFrame as table
def regit(iterid):
    return pred_ts_ls[iterid].registerTempTable(('ls_' + str(iterid)))

#function 6: create SQL query
def sqlqu(nIter):
    sqla = 'SELECT tb0.patid AS patid, tb0.label AS label, (tb0.prob_1'
    for i in range(1, nIter):
        sqla = sqla + '+tb' + str(i) + '.prob_1'
    sqlb = ')/' + str(nIter) + ' AS avg_prob FROM ls_0 AS tb0'
    for j in range(1, nIter):
        sqlb = sqlb + ' INNER JOIN ls_' + str(j) + ' AS tb' + str(j)\
               + ' ON tb0.patid = tb' + str(j) +'.patid'
    sqlquery = sqla + sqlb
    return sqlquery

#function 6: bagging random forest
def baggingRF(iterid, neg_tr_iterid, pos_tr, ts):
    #select the Non-HAE patient by iteration ID
    ineg_tr = neg_tr_iterid\
        .filter(neg_tr_iterid.iterid == iterid)\
        .select('patid', 'label', 'features')
    #combine with positive training data
    itr = pos_tr.unionAll(ineg_tr)
    #create the labelIndexer
    #transfer to RF invalid label column
    labelIndexer = StringIndexer(inputCol="label",outputCol="indexedLabel").fit(itr)
    # Train a RandomForest model.
    rf = RandomForestClassifier(labelCol="indexedLabel",
                                maxDepth=numDepth, numTrees=numTrees, seed=seed)
    # Chain indexers and forest in a Pipeline
    pipeline = Pipeline(stages=[labelIndexer, rf])
    # Train model.  This also runs the indexers.
    model = pipeline.fit(itr)
    # Make predictions.
    predictions = model.transform(ts)
    pred_score_ts = predictions.select(
            predictions.patid, predictions.label,
            getitem(0)('probability').alias('prob_0'),
            getitem(1)('probability').alias('prob_1'))
    evaluator = MulticlassClassificationEvaluator(labelCol="indexedLabel",
    predictionCol="prediction",
    metricName="precision")
    ppv = evaluator.evaluate(predictions)
    return pred_score_ts


#function 7: main function
def main(sc, pos_ori, neg_ori, path=path):
    #random split in positive cases and use HAE patient ID to select Non-HAE
    (pos_tr, pos_ts) = pos_ori.randomSplit([(1-ts_prop), ts_prop])
    #using HAE patient ID to select Non-HAE patient and only keep Non-HAE
    neg_tr = neg_ori\
        .join(pos_tr, neg_ori.hae_patid == pos_tr.patid,'inner')\
        .select(neg_ori.patid, neg_ori.label, neg_ori.hae_patid,neg_ori.features)
    #!!!after the merging, the Non-HAE patients ordered by hae_patid!!!
    #create iteration ID in Non-HAE
    nData = pos_tr.count()*200
    npIterIDs = np.array(list(range(nIter))*np.ceil(float(nData)/nIter))
    rddIterIDs = sc.parallelize(npIterIDs).map(int)
    #add index for Non-HAE patients
    neg_tr_Indexed1 = neg_tr\
        .rdd\
        .zipWithIndex()\
        .toDF()\
        .withColumnRenamed("_1", "orgData")
    #add index for iteration ID
    dfIterIDs = rddIterIDs\
        .zipWithIndex()\
        .toDF()\
        .withColumnRenamed("_1", "iterid")
    #merge them together to get the Non-HAE patients with iteration ID
    neg_tr_iterid = neg_tr_Indexed\
        .join(dfIterIDs, "_2")\
        .drop('_2')\
        .map(Parse)\
        .coalesce(par)\
        .toDF()\
        .drop('hae_patid')\
        .cache()
    #test set is the rows in original negative cases but not in training set
    neg_ts = neg_ori\
        .subtract(neg_tr)\
        .drop('hae_patid')
    #combine to test data
    ts = pos_ts.unionAll(neg_ts)
    #ts_patid = ts.select('patid', 'label')
    #do loops on baggingRF function
    pred_ts_ls = [baggingRF(iterid, neg_tr_iterid, pos_tr, ts) for iterid in
                  range(nIter)]
    return pred_ts_ls


if __name__ == "__main__":

    sc = SparkContext(appName = app_name)

    sqlContext = SQLContext(sc)

    #reading in the data from S3
    pos_ori = load_csv_file(sc, data_path, pos_file, par)
    neg_ori = load_csv_file(sc, data_path, neg_file, par, False)
    #doing the main function in for loop as well, return as list
    #[main(sc, isimu, pos_ori=pos_ori, neg_ori=neg_ori ) for isimu in range(
    #        num_simu)]
    pred_ts_ls = main(sc, pos_ori=pos_ori, neg_ori=neg_ori )

    #register DataFrame as a table
    for iterid in range(nIter):
        regit(iterid)

    #create the SQL queries
    sql_query = sqlqu(nIter)

    #using SQL queries to create dataframe including mean of prob
    avg_pred_ts = sqlContext.sql(sql_query)

    #map each row as CSV line
    avg_pred_ts_line = avg_pred_ts.map(toCSVLine)

    #output the predicted scores to S3
    avg_pred_ts_line.saveAsTextFile((resultDir_s3 + "avg_pred_ts.csv" ))

    sc.stop()








