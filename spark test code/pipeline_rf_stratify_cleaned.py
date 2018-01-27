# Import the packages
print("starting..")

import os
import time
import datetime

import sys
from pyspark import SparkConf, SparkContext
from pyspark.sql import SQLContext
from pyspark.mllib.linalg import Vectors
import numpy as np
#import random

from pyspark.ml import Pipeline
from pyspark.ml.classification import RandomForestClassifier
from pyspark.ml.feature import StringIndexer
from pyspark.ml.evaluation import BinaryClassificationEvaluator
#from pyspark.ml.feature import HashingTF, Tokenizer
from pyspark.ml.tuning import CrossValidator, ParamGridBuilder

# global
s3_path = "s3://emr-rwes-pa-spark-dev-datastore/Jie/forSpark"
inpath = ""
data_file = 'binIris_SepalWidth_dummy.csv'
app_name = "lichao_test_rf1_jieTest"
par = 3
start_tree = 5
stop_tree = 10
num_tree = 2
start_depth = 2
stop_depth = 3
num_depth = 2
fold = 3
nDesiredPartitions = 3

def load_csv_file(sc, data_path, inpath, file, par):
    #reading in data as RDD data
    data1 = sc.textFile(data_path + inpath + file, par) #in RDD format
    
    # Skip the header, and re-structure the columns as "label" and "features"
    header = data1.first()
    data2 = data1.filter(lambda x: x != header).map(lambda line: line.split(','))
    
    #column 0 is patient id, column 1 is label, from column 2 to end are features
    # data3 = data2.map(lambda line: (line[0], line[1], Vectors.dense(np.asarray(line[2:]).astype(np.float32))))
    
    n_total_vars = len(data1.take(2)[1].split(','))
    feature_indices = list(range(1, n_total_vars))
    # feature_indices = list(range(2, 3))
    if len(feature_indices) == 1:
        data3 = data2.map(lambda line: (int(line[0]), Vectors.dense(line[feature_indices[0]])))
    else:
        data3 = data2.map(lambda line: (int(line[0]), Vectors.dense(np.asarray([line[i] for i in feature_indices]).astype(np.float32))))
    
    # Convert to Spark DataFrame
    data_df = sqlContext.createDataFrame(data3, ['label', 'features'])
    
    # Convert label to double type
    return data_df.withColumn('label', data_df['label'].cast('double'))

def toCSVLine(data):
    return ','.join(str(d) for d in data)
    
def Parse(pair):
    return dict(list(pair[0].asDict().items()) + [("foldID", pair[1])])

def main(sc, s3_path=s3_path, inpath=inpath, data_file=data_file,
         par=par):

    #reading in data
    data = load_csv_file(sc=sc, data_path=s3_path, inpath=inpath, file=data_file, par=par)
    nData = data.count()
    
    # stratify
    npFoldIDs = np.array(list(range(fold))*np.ceil(float(nData) / fold))
    npFoldIDs = npFoldIDs[0:nData]
    np.random.shuffle(npFoldIDs)
    rddFoldIDs = sc.parallelize(npFoldIDs).map(int) 
    
    # add the column of fold ids    
    dfDataWithIndex = data.rdd.zipWithIndex().toDF().withColumnRenamed("_1", "orgData")
    dfNewKeyWithIndex = rddFoldIDs.zipWithIndex().toDF().withColumnRenamed("_1", "key")
    dfJoined = dfDataWithIndex.join(dfNewKeyWithIndex, "_2")
    keep = [c for c in dfJoined.columns if c != "_2"]
    dfJoined = dfJoined.select(*keep)    
    dataWithFoldID = dfJoined.map(Parse).coalesce(nDesiredPartitions).toDF()
    
    #
    start_time = time.time()
    st = datetime.datetime.fromtimestamp(start_time).strftime('%Y%m%d_%H%M%S')
    resultDir_s3 = s3_path + "Results/" + st + "/"
    if not os.path.exists(resultDir_s3):
        os.makedirs(resultDir_s3)
    resultDir_master = "/home/lichao.wang/code/lichao/test/Results/" + st + "/"
    if not os.path.exists(resultDir_master):
        os.makedirs(resultDir_master)    

    # iteration through all folds
    for iFold in range(fold):
        # stratified sampling
        ts = dataWithFoldID.filter(dataWithFoldID.foldID == iFold)
        tr = dataWithFoldID.filter(dataWithFoldID.foldID != iFold)

        # remove the fold id column
        keep = [c for c in ts.columns if c != "foldID"]
        ts = ts.select(*keep)
        tr = tr.select(*keep)
        
        #transfer to RF invalid label column
        stringIndexer = StringIndexer(inputCol="label", outputCol="indexed")
        si_model = stringIndexer.fit(tr)
        tr_td = si_model.transform(tr)
        # si_model_ts = stringIndexer.fit(ts)
        ts_td = si_model.transform(ts)
        
        # Build the model
        rf = RandomForestClassifier(labelCol="indexed", featuresCol="features")
        
        # Create the pipeline
        pipeline = Pipeline(stages=[rf])

        # Create the parameter grid builder
        paramGrid = ParamGridBuilder()\
            .addGrid(rf.numTrees, list(np.linspace(start_tree, stop_tree,
                                                   num_tree).astype('int')))\
            .addGrid(rf.maxDepth, list(np.linspace(start_depth, stop_depth,
                                                   num_depth).astype('int')))\
            .build()


        # Create the evaluator
        evaluator = BinaryClassificationEvaluator(labelCol="indexed")

        #Create the cross validator
        crossval = CrossValidator(estimator=rf,
                                  estimatorParamMaps=paramGrid,
                                  evaluator=evaluator,
                                  numFolds=fold)

        #run cross-validation and choose the best parameters
        cvModel = crossval.fit(tr_td)
        
        # Predict on training data
        prediction_tr = cvModel.transform(tr_td)
        #prediction_tr2 = prediction_tr.withColumn('label', prediction_tr[
        # 'label'].cast('double'))#convert label to double
        pred_score_tr = prediction_tr.select('label', 'indexed', 'probability')

        #predict on test data
        prediction_ts = cvModel.transform(ts_td)
        #prediction_ts2 = prediction_ts.withColumn('label', prediction_ts[
        # 'label'].cast('double'))#convert label to double
        pred_score_ts = prediction_ts.select('label', 'indexed', 'probability')

        # AUC
        AUC_tr = evaluator.evaluate(prediction_tr,{evaluator.metricName:'areaUnderROC'} )
        AUC_ts = evaluator.evaluate(prediction_ts,{evaluator.metricName:'areaUnderROC'} )

        #print out results            
        fAUC = open(resultDir_master + "AUC_fold" + str(iFold) + ".txt", "w")
        fAUC.write("Traing AUC = %s \n" % AUC_tr)
        fAUC.write("Test AUC = %s \n" % AUC_ts)
        fAUC.close()
            
        #output results to S3
        tr_lines = pred_score_tr.map(toCSVLine)
        tr_lines.saveAsTextFile((resultDir_s3 + app_name + "_pred_tr_fold" + str(iFold) + ".csv" ))

        ts_lines = pred_score_ts.map(toCSVLine)
        ts_lines.saveAsTextFile((resultDir_s3 + app_name + "_pred_ts_fold" + str(iFold) + ".csv"))


if __name__ == "__main__":

    sc = SparkContext(appName = app_name)

    sqlContext = SQLContext(sc)

    main(sc)

    sc.stop()
    
print "Program finished successfully. "

sc.