'''
This function could run LR LASSO with Spark-submit in Spark ML module.
RegParam is regularization parameter (>= 0).elasticNetParam should be 1.
'''

# Import the packages
import sys
import os
import time
import datetime
from pyspark import SparkConf, SparkContext
from pyspark.sql import SQLContext
from pyspark.sql.functions import *
from pyspark.mllib.linalg import Vectors
import numpy as np
#import pandas as pd
import random
from pyspark.sql.functions import *
from pyspark.sql.types import *

#from pyspark.ml import Pipeline
from pyspark.ml.classification import LogisticRegression
from pyspark.ml.feature import VectorAssembler
from pyspark.ml.evaluation import BinaryClassificationEvaluator
from pyspark.ml.tuning import CrossValidator, ParamGridBuilder

app_name = sys.argv[1]
s3_path = "s3://emr-rwes-pa-spark-dev-datastore"
seed = 42
par = 400

####### !!!!!!!Some variables, change before running!!!!!!
# Path variables
data_path = s3_path + "/Hui/shire_test/01_data/data_973"
s3_outpath = s3_path + "/Hui/shire_test/02_result/"
master_path = "/home/hjin/shire_test/03_results/"

# data file
pos_file = "/dat_hae.csv"
neg_file ="/dat_nonhae.csv"

# Test proportion
ts_prop = 0.2

# Number of simulation
num_sim = 1

# Cross Validation setting
fold = 3
CVCriteria = 'areaUnderPR'

# Grid-search setting
# For lambda
lambdastart = float(0.0001)
lambdastop = float(0.001)
lambdanum = int(2)
# For alpha
alphastart = float(0.9)
alphastop = float(1)
alphanum = int(2)
##!!!!!!!!Setting End!!!!!##########################################

# seed
random.seed(seed)
seed_seq = [random.randint(10, 100) for i in range(num_sim)]

# S3 output folder
start_time = time.time()
st = datetime.datetime.fromtimestamp(start_time).strftime('%Y%m%d_%H%M%S')
resultDir_s3 = s3_outpath + app_name + "_" + st + "/"

# master output folder
resultDir_master = master_path + app_name + '_' + st + "/"
if not os.path.exists(resultDir_master):
    os.makedirs(resultDir_master)
os.chmod(resultDir_master, 0o777)

#function 1: get the predicted probability in Vector
def getitem(i):
    def getitem_(v):
         return v.array.item(i)
    return udf(getitem_, DoubleType())

#function 2: check the parameter setting
#def parameter_ck():
#    global alphanum, lambdanum
#    errorCnt = 0
#    if (alphastart < 0 or alphastop > 1 or lambdastart < 0):
#        errorCnt += 1
#    if (alphastart == alphastop):
#        alphanum = int(1)
#    if (lambdastart == lambdastop):
#        lambdanum = int(1)
#    return(errorCnt)

# Define the main functions
def main(sc, isim, pos_ori, neg_ori, pos_col, ts_prop=ts_prop,
         resultDir_s3=resultDir_s3, resultDir_master=resultDir_master,
         lambdastart=lambdastart, lambdastop=lambdastop, lambdanum=lambdanum,
         alphastart=alphastart,alphastop=alphastop, alphanum=alphanum,
         CVCriteria=CVCriteria,fold=fold, par=par):

    #random split in positive cases and use HAE patient ID to select Non-HAE
    (pos_tr, pos_ts) = pos_ori.randomSplit([(1-ts_prop), ts_prop],
                                           seed=seed_seq[isim])

    #select corresponding negtive patients as training set
    neg_tr1 = neg_ori\
        .join(pos_tr, neg_ori.hae_patid == pos_tr.patid,'inner')\
        .select(neg_ori.patid, neg_ori.label, neg_ori.hae_patid,neg_ori.features)

    #test set is the rows in original negative cases but not in training set
    neg_ts = neg_ori.subtract(neg_tr1).drop('hae_patid')

    #remove hae_patid in negative trainning set
    neg_tr = neg_tr1.drop('hae_patid')

    #combine the training and test data
    tr = pos_tr.unionAll(neg_tr).repartition(par)
    ts = pos_ts.unionAll(neg_ts).repartition(par)

    # Build the model
    lr = LogisticRegression(featuresCol = "features", 
                            labelCol = "label",
                            fitIntercept = True)                         
                            #,standardization = False) This isn't usable in spark 1.5.2 but could be used in spark 1.6.1

    # Create the parameter grid builder
    paramGrid = ParamGridBuilder()\
        .addGrid(lr.regParam, list(np.linspace(lambdastart, lambdastop, lambdanum)))\
        .addGrid(lr.elasticNetParam, list(np.linspace(alphastart, alphastop, alphanum)))\
        .build()

    # Create the evaluator
    evaluator = BinaryClassificationEvaluator(metricName=CVCriteria)

    #Create the cross validator
    crossval = CrossValidator(estimator=lr,
                              estimatorParamMaps=paramGrid,
                              evaluator=evaluator,
                              numFolds=fold)

    #run cross-validation and choose the best parameters
    cvModel = crossval.fit(tr)
    
    #output the parameters to results folder
    head = pos_col
    intercept = cvModel.bestModel.intercept
    coef = cvModel.bestModel.weights
    coef_file = open(resultDir_master + 'Coef_sim' + str(isim) + '.txt', "w")
    coef_file.writelines("Intercept, %f" %intercept)
    coef_file.writelines("\n")
    for id in range(len(coef)):
        coef_file.writelines("%s , %f" %(head[id+2] ,coef[id]))
        coef_file.writelines("\n")
    coef_file.close()
    os.chmod(resultDir_master + 'Coef_sim' + str(isim) + '.txt', 0o777)

    # Predict on training data
    prediction_tr = cvModel.transform(tr)
    pred_score_tr = prediction_tr.select('patid', 'label', 'probability','prediction')

    #predict on test data
    prediction_ts = cvModel.transform(ts)
    pred_score_ts = prediction_ts.select('patid', 'label', 'probability','prediction')

    # AUC
    AUC_tr = evaluator.evaluate(prediction_tr,{evaluator.metricName:'areaUnderROC'})
    AUC_ts = evaluator.evaluate(prediction_ts,{evaluator.metricName:'areaUnderROC'})

    AUPR_tr = evaluator.evaluate(prediction_tr,
                                 {evaluator.metricName:'areaUnderPR'})
    AUPR_ts = evaluator.evaluate(prediction_ts,
                                 {evaluator.metricName:'areaUnderPR'})


    #print out AUC results
    auc = "Training data AUC = %s " % AUC_tr + "\n" + \
          "Test data AUC = %s " % AUC_ts + "\n"
    aupr = "Training data AUPR = %s " % AUPR_tr + "\n" + \
          "Test data AUPR = %s " % AUPR_ts
    auc_file = open(resultDir_master + 'AUC_AUPR_sim' + str(isim) + '.txt', "w")
    auc_file.writelines(auc)
    auc_file.writelines(aupr)
    auc_file.close()
    os.chmod(resultDir_master + 'AUC_AUPR_sim' + str(isim) + '.txt', 0o777)

    #Identify the probility of response
    pred_score_tr1 = pred_score_tr.select(pred_score_tr.patid,
                                         pred_score_tr.label,
                                         pred_score_tr.prediction,
                                         getitem(0)('probability').alias('p1'),
                                         getitem(1)('probability').alias('p2'))

    pred_score_ts1 = pred_score_ts.select(pred_score_ts.patid,
                                         pred_score_ts.label,
                                         pred_score_ts.prediction,
                                         getitem(0)('probability').alias('p1'),
                                         getitem(1)('probability').alias('p2'))

    firstone = pred_score_tr1.take(1)
    firstonezip = zip(*firstone)
    p1 = firstonezip[3]
    p2 = firstonezip[4]
    pred = firstonezip[2]

    if ((p1 > p2 and pred == 1.0) or (p1 < p2 and pred == 0.0)):
        pred_score_tr1 = pred_score_tr1.withColumnRenamed('p1','Prob_1')
        pred_score_tr1 = pred_score_tr1.withColumnRenamed('p2','Prob_0')
        pred_score_ts1 = pred_score_ts1.withColumnRenamed('p1','Prob_1')
        pred_score_ts1 = pred_score_ts1.withColumnRenamed('p2','Prob_0')
    else:
        pred_score_tr1 = pred_score_tr1.withColumnRenamed('p1','Prob_0')
        pred_score_tr1 = pred_score_tr1.withColumnRenamed('p2','Prob_1')
        pred_score_ts1 = pred_score_ts1.withColumnRenamed('p1','Prob_0')
        pred_score_ts1 = pred_score_ts1.withColumnRenamed('p2','Prob_1')

    return [pred_score_tr1, pred_score_ts1]
    
if __name__ == "__main__":

    #check the parameter setting
    #ErrCnt = parameter_ck()

    #if (ErrCnt == 0):
    sc = SparkContext(appName = app_name)
    sqlContext = SQLContext(sc)

    #reading in the data from S3
    pos = sqlContext.read.load((data_path + pos_file),
                              format='com.databricks.spark.csv',
                              header='true',
                              inferSchema='true')

    neg = sqlContext.read.load((data_path + neg_file),
                              format='com.databricks.spark.csv',
                              header='true',
                              inferSchema='true')
    #see the column names
    pos_col = pos.columns
    neg_col = neg.columns

    #combine features
    assembler_pos = VectorAssembler(inputCols=pos_col[2:],outputCol="features")
    assembler_neg = VectorAssembler(inputCols=neg_col[2:-1],outputCol="features")

    #get the input positive and negative dataframe
    pos_asmbl = assembler_pos.transform(pos)\
        .select('PATIENT_ID', 'HAE','features')\
        .withColumnRenamed('PATIENT_ID', 'patid')\
        .withColumnRenamed('HAE', 'label')

    pos_ori = pos_asmbl.withColumn('label', pos_asmbl['label'].cast('double'))

    neg_asmbl = assembler_neg.transform(neg)\
        .select('PATIENT_ID', 'HAE', 'HAE_PATIENT_ID', 'features')\
        .withColumnRenamed('PATIENT_ID', 'patid')\
        .withColumnRenamed('HAE', 'label')\
        .withColumnRenamed('HAE_PATIENT_ID', 'hae_patid')

    neg_ori = neg_asmbl.withColumn('label', neg_asmbl['label'].cast('double'))

        #call main function
    pred_ls = [main(sc, isim, pos_ori=pos_ori, neg_ori=neg_ori,
                        pos_col=pos_col) for isim in range(num_sim)]


    for isim in range(num_sim):

        pred_score_tr2 = pred_ls[isim][0]
        pred_score_ts2 = pred_ls[isim][1]

        #output results to S3
        pred_score_tr2.coalesce(1).write.format("com.databricks.spark.csv")\
            .save((resultDir_s3 +"pred_tr_sim" + str(isim) + ".csv"),
                  header="true")

        pred_score_ts2.coalesce(1).write.format("com.databricks.spark.csv")\
            .save((resultDir_s3 +"pred_ts_sim" + str(isim) + ".csv"),
                      header="true")


    sc.stop()

    #else:
     #   print('!!!Errors in parameters! alpha should be in range [0,1],
    # lambda should be >= 0')
    



