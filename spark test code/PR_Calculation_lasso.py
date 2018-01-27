# -*- coding: utf-8 -*-
"""
Created on Mon Jun 20 10:11:03 2016

@author: zywang
This function is used to calculate the precision, recall and FPR. 
    1. precision and recall is used for PR curve.
    2. recall and FPR is used for ROC curve.

The following are some parameters:
    datapath:     the overall folder path
    inpath:       the path of data
    filename:     file used to calculate precision, recall and fpr. 
    posProb_name: the name of positive probability
    response_name:the name of origin true label
    par:          number of partitions
    output:       output path
"""

import sys
import os
import time
import datetime
from pyspark import SparkContext, SparkConf
from pyspark.sql import SQLContext
from pyspark.sql import functions as F
from pyspark.sql import Row

def PreparePRComputation_OneDataPoint(thresholdLabelAndProb):
    # the predicted label
    pred = float(thresholdLabelAndProb.prob > thresholdLabelAndProb.threshold)
    
    prepared = Row(
        threshold = thresholdLabelAndProb.threshold,        
        # columns for computing tp, fp, tn, fn
        bTP = float((thresholdLabelAndProb.label == pred) & (pred == 1)),
        bFP = float((thresholdLabelAndProb.label != pred) & (pred == 1)),
        bTN = float((thresholdLabelAndProb.label == pred) & (pred == 0)),
        bFN = float((thresholdLabelAndProb.label != pred) & (pred == 0))
    )
    
    return prepared

def ComputePR(row):
    precision = round(row.nTPs / (row.nTPs + row.nFPs + 1e-9), 3)
    recall = round(row.nTPs / (row.nTPs + row.nFNs + 1e-9), 3)
    
    return Row(threshold=row.threshold, 
               precision=precision,
               recall=recall)

def PrecisionRecall_calculation(datapath, inpath, filename, response_name,\
                                posProb_name, output):  
    
    data = sqlContext.read.load(datapath + inpath + filename, 
                          format='com.databricks.spark.csv', 
                          header='true', 
                          inferSchema='true')\
        .withColumnRenamed(response_name,'label')\
        .withColumnRenamed(posProb_name,'prob_1')
                    
    labelsAndProbs = data.withColumn('prob', F.round(data.prob_1,3)).select(["label", "prob"])
    
    thresholds = labelsAndProbs.select('prob').distinct().withColumnRenamed("prob", "threshold")
    
    PRs = thresholds.rdd\
                    .cartesian(labelsAndProbs.rdd)\
                    .toDF()\
                    .map(lambda x: \
                         Row(threshold=x[0].threshold, 
                             label=x[1].label, 
                             prob=x[1].prob))\
                    .map(PreparePRComputation_OneDataPoint)\
                    .toDF()\
                    .groupBy("threshold")\
                    .agg(F.sum(F.col("bTP")), 
                         F.sum(F.col("bFP")),
                         F.sum(F.col("bTN")), 
                         F.sum(F.col("bFN")))\
                     .select(F.col("threshold"), 
                             F.col("sum(bTP)").alias("nTPs"), 
                             F.col("sum(bFP)").alias("nFPs"), 
                             F.col("sum(bTN)").alias("nTNs"), 
                             F.col("sum(bFN)").alias("nFNs"))\
                    .map(ComputePR)\
                    .toDF()\
                    .coalesce(1)\
                    .save(output,
                          "com.databricks.spark.csv",
                          header="true")


if __name__ == "__main__":

    # Some constance change here!!!!
    datapath =

    #"s3://emr-rwes-pa-spark-dev-datastore/Hui/shire_test/02_result/"

    inpath =

    #"lasso_db4_20160627_065857/"
    posProb_name = "Prob_1"
    response_name = "label"

    flag = sys.argv[1]
    app_name = sys.argv[2]

    #filename = 'pred_ts_sim0.csv/part-00000'
    filename = 'pred_score_ts'
    conf = SparkConf()
    conf.setAppName(app_name)
    #conf.set("spark.eventLog.enabled","true")
    #if not os.path.exists(logfilepath + app_name +"_log"):
    #    os.makedirs(logfilepath + app_name +"_log")
    #conf.set("spark.eventLog.dir",logfilepath + app_name +"_log")

    #set up the dynamic allocation
    conf.set("spark.dynamicAllocation.enabled", "true")
    conf.set("spark.shuffle.service.enabled", "true")
    conf.set("spark.dynamicAllocation.maxExecutors", "30")

    sc = SparkContext(conf = conf)
    sqlContext = SQLContext(sc)
    
    #Create the output folder
    start_time = time.time()
    st = datetime.datetime.fromtimestamp(start_time).strftime('%Y%m%d_%H%M%S')
    output = datapath + 'PR_curve_' + st + "/"
    if not os.path.exists(output):
        os.makedirs(output)

    #Calculate the precision, recall and FPR according to unique positive probability
    PrecisionRecall_calculation(datapath, inpath, filename, response_name, \
                                posProb_name, output)

    # change the files' mode to 777
    #for dirpath, dirnames, filenames in os.walk(logfilepath + app_name
    #                                                     +"_log"):
    #    for file in filenames:
    #        os.chmod(dirpath + "/" + file, 0o777)
