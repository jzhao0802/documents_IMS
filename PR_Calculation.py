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
import os
import time
import datetime
from pyspark import SparkContext, SparkConf
from pyspark.sql import SQLContext
from pyspark.sql import functions as F

# Some constance
datapath = "s3://emr-rwes-pa-spark-dev-datastore/zy_test/"
inpath = "model_data/"
logfilepath = "./Spark_log_file/"
app_name = "PR"
posProb_name = "avg_prob"
response_name = "label"
filename = 'resp_pred.csv'

def PrecisionRecall_calculation(datapath, inpath, filename, response_name,\
                                posProb_name, output):  
    from pyspark.sql.functions import lit
    data1 = sqlContext.read.load(datapath + inpath + filename, 
                          format='com.databricks.spark.csv', 
                          header='true', 
                          inferSchema='true')
    data1_t1 = data1.withColumnRenamed(response_name,'label')\
                    .withColumnRenamed(posProb_name,'prob_1')\
                    
    data1_t1 = data1_t1.withColumn('prob', F.round(data1_t1.prob_1,3))\
                       .cache()
    
    pos = data1_t1.select('prob').distinct().sort('prob').collect()
    pos_cnt = len(pos)
    data1_t1.registerTempTable("data2")
    
    def confMarixCal(i):
        threshold = pos[i][0]
        sql = 'select a.*, case when a.prob_1 >='\
              + str(threshold) + ' then 1 else 0'\
              + ' end as pred_new from data2 a'                   
        temp = sqlContext.sql(sql)
        cs_tb = temp.stat.crosstab("label", "pred_new")
        if (cs_tb.columns == ['label_pred_new', '0']):
            cs_tb = cs_tb.withColumn('1', lit(0))
        elif (cs_tb.columns == ['label_pred_new', '1']):
            cs_tb = cs_tb.withColumn('0', lit(0))
        
        tp = cs_tb.select(["label_pred_new","1"])\
             .filter(cs_tb.label_pred_new == 1.0)\
             .select("1").take(1)[0][0]
        fp = cs_tb.select(["label_pred_new","1"])\
             .filter(cs_tb.label_pred_new == 0.0)\
             .select("1").take(1)[0][0]
        tn = cs_tb.select(["label_pred_new","0"])\
             .filter(cs_tb.label_pred_new == 0.0)\
             .select("0").take(1)[0][0]
        fn = cs_tb.select(["label_pred_new","0"])\
             .filter(cs_tb.label_pred_new == 1.0)\
             .select("0").take(1)[0][0]
        if ((tp + fp) == 0):
            precision = float(1.0)
        elif ((tp + fp) > 0):
            precision = float(tp)/float(tp + fp)
        recall = float(tp)/float(tp + fn)
        fpr = float(fp)/float(fp + tn)            
        re = (threshold, precision, recall, fpr)
        return(re)
    
    pr = [confMarixCal(id) for id in range(0, pos_cnt)]
    pr_final = sqlContext.createDataFrame(pr,\
               ['Threshold', 'Precision', 'Recall', 'FPR'])
    pr_final1 = sqlContext.createDataFrame(pr_final.rdd.coalesce(1))
    #output the PR results
    pr_final1.save(output + app_name + '_results',\
                   "com.databricks.spark.csv",\
                   header="true")

if __name__ == "__main__":
    conf = SparkConf()
    conf.setAppName(app_name)
    conf.set("spark.eventLog.enabled","true")
    if not os.path.exists(logfilepath + app_name +"_log"):
        os.makedirs(logfilepath + app_name +"_log")
    conf.set("spark.eventLog.dir",logfilepath + app_name +"_log")
    sc = SparkContext(conf = conf)
    sqlContext = SQLContext(sc)
    
    #Create the output folder
    start_time = time.time()
    st = datetime.datetime.fromtimestamp(start_time).strftime('%Y%m%d_%H%M%S')
    output = datapath + app_name + st + "/"
    if not os.path.exists(output):
        os.makedirs(output)

    #Calculate the precision, recall and FPR according to unique positive probability
    PrecisionRecall_calculation(datapath, inpath, filename, response_name, \
                                posProb_name, output)
    # change the files' mode to 777
    for dirpath, dirnames, filenames in os.walk(logfilepath + app_name +"_log"):
        for file in filenames:
            os.chmod(dirpath + "/" + file, 0o777)


