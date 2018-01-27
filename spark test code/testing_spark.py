__author__ = 'hjin'
__project__ = 'code'

# Created by hjin on 1/18/2016

import os
import sys

os.environ['SPARK_HOME']="C:\\Program_Files\\Spark"

sys.path.append("C:\\Program_Files\\bin")
sys.path.append("C:\\Program_Files\\python")
sys.path.append("C:\\Program_Files\\python\\pyspark\\")
sys.path.append("C:\\Program_Files\\python\\lib")
sys.path.append("C:\\Program_Files\\python\\lib\\pyspark.zip")
sys.path.append("C:\\Program_Files\\python\\lib\\py4j-0.9-src.zip")
sys.path.append("C:\\Program_Files\\Java\\jdk1.7.0_79\\bin")

from pyspark import SparkContext
sc = SparkContext('local', 'pyspark')

def isprime(n):
    """
    check if integer n is a prime
    """
    # make sure n is a positive integer
    n = abs(int(n))
    # 0 and 1 are not primes
    if n < 2:
        return False
    # 2 is the only even prime number
    if n == 2:
        return True
    # all other even numbers are not primes
    if not n & 1:
        return False
    # range starts with 3 and only needs to go up the square root of n
    # for all odd numbers
    for x in range(3, int(n**0.5)+1, 2):
        if n % x == 0:
            return False
    return True

# Create an RDD of numbers from 0 to 1,000,000
nums = sc.parallelize(range(1000000))

# Compute the number of primes in the RDD
print(nums.filter(isprime).count())

data = sc.textFile("C:/spark/spark-1.6.0-bin-hadoop2.6/README.MD")
counts = data.flatMap(lambda line: line.split()) \
        .map(lambda word: (word, 1)) \
        .reduceByKey(lambda a, b:a+b)
counts.collect()

data.cache()
data.count()
