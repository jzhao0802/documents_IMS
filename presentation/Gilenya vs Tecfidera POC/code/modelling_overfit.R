#####################################################
# 1. SETUP DATA
#####################################################

#clear worksace
rm(list = ls(all = TRUE))

#set working directory 
setwd("C:/wherevever") 

#load the data
mydata <- read.csv("overfitting.csv", header=TRUE)
colnames(mydata)

#create train and test sets
trainset = mydata[mydata$train == 1,]
testset = mydata[mydata$train == 0,]

#eliminate unwanted columns from train set
trainset$case_id = NULL
trainset$train = NULL
trainset$Target_Evaluate = NULL
#trainset$Target_Practice = NULL
trainset$Target_Leaderboard = NULL


#####################################################
# 2. set the formula
#####################################################
theTarget <- "Target_Practice"
theFormula <- as.formula(paste("as.factor(",theTarget, ") ~ . "))
theFormula1 <- as.formula(paste(theTarget," ~ . "))
trainTarget = trainset[,which(names(trainset)==theTarget)] 
testTarget  = testset[,which(names(testset)==theTarget)]
library(caTools) #requireed for AUC calc
#####################################################

display_results <- function(){
  train_AUC <- colAUC(train_pred,trainTarget)
  test_AUC <- colAUC(test_pred,testTarget)
  cat("\n\n***",what,"***\ntraining:",train_AUC,"\ntesting:",test_AUC,"\n*****************************\n")
}

#####################################################
# 3. Now just apply the algorithms
#####################################################


#####################################################
# Logisic Regression
#####################################################
what <- "Logistic Regression"
LOGISTIC_model <- glm(theFormula, data=trainset, family=binomial(link="logit"))

train_pred <- predict(LOGISTIC_model, type="response", trainset)
test_pred <- predict(LOGISTIC_model, type="response", testset)

display_results()


#####################################################
# Linear Regression
#####################################################
what <- "Linear Regression"
LINEAR_model <- lm(theFormula1, data=trainset)

train_pred <- predict(LINEAR_model, type="response", trainset)
test_pred <- predict(LINEAR_model, type="response", testset)

display_results()


#####################################################
# Robust Fitting of Linear Models
#####################################################
library(MASS)
what <- "RLM"
RLM_model <- rlm(theFormula1, data=trainset)

train_pred <- predict(RLM_model, type="response", trainset)
test_pred <- predict(RLM_model, type="response", testset)

display_results()


#####################################################
# SVM
#####################################################
library('e1071')
what <- "SVM"
SVM_model <- svm(theFormula, data=trainset,type='C',kernel='linear',probability = TRUE)

outTrain <- predict(SVM_model, trainset, probability = TRUE)
outTest <- predict(SVM_model, testset, probability = TRUE)

train_pred <- attr(outTrain, "probabilities")[,2]
test_pred <- attr(outTest, "probabilities")[,2]

display_results()


#####################################################
# Tree
#####################################################
library(rpart)
what <- "TREE"
TREE_model <- rpart(theFormula, data=trainset, method="class")

train_pred <- predict(TREE_model, trainset)[,2]
test_pred <- predict(TREE_model, testset)[,2]

display_results()


#####################################################
# Random Forest
#####################################################
library(randomForest)
what <- "Random Forest"
FOREST_model <- randomForest(theFormula, data=trainset, ntree=50)

train_pred <- predict(FOREST_model, trainset, type="prob")[,2]
test_pred <- predict(FOREST_model, testset, type="prob")[,2]

display_results()


#####################################################
# Gradient Boosting Machine
#####################################################
library(gbm)
what <- "GBM"
GBM_model = gbm(theFormula1,data=trainset,n.trees=50,shrinkage=0.005 ,cv.folds=10)
best.iter <- gbm.perf(GBM_model,method="cv")

train_pred <- predict.gbm(GBM_model,trainset,best.iter)
test_pred <- predict.gbm(GBM_model,testset,best.iter)

display_results()


#####################################################
# Multivariate Adaptive Regression Splines
#####################################################

library(earth)
what <- "MARS (earth)"
EARTH_model <- earth(theFormula, data=trainset)

train_pred <- predict(EARTH_model, trainset)
test_pred <- predict(EARTH_model, testset)

display_results()

###########################################
# prepare the data
###########################################
mydata <- read.csv("overfitting.csv", header=TRUE)

trainset = mydata[mydata$train == 1,]
testset = mydata[mydata$train == 0,]

theTarget <- 'Target_Practice'
theFormula <- as.formula(paste(theTarget," ~ . "))

trainY <- trainset[[which(names(trainset)==theTarget)]]
testY <- testset[[which(names(testset)==theTarget)]]

trainset$case_id = NULL
trainset$train = NULL
trainset$Target_Evaluate = NULL
#trainset$Target_Practice = NULL
trainset$Target_Leaderboard = NULL

testset <- testset[,names(trainset)]



###########################################
# now iteratively build models, eliminating
# a variable at each iteration
###########################################

## the number of variables to remove
num <- ncol(trainset) - 2

## arrays for plot                  
trainAUC <- array(dim=num)
testAUC <- array(dim=num)
x <- array(dim=num)


######################################
# main loop for variable elimination
######################################
library(rminer)

#create 2 graphics windows
graphics.off()
windows(xpos=0)
windows()

for (i in 1:num){
  
  
  x[i] <- i
  
  #build the model
  Model=fit(theFormula,trainset,model="svm")
  
  #get train and test errors
  PredTrain=predict(Model,trainset)
  trainAUC[i]=mmetric(trainY,PredTrain,"AUC")
  PredTest=predict(Model,testset)
  testAUC[i]=mmetric(testY,PredTest,"AUC")
  
  #calculate the variable importance
  #VariableImportance=Importance(Model,trainset,method="data",RealL = 2,measure="variance")
  VariableImportance=Importance(Model,trainset,method="sensv")
  
  #plot the importance graph if required
  dev.set(dev.next())
  L=list(runs=1,sen=t(VariableImportance$imp),sresponses=VariableImportance$sresponses)
  mgraph(L,graph="IMP",leg=names(trainset),col="gray",Grid=10)
  
  #plot the graph of train and test error
  dev.set(dev.next())
  ymin = min(testAUC[1:i])
  ymax = max(testAUC[1:i])
  ymin = ymin - ((ymax - ymin)/4)
  plot(x[1:i],testAUC[1:i],type="b",col="blue",main = "eliminating variables", xlab = "Number of Variables Removed", ylab = "AUC",ylim = c(ymin,ymax) )
  legend('bottomright', c('test set'),lty=1, col=c("blue"))
  bringToTop(which = dev.cur(), stay = TRUE)
  
  #remove the worst variable
  Z <- order(VariableImportance$imp,decreasing = FALSE)
  IND <- Z[2] #seems the target will always be index 1
  var_to_remove <- names(trainset[IND])
  trainset[IND] = NULL
  testset[IND] = NULL
  
  #report
  cat("\ntrainAUC ",trainAUC[i])
  cat("\ntestAUC ",testAUC[i])
  cat("\nremoving variable ", var_to_remove)
  flush.console()
  
}



library(rgenoud)
library(snow) 
library(caret) 
setwd('C:\\Documents and Settings\\mike\\My Documents\\Overfitting') 
load('data.overfitting.Rdata') 
ga.fn.overfitting<-function(X,xga,yga) { 
  counter=1 s=NULL 
  #Generate the removal list.
  #anything greater than 0.5 gets picked up into the final model.
  for (i in 5:204) { 
    if (X[i]>=.5) s=c(s,counter) counter=counter+1 
  }
  trcontrol<-trainControl(method='boot',number=25,returnResamp = "final", returnData=FALSE, verboseIter = FALSE) 
  ans<-train(x=xga[,s],y=yga, trControl = trcontrol, method='svmRadial', metric='Accuracy', tuneGrid=expand.grid(.sigma=X[1],.epsilon=X[2],.C=X[3],.nu=X[4]))
  print(c(ans$results$Accuracy,(200-length(s)))) 
  
  return(c(ans$results$Accuracy,(200-length(s)))) }
  a3=genoud(ga.fn.overfitting, nvars=204, max=TRUE, pop.size=100, max.generations=1000, wait.generations=50, 
            hard.generation.limit=TRUE, starting.values=NULL, MemoryMatrix=FALSE, 
            Domains=cbind(c(0.000001,.01,0.00001,.001,rep(0,200)),c(1,1,1000,1,rep(1,200))), 
            solution.tolerance=0.001, gr=NULL, boundary.enforcement=2, lexical=TRUE, gradient.check=TRUE, 
            BFGS=FALSE, data.type.int=FALSE, hessian=FALSE, unif.seed=812821, int.seed=53058, print.level=1, 
            share.type=0, instance.number=0, output.path="stdout", output.append=FALSE, project.path=NULL, 
            P1=50, P2=50, P3=50, P4=50, P5=50, P6=50, P7=50, P8=50, P9=0, P9mix=NULL, BFGSburnin=0, BFGSfn=NULL,
            BFGShelp=NULL, control=list(), transform=FALSE, debug=FALSE, cluster=FALSE, balance=FALSE,
            xga=data.overfitting$train$x,yga=data.overfitting$train$y1) 

#####################################################
# 1. SETUP DATA

#####################################################
library(e1071)
data(iris)
attach(iris)
model.svm <- svm(Species ~. ,data=iris)

x <- subset(iris,select=-Species)
y <- Species

plot(cmdscale(dist(iris[,-5])),
     col = as.integer(iris[,5]),
     pch = c("o","+")[1:150 %in% model.svm$index + 1])



x_mtx <- matrix(0, nr=2, nc=3)
x_mtx <- rbind(x_mtx,c(1,2,3))
