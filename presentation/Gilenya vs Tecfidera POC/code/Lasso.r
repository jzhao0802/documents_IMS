#In addition here's some code (written in RMD format, the code is within triple backticks) to execute the lasso. 
The glmnet package has cross validation and feature scaling built in.


Ridge Regression and the Lasso
-------------------------------
Ridge Regression and Lasso methods incorporate an element to the fitting process that penalises the fit becoming too large i.e. incorporating too many coefficients and overfitting. 
Ridge uses the L2 norm, Lasso the L1 norm.

The glmnet package we use for this needs to be given a model matrix instead of a formula. Thankfully this can be produced by means of a formula.

```{r model matrix, echo=FALSE}
x <- model.matrix(f1win~., data=b07h.reg)[,-1]   # -1 removes intercept term
y <- as.numeric(as.character(b07h$f1win))   # convert factor to numeric
```

First we will fit a ridge-regression model. This is achieved by calling `glmnet` with `alpha=0` (see the helpfile). There is also a `cv.glmnet` function which will do the cross-validation for us. The feature scaling is done inside the function. 
Below I run a ridge regression, plot coefficient size versus log lambda (how the model size changes as we increase penalty), select the optimum lambda using 10-fold cross validation, select the model 1 standard error to the 'simpler side' of the best fit, print the coefficients for that model, and return the auc and 2SE bounds

```{r ridge regression, echo=FALSE}
fit.ridge <- glmnet(x,y,family="binomial", alpha=0)
plot(fit.ridge,xvar="lambda", label=TRUE)
cv.ridge=cv.glmnet(x,y,alpha=0,nfolds=10,type.measure="mse",family="gaussian")
plot(cv.ridge)
cv.ridge=cv.glmnet(x,y,alpha=0,nfolds=10,type.measure="auc",family="binomial")
plot(cv.ridge)
rows <- apply(coef(cv.ridge), 1, function(x) x!=0)
coef(cv.ridge)[rows,]

fit.index <- cv.ridge$lambda==cv.ridge$lambda.min
auc <- cv.ridge$cvm[fit.index]
up <- cv.ridge$cvup[fit.index]
lo <- cv.ridge$cvlo[fit.index]
sd <- cv.ridge$cvsd[fit.index]
data.frame(auc=auc, min.auc=auc-1.96*sd, max.auc=auc+1.96*sd)
save(fit.ridge, file=paste(data.root, "logistic_ridge.RData"))
```

Now we fit a lasso model; for this we use the default `alpha=1` and give the same outputs:

```{r lasso regression, echo=FALSE}
fit.lasso=glmnet(x, y, family="binomial", )
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y,alpha=1,nfolds=10,type.measure="mse",family="gaussian")
plot(cv.lasso)
cv.lasso=cv.glmnet(x,y,alpha=1,nfolds=10,type.measure="auc",family="binomial")
plot(cv.lasso)
rows <- apply((coef(cv.lasso)), 1, function(x) x!=0)
(coef(cv.lasso))[rows,]
fit.index <- cv.lasso$lambda==cv.lasso$lambda.min
auc <- cv.lasso$cvm[fit.index]
up <- cv.lasso$cvup[fit.index]
lo <- cv.lasso$cvlo[fit.index]
sd <- cv.lasso$cvsd[fit.index]
data.frame(auc=auc, min.auc=auc-1.96*sd, max.auc=auc+1.96*sd)
save(fit.lasso.interactions, file="logistic_lasso.RData")
```

Suppose we want to use our earlier train/validation division to select the `lambda` for the lasso. This is easy to do, we just plot how rmse changes with lambda on a training sample, and pick the best performing value
```{r train lasso with set, echo=FALSE}
lasso.tr=glmnet(x[train,],y[train])
pred=predict(lasso.tr,x[-train,])
rmse= sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]



-----------------------------------------------------------------------------------
#Here's some code for doing 10-fold CV on a logistic regression, 
then getting the output with RMSE and AUC:
 
k.folds <- 10
set.seed(1)
num.response <- as.numeric(as.character(b07h.reg$f1win))
folds <- sample(rep(1:k.folds, length=nrow(b07h.reg)))

cv.errors.mse <- rep(NA,k.folds)
cv.errors.auc <- rep(NA,k.folds)

t1<-Sys.time()
for(k in 1:k.folds){
    fit <- glm(f1win~., family=binomial, data=b07h.reg, subset=folds!=k)
    pred <- predict(fit, b07h.reg[folds==k,], type="response")
    cv.errors.mse[k] <- mean((num.response[folds==k]-pred)^2)
    cv.errors.auc[k] <- auc(num.response[folds==k], pred)
}
t2<-Sys.time()
t <- (t2-t1)
rmse<-sqrt(mean(cv.errors.mse))
rmse.se <- sqrt(var(cv.errors.mse)/k.folds)
data.frame(mean=rmse,lo=rmse-1.96*rmse.se,up=rmse+1.96*rmse.se)
auc<-mean(cv.errors.auc)
auc.se <- sqrt(var(cv.errors.auc)/k.folds)
data.frame(mean=auc,lo=auc-1.96*auc.se,up=auc+1.96*auc.se)
 
(the 1.96 figure is to find the 95% C.I.)
