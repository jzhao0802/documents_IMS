#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Part II: Model Estimation

#	Develop time: 08/26/2014 - .

#	Developer: Gui Yaming
#==========================================================================================

#------------------------------------------------------------------------------------------
#	Compile some functions and prepare evaluation data
#------------------------------------------------------------------------------------------
require(lattice)
require(Matrix)
require(glmnet)

model_auc<- function(obs , pred){
	ROC<- NULL
	for(risk_cutoff in sort(pred)){
		positive<- length(which(obs==1 & pred>=risk_cutoff))/length(which(obs==1))
		false_positive<- length(which(obs==0 & pred>=risk_cutoff))/length(which(obs==0))
		ROC<- rbind(ROC , c(risk_cutoff, positive , false_positive))
	}
	colnames(ROC)<- c('risk_cutoff' , 'positive_rate' , 'false_positive_rate')
	
	AUC<- 0
	for(i in 2:dim(ROC)[1]){
		AUC<- AUC + (ROC[i,2] + ROC[i-1,2])*(ROC[i-1,3] - ROC[i,3])/2
	}
	result<- list(roc=ROC , auc=AUC)
	return(result)
}


model_evaluate<- function(model, observe , simulate){
	f_obs_data<- observe
	f_sim_data<- simulate
	fit<- model

	pred <- predict(fit, f_obs_data, type="response")
	pred_simulate<- predict(fit, f_sim_data, type="response")
	expected_diff<- ifelse(f_obs_data$treatment==1, pred - pred_simulate , pred_simulate - pred)

	devide<- median(expected_diff)
	n1<- which(expected_diff<=devide)
	n2<- which(expected_diff>devide)
	index<- list(n1=n1, n2=n2)
	
	result<- lapply(index , function(x){
		Gilenya_pat<- length(which(f_obs_data$treatment[x]==1))
		Tecfidera_pat<- length(which(f_obs_data$treatment[x]==0))
		Gilenya_relapse<- length(which(f_obs_data$treatment[x]==1 & f_obs_data$post_relapse_persist[x]==1))
		Tecfidera_relapse<- length(which(f_obs_data$treatment[x]==0 & f_obs_data$post_relapse_persist[x]==1))
		Gilenya_relapse_rate<- Gilenya_relapse / Gilenya_pat
		Tecfidera_relapse_rate<- Tecfidera_relapse / Tecfidera_pat
		actual_treatment<- Gilenya_relapse_rate - Tecfidera_relapse_rate
		c(Gilenya_pat, Gilenya_relapse, Gilenya_relapse_rate, 
					Tecfidera_pat, Tecfidera_relapse, Tecfidera_relapse_rate, actual_treatment, 
					expect_Gilenya_relapse_rate, expect_Tecfidera_relapse_rate, expect_treatment)
	})
	result<- rbind(result$n1, result$n2, result$n3)
	result<- cbind(c('lowest 35%' , 'middle 30%', 'highest 35%') , result)
	colnames(result)<- c('Group', 'Gilenya_pat', 'Gilenya_relapse', 'Gilenya_relapse_rate',
		'Tecfidera_pat', 'Tecfidera_relapse', 'Tecfidera_relapse_rate', 'actual_treatment')
	return(result)
}

model_output<- function(model, observe , simulate){  #two values: training and test
	f_obs_data<- observe
	f_sim_data<- simulate
	fit<- model

	pred <- predict(fit, f_obs_data, type="response")
	pred_simulate<- predict(fit, f_sim_data, type="response")
	expected_diff<- ifelse(f_obs_data$treatment==1, pred - pred_simulate , pred_simulate - pred)

	devide<- quantile(expected_diff , prob=c(0.35 , 0.65))
	n1<- which(expected_diff<=devide[1])
	n2<- which(expected_diff>devide[1] & expected_diff<devide[2])
	n3<- which(expected_diff>=devide[2])
	index<- list(n1=n1, n2=n2, n3=n3)
	
	result<- lapply(index , function(x){
		Gilenya_pat<- length(which(f_obs_data$treatment[x]==1))
		Tecfidera_pat<- length(which(f_obs_data$treatment[x]==0))
		Gilenya_relapse<- length(which(f_obs_data$treatment[x]==1 & f_obs_data$post_relapse_persist[x]==1))
		Tecfidera_relapse<- length(which(f_obs_data$treatment[x]==0 & f_obs_data$post_relapse_persist[x]==1))
		Gilenya_relapse_rate<- Gilenya_relapse / Gilenya_pat
		Tecfidera_relapse_rate<- Tecfidera_relapse / Tecfidera_pat
		actual_treatment<- Gilenya_relapse_rate - Tecfidera_relapse_rate
		expect_Gilenya_relapse_rate<- mean(ifelse(f_obs_data$treatment[x]==1, pred[x], pred_simulate[x]))
		expect_Tecfidera_relapse_rate<- mean(ifelse(f_obs_data$treatment[x]==0, pred[x], pred_simulate[x]))
		expect_treatment<- mean(expected_diff[x])
		c(Gilenya_pat, Gilenya_relapse, Gilenya_relapse_rate, 
					Tecfidera_pat, Tecfidera_relapse, Tecfidera_relapse_rate, actual_treatment, 
					expect_Gilenya_relapse_rate, expect_Tecfidera_relapse_rate, expect_treatment)
	})
	result<- rbind(result$n1, result$n2, result$n3)
	result<- cbind(c('lowest 35%' , 'middle 30%', 'highest 35%') , result)
	colnames(result)<- c('Group', 'Gilenya_pat', 'Gilenya_relapse', 'Gilenya_relapse_rate',
		'Tecfidera_pat', 'Tecfidera_relapse', 'Tecfidera_relapse_rate', 'actual_treatment',
		'expect_Gilenya_relapse_rate', 'expect_Tecfidera_relapse_rate', 'expect_treatment')
	return(result)
}


simulate_data<- raw_data
simulate_data$treatment<- 1 - simulate_data$treatment
simulate_interact<- apply(raw_data[, match(total_covar_list , names(raw_data))] , 2 , function(x){x * simulate_data$treatment})
simulate_data[,match(paste(total_covar_list , '_interact' , sep='') , names(simulate_data))]<- simulate_interact

#QC
dim(simulate_data); dim(raw_data)
table(simulate_data$pchrlson_2 , simulate_data$treatment)
table(simulate_data$pchrlson_2_interact)

#------------------------------------------------------------------------------------------
#	(1A) Logistic regression without regularization
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------

#----------------------based on detail variables---------------------------
covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

training_data<- raw_data[which(raw_data$training==1) , match(model_variable, names(raw_data))]
test_data<- raw_data[which(raw_data$training==0) , match(model_variable, names(raw_data))]
training_simulate_data<- simulate_data[which(simulate_data$training==1) , match(model_variable, names(simulate_data))]
test_simulate_data<- simulate_data[which(simulate_data$training==0) , match(model_variable, names(simulate_data))]

#
fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
training_data_evaluate<- model_output(model=fit, observe=training_data , simulate=training_simulate_data)
test_data_evaluate<- model_output(model=fit, observe=test_data , simulate=test_simulate_data)
result<- rbind(cbind(rep('training' , 3) , training_data_evaluate) , cbind(rep('test' , 3) , test_data_evaluate))
write.csv(result , 'Model 1A DetailVar_results.csv', row.names=F, quote=F)

# AUC
training_auc<- model_auc(obs=training_data$post_relapse_persist , pred=predict(fit, training_data, type="response"))
test_auc<- model_auc(obs=test_data$post_relapse_persist , pred=predict(fit, test_data, type="response"))
c(training_auc$auc , test_auc$auc)

#QC
table(training_data$treatment , training_data$post_relapse_persist)

#----------------------based on summary variables---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

training_data<- raw_data[which(raw_data$training==1) , match(model_variable, names(raw_data))]
test_data<- raw_data[which(raw_data$training==0) , match(model_variable, names(raw_data))]
training_simulate_data<- simulate_data[which(simulate_data$training==1) , match(model_variable, names(simulate_data))]
test_simulate_data<- simulate_data[which(simulate_data$training==0) , match(model_variable, names(simulate_data))]

#
fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
training_data_evaluate<- model_output(model=fit, observe=training_data , simulate=training_simulate_data)
test_data_evaluate<- model_output(model=fit, observe=test_data , simulate=test_simulate_data)
result<- rbind(cbind(rep('training' , 3) , training_data_evaluate) , cbind(rep('test' , 3) , test_data_evaluate))
write.csv(result , 'Model 1A SummaryVar_results.csv', row.names=F, quote=F)

# AUC
training_auc<- model_auc(obs=training_data$post_relapse_persist , pred=predict(fit, training_data, type="response"))
test_auc<- model_auc(obs=test_data$post_relapse_persist , pred=predict(fit, test_data, type="response"))
c(training_auc$auc , test_auc$auc)

#QC
table(training_data$treatment , training_data$post_relapse_persist)



#------------------------------------------------------------------------------------------
#	(1B) Logistic regression step-wise variable selection, backward algorithm				
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------

#----------------------based on detail variables---------------------------
covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

training_data<- raw_data[which(raw_data$training==1) , match(model_variable, names(raw_data))]
test_data<- raw_data[which(raw_data$training==0) , match(model_variable, names(raw_data))]
training_simulate_data<- simulate_data[which(simulate_data$training==1) , match(model_variable, names(simulate_data))]
test_simulate_data<- simulate_data[which(simulate_data$training==0) , match(model_variable, names(simulate_data))]

fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
step_wise<- step(fit , direction="backward") # cost 15 hours
#save(step_wise , file='model_1B_stepwise_detailvar.Rdata')

# actual and expected treatment effects
training_data_evaluate<- model_output(model=step_wise, observe=training_data , simulate=training_simulate_data)
test_data_evaluate<- model_output(model=step_wise, observe=test_data , simulate=test_simulate_data)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 3) , test_data_evaluate))
write.csv(result , 'Model 1B DetailVar_results.csv', row.names=F, quote=F)

# AUC
training_auc<- model_auc(obs=training_data$post_relapse_persist , pred=predict(step_wise, training_data, type="response"))
test_auc<- model_auc(obs=test_data$post_relapse_persist , pred=predict(step_wise, test_data, type="response"))
c(training_auc$auc , test_auc$auc)
#0.7867035     0.6893917

# coefficient of each variable for the final model
coefficient<- summary(step_wise)$coefficient
write.csv(data.frame(coefficient , odds_ratio=exp(coefficient[,1])) , 'Model 1B DetailVar_coefficient.csv', row.names=T, quote=F)

#----------------------based on summary variables---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

training_data<- raw_data[which(raw_data$training==1) , match(model_variable, names(raw_data))]
test_data<- raw_data[which(raw_data$training==0) , match(model_variable, names(raw_data))]
training_simulate_data<- simulate_data[which(simulate_data$training==1) , match(model_variable, names(simulate_data))]
test_simulate_data<- simulate_data[which(simulate_data$training==0) , match(model_variable, names(simulate_data))]

fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
step_wise<- step(fit , direction="backward") # cost 5 hours
#save(step_wise , file='model_1B_stepwise_summaryvar.Rdata')

# actual and expected treatment effects
training_data_evaluate<- model_output(model=step_wise, observe=training_data , simulate=training_simulate_data)
test_data_evaluate<- model_output(model=step_wise, observe=test_data , simulate=test_simulate_data)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 3) , test_data_evaluate))
write.csv(result , 'Model 1B SummaryVar_results.csv', row.names=F, quote=F)

# AUC
training_auc<- model_auc(obs=training_data$post_relapse_persist , pred=predict(step_wise, training_data, type="response"))
test_auc<- model_auc(obs=test_data$post_relapse_persist , pred=predict(step_wise, test_data, type="response"))
c(training_auc$auc , test_auc$auc)
#0.7593492     0.7018702

# coefficient of each variable for the final model
coefficient<- summary(step_wise)$coefficient
write.csv(data.frame(coefficient , odds_ratio=exp(coefficient[,1])) , 'Model 1B SummaryVar_coefficient.csv', row.names=T, quote=F)



#------------------------------------------------------------------------------------------
#	(1C) Logistic regression lasso and 10 fold cross-validation
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------

#---------------------------------------based on detail variables---------------------------
covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
covar_list<- covar_list[sample(1:length(covar_list) , 20)]
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

model_data<- raw_data[, match(model_variable, names(raw_data))]
model_simulate_data<- simulate_data[, match(model_variable, names(simulate_data))]

k.folds <- 10
foldid<- nrow(model_data)
set.seed(1)
foldid[model_data$post_relapse_persist==1]<- sample(rep(1:k.folds, length=length(which(model_data$post_relapse_persist==1))))
set.seed(1)
foldid[model_data$post_relapse_persist==0]<- sample(rep(1:k.folds, length=length(which(model_data$post_relapse_persist==0))))

table(model_data$post_relapse_persist , foldid) # QC

model_matrix<- model.matrix(post_relapse_persist~., data=model_data)[,-1]
model_lambda<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, standardize=F, family="binomial", alpha=1)$lambda

cv_auc <- matrix(nr=k.folds , nc=length(model_lambda))
for(k in 1:k.folds){
	training_data<- model_data[foldid != k,]
	training_matrix <- model.matrix(post_relapse_persist~., data=training_data)[,-1]  # removes intercept term

	test_data<- model_data[foldid == k,]
	test_matrix <- model.matrix(post_relapse_persist~., data=test_data)[,-1]

	fit_lasso<- glmnet(training_matrix, training_data$post_relapse_persist, 
		lambda=model_lambda, family="binomial", alpha=1, standardize=F)
	test_pred<- predict(fit_lasso, test_matrix, type="response")
	
	cv_auc[k,]<- apply(test_pred, 2, function(x){ auc(test_data$post_relapse_persist , x)})
	# tailored model metrics ...
}

cv_auc_mean<- apply(cv_auc , 2 , mean)
lambda_optimum<- model_lambda[which.max(cv_auc_mean)]

log_rr<- test_matrix %*% fit_lasso$beta
p<- exp(log_rr)/(1+exp(log_rr))


#---------------------- test 10 fold CV Lasso---------------------------------------
covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
covar_list<- covar_list[sample(1:length(covar_list) , 40)]
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

model_data<- raw_data[, match(model_variable, names(raw_data))]
model_simulate_data<- simulate_data[, match(model_variable, names(simulate_data))]

k.folds <- 10
foldid<- nrow(model_data)
set.seed(1)
foldid[model_data$post_relapse_persist==1]<- sample(rep(1:k.folds, length=length(which(model_data$post_relapse_persist==1))))
set.seed(1)
foldid[model_data$post_relapse_persist==0]<- sample(rep(1:k.folds, length=length(which(model_data$post_relapse_persist==0))))

table(model_data$post_relapse_persist , foldid) # QC
model_matrix<- model.matrix(post_relapse_persist~., data=model_data)[,-1]


fit_lasso1<- cv.glmnet(x=model_matrix, y=model_data$post_relapse_persist, type.measure="auc",
	nfolds=10, foldid=foldid, standardize=F, family="binomial", alpha=1)
fit_lasso2<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, standardize=F, family="binomial", alpha=1)
fit_lasso3<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, lambda=fit_lasso1$lambda.min, standardize=F, family="binomial", alpha=1)

cbind(as.vector(coef(fit_lasso1, s = "lambda.min")) , coef(fit_lasso2)[,which.max(fit_lasso1$cvm)])
cbind(as.vector(coef(fit_lasso1, s = "lambda.min")) , as.vector(coef(fit_lasso3)))

fit_lasso1$cvm == fit_lasso2$cvm

fit_lasso1$lambda.min == fit_lasso2$lambda

which(fit_lasso1$lambda == fit_lasso1$lambda.min ); which.max(fit_lasso1$cvm)

#log_rr<- test_matrix %*% fit_lasso$beta + matrix(rep(coef(fit_lasso)[1,], nrow(test_matrix)), nrow=nrow(test_matrix), byrow=T)
#p<- exp(log_rr)/(1+exp(log_rr))