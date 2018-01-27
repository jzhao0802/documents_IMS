#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Part II: Model Estimation

#	Develop time: 08/26/2014 - .

#	Developer: Gui Yaming
#==========================================================================================

#------------------------------------------------------------------------------------------
#	Compile some functions and prepare evaluation data
#------------------------------------------------------------------------------------------

library(glmnet) # function: auc, glmnet

library(uplift)
model_evaluate<- function(observe, simulate, treatment, response){
	expected_diff<- ifelse(treatment==1, observe - simulate , simulate - observe)
	devide<- median(expected_diff)
	n1<- which(expected_diff<=devide[1])
	n2<- which(expected_diff>devide[1])
	index<- list(n1=n1, n2=n2)
	
	result<- lapply(index , function(x){
		Gilenya_pat<- length(which(treatment[x]==1))
		Tecfidera_pat<- length(which(treatment[x]!=1))
		Gilenya_relapse<- length(which(treatment[x]==1 & response[x]==1))
		Tecfidera_relapse<- length(which(treatment[x]!=1 & response[x]==1))
		Gilenya_relapse_rate<- Gilenya_relapse / Gilenya_pat
		Tecfidera_relapse_rate<- Tecfidera_relapse / Tecfidera_pat
		actual_treatment<- Gilenya_relapse_rate - Tecfidera_relapse_rate
		c(Gilenya_pat, Gilenya_relapse_rate, Tecfidera_pat, Tecfidera_relapse_rate, actual_treatment)
	})
	evaluation<- result$n1[5] - result$n2[5]
	return(evaluation)
}

model_output<- function(group, observe, simulate, treatment, response){  # set group parameter as training and test
	expected_diff<- ifelse(treatment==1, observe - simulate , simulate - observe)
	index<- list()
	if(group=='training'){
		devide<- quantile(expected_diff , prob=c(0.35 , 0.65))
		n1<- which(expected_diff<=devide[1])
		n2<- which(expected_diff>devide[1] & expected_diff<devide[2])
		n3<- which(expected_diff>=devide[2])
		index<- list(n1=n1, n2=n2, n3=n3)
	}
	if(group=='test'){
		devide<- median(expected_diff)
		n1<- which(expected_diff<=devide[1])
		n2<- which(expected_diff>devide[1])
		index<- list(n1=n1, n2=n2)
	}

	result<- lapply(index , function(x){
		Gilenya_pat<- length(which(treatment[x]==1))
		Tecfidera_pat<- length(which(treatment[x]!=1))
		Gilenya_relapse<- length(which(treatment[x]==1 & response[x]==1))
		Tecfidera_relapse<- length(which(treatment[x]!=1 & response[x]==1))
		Gilenya_relapse_rate<- Gilenya_relapse / Gilenya_pat
		Tecfidera_relapse_rate<- Tecfidera_relapse / Tecfidera_pat
		actual_treatment<- Gilenya_relapse_rate - Tecfidera_relapse_rate
		expect_Gilenya_relapse_rate<- mean(ifelse(treatment[x]==1, observe[x], simulate[x]))
		expect_Tecfidera_relapse_rate<- mean(ifelse(treatment[x]!=1, observe[x], simulate[x]))
		expect_treatment<- mean(expected_diff[x])
		c(Gilenya_pat, Gilenya_relapse, Gilenya_relapse_rate, 
					Tecfidera_pat, Tecfidera_relapse, Tecfidera_relapse_rate, actual_treatment, 
					expect_Gilenya_relapse_rate, expect_Tecfidera_relapse_rate, expect_treatment)
	})
	if(group=='training'){
		result<- rbind(result$n1, result$n2, result$n3)
		result<- cbind(c('lowest 35%', 'middle 30%', 'highest 35%') , result)
	}
	if(group=='test'){
		result<- rbind(result$n1, result$n2)
		result<- cbind(c('lowest 50%' , 'highest 50%') , result)
	}
	colnames(result)<- c('Group', 'Gilenya_pat', 'Gilenya_relapse', 'Gilenya_relapse_rate',
		'Tecfidera_pat', 'Tecfidera_relapse', 'Tecfidera_relapse_rate', 'actual_treatment',
		'expect_Gilenya_relapse_rate', 'expect_Tecfidera_relapse_rate', 'expect_treatment')
	return(result)
}


summary_treatment_effect<- function(observe, simulate, treatment, response){
	expected_diff<- ifelse(treatment==1, observe - simulate , simulate - observe)
	index<- list()
	devide<- median(expected_diff)
	n1<- which(expected_diff<=devide[1])
	n2<- which(expected_diff>devide[1])
	index<- list(n1=n1, n2=n2)
	
	result<- lapply(index , function(x){
		Gilenya_pat<- length(which(treatment[x]==1))
		Tecfidera_pat<- length(which(treatment[x]!=1))
		Gilenya_relapse<- length(which(treatment[x]==1 & response[x]==1))
		Tecfidera_relapse<- length(which(treatment[x]!=1 & response[x]==1))
		Gilenya_relapse_rate<- Gilenya_relapse / Gilenya_pat
		Tecfidera_relapse_rate<- Tecfidera_relapse / Tecfidera_pat
		actual_treatment<- Gilenya_relapse_rate - Tecfidera_relapse_rate
		expect_Gilenya_relapse_rate<- mean(ifelse(treatment[x]==1, observe[x], simulate[x]))
		expect_Tecfidera_relapse_rate<- mean(ifelse(treatment[x]!=1, observe[x], simulate[x]))
		expect_treatment<- mean(expected_diff[x])
		c(Gilenya_relapse_rate, Tecfidera_relapse_rate, actual_treatment, 
					expect_Gilenya_relapse_rate, expect_Tecfidera_relapse_rate, expect_treatment)
	})
	#summary_actual_treatment_effect = result$n1[3] - result$n2[3]
	#summary_expect_treatment_effect = result$n1[4] - result$n2[4]
	#result<- data.frame(summary_actual_treatment_effect , summary_expect_treatment_effect)
	result<- rbind(result$n1, result$n2)
	colnames(result)<- c('Gilenya_relapse_rate', 'Tecfidera_relapse_rate', 'actual_treatment',
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

#----------------------based on detail variables (231 predictors)---------------------------
covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

training_data<- raw_data[which(raw_data$training==1) , match(model_variable, names(raw_data))]
test_data<- raw_data[which(raw_data$training==0) , match(model_variable, names(raw_data))]
training_simulate_data<- simulate_data[which(simulate_data$training==1) , match(model_variable, names(simulate_data))]
test_simulate_data<- simulate_data[which(simulate_data$training==0) , match(model_variable, names(simulate_data))]

# model output: actual and expected treatment effects
fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
coef<- data.frame(coefficient=coef(fit) , odds_ratio=exp(coef(fit)))
p_value<- summary(fit)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 1A DetailVar_coefficient.csv', row.names=T, quote=F)

training_obs<- predict(fit, training_data, type="response")
training_sim<- predict(fit, training_simulate_data, type="response")
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
test_obs<- predict(fit, test_data, type="response")
test_sim<- predict(fit, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 1A DetailVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$post_relapse_persist , training_obs)
test_auc<- auc(test_data$post_relapse_persist , test_obs)
c(training_auc , test_auc)
# 0.8108393	0.6635532

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

#QC
table(training_data$treatment , training_data$post_relapse_persist)

#----------------------based on summary variables (171 predictors)---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

training_data<- raw_data[which(raw_data$training==1) , match(model_variable, names(raw_data))]
test_data<- raw_data[which(raw_data$training==0) , match(model_variable, names(raw_data))]
training_simulate_data<- simulate_data[which(simulate_data$training==1) , match(model_variable, names(simulate_data))]
test_simulate_data<- simulate_data[which(simulate_data$training==0) , match(model_variable, names(simulate_data))]

# model output: actual and expected treatment effects
fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
coef<- data.frame(coefficient=coef(fit) , odds_ratio=exp(coef(fit)))
p_value<- summary(fit)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 1A SummaryVar_coefficient.csv', row.names=T, quote=F)

training_obs<- predict(fit, training_data, type="response")
training_sim<- predict(fit, training_simulate_data, type="response")
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
test_obs<- predict(fit, test_data, type="response")
test_sim<- predict(fit, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 1A SummaryVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$post_relapse_persist , training_obs)
test_auc<- auc(test_data$post_relapse_persist , test_obs)
c(training_auc , test_auc)
# 0.7815993 0.6610974

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

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
coef<- data.frame(coefficient=coef(step_wise) , odds_ratio=exp(coef(step_wise)))
p_value<- summary(step_wise)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 1B DetailVar_coefficient.csv', row.names=T, quote=F)

training_obs<- predict(step_wise, training_data, type="response")
training_sim<- predict(step_wise, training_simulate_data, type="response")
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
test_obs<- predict(step_wise, test_data, type="response")
test_sim<- predict(step_wise, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 1B DetailVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$post_relapse_persist , training_obs)
test_auc<- auc(test_data$post_relapse_persist , test_obs)
c(training_auc , test_auc)
# 0.7867035 0.6893917

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)


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

coef<- data.frame(coefficient=coef(step_wise) , odds_ratio=exp(coef(step_wise)))
p_value<- summary(step_wise)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 1B DetailVar_coefficient.csv', row.names=T, quote=F)

training_obs<- predict(step_wise, training_data, type="response")
training_sim<- predict(step_wise, training_simulate_data, type="response")
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
test_obs<- predict(step_wise, test_data, type="response")
test_sim<- predict(step_wise, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 1B SummaryVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$post_relapse_persist , training_obs)
test_auc<- auc(test_data$post_relapse_persist , test_obs)
c(training_auc , test_auc)
#

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)


#------------------------------------------------------------------------------------------
#	(1C) Logistic regression lasso regularization and 10 fold cross-validation
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------
training_data<- raw_data[raw_data$training==1,]
training_simulate_data<- simulate_data[simulate_data$training==1,]
test_data<- raw_data[raw_data$training==0,]
test_simulate_data<- simulate_data[simulate_data$training==0,]

k.folds <- 10
foldid<- nrow(training_data)
set.seed(1)
foldid[training_data$post_relapse_persist==1]<- sample(rep(1:k.folds, length=length(which(training_data$post_relapse_persist==1))))
set.seed(1)
foldid[training_data$post_relapse_persist==0]<- sample(rep(1:k.folds, length=length(which(training_data$post_relapse_persist==0))))

table(training_data$post_relapse_persist , foldid) # QC
#   foldid
#      1   2   3   4   5   6   7   8   9  10
#  0 233 233 233 233 233 233 232 232 232 232
#  1  33  33  33  33  33  33  33  33  33  32


#---------------------------based on detail variables, AUC or treatment effect evaluation---------------------------
covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

model_data<- training_data[, match(model_variable, names(training_data))]
model_matrix<- model.matrix(post_relapse_persist~., data=model_data)[,-1]
model_simulate_data<- training_simulate_data[, match(model_variable, names(training_data))]
model_simulate_matrix<- model.matrix(post_relapse_persist~., data=model_simulate_data)[,-1]

initial_lambda<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, family="binomial", alpha=1, standardize=F)$lambda
lambda_seq<- c(initial_lambda[-1] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500))
cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
cv_treatment_effect<- matrix(nr=k.folds , nc=length(lambda_seq))
for(k in 1:k.folds){
	cv_training_data<- model_data[foldid!=k,]
	cv_training_matrix<- model.matrix(post_relapse_persist~., data=cv_training_data)[,-1] # removes intercept term
	cv_test_data<- model_data[foldid==k,]
	cv_test_matrix<- model.matrix(post_relapse_persist~., data=cv_test_data)[,-1]
	cv_test_simulate_data<- model_simulate_data[foldid==k,]
	cv_test_simulate_matrix<- model.matrix(post_relapse_persist~., data=cv_test_simulate_data)[,-1]

	fit_lasso<- glmnet(cv_training_matrix, cv_training_data$post_relapse_persist, 
		lambda=lambda_seq, family="binomial", alpha=1, standardize=F)

	test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
	cv_auc[k,]<- apply(test_pred, 2, function(x){auc(cv_test_data$post_relapse_persist , x)})

	# tailored model metrics ...
	test_simulate_pred<- predict(fit_lasso, cv_test_simulate_matrix, type="response")
	for(i in 1:length(lambda_seq)){
		cv_treatment_effect[k,i]<- model_evaluate(observe=test_pred[,i], simulate=test_simulate_pred[,i], treatment=cv_test_data$treatment, response=cv_test_data$post_relapse_persist)
	}
}
cv_auc_mean<- apply(cv_auc , 2 , mean)
cv_auc_sd<- apply(cv_auc , 2 , sd)
cv_treatment_effect_mean<- apply(cv_treatment_effect , 2 , function(x) mean(x[!is.na(x)]))

optimum_model<- which.max(cv_auc_mean)
total_model<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
write.csv(coef(total_model)[,optimum_model] , 'Model 1C DetailVar_coefficient.csv', row.names=T, quote=F)

# report: actual and expected treatment effects
optimum_model<- which.max(cv_treatment_effect_mean)
training_obs<- predict(total_model, model_matrix, type="response")[,optimum_model]
training_sim<- predict(total_model, model_simulate_matrix, type="response")[,optimum_model]
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)

test_matrix<- model.matrix(post_relapse_persist~., data=test_data[,match(model_variable, names(test_data))])[,-1]
test_simulate_matrix<- model.matrix(post_relapse_persist~., data=test_simulate_data[,match(model_variable, names(test_simulate_data))])[,-1]
test_obs<- predict(total_model, test_matrix, type="response")[,optimum_model]
test_sim<- predict(total_model, test_simulate_matrix, type="response")[,optimum_model]
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

result<- rbind(cbind(rep('training' , 3) , training_data_evaluate) , cbind(rep('test' , 2) , test_data_evaluate))
write.csv(result , 'Model 1C DetailVar_results.csv', row.names=F, quote=F)

c(auc(training_data$post_relapse_persist , training_obs) , auc(test_data$post_relapse_persist , test_obs))
# 0.7497785 0.7407225
# 0.7605442 0.7408589

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)


#computation of Qini measure for test data, 

perf <- performance(test_obs, test_sim, test_data$post_relapse_persist, test_data$treatment, direction = 1,groups=10)
Q <- qini(perf, plotit = TRUE)


#---------------------------based on summary variables, AUC or treatment effect evaluation---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)

model_data<- training_data[, match(model_variable, names(training_data))]
model_matrix<- model.matrix(post_relapse_persist~., data=model_data)[,-1]
model_simulate_data<- training_simulate_data[, match(model_variable, names(training_data))]
model_simulate_matrix<- model.matrix(post_relapse_persist~., data=model_simulate_data)[,-1]

initial_lambda<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, family="binomial", alpha=1, standardize=F)$lambda
lambda_seq<- c(initial_lambda[-1] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500))
cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
cv_treatment_effect<- matrix(nr=k.folds , nc=length(lambda_seq))
for(k in 1:k.folds){
	cv_training_data<- model_data[foldid!=k,]
	training_matrix<- model.matrix(post_relapse_persist~., data=cv_training_data)[,-1] # removes intercept term
	cv_test_data<- model_data[foldid==k,]
	test_matrix<- model.matrix(post_relapse_persist~., data=cv_test_data)[,-1]

	fit_lasso<- glmnet(training_matrix, cv_training_data$post_relapse_persist, 
		lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
	test_pred<- predict(fit_lasso, test_matrix, type="response")

	cv_auc[k,]<- apply(test_pred, 2, function(x){auc(cv_test_data$post_relapse_persist , x)})

	# tailored model metrics ...
	cv_test_simulate_data<- model_simulate_data[foldid==k,]
	test_simulate_matrix<- model.matrix(post_relapse_persist~., data=cv_test_simulate_data)[,-1]
	test_simulate_pred<- predict(fit_lasso, test_simulate_matrix, type="response")
	for(i in 1:length(lambda_seq)){
		cv_treatment_effect[k,i]<- model_evaluate(observe=test_pred[,i], simulate=test_simulate_pred[,i], treatment=cv_test_data$treatment, response=cv_test_data$post_relapse_persist)
	}
}
total_model<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)]))
cv_treatment_effect_mean<- apply(cv_treatment_effect , 2 , mean)

optimum_model<- which.max(cv_auc_mean)
optimum_lambda<- lambda_seq[optimum_model] # lambda=0.00280098

write.csv(coef(total_model)[,optimum_model] , 'Model 1C SummaryVar_coefficient.csv', row.names=T, quote=F)

# report: actual and expected treatment effects
optimum_model<- which.max(cv_treatment_effect_mean)	
training_obs<- predict(total_model, model_matrix, type="response")[,optimum_model]
training_sim<- predict(total_model, model_simulate_matrix, type="response")[,optimum_model]
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)

test_matrix<- model.matrix(post_relapse_persist~., data=test_data[,match(model_variable, names(test_data))])[,-1]
test_simulate_matrix<- model.matrix(post_relapse_persist~., data=test_simulate_data[,match(model_variable, names(test_simulate_data))])[,-1]
test_obs<- predict(total_model, test_matrix, type="response")[,optimum_model]
test_sim<- predict(total_model, test_simulate_matrix, type="response")[,optimum_model]
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

result<- rbind(cbind(rep('training' , 3) , training_data_evaluate) , cbind(rep('test' , 2) , test_data_evaluate))
write.csv(result , 'Model 1C SummaryVar_results.csv', row.names=F, quote=F)

c(auc(training_data$post_relapse_persist , training_obs) , auc(test_data$post_relapse_persist , test_obs))
#0.7399582 0.7146530
#0.7576818 0.7101297

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)



#------------------------------------------------------------------------------------------
#  (1D) Uplift Random Forest
#------------------------------------------------------------------------------------------
#----------------------based on detail variables---------------------------

simulate_data<- raw_data
simulate_data$treatment<- 1 - simulate_data$treatment
#simulate_interact<- apply(raw_data[, match(total_covar_list , names(raw_data))] , 2 , function(x){x * simulate_data$treatment})
#simulate_data[,match(paste(total_covar_list , '_interact' , sep='') , names(simulate_data))]<- simulate_interact


training_data<- raw_data[raw_data$training==1,]
training_simulate_data<- simulate_data[simulate_data$training==1,]
test_data<- raw_data[raw_data$training==0,]
test_simulate_data<- simulate_data[simulate_data$training==0,]

covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
#interact_var<- paste(covar_list , '_interact' , sep='')
#model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list, interact_var) , constant_covar)
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list) , constant_covar)

model_data<- training_data[, match(model_variable, names(training_data))]
model_simulate_data<- training_simulate_data[, match(model_variable, names(training_data))]

delete <- c("post_relapse_persist","treatment","persist_days")
x_training_data <- model_data[,!(colnames(model_data) %in% delete),drop=FALSE]
t_training_treatment <- training_data$treatment
y_training <- training_data$post_relapse_persist

x_training_sim_data <- model_data[,!(colnames(model_simulate_data) %in% delete),drop=FALSE] # ERROR!!!
t_training_sim_treatment <- training_simulate_data$treatment
y_training_sim <- training_simulate_data$post_relapse_persist

set.seed(12345)
numtree=100

fit_RF<- upliftRF(x=x_training_data,y=y_training,ct=t_training_treatment,ntree=numtree,split_method= "KL",verbose=TRUE)
pred_RF <- predict(fit_RF,x_training_data)
uplift_pred_RF <- pred_RF[, 1] - pred_RF[, 2]
perf <- performance(pred_RF[, 1], pred_RF[, 2], y_training, t_training_treatment, direction = 1,groups=10)
Q <- qini(perf, plotit = TRUE)

pred_sim_RF <- predict(fit_RF,x_training_sim_data)
uplift_pred_sim_RF <- pred_sim_RF[, 1] - pred_sim_RF[, 2]
perf <- performance(pred_sim_RF[, 1], pred_sim_RF[, 2], y_training_sim, t_training_sim_treatment, direction = 1,groups=10)
Q <- qini(perf, plotit = TRUE)

# report: actual and expected treatment effects

training_obs<- pred_RF[, 1]
training_sim<- pred_sim_RF[, 1]
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
#

x_test_data <-  test_data[,!(colnames(test_data) %in% delete),drop=FALSE]
t_test_treatment <- test_data$treatment
y_test <- test_data$post_relapse_persist

x_test_sim <- test_simulate_data[,!(colnames(test_data) %in% delete),drop=FALSE]
t_test_treatment_sim <- test_simulate_data$treatment
y_test_sim <- test_simulate_data$post_relapse_persist

pred_test_RF <- predict(fit_RF,x_test_data)
uplift_pred_test_RF <- pred_test_RF[, 1] - pred_test_RF[, 2]
perf <- performance(pred_test_RF[, 1], pred_test_RF[, 2], y_test, t_test_treatment, direction = 1,groups=10)
Q <- qini(perf, plotit = TRUE)

pred_test_sim_RF <- predict(fit_RF,x_test_sim)
uplift_pred_test_sim_RF <- pred_test_sim_RF[, 1] - pred_test_sim_RF[, 2]
perf <- performance(pred_test_sim_RF[, 1], pred_test_sim_RF[, 2], y_test_sim, t_test_treatment_sim, direction = 1,groups=10)
Q <- qini(perf, plotit = TRUE)

test_obs<- pred_test_RF[, 1]
test_sim<- pred_test_sim_RF[, 1]

test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

#result<- rbind(cbind(rep('training' , 3) , training_data_evaluate) , cbind(rep('test' , 2) , test_data_evaluate))
#write.csv(result , 'Model 1C DetailVar_results.csv', row.names=F, quote=F)

result<- rbind(cbind(rep('training' , 3) , training_data_evaluate) , cbind(rep('test' , 2) , test_data_evaluate))
write.csv(result , 'Model 1C SummaryVar_results.csv', row.names=F, quote=F)

c(auc(training_data$post_relapse_persist , training_obs) , auc(test_data$post_relapse_persist , test_obs))
