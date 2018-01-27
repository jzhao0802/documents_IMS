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
training_data[1:5,]
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

summary_treatment_effect<- function(observe, simulate, treatment, response, direction=1){
	if(direction==1)
		expected_diff<- ifelse(treatment==1, observe - simulate , simulate - observe)
	if(direction==2)
		expected_diff<- ifelse(treatment==1, simulate - observe , observe - simulate)
	index<- list()
	devide<- median(expected_diff)
	n1<- which(expected_diff>=devide[1])
	n2<- which(expected_diff<devide[1])
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
	summary_actual_treatment_effect = result$n1[3] - result$n2[3]
	summary_expect_treatment_effect = result$n1[6] - result$n2[6]
	result<- rbind(result$n1, result$n2)
	result<- cbind(result , c(summary_actual_treatment_effect, summary_actual_treatment_effect))
	result<- cbind(result , c(summary_expect_treatment_effect, summary_expect_treatment_effect))
	colnames(result)<- c('Gilenya_relapse_rate', 'Tecfidera_relapse_rate', 'actual_treatment',
		'expect_Gilenya_relapse_rate', 'expect_Tecfidera_relapse_rate', 'expect_treatment',
		'summary_actual_treatment_effect', 'summary_expect_treatment_effect')
	return(result)
}

# create simulate data which code Gilenya as 0 while Tecfidera as 1, and re-create two-ways interaction terms
simulate_data<- raw_data
simulate_data$treatment<- 1 - simulate_data$treatment
simulate_interact<- apply(raw_data[, match(total_covar_list , names(raw_data))] , 2 , function(x){x * simulate_data$treatment})
simulate_data[,match(paste(total_covar_list , '_interact' , sep='') , names(simulate_data))]<- simulate_interact
length(setdiff(names(simulate_data),c(paste(total_covar_list,'_interact',sep=''),total_covar_list))) 
#130+130+30=290

#QC
dim(simulate_data); dim(raw_data);dim(simulate_interact)
table(simulate_data$pchrlson_2 , simulate_data$treatment)
table(simulate_data$pchrlson_2_interact)


#------------------------------------------------------------------------------------------
#	(1A) Logistic regression without regularization
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------
#----------------------based on summary variables (171 predictors)---------------------------
raw_data$response<- raw_data$post_relapse_persist

covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
covar_list
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('response', 'treatment', covar_list, interact_var) , constant_covar)

training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]
training_simulate_data<- simulate_data[simulate_data$training==1 , match(model_variable, names(simulate_data))]
test_simulate_data<- simulate_data[simulate_data$training==0 , match(model_variable, names(simulate_data))]

# model output: actual and expected treatment effects
fit<- glm(response~., data=training_data, family=binomial)
coef<- data.frame(coefficient=coef(fit) , odds_ratio=exp(coef(fit)))
p_value<- summary(fit)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 1A SummaryVar_coefficient.csv', row.names=T, quote=F)

summary(fit)
training_obs<- predict(fit, training_data, type="response")
length(training_obs)
names(training_obs)
training_sim<- predict(fit, training_simulate_data, type="response")
training_obs_sim <- cbind(training_obs,training_sim)
training_obs_sim[1:10,]
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)
test_obs<- predict(fit, test_data, type="response")
test_sim<- predict(fit, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 1A SummaryVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$response , training_obs)
test_auc<- auc(test_data$response , test_obs)
c(training_auc , test_auc)
# 0.7786064 0.6926569

#roc plot
roc_training_obs <- roc(training_data[,-1],training_data$response,1)
plot(roc(test_obs,test_data$response,1))
summary(roc_training_obs)
data(Daim.data3)
Daim.data3
M <- roc(Daim.data3[,2:5], Daim.data3$Gold, "pos")
  summary(M)
  plot(M,color=c("black","blue","green3","red"))

  roc.area(M)


# summary treatment effect, actual & expect
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)

# computation of Qini measure for test data: Prob(y=1|treated, x), Prob(y=1|control, x)
prob_treatment<- ifelse(training_data$treatment==1, training_obs, training_sim)
prob_control<- ifelse(training_data$treatment!=1, training_obs, training_sim)
perf <- performance(prob_treatment, prob_control, training_data$response, training_data$treatment, direction = 2, groups=10)
qini_training <- qini(perf, direction=2, plotit = F)

prob_treatment<- ifelse(test_data$treatment==1, test_obs, test_sim)
prob_control<- ifelse(test_data$treatment!=1, test_obs, test_sim)
perf <- performance(prob_treatment, prob_control, test_data$response, test_data$treatment, direction = 2, groups=10)
qini_test <- qini(perf, direction=2, plotit = F)
c(qini_training$Qini , qini_test$Qini)
#0.02581728 0.01253947

#QC
table(training_data$treatment , training_data$response)


#------------------------------------------------------------------------------------------
#	(1B) Logistic regression step-wise variable selection, backward algorithm				
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------

#----------------------based on summary variables---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('response', 'treatment', covar_list, interact_var) , constant_covar)

training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]
training_simulate_data<- simulate_data[simulate_data$training==1 , match(model_variable, names(simulate_data))]
test_simulate_data<- simulate_data[simulate_data$training==0 , match(model_variable, names(simulate_data))]

fit<- glm(response~., data=training_data, family=binomial)
step_wise<- step(fit , direction="backward") # cost 5 hours
#save(step_wise , file='model_1B_stepwise_summaryvar.Rdata')

coef<- data.frame(coefficient=coef(step_wise) , odds_ratio=exp(coef(step_wise)))
p_value<- summary(step_wise)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 1B DetailVar_coefficient.csv', row.names=T, quote=F)

training_obs<- predict(step_wise, training_data, type="response")
training_sim<- predict(step_wise, training_simulate_data, type="response")
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)
test_obs<- predict(step_wise, test_data, type="response")
test_sim<- predict(step_wise, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 1B SummaryVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$response , training_obs)
test_auc<- auc(test_data$response , test_obs)
c(training_auc , test_auc)
# 0.7591524 0.6932940

# computation of Qini measure for test data: Prob(y=1|treated, x), Prob(y=1|control, x)
prob_treatment<- ifelse(training_data$treatment==1, training_obs, training_sim)
prob_control<- ifelse(training_data$treatment!=1, training_obs, training_sim)
perf <- performance(prob_treatment, prob_control, training_data$response, training_data$treatment, direction = 2, groups=10)
qini_training <- qini(perf, direction=2, plotit = F)

prob_treatment<- ifelse(test_data$treatment==1, test_obs, test_sim)
prob_control<- ifelse(test_data$treatment!=1, test_obs, test_sim)
perf <- performance(prob_treatment, prob_control, test_data$response, test_data$treatment, direction = 2, groups=10)
qini_test <- qini(perf, direction=2, plotit = F)
c(qini_training$Qini , qini_test$Qini)
# 0.017453439  0.005966905

# summary treatment effect, actual & expect
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)


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
set.se ed(1)
foldid[training_data$response==1]<- sample(rep(1:k.folds, length=length(which(training_data$response==1))))
set.seed(1)
foldid[training_data$response==0]<- sample(rep(1:k.folds, length=length(which(training_data$response==0))))

table(training_data$response , foldid) # QC

#---------------------------based on summary variables, AUC or treatment effect evaluation---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('response', 'treatment', covar_list, interact_var) , constant_covar)

model_data<- training_data[, match(model_variable, names(training_data))]
model_matrix<- model.matrix(response~., data=model_data)[,-1]
model_simulate_data<- training_simulate_data[, match(model_variable, names(training_data))]
model_simulate_matrix<- model.matrix(response~., data=model_simulate_data)[,-1]

initial_lambda<- glmnet(x=model_matrix, y=model_data$response, family="binomial", alpha=1, standardize=F)$lambda
lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500))
cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
cv_treatment_effect<- matrix(nr=k.folds , nc=length(lambda_seq))
for(k in 1:k.folds){
	cv_training_data<- model_data[foldid!=k,]
	training_matrix<- model.matrix(response~., data=cv_training_data)[,-1] # removes intercept term
	cv_test_data<- model_data[foldid==k,]
	test_matrix<- model.matrix(response~., data=cv_test_data)[,-1]

	fit_lasso<- glmnet(training_matrix, cv_training_data$response, 
		lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
	test_pred<- predict(fit_lasso, test_matrix, type="response")

	cv_auc[k,]<- apply(test_pred, 2, function(x){auc(cv_test_data$response , x)})

	# tailored model metrics ...
	cv_test_simulate_data<- model_simulate_data[foldid==k,]
	test_simulate_matrix<- model.matrix(response~., data=cv_test_simulate_data)[,-1]
	test_simulate_pred<- predict(fit_lasso, test_simulate_matrix, type="response")
	for(i in 1:length(lambda_seq)){
		cv_treatment_effect[k,i]<- model_evaluate(observe=test_pred[,i], simulate=test_simulate_pred[,i], treatment=cv_test_data$treatment, response=cv_test_data$response)
	}
}
total_model<- glmnet(x=model_matrix, y=model_data$response, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)]))
cv_treatment_effect_mean<- apply(cv_treatment_effect , 2 , mean)
cbind(lambda_seq, cv_auc_mean, cv_treatment_effect_mean)

optimum_model<- which.max(cv_auc_mean)
write.csv(coef(total_model)[,optimum_model] , 'Model 1C SummaryVar_coefficient_2.csv', row.names=T, quote=F)

# report: actual and expected treatment effects
optimum_model<- which.max(cv_treatment_effect_mean)	
training_obs<- predict(total_model, model_matrix, type="response")[,optimum_model]
training_sim<- predict(total_model, model_simulate_matrix, type="response")[,optimum_model]
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)

test_matrix<- model.matrix(response~., data=test_data[,match(model_variable, names(test_data))])[,-1]
test_simulate_matrix<- model.matrix(response~., data=test_simulate_data[,match(model_variable, names(test_simulate_data))])[,-1]
test_obs<- predict(total_model, test_matrix, type="response")[,optimum_model]
test_sim<- predict(total_model, test_simulate_matrix, type="response")[,optimum_model]
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)

result<- rbind(cbind(rep('training' , 3) , training_data_evaluate) , cbind(rep('test' , 2) , test_data_evaluate))
write.csv(result , 'Model 1C SummaryVar_results_2.csv', row.names=F, quote=F)

c(auc(training_data$response , training_obs) , auc(test_data$response , test_obs))
#0.7253715 0.7096109
#0.7747662 0.7012385

# computation of Qini measure for test data: Prob(y=1|treated, x), Prob(y=1|control, x)
prob_treatment<- ifelse(training_data$treatment==1, training_obs, training_sim)
prob_control<- ifelse(training_data$treatment!=1, training_obs, training_sim)
perf <- performance(prob_treatment, prob_control, training_data$response, training_data$treatment, direction = 2, groups=10)
qini_training <- qini(perf, direction=2, plotit = F)

prob_treatment<- ifelse(test_data$treatment==1, test_obs, test_sim)
prob_control<- ifelse(test_data$treatment!=1, test_obs, test_sim)
perf <- performance(prob_treatment, prob_control, test_data$response, test_data$treatment, direction = 2, groups=10)
qini_test <- qini(perf, direction=2, plotit = F)
c(qini_training$Qini , qini_test$Qini)
# 0.0003932802 -0.0134683312
# 0.021934899 0.009548136

# summary treatment effect, actual & expect
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)


#------------------------------------------------------------------------------------------
#  (1D) Uplift Random Forest
#------------------------------------------------------------------------------------------
#---------------------------------based on summary variables-------------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)

training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]

delete<- c("response", "treatment")
x_training_data<- training_data[, !(colnames(training_data) %in% delete), drop=FALSE]
x_test_data<- test_data[, !(colnames(test_data) %in% delete), drop=FALSE]

set.seed(12345)
fit_RF<- upliftRF(x=x_training_data,
				y=training_data$response,
				ct=training_data$treatment,
				ntree=500,
				split_method= "KL",
				verbose=TRUE)
summary(fit_RF)
print(fit_RF)
varImportance(fit_RF,plotit=TRUE)
save(fit_RF , file='model_1D_uplift_RF_KL.Rdata')

# Qini metric				
pred_training_RF <- predict(fit_RF , x_training_data)
pred_training_RF[1:2,]
dim(pred_training_RF)
training_perf <- performance(pred_training_RF[,1], pred_training_RF[,2], training_data$response, training_data$treatment, direction=2)
training_perf
plot(perf[, 8] ~ perf[, 1], type ="l", xlab = "Decile", ylab = "uplift")
qini_training <- qini(training_perf, direction=2, plotit=F)
qini_training
pred_test_RF <- predict(fit_RF, x_test_data)
test_perf <- performance(pred_test_RF[,1], pred_test_RF[,2], test_data$response, test_data$treatment, direction=2)
qini_test <- qini(test_perf, direction=2, plotit=F)
c(qini_training$Qini , qini_test$Qini)
# 0.070479062 0.003865751

# Treatment effect
training_obs<- ifelse(training_data$treatment==1, pred_training_RF[,1], pred_training_RF[,2])
training_sim<- ifelse(training_data$treatment!=1, pred_training_RF[,1], pred_training_RF[,2])
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)
test_obs<- ifelse(test_data$treatment==1, pred_test_RF[,1], pred_test_RF[,2])
test_sim<- ifelse(test_data$treatment!=1, pred_test_RF[,1], pred_test_RF[,2])
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)

# AUC
AUC<- c(auc(training_data$response , training_obs) , auc(test_data$response , test_obs))
# 0.878864005	0.709859828


#------------------------------------------------------------------------------------------
#  (1E) Uplift causal conditional inference
#------------------------------------------------------------------------------------------
#-------------------------------based on summary variables---------------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)

training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]

delete<- c("response", "treatment")
x_training_data<- training_data[, !(colnames(training_data) %in% delete), drop=FALSE]
x_test_data<- test_data[, !(colnames(test_data) %in% delete), drop=FALSE]

set.seed(12345)
fit_ccif<- ccif(x=x_training_data,
				y=training_data$response,
				ct=training_data$treatment,
				ntree=500,
				split_method= "KL",
				pvalue=0.05,
				verbose=TRUE)
save(fit_ccif , file='model_1E_uplift_CCIF_KL.Rdata')
pred_training_ccif <- predict(fit_ccif , x_training_data)
training_perf <- performance(pred_training_ccif[,1], pred_training_ccif[,2], training_data$response, training_data$treatment, direction=2)
qini_training <- qini(training_perf, direction=2, plotit=F)

pred_test_ccif <- predict(fit_ccif, x_test_data)
test_perf <- performance(pred_test_ccif[,1], pred_test_ccif[,2], test_data$response, test_data$treatment, direction=2)
qini_test <- qini(test_perf, direction=2, plotit=F)
c(qini_training$Qini , qini_test$Qini)
# 0.030636180 -0.009135034

# Treatment effect
training_obs<- ifelse(training_data$treatment==1, pred_training_ccif[,1], pred_training_ccif[,2])
training_sim<- ifelse(training_data$treatment!=1, pred_training_ccif[,1], pred_training_ccif[,2])
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)
test_obs<- ifelse(test_data$treatment==1, pred_test_ccif[,1], pred_test_ccif[,2])
test_sim<- ifelse(test_data$treatment!=1, pred_test_ccif[,1], pred_test_ccif[,2])
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)

# AUC
c(auc(training_data$response , training_obs) , auc(test_data$response , test_obs))
# 0.7712051 0.6594855


#------------------------------------------------------------------------------------------
#  (1G) variable selection and Uplift Random Forest
#------------------------------------------------------------------------------------------
#-------------------------------based on summary variables---------------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)

# variable selection based on prediction power, select the top 25% covariates to estimate model
set.seed(10)
covar_NIV<- niv(response ~ trt(treatment) + ., data=raw_data[, match(model_variable, names(raw_data))], B=10, direction=2)
is.vector(covar_NIV$niv_val[,3])
covar_NIV$niv_val[order(covar_NIV$niv_val[,3],decreasing=T),]
variable_selection<- covar_NIV$niv_val[order(covar_NIV$niv_val[,3], decreasing=T),]
percent_variable<- 0.15
final_covar_list<- rownames(variable_selection)[1:round(percent_variable*nrow(variable_selection))]

# estimate uplift RF model
training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]
x_training_data<- training_data[, match(final_covar_list, names(training_data))]
x_test_data<- test_data[, match(final_covar_list, names(test_data))]
x_training_data1<- raw_data[raw_data$training==1 , match(final_covar_list, names(raw_data))]
test <- cbind(x_training_data,x_training_data1)
test1 <- x_training_data1-x_training_data
test1[!which(test1[,]==0),]
x_test_data<- test_data[, match(final_covar_list, names(test_data))]

set.seed(12345)
fit_RF<- upliftRF(x=x_training_data,
				y=training_data$response,
				ct=training_data$treatment,
				ntree=500,
				split_method= "KL",
				verbose=TRUE)
save(fit_RF , file='model_1G_uplift_RF_KL_top15.Rdata')

# Qini metric				
pred_training_RF <- predict(fit_RF , x_training_data)
training_perf <- performance(pred_training_RF[,1], pred_training_RF[,2], training_data$response, training_data$treatment, direction=2)
qini_training <- qini(training_perf, direction=2, plotit=F)

pred_test_RF <- predict(fit_RF, x_test_data)
test_perf <- performance(pred_test_RF[,1], pred_test_RF[,2], test_data$response, test_data$treatment, direction=2)
qini_test <- qini(test_perf, direction=2, plotit=F)
c(qini_training$Qini , qini_test$Qini)
#(top15) 0.035050934 0.008187464
#(top25) 0.04382333 0.01772108
#(top50) 0.05972683 0.01280858

# Treatment effect
training_obs<- ifelse(training_data$treatment==1, pred_training_RF[,1], pred_training_RF[,2])
training_sim<- ifelse(training_data$treatment!=1, pred_training_RF[,1], pred_training_RF[,2])
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)
test_obs<- ifelse(test_data$treatment==1, pred_test_RF[,1], pred_test_RF[,2])
test_sim<- ifelse(test_data$treatment!=1, pred_test_RF[,1], pred_test_RF[,2])
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)

# AUC
c(auc(training_data$response , training_obs) , auc(test_data$response , test_obs))
#(top15) 0.7887821 0.7060867
#(top25) 0.8148013 0.7162412
#(top50) 0.8552794 0.7104372


#------------------------------------------------------------------------------------------
#  (1H) variable selection and Uplift Causal Conditional inference forest
#------------------------------------------------------------------------------------------
#----------------------based on summary variables---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)

# variable selection based on prediction power, select the top 25% covariates to estimate model
set.seed(10)
covar_NIV<- niv(response ~ trt(treatment) + ., data=raw_data[, match(model_variable, names(raw_data))], B=10, direction=2)
variable_selection<- covar_NIV$niv_val[order(covar_NIV$niv_val[,3], decreasing=T),]
percent_variable<- 0.5
final_covar_list<- rownames(variable_selection)[1:round(percent_variable*nrow(variable_selection))]

# estimate uplift causal conditional inference forest model
training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]
x_training_data<- training_data[, match(final_covar_list, names(training_data))]
x_test_data<- test_data[, match(final_covar_list, names(test_data))]

set.seed(12345)
fit_ccif<- ccif(x=x_training_data,
				y=training_data$response,
				ct=training_data$treatment,
				ntree=500,
				split_method= "KL",
				pvalue=0.05,
				verbose=TRUE)
save(fit_ccif , file='model_1H_uplift_CCIF_KL_top50.Rdata')

# Qini metric
pred_training_ccif <- predict(fit_ccif , x_training_data)
training_perf <- performance(pred_training_ccif[,1], pred_training_ccif[,2], training_data$response, training_data$treatment, direction=2)
qini_training <- qini(training_perf, direction=2, plotit=F)

pred_test_ccif <- predict(fit_ccif, x_test_data)
test_perf <- performance(pred_test_ccif[,1], pred_test_ccif[,2], test_data$response, test_data$treatment, direction=2)
qini_test <- qini(test_perf, direction=2, plotit=F)
c(qini_training$Qini , qini_test$Qini)
#(top15%) 0.008851382 0.011105735
#(top25%) 0.01410616 0.01084643
#(top50%) 0.024180343 -0.006901108

# Treatment effect
training_obs<- ifelse(training_data$treatment==1, pred_training_ccif[,1], pred_training_ccif[,2])
training_sim<- ifelse(training_data$treatment!=1, pred_training_ccif[,1], pred_training_ccif[,2])
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$response)
test_obs<- ifelse(test_data$treatment==1, pred_test_ccif[,1], pred_test_ccif[,2])
test_sim<- ifelse(test_data$treatment!=1, pred_test_ccif[,1], pred_test_ccif[,2])
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response)

# AUC
c(auc(training_data$response , training_obs) , auc(test_data$response , test_obs))
#(top15%) 0.6789127 0.6251991
#(top25%) 0.7192569 0.6653791
#(top50%) 0.7631441 0.6672109


#------------------------------------------------------------------------------------------
#  (1I) modelling persistent (take it as response variable):
#	tenfold cross-validation
#	variable selection based on NIV
#	Uplift Random Forest
#------------------------------------------------------------------------------------------
#----------------------based on summary variables---------------------------
raw_data$response<- raw_data$persistent

covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persist_days', 'persistent'))
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)
model_data<- raw_data[, match(model_variable, names(raw_data))]

# split the full data set into k folds with the same proportion of persistent
k.folds<- 10
foldid<- nrow(raw_data)
set.seed(1)
foldid[raw_data$response==1]<- sample(rep(1:k.folds, length=length(which(raw_data$response==1))))
set.seed(1)
foldid[raw_data$response==0]<- sample(rep(1:k.folds, length=length(which(raw_data$response==0))))
table(raw_data$response , foldid) # QC

cv_auc<- matrix(nr=k.folds , nc=2)
cv_qini<- matrix(nr=k.folds , nc=2)
cv_covar<- list()
cv_model<- list()

for(k in 1:k.folds){
	cv_training_data<- model_data[foldid!=k,]
	cv_test_data<- model_data[foldid==k,]
	
	# select variable based on 9 folds data
	set.seed(k)
	covar_NIV<- niv(response ~ trt(treatment) + ., data=cv_training_data, B=10, direction=2)
	variable_selection<- covar_NIV$niv_val[order(covar_NIV$niv_val[,3], decreasing=T),]
	percent_variable<- 0.25
	final_covar_list<- rownames(variable_selection)[1:round(percent_variable*nrow(variable_selection))]
	eval(parse(text = paste('cv_covar$k', k, '<- final_covar_list' , sep='')))

	x_training_data<- cv_training_data[, match(final_covar_list , names(cv_training_data))]
	x_test_data<- cv_test_data[, match(final_covar_list , names(cv_test_data))]

	set.seed(k)
	fit_RF<- upliftRF(x=x_training_data,
					y=cv_training_data$response,
					ct=cv_training_data$treatment,
					ntree=500,
					split_method= "KL",
					verbose=TRUE)
	eval(parse(text = paste('cv_model$k', k, '<- fit_RF' , sep='')))

	# Qini metric
	pred_training <- predict(fit_RF , x_training_data)
	training_perf <- performance(pred_training[,1], pred_training[,2], cv_training_data$response, cv_training_data$treatment, direction=1)
	qini_training <- qini(training_perf, direction=2, plotit=F)

	pred_test <- predict(fit_RF, x_test_data)
	test_perf <- performance(pred_test[,1], pred_test[,2], cv_test_data$response, cv_test_data$treatment, direction=2)
	qini_test <- qini(test_perf, direction=2, plotit=F)
	cv_qini[k,]<- c(qini_training$Qini , qini_test$Qini)

	# AUC
	training_obs<- ifelse(cv_training_data$treatment==1, pred_training[,1], pred_training[,2])
	test_obs<- ifelse(cv_test_data$treatment==1, pred_test[,1], pred_test[,2])
	cv_auc[k,]<- c(auc(cv_training_data$response , training_obs) , auc(cv_test_data$response , test_obs))
}
save(cv_model , file='model_1I_CV_uplift_RF_top25_model.Rdata')
save(cv_covar , file='model_1I_CV_uplift_RF_top25_covar.Rdata')
save(cv_qini , file='model_1I_CV_uplift_RF_top25_qini.Rdata')
save(cv_auc , file='model_1I_CV_uplift_RF_top25_auc.Rdata')

# calculate summary effect
load('model_1I_CV_uplift_RF_top25_model.Rdata')
load('model_1I_CV_uplift_RF_top25_covar.Rdata')
load('model_1I_CV_uplift_RF_top25_qini.Rdata')
load('model_1I_CV_uplift_RF_top25_auc.Rdata')
cv_summary_effect<- 0
for(k in 1:k.folds){
	eval(parse(text = paste('fit_RF<- cv_model$k', k, sep='')))
	eval(parse(text = paste('final_covar_list<- cv_covar$k', k , sep='')))
	cv_training_data<- model_data[foldid!=k,]
	cv_test_data<- model_data[foldid==k,]
	
	x_training_data<- cv_training_data[, match(final_covar_list , names(cv_training_data))]
	x_test_data<- cv_test_data[, match(final_covar_list , names(cv_test_data))]
	
	# Qini
	pred_training <- predict(fit_RF , x_training_data)
	pred_test <- predict(fit_RF, x_test_data)
	
	# summary effect
	training_obs<- ifelse(cv_training_data$treatment==1, pred_training[,1], pred_training[,2])
	training_sim<- ifelse(cv_training_data$treatment!=1, pred_training[,1], pred_training[,2])
	training_summary<- summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=cv_training_data$treatment, response=cv_training_data$response)
	test_obs<- ifelse(cv_test_data$treatment==1, pred_test[,1], pred_test[,2])
	test_sim<- ifelse(cv_test_data$treatment!=1, pred_test[,1], pred_test[,2])
	test_summary<- summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=cv_test_data$treatment, response=cv_test_data$response)
	cv_summary_effect<- cv_summary_effect + rbind(training_summary , test_summary)
}
cv_summary_effect<- cv_summary_effect/10
avg_auc<- c(mean(cv_auc[,1]), mean(cv_auc[,1]), mean(cv_auc[,2]), mean(cv_auc[,2]))
avg_qini<- c(mean(cv_qini[,1]), mean(cv_qini[,1]), mean(cv_qini[,2]), mean(cv_qini[,2]))
cbind(cv_summary_effect , avg_auc , avg_qini)


#------------------------------------------------------------------------------------------
#  (1J) modelling relapse,
#	Causal Conditional Inference Forest,
#	variable selection based on NIV
#	tenfold cross-validation
#------------------------------------------------------------------------------------------
#----------------------based on summary variables---------------------------
k.folds<- 10
foldid<- nrow(raw_data)
set.seed(1)
foldid[raw_data$response==1]<- sample(rep(1:k.folds, length=length(which(raw_data$response==1))))
set.seed(1)
foldid[raw_data$response==0]<- sample(rep(1:k.folds, length=length(which(raw_data$response==0))))

table(raw_data$response , foldid) # QC

covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)
model_data<- raw_data[, match(model_variable, names(raw_data))]

cv_auc<- matrix(nr=k.folds , nc=2)
cv_qini<- matrix(nr=k.folds , nc=2)
cv_covar<- list()
cv_model<- list()
for(k in 1:k.folds){
	cv_training_data<- model_data[foldid!=k,]
	cv_test_data<- model_data[foldid==k,]
	
	# select variable based on 9 folds data
	set.seed(10)
	covar_NIV<- niv(response ~ trt(treatment) + ., data=cv_training_data, B=10, direction=2)
	variable_selection<- covar_NIV$niv_val[order(covar_NIV$niv_val[,3], decreasing=T),]
	percent_variable<- 0.25
	final_covar_list<- rownames(variable_selection)[1:round(percent_variable*nrow(variable_selection))]
	eval(parse(text = paste('cv_covar$k', k, '<- final_covar_list' , sep='')))

	x_training_data<- cv_training_data[, match(final_covar_list , names(cv_training_data))]
	x_test_data<- cv_test_data[, match(final_covar_list , names(cv_test_data))]

	set.seed(k)			
	fit_RF<- upliftRF(x=x_training_data,
				y=cv_training_data$response,
				ct=cv_training_data$treatment,
				ntree=500,
				split_method= "KL",
				verbose=TRUE)
	eval(parse(text = paste('cv_model$k', k, '<- fit_RF' , sep='')))
	
	# Qini
	pred_training <- predict(fit_RF , x_training_data)
	training_perf <- performance(pred_training[,1], pred_training[,2], cv_training_data$response, cv_training_data$treatment, direction=2)
	qini_training <- qini(training_perf, direction=2, plotit=F)
	pred_test <- predict(fit_RF, x_test_data)
	test_perf <- performance(pred_test[,1], pred_test[,2], cv_test_data$response, cv_test_data$treatment, direction=2)
	qini_test <- qini(test_perf, direction=2, plotit=F)
	cv_qini[k,]<- c(qini_training$Qini , qini_test$Qini)
	
	# AUC
	training_obs<- ifelse(cv_training_data$treatment==1, pred_training[,1], pred_training[,2])
	test_obs<- ifelse(cv_test_data$treatment==1, pred_test[,1], pred_test[,2])
	cv_auc[k,]<- c(auc(cv_training_data$response , training_obs) , auc(cv_test_data$response , test_obs))
}
save(cv_model , file='model_1J_CV_uplift_RF_top50_model.Rdata')
save(cv_covar , file='model_1J_CV_uplift_RF_top50_covar.Rdata')
save(cv_qini , file='model_1J_CV_uplift_RF_top50_qini.Rdata')
save(cv_auc , file='model_1J_CV_uplift_RF_top50_auc.Rdata')


load('model_1J_CV_uplift_RF_top50_model.Rdata')
load('model_1J_CV_uplift_RF_top50_covar.Rdata')
load('model_1J_CV_uplift_RF_top50_qini.Rdata')
load('model_1J_CV_uplift_RF_top50_auc.Rdata')
cv_summary_effect<- 0
for(k in 1:k.folds){
	eval(parse(text = paste('fit_RF<- cv_model$k', k, sep='')))
	eval(parse(text = paste('final_covar_list<- cv_covar$k', k , sep='')))
	cv_training_data<- model_data[foldid!=k,]
	cv_test_data<- model_data[foldid==k,]
	
	x_training_data<- cv_training_data[, match(final_covar_list , names(cv_training_data))]
	x_test_data<- cv_test_data[, match(final_covar_list , names(cv_test_data))]
	
	# Qini
	pred_training <- predict(fit_RF , x_training_data)
	pred_test <- predict(fit_RF, x_test_data)
	
	# summary effect
	training_obs<- ifelse(cv_training_data$treatment==1, pred_training[,1], pred_training[,2])
	training_sim<- ifelse(cv_training_data$treatment!=1, pred_training[,1], pred_training[,2])
	training_summary<- summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=cv_training_data$treatment, response=cv_training_data$response)
	test_obs<- ifelse(cv_test_data$treatment==1, pred_test[,1], pred_test[,2])
	test_sim<- ifelse(cv_test_data$treatment!=1, pred_test[,1], pred_test[,2])
	test_summary<- summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=cv_test_data$treatment, response=cv_test_data$response)
	cv_summary_effect<- cv_summary_effect + rbind(training_summary , test_summary)
}
cv_summary_effect<- cv_summary_effect/10
avg_auc<- c(mean(cv_auc[,1]), mean(cv_auc[,1]), mean(cv_auc[,2]), mean(cv_auc[,2]))
avg_qini<- c(mean(cv_qini[,1]), mean(cv_qini[,1]), mean(cv_qini[,2]), mean(cv_qini[,2]))
cbind(cv_summary_effect , avg_auc , avg_qini)
