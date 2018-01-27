#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Part III: Model Estimation Based on data transformed as Tian et al.

#	Develop time: 08/26/2014 - .

#	Developer: Gui Yaming
#==========================================================================================

#------------------------------------------------------------------------------------------
#	Tian's method of data transformation: code treatment/non-treatment as 1/-1
#------------------------------------------------------------------------------------------

raw_data_recode<- raw_data
raw_data_recode$treatment<- ifelse(raw_data_recode$treatment==1, 1, -1)
interaction_term<- apply(raw_data_recode[, match(total_covar_list , names(raw_data_recode))] , 2 , function(x){x * raw_data_recode$treatment})
raw_data_recode[,match(total_covar_list , names(raw_data_recode))]<- interaction_term

# QC
table(raw_data$treatment , raw_data_recode$treatment)
table(raw_data$pchrlson_2 , raw_data_recode$treatment)
table(raw_data_recode$pchrlson_2)

# create simulate data which code Gilenya as -1 while Tecfidera as 1, and re-create two-ways interaction terms
simulate_data_recode<- raw_data
simulate_data_recode$treatment<- ifelse(simulate_data_recode$treatment==1, -1, 1)
simulate_interact<- apply(simulate_data_recode[, match(total_covar_list, names(simulate_data_recode))] , 2 , function(x){x * simulate_data_recode$treatment})
simulate_data_recode[, match(total_covar_list, names(simulate_data_recode))]<- simulate_interact

#QC
table(raw_data_recode$treatment , simulate_data_recode$treatment)
table(raw_data$pchrlson_2 , simulate_data_recode$treatment)
table(simulate_data_recode$pchrlson_2)


#------------------------------------------------------------------------------------------
#	(2A) Logistic regression without regularization
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------

#----------------------based on summary variables---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
model_variable<- setdiff(c('post_relapse_persist', 'treatment', 'persist_days', covar_list) , constant_covar)

training_data<- raw_data_recode[raw_data_recode$training==1 , match(model_variable, names(raw_data_recode))]
test_data<- raw_data_recode[raw_data_recode$training==0 , match(model_variable, names(raw_data_recode))]
training_simulate_data<- simulate_data_recode[simulate_data_recode$training==1 , match(model_variable, names(simulate_data_recode))]
test_simulate_data<- simulate_data_recode[simulate_data_recode$training==0 , match(model_variable, names(simulate_data_recode))]

# model output: actual and expected treatment effects
fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
coef<- data.frame(coefficient=coef(fit) , odds_ratio=exp(coef(fit)))
p_value<- summary(fit)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 2A SummaryVar_coefficient.csv', row.names=T, quote=F)

training_obs<- predict(fit, training_data, type="response")
training_sim<- predict(fit, training_simulate_data, type="response")
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
test_obs<- predict(fit, test_data, type="response")
test_sim<- predict(fit, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 2A SummaryVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$post_relapse_persist , training_obs)
test_auc<- auc(test_data$post_relapse_persist , test_obs)
c(training_auc , test_auc)
# 0.6387625 0.5210656

# computation of Qini measure for test data: Prob(y=1|treated, x), Prob(y=1|control, x)
prob_treatment<- ifelse(training_data$treatment==1, training_obs, training_sim)
prob_control<- ifelse(training_data$treatment!=1, training_obs, training_sim)
perf <- performance(prob_treatment, prob_control, training_data$post_relapse_persist, ifelse(training_data$treatment==1, 1, 0), direction = 2, groups=10)
qini_training <- qini(perf, direction=2, plotit = F)

prob_treatment<- ifelse(test_data$treatment==1, test_obs, test_sim)
prob_control<- ifelse(test_data$treatment!=1, test_obs, test_sim)
perf <- performance(prob_treatment, prob_control, test_data$post_relapse_persist, ifelse(test_data$treatment==1, 1, 0), direction = 2, groups=10)
qini_test <- qini(perf, direction=2, plotit = F)
c(qini_training$Qini , qini_test$Qini)
# 0.027048234 0.002822302

# summary treatment effect, actual & expect
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

#------------------------------------------------------------------------------------------
#	(2B) Logistic regression step-wise variable selection, backward algorithm				
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------

#----------------------based on summary variables---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list) , constant_covar)

training_data<- raw_data_recode[raw_data_recode$training==1 , match(model_variable, names(raw_data_recode))]
test_data<- raw_data_recode[raw_data_recode$training==0 , match(model_variable, names(raw_data_recode))]
training_simulate_data<- simulate_data_recode[simulate_data_recode$training==1 , match(model_variable, names(simulate_data_recode))]
test_simulate_data<- simulate_data_recode[simulate_data_recode$training==0 , match(model_variable, names(simulate_data_recode))]

fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
step_wise<- step(fit , direction="backward") 
#save(step_wise , file='model_2B_stepwise_summaryvar.Rdata')
coef<- data.frame(coefficient=coef(step_wise) , odds_ratio=exp(coef(step_wise)))
p_value<- summary(step_wise)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 2B SummaryVar_coefficient.csv', row.names=T, quote=F)

training_obs<- predict(step_wise, training_data, type="response")
training_sim<- predict(step_wise, training_simulate_data, type="response")
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
test_obs<- predict(step_wise, test_data, type="response")
test_sim<- predict(step_wise, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 2B SummaryVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$post_relapse_persist , training_obs)
test_auc<- auc(test_data$post_relapse_persist , test_obs)
c(training_auc , test_auc)
# 0.6036716 0.5381690

# summary treatment effect, actual & expect
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

# computation of Qini measure for test data: Prob(y=1|treated, x), Prob(y=1|control, x)
prob_treatment<- ifelse(training_data$treatment==1, training_obs, training_sim)
prob_control<- ifelse(training_data$treatment!=1, training_obs, training_sim)
perf <- performance(prob_treatment, prob_control, training_data$post_relapse_persist, ifelse(training_data$treatment==1, 1, 0), direction = 2, groups=10)
qini_training <- qini(perf, direction=2, plotit = F)

prob_treatment<- ifelse(test_data$treatment==1, test_obs, test_sim)
prob_control<- ifelse(test_data$treatment!=1, test_obs, test_sim)
perf <- performance(prob_treatment, prob_control, test_data$post_relapse_persist, ifelse(test_data$treatment==1, 1, 0), direction = 2, groups=10)
qini_test <- qini(perf, direction=2, plotit = F)
c(qini_training$Qini , qini_test$Qini)
# 0.016068101 0.003275859


#------------------------------------------------------------------------------------------
#	(2C) Logistic regression lasso regularization and 10 fold cross-validation
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------
training_data<- raw_data_recode[raw_data_recode$training==1,]
training_simulate_data<- simulate_data_recode[simulate_data_recode$training==1,]
test_data<- raw_data_recode[raw_data_recode$training==0,]
test_simulate_data<- simulate_data_recode[simulate_data_recode$training==0,]

k.folds<- 10
foldid<- nrow(training_data)
set.seed(1)
foldid[training_data$post_relapse_persist==1]<- sample(rep(1:k.folds, length=length(which(training_data$post_relapse_persist==1))))
set.seed(1)
foldid[training_data$post_relapse_persist==0]<- sample(rep(1:k.folds, length=length(which(training_data$post_relapse_persist==0))))

table(training_data$post_relapse_persist , foldid) # QC

#---------------------------based on summary variables, AUC or treatment effect evaluation---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list) , constant_covar)

model_data<- training_data[, match(model_variable, names(training_data))]
model_matrix<- model.matrix(post_relapse_persist~., data=model_data)[,-1]
model_simulate_data<- training_simulate_data[, match(model_variable, names(training_simulate_data))]
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
total_model<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
cv_auc_mean<- apply(cv_auc , 2 , mean)
cv_treatment_effect_mean<- apply(cv_treatment_effect , 2 , function(x)mean(x[!is.na(x)]))
cbind(lambda_seq, cv_auc_mean, cv_treatment_effect_mean)

optimum_model<- which.max(cv_treatment_effect_mean)
write.csv(coef(total_model)[,optimum_model] , 'Model 2C SummaryVar_coefficient_2.csv', row.names=T, quote=F)

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
write.csv(result , 'Model 2C SummaryVar_results.csv', row.names=F, quote=F)

c(auc(training_data$post_relapse_persist , training_obs) , auc(test_data$post_relapse_persist , test_obs))
# 0.5522486 0.5202891
#0.6235443 0.5266406

# summary treatment effect, actual & expect
summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

# computation of Qini measure for test data: Prob(y=1|treated, x), Prob(y=1|control, x)
prob_treatment<- ifelse(training_data$treatment==1, training_obs, training_sim)
prob_control<- ifelse(training_data$treatment!=1, training_obs, training_sim)
perf <- performance(prob_treatment, prob_control, training_data$post_relapse_persist, ifelse(training_data$treatment==1, 1, 0), direction = 2, groups=10)
qini_training <- qini(perf, direction=2, plotit = F)

prob_treatment<- ifelse(test_data$treatment==1, test_obs, test_sim)
prob_control<- ifelse(test_data$treatment!=1, test_obs, test_sim)
perf <- performance(prob_treatment, prob_control, test_data$post_relapse_persist, ifelse(test_data$treatment==1, 1, 0), direction = 2, groups=10)
qini_test <- qini(perf, direction=2, plotit = F)
c(qini_training$Qini , qini_test$Qini)
# 0.005358236 0.000128649
# 0.021745488 0.002086274
