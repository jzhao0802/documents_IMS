#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Part III: Model Estimation Based on data transformed as Tian et al.

#	Develop time: 08/26/2014 - .

#	Developer: Gui Yaming
#==========================================================================================

#------------------------------------------------------------------------------------------
#	Compile some functions and prepare model data
#------------------------------------------------------------------------------------------

raw_data_recode<- raw_data
raw_data_recode$treatment<- ifelse(raw_data_recode$treatment==1, 1, -1)
interaction_term<- apply(raw_data_recode[, match(total_covar_list , names(raw_data_recode))] , 2 , function(x){x * raw_data_recode$treatment})
raw_data_recode[, match(total_covar_list , names(raw_data_recode))]<- interaction_term
# QC
table(raw_data$treatment , raw_data_recode$treatment)
table(raw_data$pchrlson_2 , raw_data_recode$treatment)
table(raw_data_recode$pchrlson_2)

simulate_data_recode<- raw_data
simulate_data_recode$treatment<- ifelse(simulate_data_recode$treatment==1, -1, 1)
simulate_interact<- apply(simulate_data_recode[, match(total_covar_list , names(simulate_data_recode))] , 2 , function(x){x * simulate_data_recode$treatment})
simulate_data_recode[,match(total_covar_list , names(simulate_data_recode))]<- simulate_interact
#QC
table(raw_data_recode$treatment , simulate_data_recode$treatment)
table(raw_data$pchrlson_2 , simulate_data_recode$treatment)
table(simulate_data_recode$pchrlson_2)


#------------------------------------------------------------------------------------------
#	(1A) Logistic regression without regularization
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------

#----------------------based on detail variables---------------------------
covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list) , constant_covar)

training_data<- raw_data_recode[which(raw_data_recode$training==1) , match(model_variable, names(raw_data_recode))]
test_data<- raw_data_recode[which(raw_data_recode$training==0) , match(model_variable, names(raw_data_recode))]
training_simulate_data<- simulate_data_recode[which(simulate_data_recode$training==1) , match(model_variable, names(simulate_data_recode))]
test_simulate_data<- simulate_data_recode[which(simulate_data_recode$training==0) , match(model_variable, names(simulate_data_recode))]

# model output: actual and expected treatment effects
fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
coef<- data.frame(coefficient=coef(fit) , odds_ratio=exp(coef(fit)))
p_value<- summary(fit)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 2A DetailVar_coefficient.csv', row.names=T, quote=F)

training_obs<- predict(fit, training_data, type="response")
training_sim<- predict(fit, training_simulate_data, type="response")
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
test_obs<- predict(fit, test_data, type="response")
test_sim<- predict(fit, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 2A DetailVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$post_relapse_persist , training_obs)
test_auc<- auc(test_data$post_relapse_persist , test_obs)
c(training_auc , test_auc)
# 0.6744388 0.5010705

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
#summary_actual_treatment_effect summary_expect_treatment_effect
#                     -0.1890432                      -0.2240178
#                     0.02827078                      -0.2282571

#QC
table(training_data$treatment , training_data$post_relapse_persist)


#----------------------based on summary variables---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male'))
model_variable<- setdiff(c('post_relapse_persist', 'treatment', '', covar_list) , constant_covar)

training_data<- raw_data_recode[which(raw_data_recode$training==1) , match(model_variable, names(raw_data_recode))]
test_data<- raw_data_recode[which(raw_data_recode$training==0) , match(model_variable, names(raw_data_recode))]
training_simulate_data<- simulate_data_recode[which(simulate_data_recode$training==1) , match(model_variable, names(simulate_data_recode))]
test_simulate_data<- simulate_data_recode[which(simulate_data_recode$training==0) , match(model_variable, names(simulate_data_recode))]

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
# 0.6434465 0.4840687

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
#summary_actual_treatment_effect summary_expect_treatment_effect
#                     -0.1212788                      -0.1714061
#                     0.05168186                      -0.1726576


#------------------------------------------------------------------------------------------
#	(2B) Logistic regression step-wise variable selection, backward algorithm				
#	try two models: all detail variables and only summary variables
#------------------------------------------------------------------------------------------

#----------------------based on detail variables---------------------------
covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list) , constant_covar)

training_data<- raw_data_recode[which(raw_data_recode$training==1) , match(model_variable, names(raw_data_recode))]
test_data<- raw_data_recode[which(raw_data_recode$training==0) , match(model_variable, names(raw_data_recode))]
training_simulate_data<- simulate_data_recode[which(simulate_data_recode$training==1) , match(model_variable, names(simulate_data_recode))]
test_simulate_data<- simulate_data_recode[which(simulate_data_recode$training==0) , match(model_variable, names(simulate_data_recode))]

fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
step_wise<- step(fit , direction="backward")
#save(step_wise , file='model_2B_stepwise_detailvar.Rdata')
coef<- data.frame(coefficient=coef(step_wise) , odds_ratio=exp(coef(step_wise)))
p_value<- summary(step_wise)$coef[, "Pr(>|z|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
write.csv(model , 'Model 2B DetailVar_coefficient.csv', row.names=T, quote=F)

training_obs<- predict(step_wise, training_data, type="response")
training_sim<- predict(step_wise, training_simulate_data, type="response")
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
test_obs<- predict(step_wise, test_data, type="response")
test_sim<- predict(step_wise, test_simulate_data, type="response")
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
result<- rbind(cbind(rep('training', 3) , training_data_evaluate) , cbind(rep('test', 2) , test_data_evaluate))
write.csv(result , 'Model 2B DetailVar_results.csv', row.names=F, quote=F)

# AUC and summary treatment effect
training_auc<- auc(training_data$post_relapse_persist , training_obs)
test_auc<- auc(test_data$post_relapse_persist , test_obs)
c(training_auc , test_auc)
# 0.6350258 0.5387998

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
#summary_actual_treatment_effect summary_expect_treatment_effect
#                     -0.1182491                       -0.174051
#                    -0.01254275                      -0.1813799

#----------------------based on summary variables---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male'))
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list) , constant_covar)

training_data<- raw_data_recode[which(raw_data_recode$training==1) , match(model_variable, names(raw_data_recode))]
test_data<- raw_data_recode[which(raw_data_recode$training==0) , match(model_variable, names(raw_data_recode))]
training_simulate_data<- simulate_data_recode[which(simulate_data_recode$training==1) , match(model_variable, names(simulate_data_recode))]
test_simulate_data<- simulate_data_recode[which(simulate_data_recode$training==0) , match(model_variable, names(simulate_data_recode))]

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
# 0.6242254 0.5271084

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
#summary_actual_treatment_effect summary_expect_treatment_effect
#                     -0.1310044                      -0.1366708
#                    -0.03030001                      -0.1419336


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

#---------------based on detail variables, AUC or treatment effect evaluation (optimum lambda=0)---------------------------
covar_list<- setdiff(total_covar_list , c(summary_covar , 'der_sex_male'))
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list) , constant_covar)

model_data<- training_data[, match(model_variable, names(training_data))]
model_matrix<- model.matrix(post_relapse_persist~., data=model_data)[,-1]
model_simulate_data<- training_simulate_data[, match(model_variable, names(training_simulate_data))]
model_simulate_matrix<- model.matrix(post_relapse_persist~., data=model_simulate_data)[,-1]

initial_lambda<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, family="binomial", alpha=1, standardize=F)$lambda
lambda_seq<- c(initial_lambda[-1] , seq(initial_lambda[length(initial_lambda)] , 0 , length=1000))
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
cv_treatment_effect_mean<- apply(cv_treatment_effect , 2 , function(x) mean(x[!is.na(x)]))
optimum_model<- which.max(cv_auc_mean) #lambda=0

total_model<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
write.csv(coef(total_model)[,optimum_model] , 'Model 2C DetailVar_coefficient.csv', row.names=T, quote=F)

# report: actual and expected treatment effects
optimum_model<- which.max(cv_treatment_effect_mean)
training_obs<- predict(total_model, model_matrix, type="response")[,optimum_model]
training_sim<- predict(total_model, model_simulate_matrix, type="response")[,optimum_model]
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)

test_matrix<- model.matrix(post_relapse_persist~., data=test_data[,match(model_variable, names(test_data))])[,-1]
test_simulate_matrix<- model.matrix(post_relapse_persist~., data=test_simulate_data[,match(model_variable, names(test_simulate_data_recode))])[,-1]
test_obs<- predict(total_model, test_matrix, type="response")[,optimum_model]
test_sim<- predict(total_model, test_simulate_matrix, type="response")[,optimum_model]
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

result<- rbind(cbind(rep('training' , 3) , training_data_evaluate) , cbind(rep('test' , 2) , test_data_evaluate))
write.csv(result , 'Model 2C DetailVar_results_2.csv', row.names=F, quote=F)

c(auc(training_data$post_relapse_persist , training_obs) , auc(test_data$post_relapse_persist , test_obs))
# 0.6744467 0.5010915
# 0.5774370 0.5690462

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
#summary_actual_treatment_effect summary_expect_treatment_effect
#                    -0.05816788                     -0.02777406
#                    -0.08523853                     -0.02845115


#---------------------------based on summary variables, AUC or treatment effect evaluation---------------------------
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male'))
model_variable<- setdiff(c('post_relapse_persist', 'treatment', covar_list) , constant_covar)

model_data<- training_data[, match(model_variable, names(training_data))]
model_matrix<- model.matrix(post_relapse_persist~., data=model_data)[,-1]
model_simulate_data<- training_simulate_data[, match(model_variable, names(training_simulate_data))]
model_simulate_matrix<- model.matrix(post_relapse_persist~., data=model_simulate_data)[,-1]

initial_lambda<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, family="binomial", alpha=1, standardize=F)$lambda
lambda_seq<- c(initial_lambda[-1] , seq(initial_lambda[length(initial_lambda)] , 0 , length=100))
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
cv_treatment_effect_mean<- apply(cv_treatment_effect , 2 , function(x)mean(x[!is.na(x)]))

total_model<- glmnet(x=model_matrix, y=model_data$post_relapse_persist, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
optimum_model<- which.max(cv_treatment_effect_mean)
write.csv(coef(total_model)[,optimum_model] , 'Model 2C SummaryVar_coefficient.csv', row.names=T, quote=F)

# report: actual and expected treatment effects
optimum_model<- which.max(cv_auc_mean)
training_obs<- predict(total_model, model_matrix, type="response")[,optimum_model]
training_sim<- predict(total_model, model_simulate_matrix, type="response")[,optimum_model]
training_data_evaluate<- model_output(group='training', observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)

test_matrix<- model.matrix(post_relapse_persist~., data=test_data[,match(model_variable, names(test_data))])[,-1]
test_simulate_matrix<- model.matrix(post_relapse_persist~., data=test_simulate_data[,match(model_variable, names(test_simulate_data_recode))])[,-1]
test_obs<- predict(total_model, test_matrix, type="response")[,optimum_model]
test_sim<- predict(total_model, test_simulate_matrix, type="response")[,optimum_model]
test_data_evaluate<- model_output(group='test', observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)

result<- rbind(cbind(rep('training' , 3) , training_data_evaluate) , cbind(rep('test' , 2) , test_data_evaluate))
write.csv(result , 'Model 2C SummaryVar_results.csv', row.names=F, quote=F)

c(auc(training_data$post_relapse_persist , training_obs) , auc(test_data$post_relapse_persist , test_obs))
# 0.5984928 0.5768754
# 0.6402998 0.5050796

summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_data$post_relapse_persist)
summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$post_relapse_persist)
#summary_actual_treatment_effect summary_expect_treatment_effect


