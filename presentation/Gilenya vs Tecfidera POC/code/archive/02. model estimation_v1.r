#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Part II: Model Estimation

#	Develop time: 08/26/2014 - .
#==========================================================================================

#-----------------------------------------------------------------------
#	Compile some functions and prepare evaluation data
#-----------------------------------------------------------------------
auc<- function(obs , pred){
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


model_evaluate<- function(data='training'){
	eval(parse(text = paste('f_eval_data<-', data, '_data' , sep='')))
	eval(parse(text = paste('f_eval_simulate_data<-', data, '_simulate_data' , sep='')))

	pred <- predict(fit, f_eval_data, type="response")
	pred_simulate<- predict(fit, f_eval_simulate_data, type="response")

	expected_diff<- ifelse(f_eval_data$treatment==1, pred - pred_simulate , pred_simulate - pred)

	devide<- quantile(expected_diff , prob=c(0.35 , 0.65))
	n1<- which(expected_diff<=devide[1])
	n2<- which(expected_diff>devide[1] & expected_diff<devide[2])
	n3<- which(expected_diff>=devide[2])
	
	result<- NULL
	for(i in c('n1' , 'n2', 'n3')){
		eval(parse(text = paste('f_treatment<-f_eval_data$treatment[', i, ']' , sep='')))
		eval(parse(text = paste('f_post_relapse_persist<-f_eval_data$post_relapse_persist[', i, ']' , sep='')))
		eval(parse(text = paste('f_expected_diff<-expected_diff[', i, ']' , sep='')))
		Gilenya_pat<- length(which(f_treatment==1))
		Tecfidera_pat<- length(which(f_treatment==0))
		Gilenya_relapse<- length(which(f_treatment==1 & f_post_relapse_persist==1))
		Tecfidera_relapse<- length(which(f_treatment==0 & f_post_relapse_persist==0))
		Gilenya_relapse_rate<- Gilenya_relapse / Gilenya_pat
		Tecfidera_relapse_rate<- Tecfidera_relapse / Tecfidera_pat
		actual_treatment<- Gilenya_relapse_rate - Tecfidera_relapse_rate
		expect_treatment<- mean(f_expected_diff)
		group<- ifelse(i=='n3', 'highest 35%', ifelse(i=='n2', 'middle 30%' , 'lowest 35%'))
		result<- rbind(result , c(group, Gilenya_pat, Gilenya_relapse, Gilenya_relapse_rate, Tecfidera_pat, Tecfidera_relapse, Tecfidera_relapse_rate, actual_treatment, expect_treatment))
	}
	colnames(result)<- c('Group', 'Gilenya_pat', 'Gilenya_relapse', 'Gilenya_relapse_rate', 'Tecfidera_pat', 'Tecfidera_relapse', 'Tecfidera_relapse_rate', 'actual_treatment', 'expect_treatment')
	return(result)
}

treatment_reverse<- 1 - raw_data$treatment
simulate_interact<- apply(raw_data[, match(total_covar_list , names(raw_data))] , 2 , function(x){x * treatment_reverse})
simulate_data<- raw_data
simulate_data[,match(paste(total_covar_list , '_interact' , sep='') , names(simulate_data))]<- simulate_interact
simulate_data$treatment<- treatment_reverse

#QC
dim(simulate_data)
table(simulate_data$pchrlson_2 , simulate_data$treatment)
table(simulate_data$pchrlson_2_interact)

#-----------------------------------------------------------------------
#	(1A) Logistic regression without regularisation
#	try two models: all detail variables and only summary variables
#-----------------------------------------------------------------------
## based on detail variables
covar_list<- setdiff(total_covar_list , summary_covar)
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('treatment', covar_list, interact_var, 'post_relapse_persist') , constant_covar)

training_data<- raw_data[which(raw_data$training==1) , match(model_variable, names(raw_data))]
test_data<- raw_data[which(raw_data$training==0) , match(model_variable, names(raw_data))]
training_simulate_data<- simulate_data[which(simulate_data$training==1) , match(model_variable, names(simulate_data))]
test_simulate_data<- simulate_data[which(simulate_data$training==0) , match(model_variable, names(simulate_data))]

fit<- glm(post_relapse_persist~., data=training_data, family=binomial)
training_data_evaluate<- model_evaluate('training')
test_data_evaluate<- model_evaluate('test')

#coefficient<- summary(fit)$coefficient
#odds_ratio<- exp(coefficient[,1])
#write.csv(data.frame(coefficient , OR=odds_ratio) , 'Model 1A results_detail.csv', row.names=T, quote=F)

#------------------------------------------------------------------------------------
evaluate_covar_data<- raw_data[which(raw_data$training==1) , match(covar_list, names(raw_data))]
treatment_reverse<- 1 - raw_data$treatment[which(raw_data$training==1)]
evaluate_interact<- apply(evaluate_covar_data , 2 , function(x){x * treatment_reverse})
colnames(evaluate_interact)<- paste(covar_list , '_interact' , sep='')
evaluate_data<- cbind(post_relapse_persist=raw_data$post_relapse_persist[which(raw_data$training==1)], treatment=treatment_reverse, evaluate_covar_data, evaluate_interact)
reverse_pred<- predict(fit, evaluate_data, type="response")

re<- ifelse(raw_data$treatment[which(raw_data$training==1)]==1, pred - reverse_pred , reverse_pred-pred)

#--------------------------------------------------------------------------------------
## base on summary variables
model_variable<- setdiff(total_covar_list , detail_covar)
interact_var<- paste(model_variable , '_interact' , sep='')
model_variable<- setdiff(c(model_variable, interact_var, 'post_relapse_persist', 'treatment') , constant_covar)

training_group<- raw_data[which(raw_data$training==1) , match(model_variable, names(raw_data))]
test_group<- raw_data[which(raw_data$training==0) , match(model_variable, names(raw_data))]


fit<- glm(post_relapse_persist~., data=training_group, family=binomial)
coefficient<- summary(fit)$coefficient
odds_ratio<- exp(coefficient[,1])
write.csv(data.frame(coefficient , OR=odds_ratio) , 'Model 1A results_summary.csv', row.names=T, quote=F)

pred <- predict(fit, test_group, type="response")
validate <- auc(test_group$post_relapse_persist , pred)

#0.7815993
#0.6610974

# QC
fit<- glm(post_relapse_persist~treatment+ idx_paytype_C+ idx_paytype_M+ idx_paytype_R+ idx_paytype_S, data=training_group, family=binomial)

#-----------------------------------------------------------------------
#	(1B) Logistic regression stepwise
#	try two models: all detail variables and only summary variables
#-----------------------------------------------------------------------

