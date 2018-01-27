#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Develop time: 08/26/2014 - .

#	Developer: Gui Yaming
#==========================================================================================

#------------------------------------------------------------------------------------------
#	Compile some functions and prepare evaluation data
#------------------------------------------------------------------------------------------

library(glmnet) # function: auc, glmnet
library(uplift)
library(e1071)

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

#QC
dim(simulate_data); dim(raw_data)
table(simulate_data$pchrlson_2 , simulate_data$treatment)
table(simulate_data$pchrlson_2_interact)

#------------------------------------------------------------------------------------------
#  Create final reports: modelling relapse (table 1 - 3)
#	calculate uplift value on each group
#	calculate variable importance
#	calculate covariates difference in group 1 and 5
#------------------------------------------------------------------------------------------
#---------------------modelling relapse by RF, flip-label response-------------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'post_relapse_persist'	#value: post_relapse_persist or persistent
flip_label<- TRUE		#value: TRUE/FALSE
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)
predictors<- setdiff(model_variable , c("response", "treatment")) # model variables except for response and treatment

list_qini<- numeric()
list_performance<- list()
list_uplift<- numeric()
list_uplift_training<- numeric()
list_var_importance<- character()
list_var_importance_score<- numeric()
list_var_compare<- list()
for(k in 1:100){
	training_sample_proportion<- 0.5
	training<- numeric(length=nrow(raw_data))
	case<- which(raw_data$response==1)
	control<- which(raw_data$response==0)
	#set.seed(k)
	index_case<- sample(case , round(length(case)*training_sample_proportion))
	#set.seed(k)
	index_control<- sample(control , round(length(control)*training_sample_proportion))
	training[c(index_case , index_control)]<- 1
	raw_data$training<- training

	training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
	test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]
	
	# flip-label the response variable among control patients of training data set
	training_original_respose<- training_data$response
	if(flip_label) {training_data$response[training_data$treatment==0]<- 1 - training_data$response[training_data$treatment==0]}
	table(training_data$treatment , training_data$response); table(training_data$treatment , training_original_respose)

	delete<- c("response", "treatment")
	x_training_data<- training_data[, !(colnames(training_data) %in% delete)]
	x_test_data<- test_data[, !(colnames(test_data) %in% delete)]
	dim(x_training_data); dim(x_test_data)

	fit_RF<- upliftRF(x=x_training_data,
					y=training_data$response,
					ct=training_data$treatment,
					ntree=100,
					split_method='KL',
					verbose=TRUE)

	pred_training<- predict(fit_RF , x_training_data)
	if(flip_label) {pred_training[,2]<- 1 - pred_training[,2]} # change back to the possibility of relapse
	training_perf<- performance(pred_training[,1], pred_training[,2], training_original_respose , training_data$treatment, direction=perf_direction, groups=5)
	qini_training<- qini(training_perf, direction=perf_direction, plotit=F)
	list_uplift_training<- rbind(list_uplift_training , c(k , training_perf[,8]))
	
	pred_test<- predict(fit_RF, x_test_data)
	if(flip_label) {pred_test[,2]<- 1 - pred_test[,2]} # change back to the possibility of relapse
	test_perf<- performance(pred_test[,1], pred_test[,2], test_data$response , test_data$treatment, direction=perf_direction, groups=5)
	qini_test<- qini(test_perf, direction=perf_direction, plotit=F)
	
	list_qini<- rbind(list_qini , c(k , c(qini_training$Qini , qini_test$Qini)))
	list_uplift<- rbind(list_uplift , c(k , test_perf[,8]))
	eval(parse(text = paste('list_performance$k', k, '<- test_perf' , sep='')))

	# store variable importance and report top variables
	var_importance<- varImportance(fit_RF , plot=F)
	var_imp_list<- as.character(var_importance[,1])
	var_imp_value<- var_importance[,2]
	list_var_importance<- rbind(list_var_importance , c(var_imp_list , rep(NA, length(predictors)-length(var_imp_list))))
	list_var_importance_score<- rbind(list_var_importance_score , var_imp_value[match(predictors , var_imp_list)])
	
	# store the distribution of covariate between group 1 and group 5
	if(perf_direction==1) dif_pred<- pred_test[,1] - pred_test[,2] # direction=1 (i.e, treatment - control)
	else dif_pred<- pred_test[,2] - pred_test[,1]
	q<- quantile(dif_pred , c(0.2, 0.4, 0.6, 0.8))
	index_group1<- which(dif_pred >= q[4])
	index_group5<- which(dif_pred < q[1])
	#table(test_data$treatment[index_group1] , test_data$response[index_group1]) 
	for(varname in predictors){
		var_value<- test_data[,which(names(test_data)==varname)]
		var_group1<- mean(var_value[index_group1])
		var_group5<- mean(var_value[index_group5])
		eval(parse(text=paste('list_var_compare$', varname, '<- rbind(list_var_compare$', varname, ' , c(var_group1 , var_group5))', sep='')))
	}
}

# table2: report variable importance, number of times in top 20
score_avg<- apply(list_var_importance_score , 2 , function(x)mean(x , na.rm=T))
score_sd<- apply(list_var_importance_score , 2 , function(x)sd(x , na.rm=T))
top20_occupation<- as.data.frame(table(list_var_importance[,1:20]))
colnames(top20_occupation)<- c('Var' , 'Freq')
top20_occupation$Freq<- top20_occupation$Freq/100

report_covar_importance<- data.frame(covariate_name=predictors, mean_score=score_avg, sd_score=score_sd, top20_occupation=top20_occupation$Freq[match(predictors , top20_occupation$Var)])
report_covar_importance<- report_covar_importance[order(report_covar_importance$mean_score , decreasing=T),]
row.names(report_covar_importance)<- NULL

# table3: report variable distribution within group1 and group5
report_covar_compare<- NULL
for(varname in predictors){
	eval(parse(text = paste('distribution<- list_var_compare$' , varname, sep='')))
	var_g1<- distribution[,1]
	var_g5<- distribution[,2]
	#p_value<- t.test(var_g1 , var_g5 , paired=T)$p.value
	p_value<- wilcox.test(var_g1 , var_g5 , paired=T)$p.value
	#p_value<- friedman.test(distribution)$p.value
	report_covar_compare<- rbind(report_covar_compare , c(mean(var_g1), sd(var_g1), mean(var_g5), sd(var_g5), mean(var_g1)-mean(var_g5), p_value))
}
colnames(report_covar_compare)<- c('Group1_Mean', 'Group1_SD', 'Group5_Mean', 'Group5_SD', 'Difference_between_Group', 'p_value')
report_covar_compare<- as.data.frame(report_covar_compare)
report_covar_compare<- cbind(covariate_name=predictors, report_covar_compare)
report_covar_compare<- report_covar_compare[order(report_covar_compare$p_value),]
row.names(report_covar_compare)<- NULL

# align variable description to final report
var_description<- read.csv('variable definition.csv')
table2<- report_covar_importance[1:30,]
table2<- cbind(table2 , var_description=var_description$Variable.Definition[match(table2$covariate_name , var_description$Covariate)])

table3<- report_covar_compare[1:30,]
table3<- cbind(table3 , var_description=var_description$Variable.Definition[match(table3$covariate_name , var_description$Covariate)])

write.table(table2 , 'final_report_table2.txt', quote=F, row.names=F, sep='\t')
write.table(table3 , 'final_report_table3.txt', quote=F, row.names=F, sep='\t')

# QC
intersect(table2$covariate_name , table3$covariate_name)
intersect(report_covar_importance$covariate_name[1:30] , report_covar_compare$covariate_name[1:30])

#------------------------------------------------------------------------------------------
#  Create final reports: modelling persistent (table 4 - 6)
#	calculate uplift value on each group
#	calculate variable importance
#	calculate covariates difference in group 1 and 5
#------------------------------------------------------------------------------------------

#-----------------------modelling persistent by RF, flip-label response--------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'persistent'	#value: post_relapse_persist or persistent
flip_label<- TRUE		#value: TRUE/FALSE
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)
predictors<- setdiff(model_variable , c("response", "treatment")) # model variables except for response and treatment

list_qini<- numeric()
list_performance<- list()
list_uplift<- numeric()
list_uplift_training<- numeric()
list_var_importance<- character()
list_var_importance_score<- numeric()
list_var_compare<- list()
for(k in 1:5){
	training_sample_proportion<- 0.5
	training<- numeric(length=nrow(raw_data))
	case<- which(raw_data$response==1)
	control<- which(raw_data$response==0)
	#set.seed(k)
	index_case<- sample(case , round(length(case)*training_sample_proportion))
	#set.seed(k)
	index_control<- sample(control , round(length(control)*training_sample_proportion))
	training[c(index_case , index_control)]<- 1
	raw_data$training<- training

	training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
	test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]
	
	# flip-label the response variable among control patients of training data set
	training_original_respose<- training_data$response
	if(flip_label) {training_data$response[training_data$treatment==0]<- 1 - training_data$response[training_data$treatment==0]}
	table(training_data$treatment , training_data$response); table(training_data$treatment , training_original_respose)

	delete<- c("response", "treatment")
	x_training_data<- training_data[, !(colnames(training_data) %in% delete)]
	x_test_data<- test_data[, !(colnames(test_data) %in% delete)]
	dim(x_training_data); dim(x_test_data)

	fit_RF<- upliftRF(x=x_training_data,
					y=training_data$response,
					ct=training_data$treatment,
					ntree=100,
					split_method='KL',
					verbose=TRUE)

	pred_training<- predict(fit_RF , x_training_data)
	if(flip_label) {pred_training[,2]<- 1 - pred_training[,2]} # change back to the possibility of relapse
	training_perf<- performance(pred_training[,1], pred_training[,2], training_original_respose , training_data$treatment, direction=perf_direction, groups=5)
	qini_training<- qini(training_perf, direction=perf_direction, plotit=F)
	list_uplift_training<- rbind(list_uplift_training , c(k , training_perf[,8]))
	
	pred_test<- predict(fit_RF, x_test_data)
	if(flip_label) {pred_test[,2]<- 1 - pred_test[,2]} # change back to the possibility of relapse
	test_perf<- performance(pred_test[,1], pred_test[,2], test_data$response , test_data$treatment, direction=perf_direction, groups=5)
	qini_test<- qini(test_perf, direction=perf_direction, plotit=F)
	
	list_qini<- rbind(list_qini , c(k , c(qini_training$Qini , qini_test$Qini)))
	list_uplift<- rbind(list_uplift , c(k , test_perf[,8]))
	eval(parse(text = paste('list_performance$k', k, '<- test_perf' , sep='')))

	# store variable importance and report top 20 variables
	var_importance<- varImportance(fit_RF , plot=F)
	var_imp_list<- as.character(var_importance[,1])
	var_imp_value<- var_importance[,2]
	list_var_importance<- rbind(list_var_importance , c(var_imp_list , rep(NA, length(predictors)-length(var_imp_list))))
	list_var_importance_score<- rbind(list_var_importance_score , var_imp_value[match(predictors , var_imp_list)])
	
	# store the distribution of covariate between group 1 and group 5
	if(perf_direction==1) dif_pred<- pred_test[,1] - pred_test[,2] # direction=1 (i.e, treatment - control)
	else dif_pred<- pred_test[,2] - pred_test[,1]
	q<- quantile(dif_pred , c(0.2, 0.4, 0.6, 0.8))
	index_group1<- which(dif_pred >= q[4])
	index_group5<- which(dif_pred < q[1])
	#table(test_data$treatment[index_group1] , test_data$response[index_group1]) 
	for(varname in predictors){
		var_value<- test_data[,which(names(test_data)==varname)]
		var_group1<- mean(var_value[index_group1])
		var_group5<- mean(var_value[index_group5])
		eval(parse(text=paste('list_var_compare$', varname, '<- rbind(list_var_compare$', varname, ' , c(var_group1 , var_group5))', sep='')))
	}
}

#-- report variable importance, number of times in top 20
score_avg<- apply(list_var_importance_score , 2 , function(x)mean(x , na.rm=T))
score_sd<- apply(list_var_importance_score , 2 , function(x)sd(x , na.rm=T))
top20_occupation<- as.data.frame(table(list_var_importance[,1:20]))
colnames(top20_occupation)<- c('Var' , 'Freq')
top20_occupation$Freq<- top20_occupation$Freq/100

report_covar_importance<- data.frame(covariate_name=predictors, mean_score=score_avg, sd_score=score_sd, top20_occupation=top20_occupation$Freq[match(predictors , top20_occupation$Var)])
report_covar_importance<- report_covar_importance[order(report_covar_importance$mean_score , decreasing=T),]
row.names(report_covar_importance)<- NULL

#-- report variable distribution within group1 and group5
report_covar_compare<- NULL
for(varname in predictors){
	eval(parse(text = paste('distribution<- list_var_compare$' , varname, sep='')))
	var_g1<- distribution[,1]
	var_g5<- distribution[,2]
	#p_value<- t.test(var_g1 , var_g5 , paired=T)$p.value
	p_value<- wilcox.test(distribution[,1] , distribution[,2] , paired=T)$p.value
	#p_value<- friedman.test(distribution)$p.value
	report_covar_compare<- rbind(report_covar_compare , c(mean(var_g1), sd(var_g1), mean(var_g5), sd(var_g5), mean(var_g1)-mean(var_g5), p_value))
}
colnames(report_covar_compare)<- c('Group1_Mean', 'Group1_SD', 'Group5_Mean', 'Group5_SD', 'Difference_between_Group', 'p_value')
report_covar_compare<- as.data.frame(report_covar_compare)
report_covar_compare<- cbind(covariate_name=predictors, report_covar_compare)
report_covar_compare<- report_covar_compare[order(report_covar_compare$p_value),]
row.names(report_covar_compare)<- NULL

#-- report actual outcome rate for treatment and control by group
report_outcome_rate<- 0
for(k in 1:5){
	eval(parse(text = paste('test_perf<- list_performance$k', k , sep='')))
	pos_treatment<- test_perf[,colnames(test_perf)=='n.y1_ct1']	#. Number of patients persistent on Gilenya 
	neg_treatment<- test_perf[,colnames(test_perf)=='n.ct1'] - test_perf[,colnames(test_perf)=='n.y1_ct1']	#. Number of patients non-persistent on Gilenya 
	pos_rate_treatment<- test_perf[,colnames(test_perf)=='r.y1_ct1']	#. Persistence rate for Gilenya 
	pos_control<- test_perf[,colnames(test_perf)=='n.y1_ct0']	#. Number of patients persistent on Tecfidera
	neg_control<- test_perf[,colnames(test_perf)=='n.ct0'] - test_perf[,colnames(test_perf)=='n.y1_ct0']	#. Number of patients non-persistent on Tecfidera 
	pos_rate_control<- test_perf[,colnames(test_perf)=='r.y1_ct0']	#. Persistence rate for Tecfidera 
	dif_pos_rate<- pos_rate_treatment - pos_rate_control	#. Difference in persistence rates / uplift 
	report_outcome_rate<- report_outcome_rate + data.frame(pos_treatment, neg_treatment, pos_rate_treatment, pos_control, neg_control, pos_rate_control, dif_pos_rate)
}
report_outcome_rate<- report_outcome_rate/5

# align variable description to final report
var_description<- read.csv('variable definition.csv')
table5<- report_covar_importance[1:20,]
table5<- cbind(table5 , var_description=var_description$Variable.Definition[match(table5$covariate_name , var_description$Covariate)])

table6<- report_covar_compare[1:20,]
table6<- cbind(table6 , var_description=var_description$Variable.Definition[match(table6$covariate_name , var_description$Covariate)])

write.table(table5 , 'final_report_table5.txt', quote=F, row.names=F, sep='\t')
write.table(table6 , 'final_report_table6.txt', quote=F, row.names=F, sep='\t')

# QC
intersect(table5$covariate_name , table6$covariate_name)
intersect(report_covar_importance$covariate_name[1:30] , report_covar_compare$covariate_name[1:30])


#--------------------------------modelling using logistic regression----------------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'persistent'	#value: post_relapse_persist or persistent
flip_label<- FALSE		#value: TRUE/FALSE
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)
predictors<- setdiff(model_variable , c("response", "treatment")) # model variables except for response and treatment

list_qini<- numeric()
list_performance<- list()
list_uplift<- numeric()
list_uplift_training<- numeric()
list_var_importance<- character()
list_var_importance_score<- numeric()
list_var_compare<- list()
for(k in 1:10){
	training_sample_proportion<- 0.5
	training<- numeric(length=nrow(raw_data))
	case<- which(raw_data$response==1)
	control<- which(raw_data$response==0)
	#set.seed(k)
	index_case<- sample(case , round(length(case)*training_sample_proportion))
	#set.seed(k)
	index_control<- sample(control , round(length(control)*training_sample_proportion))
	training[c(index_case , index_control)]<- 1
	raw_data$training<- training

	training_data<- raw_data[training==1 , match(model_variable, names(raw_data))]
	test_data<- raw_data[training==0 , match(model_variable, names(raw_data))]
	training_simulate_data<- simulate_data[training==1 , match(model_variable, names(simulate_data))]
	test_simulate_data<- simulate_data[training==0 , match(model_variable, names(simulate_data))]

	# flip-label the response variable among control patients on training data set
	training_original_respose<- training_data$response
	if(flip_label) {training_data$response[training_data$treatment==0]<- 1 - training_data$response[training_data$treatment==0]}
	table(training_data$treatment , training_data$response); table(training_data$treatment , training_original_respose)

	fit<- glm(response~., data=training_data, family=binomial)

	training_obs<- predict(fit, training_data, type="response")
	training_sim<- predict(fit, training_simulate_data, type="response")
	test_obs<- predict(fit, test_data, type="response")
	test_sim<- predict(fit, test_simulate_data, type="response")

	# computation of Qini measure for test data: Prob(y=1|treated, x), Prob(y=1|control, x)
	training_prob_treatment<- ifelse(training_data$treatment==1, training_obs, training_sim)
	training_prob_control<- ifelse(training_data$treatment!=1, training_obs, training_sim)
	if(flip_label) {training_prob_control<- 1 - ifelse(training_data$treatment!=1, training_obs, training_sim)} # change back to the possibility of relapse
	training_perf <- performance(training_prob_treatment, training_prob_control, training_original_respose, training_data$treatment, direction = perf_direction, groups=5)
	qini_training <- qini(training_perf, direction=perf_direction, plotit = F)

	test_prob_treatment<- ifelse(test_data$treatment==1, test_obs, test_sim)
	test_prob_control<- ifelse(test_data$treatment!=1, test_obs, test_sim)
	if(flip_label) {test_prob_control<- 1 - ifelse(test_data$treatment!=1, test_obs, test_sim)} # change back to the possibility of relapse
	test_perf <- performance(test_prob_treatment, test_prob_control, test_data$response, test_data$treatment, direction = perf_direction, groups=5)
	qini_test <- qini(test_perf, direction=perf_direction, plotit = F)
	
	list_qini<- rbind(list_qini , c(k , c(qini_training$Qini , qini_test$Qini)))
	list_uplift<- rbind(list_uplift , c(k , test_perf[,8]))
	eval(parse(text = paste('list_performance$k', k, '<- test_perf' , sep='')))

	# store variable importance and report top 20 variables
	coef<- data.frame(coefficient=coef(fit) , odds_ratio=exp(coef(fit)))
	p_value<- summary(fit)$coef[, "Pr(>|z|)"]
	model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
	
	var_imp_list<- model[match(predictors , rownames(model)),]
	var_imp_list<- var_imp_list[order(var_imp_list$p_value),]
	var_imp_value<- 1/var_imp_list$p_value
	list_var_importance<- rbind(list_var_importance , rownames(var_imp_list))
	list_var_importance_score<- rbind(list_var_importance_score , var_imp_value[match(predictors , rownames(var_imp_list))])
	
	# store the distribution of covariate between group 1 and group 5
	if(perf_direction==1) dif_pred<- test_prob_treatment-test_prob_control
	else dif_pred<- test_prob_control-test_prob_treatment
	q<- quantile(dif_pred , c(0.2, 0.4, 0.6, 0.8))
	index_group1<- which(dif_pred >= q[4])
	index_group5<- which(dif_pred < q[1])
	#table(test_data$treatment[index_group1] , test_data$response[index_group1]) 
	for(varname in predictors){
		var_value<- test_data[,which(names(test_data)==varname)]
		var_group1<- mean(var_value[index_group1])
		var_group5<- mean(var_value[index_group5])
		eval(parse(text=paste('list_var_compare$', varname, '<- rbind(list_var_compare$', varname, ' , c(var_group1 , var_group5))', sep='')))
	}
}

