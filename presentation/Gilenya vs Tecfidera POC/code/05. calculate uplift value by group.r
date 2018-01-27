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
#	Uplift Random Forest
#	report by decile: average uplift effect, SD and p-value
#------------------------------------------------------------------------------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'post_relapse_persist'	# post_relapse_persist or persistent
flip_label<- TRUE		# TRUE/FALSE
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)

list_auc<- numeric()
list_qini<- numeric()
list_performance<- list()
list_uplift<- numeric()
list_uplift_training<- numeric()
list_pvalue<- numeric()
list_odds_ratio<- numeric()
for(k in 1:10){
	# sample training and test data set
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
	
	# select variable based on training data
	#set.seed(k)
	covar_NIV<- niv(response ~ trt(treatment) + ., data=training_data, B=10, direction=2, plotit=F)
	variable_selection<- covar_NIV$niv_val[order(covar_NIV$niv_val[,3], decreasing=T),]
	percent_variable<- 0.5	# top 50%
	final_covar_list<- rownames(variable_selection)[1:round(percent_variable*nrow(variable_selection))]

	# re-coding the response variable among control patients of training data set
	training_original_respose<- training_data$response
	if(flip_label){training_data$response[training_data$treatment==0]<- 1 - training_data$response[training_data$treatment==0]}

	x_training_data<- training_data[, match(final_covar_list , names(training_data))]
	x_test_data<- test_data[, match(final_covar_list , names(test_data))]
	dim(x_training_data); dim(x_test_data)
	
	fit_RF<- upliftRF(x=x_training_data,
					y=training_data$response,
					ct=training_data$treatment,
					ntree=100,
					split_method='KL',
					verbose=TRUE)

	pred_training <- predict(fit_RF , x_training_data)
	if(flip_label){pred_training[,2]<- 1 - pred_training[,2]} # change back to the possibility of response when flip-labeled
	training_perf <- performance(pred_training[,1], pred_training[,2], training_original_respose , training_data$treatment, direction=perf_direction, groups=5)
	qini_training <- qini(training_perf, direction=perf_direction, plotit=F)
	list_uplift_training<- rbind(list_uplift_training , c(k , training_perf[,8]))

	pred_test <- predict(fit_RF, x_test_data)
	if(flip_label){pred_test[,2]<- 1 - pred_test[,2]} # change back to the possibility of response when flip-labeled
	test_perf <- performance(pred_test[,1], pred_test[,2], test_data$response , test_data$treatment, direction=perf_direction, groups=5)
	qini_test <- qini(test_perf, direction=perf_direction, plotit=F)
	re_qini<- c(qini_training$Qini , qini_test$Qini)
	list_qini<- rbind(list_qini , c(k , re_qini))
	list_uplift<- rbind(list_uplift , c(k , test_perf[,8]))
	eval(parse(text = paste('list_performance$k', k, '<- test_perf' , sep='')))

	# p-value of correlation between treatment and response within each sub-population
	result<- apply(test_perf[,2:5] , 1 , function(x){
		Gilenya_relapse<- x[3]
		Tecfidera_relapse<- x[4]
		Gilenya_not_relapse<- x[1] - x[3]
		Tecfidera_not_relapse<- x[2] - x[4]
		contigency_table<- matrix(c(Gilenya_relapse, Tecfidera_relapse, Gilenya_not_relapse, Tecfidera_not_relapse), nc=2, byrow=T)
		association_test<- fisher.test(contigency_table , alternative = "two.sided")
		p_value<- association_test$p.value
		odds_ratio<- association_test$estimate
		c(p_value , odds_ratio)
	})
	list_pvalue<- rbind(list_pvalue , c(k , result[1,]))
	list_odds_ratio<- rbind(list_odds_ratio , c(k , result[2,]))
	
	# AUC
	training_obs<- ifelse(training_data$treatment==1, pred_training[,1], pred_training[,2])
	test_obs<- ifelse(test_data$treatment==1, pred_test[,1], pred_test[,2])
	AUC<- c(auc(training_original_respose , training_obs) , auc(test_data$response, test_obs))
	list_auc<- rbind(list_auc , c(k , AUC))
}


#------------------------------------------------------------------------------------------
#	Uplift CCIF model
#	report by decile: average uplift effect, SD and p-value
#------------------------------------------------------------------------------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'post_relapse_persist'	# post_relapse_persist or persistent
flip_label<- TRUE		# TRUE/FALSE
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)

list_auc<- numeric()
list_qini<- numeric()
list_performance<- list()
list_uplift<- numeric()
list_uplift_training<- numeric()
list_pvalue<- numeric()
list_odds_ratio<- numeric()
for(k in 1:10){
	# sample training and test data set
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
	
	# select variable based on training data
	#set.seed(k)
	covar_NIV<- niv(response ~ trt(treatment) + ., data=training_data, B=10, direction=2, plotit=F)
	variable_selection<- covar_NIV$niv_val[order(covar_NIV$niv_val[,3], decreasing=T),]
	percent_variable<- 0.5	# top 50%
	final_covar_list<- rownames(variable_selection)[1:round(percent_variable*nrow(variable_selection))]

	# re-coding the response variable among control patients of training data set
	training_original_respose<- training_data$response
	if(flip_label){training_data$response[training_data$treatment==0]<- 1 - training_data$response[training_data$treatment==0]}

	x_training_data<- training_data[, match(final_covar_list , names(training_data))]
	x_test_data<- test_data[, match(final_covar_list , names(test_data))]
	dim(x_training_data); dim(x_test_data)
	
	fit_ccif<- ccif(x=x_training_data,
				y=training_data$response,
				ct=training_data$treatment,
				ntree=100,
				split_method='KL',
				verbose=TRUE)

	pred_training <- predict(fit_ccif , x_training_data)
	if(flip_label){pred_training[,2]<- 1 - pred_training[,2]} # change back to the possibility of response when flip-labeled
	training_perf <- performance(pred_training[,1], pred_training[,2], training_original_respose , training_data$treatment, direction=perf_direction, groups=5)
	qini_training <- qini(training_perf, direction=perf_direction, plotit=F)
	list_uplift_training<- rbind(list_uplift_training , c(k , training_perf[,8]))

	pred_test <- predict(fit_ccif, x_test_data)
	if(flip_label){pred_test[,2]<- 1 - pred_test[,2]} # change back to the possibility of response when flip-labeled
	test_perf <- performance(pred_test[,1], pred_test[,2], test_data$response , test_data$treatment, direction=perf_direction, groups=5)
	qini_test <- qini(test_perf, direction=perf_direction, plotit=F)
	re_qini<- c(qini_training$Qini , qini_test$Qini)
	list_qini<- rbind(list_qini , c(k , re_qini))
	list_uplift<- rbind(list_uplift , c(k , test_perf[,8]))
	eval(parse(text = paste('list_performance$k', k, '<- test_perf' , sep='')))

	# p-value of correlation between treatment and response within each sub-population
	result<- apply(test_perf[,2:5] , 1 , function(x){
		Gilenya_relapse<- x[3]
		Tecfidera_relapse<- x[4]
		Gilenya_not_relapse<- x[1] - x[3]
		Tecfidera_not_relapse<- x[2] - x[4]
		contigency_table<- matrix(c(Gilenya_relapse, Tecfidera_relapse, Gilenya_not_relapse, Tecfidera_not_relapse), nc=2, byrow=T)
		association_test<- fisher.test(contigency_table , alternative = "two.sided")
		p_value<- association_test$p.value
		odds_ratio<- association_test$estimate
		c(p_value , odds_ratio)
	})
	list_pvalue<- rbind(list_pvalue , c(k , result[1,]))
	list_odds_ratio<- rbind(list_odds_ratio , c(k , result[2,]))
	
	# AUC
	training_obs<- ifelse(training_data$treatment==1, pred_training[,1], pred_training[,2])
	test_obs<- ifelse(test_data$treatment==1, pred_test[,1], pred_test[,2])
	AUC<- c(auc(training_original_respose , training_obs) , auc(test_data$response, test_obs))
	list_auc<- rbind(list_auc , c(k , AUC))
}

				
#------------------------------------------------------------------------------------------
#	logistic regression and Lasso
#	use only summary variables without interaction-terms
#	report by decile: average uplift effect, SD and p-value
#------------------------------------------------------------------------------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'persistent'	# post_relapse_persist or persistent
flip_label<- TRUE		# TRUE/FALSE
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
simulate_data$response<- simulate_data[,names(simulate_data)==response_variable]

covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('response', 'treatment', covar_list, interact_var) , constant_covar)

# create object to store model results
list_qini<- numeric()
list_performance<- list()
list_uplift<- numeric()
list_uplift_training<- numeric()
list_pvalue<- numeric()
list_odds_ratio<- numeric()
for(k in 1:20){
	training_sample_proportion<- 0.5
	training<- numeric(length=nrow(raw_data))
	case<- which(raw_data$response==1)
	control<- which(raw_data$response==0)
	index_case<- sample(case , round(length(case)*training_sample_proportion))
	index_control<- sample(control , round(length(control)*training_sample_proportion))
	training[c(index_case , index_control)]<- 1

	training_data<- raw_data[training==1 , match(model_variable, names(raw_data))]
	test_data<- raw_data[training==0 , match(model_variable, names(raw_data))]
	training_simulate_data<- simulate_data[training==1 , match(model_variable, names(simulate_data))]
	test_simulate_data<- simulate_data[training==0 , match(model_variable, names(simulate_data))]

	# flip-label the response variable among control patients on training data set
	training_original_respose<- training_data$response
	if(flip_label){training_data$response[training_data$treatment==0]<- 1 - training_data$response[training_data$treatment==0]}
	table(training_data$treatment , training_data$response) # QC

	fit<- glm(response~., data=training_data, family=binomial)

	training_obs<- predict(fit, training_data, type="response")
	training_sim<- predict(fit, training_simulate_data, type="response")
	test_obs<- predict(fit, test_data, type="response")
	test_sim<- predict(fit, test_simulate_data, type="response")

	# computation of Qini measure for test data: Prob(y=1|treated, x), Prob(y=1|control, x)
	prob_treatment<- ifelse(training_data$treatment==1, training_obs, training_sim)
	prob_control<- ifelse(training_data$treatment!=1, training_obs, training_sim)
	if(flip_label) {prob_control<- 1 - ifelse(training_data$treatment!=1, training_obs, training_sim)} # change back to the possibility of relapse
	training_perf <- performance(prob_treatment, prob_control, training_original_respose, training_data$treatment, direction=perf_direction, groups=5)
	qini_training <- qini(training_perf, direction=perf_direction, plotit = F)

	prob_treatment<- ifelse(test_data$treatment==1, test_obs, test_sim)
	prob_control<- ifelse(test_data$treatment!=1, test_obs, test_sim)
	if(flip_label) {prob_control<- 1 - ifelse(test_data$treatment!=1, test_obs, test_sim)} # change back to the possibility of relapse
	test_perf <- performance(prob_treatment, prob_control, test_data$response, test_data$treatment, direction=perf_direction, groups=5)
	qini_test <- qini(test_perf, direction=perf_direction, plotit = F)
	
	list_qini<- rbind(list_qini , c(k, qini_training$Qini , qini_test$Qini))
	list_uplift_training<- rbind(list_uplift_training , c(k , training_perf[,8]))
	list_uplift<- rbind(list_uplift , c(k , test_perf[,8]))
	eval(parse(text = paste('list_performance$k', k, '<- test_perf' , sep='')))

	# p-value of correlation between treatment and response within each sub-population
	result<- apply(test_perf[,2:5] , 1 , function(x){
		Gilenya_relapse<- x[3]
		Tecfidera_relapse<- x[4]
		Gilenya_not_relapse<- x[1] - x[3]
		Tecfidera_not_relapse<- x[2] - x[4]
		contigency_table<- matrix(c(Gilenya_relapse, Tecfidera_relapse, Gilenya_not_relapse, Tecfidera_not_relapse), nc=2, byrow=T)
		association_test<- fisher.test(contigency_table , alternative = "two.sided")
		p_value<- association_test$p.value
		odds_ratio<- association_test$estimate
		c(p_value , odds_ratio)
	})
	list_pvalue<- rbind(list_pvalue , c(k , result[1,]))
	list_odds_ratio<- rbind(list_odds_ratio , c(k , result[2,]))
}


#-------------------------Lasso----------------------------------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'persistent'	# post_relapse_persist or persistent
flip_label<- TRUE		# TRUE/FALSE
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
simulate_data$response<- simulate_data[,names(simulate_data)==response_variable]

covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
interact_var<- paste(covar_list , '_interact' , sep='')
model_variable<- setdiff(c('response', 'treatment', covar_list, interact_var) , constant_covar)

model_data<- raw_data[, match(model_variable, names(raw_data))]
model_matrix<- model.matrix(response~., data=model_data)[,-1]
initial_lambda<- glmnet(x=model_matrix, y=model_data$response, family="binomial", alpha=1, standardize=F)$lambda
lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500))

list_qini<- list()
list_uplift<- list()
for(k in 1:10){
	training_sample_proportion<- 0.5
	training<- numeric(length=nrow(raw_data))
	case<- which(raw_data$response==1)
	control<- which(raw_data$response==0)
	index_case<- sample(case , round(length(case)*training_sample_proportion))
	index_control<- sample(control , round(length(control)*training_sample_proportion))
	training[c(index_case , index_control)]<- 1

	training_data<- raw_data[training==1 , match(model_variable, names(raw_data))]
	test_data<- raw_data[training==0 , match(model_variable, names(raw_data))]
	training_simulate_data<- simulate_data[training==1 , match(model_variable, names(simulate_data))]
	test_simulate_data<- simulate_data[training==0 , match(model_variable, names(simulate_data))]

	# flip-label the response variable among control patients on training data set
	training_original_respose<- training_data$response
	if(flip_label){training_data$response[training_data$treatment==0]<- 1 - training_data$response[training_data$treatment==0]}
	table(training_data$treatment , training_data$response)

	training_matrix<- model.matrix(response~., data=training_data)[,-1] # removes intercept term
	test_matrix<- model.matrix(response~., data=test_data)[,-1]

	fit_lasso<- glmnet(training_matrix, training_data$response, 
		lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
	test_pred<- predict(fit_lasso, test_matrix, type="response")

	test_simulate_matrix<- model.matrix(response~., data=test_simulate_data)[,-1]
	test_simulate_pred<- predict(fit_lasso, test_simulate_matrix, type="response")
	for(i in 1:length(lambda_seq)){
		test_obs<- test_pred[,i]
		test_sim<- test_simulate_pred[,i]
		prob_treatment<- ifelse(test_data$treatment==1, test_obs, test_sim)
		prob_control<- ifelse(test_data$treatment!=1, test_obs, test_sim)
		if(flip_label){prob_control<- 1 - ifelse(test_data$treatment!=1, test_obs, test_sim)} # change back to the possibility of relapse
		test_perf <- performance(prob_treatment, prob_control, test_data$response, test_data$treatment, direction=perf_direction, groups=5)
		qini_test <- qini(test_perf, direction=perf_direction, plotit = F)

		eval(parse(text=paste('list_qini$lambda', i, '<- rbind(list_qini$lambda', i, ' , c(k, qini_test$Qini))' , sep='')))
		eval(parse(text=paste('list_uplift$lambda', i, '<- rbind(list_uplift$lambda', i, ' , c(k , test_perf[,8]))' , sep='')))
	}
}

# select optimum lambda
list_qini_avg<- numeric(length=length(lambda_seq))
for(i in 1:length(lambda_seq)){
	eval(parse(text = paste('qini<- list_qini$lambda' , i , sep='')))
	eval(parse(text = paste('uplift<- list_uplift$lambda' , i , sep='')))
	if(ncol(qini)==2 & ncol(uplift)==6){
		index<- which(qini[,1]!=qini[,2]) # some times performance's groups tie up together
		list_qini_avg[i]<- mean(qini[index,2])
	}
	else
		list_qini_avg[i]<- NA
}

optimum_lambda<- which.max(list_qini_avg)
eval(parse(text = paste('uplift<- list_uplift$lambda' , optimum_lambda , sep='')))
uplift
