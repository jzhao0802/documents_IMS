#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Part III: experiment parameter for Uplift RF/CCIF/SVM

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

#------------------------------------------------------------------------------------------
#  Uplift Random Forest, experiment the following to minimize over-fitting
#	. Number of splits allowed i.e. depth of tree
#	. Number of variables considered at each split
#	. Proportion of available sample used for training vs out-of-bag
#	. Number of trees
#	. Split method
#------------------------------------------------------------------------------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'persistent'	# post_relapse_persist or persistent
flip_label<- TRUE		# TRUE/FALSE
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)
training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]

# flip-lable the response variable among control patients of training data set
training_original_response<- training_data$response
as.factor(training_original_response)
training_data_flip <- training_data
if(flip_label){training_data_flip$response[training_data_flip$treatment==0]<- 1 - training_data_flip$response[training_data_flip$treatment==0]}
cbind(training_data$response,training_data_flip$response)[1:20,]

delete<- c("response", "treatment")
x_training_data<- training_data[, !(colnames(training_data) %in% delete)]
x_test_data<- test_data[, !(colnames(test_data) %in% delete)]
dim(x_training_data); dim(x_test_data)

#orthogonal_experiment<- read.csv('orthogonal_experiment_5_factors.csv')
orthogonal_experiment<- expand.grid(1:4, 1:5, 1:5, 1:4, 1:4)[1:10,]# permutation
ntree_list<- c(50, 100, 300, 500)
bag_fraction_list<- c(0.1, 0.3, 0.5, 0.7, 0.9)
mtry_list<- floor(c(0.6, 0.8, 1, 1.3, 1.5)*sqrt(ncol(x_training_data)))
interaction_depth_list<- c(3, 5, 7, 9, 11)
split_method_list<- c("ED", "Chisq", "KL", "Int")

list_parameter<- numeric()
list_auc<- numeric()
list_qini<- numeric()
list_summary_effect<- list()
list_model<- list()
for(k in 1:nrow(orthogonal_experiment)){
	level_comb<- as.numeric(orthogonal_experiment[k,])
	adjust_ntree<- ntree_list[level_comb[1]]
	adjust_bag_fraction<- bag_fraction_list[level_comb[2]]
	adjust_mtry<- mtry_list[level_comb[3]]
	adjust_interaction_depth<- interaction_depth_list[level_comb[4]]
	adjust_split_method<- split_method_list[level_comb[5]]
	list_parameter<- rbind(list_parameter , c(k, adjust_ntree, adjust_bag_fraction, adjust_mtry, adjust_interaction_depth, adjust_split_method))

	set.seed(1)
	fit_RF<- upliftRF(x=x_training_data,
					y=training_data$response,
					ct=training_data$treatment,
					ntree=adjust_ntree,
					mtry=adjust_mtry,
					bag.fraction=adjust_bag_fraction,
					interaction.depth=adjust_interaction_depth,
					split_method=adjust_split_method,
					verbose=TRUE)

	pred_training <- predict(fit_RF , x_training_data)
	if(flip_label){pred_training[,2]<- 1 - pred_training[,2]} # change back to the possibility of response when flip-labeled
	training_perf <- performance(pred_training[,1], pred_training[,2], training_original_respose , training_data$treatment, direction=perf_direction, groups=10)
	qini_training <- qini(training_perf, direction=perf_direction, plotit=F)

	pred_test <- predict(fit_RF, x_test_data)
	if(flip_label){pred_test[,2]<- 1 - pred_test[,2]} # change back to the possibility of response when flip-labeled
	test_perf <- performance(pred_test[,1], pred_test[,2], test_data$response , test_data$treatment, direction=perf_direction, groups=10)
	qini_test <- qini(test_perf, direction=perf_direction, plotit=F)
	re_qini<- c(qini_training$Qini , qini_test$Qini)
	list_qini<- rbind(list_qini , c(k , re_qini))
	
	training_obs<- ifelse(training_data$treatment==1, pred_training[,1], pred_training[,2])
	training_sim<- ifelse(training_data$treatment!=1, pred_training[,1], pred_training[,2])
	training_summary<- summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_original_respose, direction=perf_direction)
	test_obs<- ifelse(test_data$treatment==1, pred_test[,1], pred_test[,2])
	test_sim<- ifelse(test_data$treatment!=1, pred_test[,1], pred_test[,2])
	test_summary<- summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response, direction=perf_direction)
	summary_effect<- rbind(training_summary , test_summary)

	# AUC
	AUC<- c(auc(training_original_respose , training_obs) , auc(test_data$response, test_obs))
	report<- cbind(summary_effect , AUC=rep(AUC, each=2) , Qini=rep(re_qini, each=2))
	list_auc<- rbind(list_auc , c(k , AUC))
	eval(parse(text = paste('list_summary_effect$k', k, '<- report' , sep='')))
}

# output reports
AUC_Qini<- cbind(list_parameter , list_auc , list_qini)
rownames(AUC_Qini)<- NULL
colnames(AUC_Qini)<- c('k', 'ntree', 'bag.fraction', 'mtry', 'interaction.depth', 'split_method', 'AUC_training', 'AUC_test', 'Qini_training', 'Qini_test')
AUC_Qini<- as.data.frame(AUC_Qini)
write.csv(AUC_Qini , 'uplift RF experiment summary.csv', row.names=F, quote=F)

OUT<- file('adjust RF experiment detail.txt' , 'w')
for(k in 1:100){
	parameter<- list_parameter[list_parameter[,1]==k,2:5]
	eval(parse(text = paste('report<- list_summary_effect$k', k , sep='')))
	parameter_str<- paste(paste('ntree',parameter[1], sep='='), paste('bag.fraction',parameter[2], sep='='), paste('mtry',parameter[3], sep='='), paste('interaction.depth',parameter[4], sep='='), sep='; ')
	writeLines(parameter_str , OUT)
	writeLines(paste('Gilenya_relapse_rate', 'Tecfidera_relapse_rate', 'actual_treatment',
		'expect_Gilenya_relapse_rate', 'expect_Tecfidera_relapse_rate', 'expect_treatment',
		'summary_actual_treatment_effect', 'summary_expect_treatment_effect', 'AUC', 'Qini', sep=',') , OUT)
	for(i in 1:nrow(report)){
		writeLines(paste(report[i,] , collapse=',') , OUT)
	}
	writeLines('' , OUT)
	writeLines('' , OUT)
}
close(OUT)

# analyse which factor effects over-fitting of AUC and Qini
x<- read.csv('uplift RF experiment summary.csv')
x<- x[!is.na(x$Qini_training),]
split_method_KL<- ifelse(x$split.method=='KL', 1, 0)
split_method_ED<- ifelse(x$split.method=='ED', 1, 0)
split_method_Chisq<- ifelse(x$split.method=='Chisq', 1, 0)
split_method_Int<- ifelse(x$split.method=='Int', 1, 0)
model_data<- data.frame(split_method_KL,split_method_ED,split_method_Chisq,split_method_Int,
	ntree=x$ntree, bag.fraction=x$bag.fraction, mtry=x$mtry, interaction.depth=x$interaction.depth, Qini_test=x$Qini_test)
model_data<- as.data.frame(apply(model_data , 2 , scale))
fit_qini<- glm(Qini_test ~ ntree+bag.fraction+mtry+interaction.depth+split_method_KL+split_method_ED+split_method_Chisq+split_method_Int, 
			data=model_data, family=gaussian)
coef<- data.frame(coefficient=coef(fit_qini))
p_value<- summary(fit_qini)$coef[, "Pr(>|t|)"]
model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])


#------------------------------------------------------------------------------------------
#	Experiment Uplift Causal Conditional Inference Forest (CCIF)
#	Compare re-labelling response variable with not re-labelling
#	report by decile: average uplift effect, SD and p-value
#------------------------------------------------------------------------------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'persistent'	# post_relapse_persist or persistent
flip_label<- TRUE		# TRUE/FALSE
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)
training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]

# flip-lable the response variable among control patients of training data set
training_original_respose<- training_data$response
if(flip_label){training_data$response[training_data$treatment==0]<- 1 - training_data$response[training_data$treatment==0]}

delete<- c("response", "treatment")
x_training_data<- training_data[, !(colnames(training_data) %in% delete)]
x_test_data<- test_data[, !(colnames(test_data) %in% delete)]
dim(x_training_data); dim(x_test_data)

#orthogonal_experiment<- read.csv('orthogonal_experiment_5_factors.csv')
orthogonal_experiment<- expand.grid(1:4, 1:5, 1:5, 1:4, 1:4)	# permutation
ntree_list<- c(50, 100, 300, 500)
bag_fraction_list<- c(0.1, 0.3, 0.5, 0.7, 0.9)
mtry_list<- floor(c(0.6, 0.8, 1, 1.3, 1.5)*sqrt(ncol(x_training_data)))
interaction_depth_list<- c(3, 5, 7, 9, 11)
pvalue_list<- c(0.05, 0.1, 0.3, 0.5)

list_parameter<- numeric()
list_auc<- numeric()
list_qini<- numeric()
list_summary_effect<- list()
list_model<- list()
for(k in 1:nrow(orthogonal_experiment)){
	level_comb<- as.numeric(orthogonal_experiment[k,])
	adjust_ntree<- ntree_list[level_comb[1]]
	adjust_bag_fraction<- bag_fraction_list[level_comb[2]]
	adjust_mtry<- mtry_list[level_comb[3]]
	adjust_interaction_depth<- interaction_depth_list[level_comb[4]]
	adjust_pvalue<- pvalue_list[level_comb[5]]
	list_parameter<- rbind(list_parameter , c(k, adjust_ntree, adjust_bag_fraction, adjust_mtry, adjust_interaction_depth, adjust_pvalue))

	set.seed(11)
	fit_ccif<- ccif(x=x_training_data,
			y=training_data$response,
			ct=training_data$treatment,
			ntree=adjust_ntree,
			mtry=adjust_mtry,
			bag.fraction=adjust_bag_fraction,
			interaction.depth=adjust_interaction_depth,
			pvalue=adjust_pvalue,
			split_method= "KL",
			verbose=TRUE)
	eval(parse(text = paste('list_model$k', k, '<- fit_ccif' , sep='')))

	pred_training <- predict(fit_ccif , x_training_data)
	if(flip_label){pred_training[,2]<- 1 - pred_training[,2]} # change back to the possibility of response when flip-labeled
	training_perf <- performance(pred_training[,1], pred_training[,2], training_original_respose , training_data$treatment, direction=perf_direction, groups=10)
	qini_training <- qini(training_perf, direction=perf_direction, plotit=F)

	pred_test <- predict(fit_ccif, x_test_data)
	if(flip_label){pred_test[,2]<- 1 - pred_test[,2]} # change back to the possibility of response when flip-labeled
	test_perf <- performance(pred_test[,1], pred_test[,2], test_data$response , test_data$treatment, direction=perf_direction, groups=10)
	qini_test <- qini(test_perf, direction=perf_direction, plotit=F)
	re_qini<- c(qini_training$Qini , qini_test$Qini)
	list_qini<- rbind(list_qini , c(k , re_qini))
	
	training_obs<- ifelse(training_data$treatment==1, pred_training[,1], pred_training[,2])
	training_sim<- ifelse(training_data$treatment!=1, pred_training[,1], pred_training[,2])
	training_summary<- summary_treatment_effect(observe=training_obs, simulate=training_sim, treatment=training_data$treatment, response=training_original_respose, direction=perf_direction)
	test_obs<- ifelse(test_data$treatment==1, pred_test[,1], pred_test[,2])
	test_sim<- ifelse(test_data$treatment!=1, pred_test[,1], pred_test[,2])
	test_summary<- summary_treatment_effect(observe=test_obs, simulate=test_sim, treatment=test_data$treatment, response=test_data$response, direction=perf_direction)
	summary_effect<- rbind(training_summary , test_summary)
	
	# AUC
	AUC<- c(auc(training_data$response, training_obs) , auc(test_data$response, test_obs))
	list_auc<- rbind(list_auc , AUC)
	report<- cbind(summary_effect , AUC=rep(AUC, each=2) , Qini=rep(re_qini, each=2))
	eval(parse(text = paste('list_summary_effect$k', k, '<- report' , sep='')))
}


#------------------------------------------------------------------------------------------
#	Experiment SVM
#	make sure training data has the same distribution of treatment/control: Under equal proportion of control and treated observations, it is easy to prove that 2*Prob(z = 1|x) - 1 = Prob(y = 1|treated,x) - Prob(y = 1|control,x)
#	Compare re-labelling response variable with not re-labelling
#	report by decile: average uplift effect, SD and p-value
#------------------------------------------------------------------------------------------

# setting: flip-label or not, modelling post_relapse_persist or persistent, performance direction
response_variable<- 'persistent'	# post_relapse_persist or persistent
# set perform direction as 2 when modelling relapse(negative), 1 when modelling persistent(positive)
perf_direction<- ifelse(response_variable=='persistent', 1, ifelse(response_variable=='post_relapse_persist', 2, NA))

raw_data$response<- raw_data[,names(raw_data)==response_variable]
covar_list<- setdiff(total_covar_list , c(detail_covar , 'der_sex_male', 'persistent'))
if(response_variable == 'persistent'){covar_list<- setdiff(covar_list , c('persist_days'))} # when modelling persistent, don't include persist_days in the model
model_variable<- setdiff(c('response', 'treatment', covar_list) , constant_covar)

list_pvalue<- numeric()
list_odds_ratio<- numeric()
list_svm_parameter<- list()
list_svm_uplift<- list()
list_svm_qini<- list()
for(k in 1:5){
	training_sample_proportion<- 0.5
	training<- numeric(length=nrow(raw_data))
	case<- which(raw_data$treatment==1)
	control<- which(raw_data$treatment==0)
	#set.seed(k)
	index_case<- sample(case , round(length(case)*training_sample_proportion))
	#set.seed(k)
	index_control<- sample(control , round(length(control)*training_sample_proportion))
	training[c(index_case , index_control)]<- 1
	raw_data$training<- training

	training_data<- raw_data[raw_data$training==1 , match(model_variable, names(raw_data))]
	test_data<- raw_data[raw_data$training==0 , match(model_variable, names(raw_data))]

	# re-coding the response variable among control patients of training data set
	training_original_respose<- training_data$response
	training_data$response[training_data$treatment==0]<- 1 - training_data$response[training_data$treatment==0]
	table(training_data$treatment , training_data$response); table(training_data$treatment , training_original_respose)

	delete<- c("response" , "treatment")
	x_training_data<- training_data[, !(colnames(training_data) %in% delete), drop=FALSE]
	x_test_data<- test_data[, !(colnames(test_data) %in% delete), drop=FALSE]
	dim(x_training_data); dim(x_test_data)

	svmtype <- "C-classification"
	prmv <- c(10, 100)
	gammav <- c(0.1, 1, 2, 4)
	index <- 1
	kertype <- "linear"
	for (iprm in 1 : length(prmv)){
		svm_model<- svm(x=x_training_data, y=training_data$response, scale=FALSE, type=svmtype, cost=prmv[iprm], kernel=kertype, probability=TRUE, cross=0)

		pred_svm <- predict(svm_model, x_test_data, decision.values=TRUE, probability=TRUE)
		pred_test <- attr(pred_svm , "probabilities")[,2]
		#Under equal proportion of control and treated observations, it is easy to prove that 2*Prob(z = 1|x) - 1 = Prob(y = 1|treated,x) - Prob(y = 1|control,x)
		svm_perf <- performance(2*pred_test-1, rep(0, length(pred_test)), test_data$response, test_data$treatment, direction=perf_direction, groups=5)
		Qini<- qini(svm_perf , direction=perf_direction , plot=F)$Qini
		eval(parse(text=paste('list_svm_parameter$p', index, '<- rbind(list_svm_parameter$p', index, ' , c(kertype, prmv[iprm], NA))', sep='')))
		eval(parse(text=paste('list_svm_uplift$p', index, '<- rbind(list_svm_uplift$p', index, ' , svm_perf[,8])', sep='')))
		eval(parse(text=paste('list_svm_qini$p', index, '<- rbind(list_svm_qini$p', index, ' , Qini)', sep='')))
		index <- index + 1
	}
	
	kertype <- "radial"
	index<- 3
	for (iprm in 1 : length(prmv)){
		for (ig in 1:length(gammav)){
			sigma = gamZZZZZZmav[ig]
			svm_model <- svm(x=x_training_data, y=training_data$response, scale=FALSE, type=svmtype, cost=prmv[iprm], kernel=kertype, gamma=sigma, probability=TRUE, cross=0)
			pred_svm <- predict(svm_model, x_test_data, decision.values=TRUE, probability=TRUE)
			pred_test <- attr(pred_svm, "probabilities")[,2]
				
			#Under equal proportion of control and treated observations, it is easy to prove that 2*Prob(z = 1|x) - 1 = Prob(y = 1|treated,x) - Prob(y = 1|control,x)
			svm_perf <- performance(2*pred_test-1, rep(0, length(pred_test)), test_data$response, test_data$treatment, direction=perf_direction, groups=5)
			Qini<- qini(svm_perf , direction=perf_direction, plot=F)$Qini
			eval(parse(text=paste('list_svm_parameter$p', index, '<- rbind(list_svm_parameter$p', index, ' , c(kertype, prmv[iprm], gammav[ig]))', sep='')))
			eval(parse(text=paste('list_svm_uplift$p', index, '<- rbind(list_svm_uplift$p', index, ' , svm_perf[,8])', sep='')))
			eval(parse(text=paste('list_svm_qini$p', index, '<- rbind(list_svm_qini$p', index, ' , Qini)', sep='')))
			index <- index + 1
		}
	}
}

