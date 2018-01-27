#==========================================================================================
#	Project: Compare treatment response of Gilenya with Tecfidera

#	Part II: Model Estimation via Spec v6

#	Develop time: 12/12/2014 - .

#	Developer: Hui Jin
#==========================================================================================

#------------------------------------------------------------------------------------------
#	Experiment over-fitting: Logistic regression, Stepwise and Lasso
#	Modelling persistent without interaction terms
# There are 4 different *primary* runs
# Run1: Outcome=persistent, training / test= ~1600/1600
# Run2: Outcome=persistent, training / test= ~400/2800
# Run2: Outcome=relapse, training / test= ~1600/1600
# Run2: Outcome=relapse, training / test= ~400/2800
#------------------------------------------------------------------------------------------
library(glmnet)

#Generating raw_data by loading 01 code----------------------------------------------------
#source('D:/MS_BRACE_Hui/002code/01. data transformation_hui.r')
#write.csv(raw_data,'raw_data_finalized.csv',quote=F, row.names=F)

raw_data<-read.csv('D:\\working materials\\presentation\\Gilenya vs Tecfidera POC\\output\\raw_data_finalized.csv',header=T)

out_path<-'D:\\working materials\\presentation\\Gilenya vs Tecfidera POC\\output'
setwd(out_path)

# Create Variable list for Experiment v5
#----------------Variable List----------------------------------------------------------------------------
# Collapse if small N e.g. <100 obs based on whole sample:
group1_covar<- c(
	'pre_ampyra1',
	'pre_mri_any',
	'pre_90_cort_any',
	'pre_cort_oral',
	'pre_cort_iv',
	'pre_relapse',
	'pat_region_E',
	'pat_region_MW',
	'pat_region_S',
	'idx_paytype_C',
	'idx_paytype_M',
	'idx_paytype_R',
	'idx_paytype_S',
	'idx_prodtype_D',
	'idx_prodtype_H',
	'idx_prodtype_I',
	'idx_prodtype_P',
	'idx_prodtype_S',
	'idx_spec_01',
	'idx_spec_02',
	'idx_spec_03',
	'idx_spec_04',
  'pre_comor1',
	'pre_comor2',
	'pre_comor3',
	'pre_comor4',
	'pre_comor5',
	'pre_comor6',
	'pre_comor7',
	'pre_comor8',
	'pre_comor9',
	'pre_comor10',
	'pre_comor11',
	'pre_comor12',
	'pre_comor13',
	'pre_comor14',
	'pre_comor15',
	'pre_comor16',
	'pre_comor17',
	'pre_comor19',
	'pre_comor20',
	'pre_comor22',
	'pre_comor23',
	'pre_comor24',
	'pre_comor25',
	'pre_comor26',
	'pre_comor27',
	'pre_comor28',
	'pre_comor29',
	'pre_comor30',
	'pre_comor31',
	'pre_comor32',
	'pre_comor33'
	
)
group1_covar_distribution<- apply(raw_data[,match(group1_covar , names(raw_data))] , 2 , sum)
group1_covar_retain<- names(group1_covar_distribution[group1_covar_distribution > 100])

# split at median e.g. include binary flag for above median; reference = below median:
# Create binary flags for pchrlson, put pchrlson=0 as reference
raw_data$num_pre_meds_upmedian<- ifelse(raw_data$num_pre_meds >= median(raw_data$num_pre_meds) , 1 , 0)
raw_data$num_pre_op_dx_upmedian<- ifelse(raw_data$num_pre_op_dx >= median(raw_data$num_pre_op_dx) , 1 , 0)
raw_data$pre_non_ms_total_allowed_upmedian<- ifelse(raw_data$pre_non_ms_total_allowed >= median(raw_data$pre_non_ms_total_allowed) , 1 , 0)
raw_data$pre_ms_total_allowed_upmedian<- ifelse(raw_data$pre_ms_total_allowed >= median(raw_data$pre_ms_total_allowed) , 1 , 0)
group2_covar<- c('pchrlson_2','pchrlson_3','num_pre_meds_upmedian', 'num_pre_op_dx_upmedian', 'pre_non_ms_total_allowed_upmedian', 'pre_ms_total_allowed_upmedian')


# the total list of variables including treatment and response
model_variable_v5<- c('response', 'treatment',  'age', 'der_sex_female', 'pre_rx_any', group1_covar_retain, group2_covar)
variable_list <- setdiff(model_variable_v5,'response')
#if(response_variable == 'persistent'){model_variable_v5<- setdiff(model_variable_v5 , c('persist_days'))} # when modelling persistent, don't include persist_days in the model

#------------------Create Table 1--------------------------------------
# One table for relapes on one teble for persistent
# Descriptive statistics should show frequency and mean for each variable by each class of the outcome variable
#----------------------------------------------------------------------
# Choosing the response variable: persistent or post_relapse_persist
#response_variable<- 'persistent'  #value: post_relapse_persist or persistent
#response_variable<- 'post_relapse_persist'
#
Des_stat<-function(response_variable){
  raw_data$response<- raw_data[,names(raw_data)==response_variable]
  model_variable<- model_variable_v5  # model variable v5
  model_variable<-setdiff(model_variable,'response')
  
  OUT<- file('Descriptive_statistics_on_response_modify.txt' , 'w')
  for(i in model_variable){
    eval(parse(text = paste('x<- raw_data$' , i , sep='')))
    re<- table(x,raw_data$response)
    nm<- rownames(re)
    mean0<-tapply(raw_data$response,x,mean)
    writeLines(paste(i, 'response=0', 'response=1' ,'rate', sep='\t\t') , OUT)
    writeLines(paste(rep('-' , 150) , collapse='') , OUT)
    
    for(j in 1:nrow(re)){
      writeLines(paste(nm[j] , re[j,1]  ,re[j,2], round(mean0[j],4),sep='\t\t') , OUT)
    }
    writeLines('' , OUT)
  }
  close(OUT) 
}


Des_stat('persistent')
Des_stat('post_relapse_persist')
# end for 20141210

# Start on 20141211
#-----------------Create table2 and 3---------------------------------
#------------------ set object to store model output ------------------

table23<-function(res_var,sam_pro){
  
  #response_variable<- res_var  #value: post_relapse_persist or persistent
  raw_data$response<- raw_data[,names(raw_data)==res_var]
  sam_pro1<-ifelse(sam_pro==400/nrow(raw_data),400,0.5)
  res_var1<-ifelse(res_var=='persistent','persistent','relapse')
  model_variable<- model_variable_v5  # model variable v5
  list_training_data<- numeric()
  
  # store results for standard logistic regression
  list_model_std<- list()
  list_auc_std<- numeric()
  list_predict_prob_training_std<- numeric()
  list_predict_prob_test_std<- numeric()
  
  # store results for stepwise logistic regression
  list_model_stepwise<- list()
  list_auc_stepwise<- numeric()
  list_predict_prob_training_stepwise<- numeric()
  list_predict_prob_test_stepwise<- numeric()
  
  # store results for lasso
  list_model_lasso<- list()
  list_auc_lasso<- numeric()
  list_predict_prob_training_lasso<- numeric()
  list_predict_prob_test_lasso<- numeric()
  list_lasso_lambda<- numeric()
  list_lasso_foldid<- list()
  
  
  for(k in 1:100){
    # sample training and test data set, proportionally split response outcome into training and test
    #	training_sample_proportion<- sam_pro	# set 0.5 or 400/nrow(raw_data), sample 50% and 200 pairs patients respectively
    training<- numeric(length=nrow(raw_data))
    case<- which(raw_data$response==1)
    control<- which(raw_data$response==0)
    index_case<- sample(case , round(length(case)*sam_pro))
    index_control<- sample(control , round(length(control)*sam_pro))
    training[c(index_case , index_control)]<- 1
    table(training , raw_data$response)
    
    list_training_data<- rbind(list_training_data , training)
    
    training_data<- raw_data[training==1 , match(model_variable, names(raw_data))]
    test_data<- raw_data[training==0 , match(model_variable, names(raw_data))]
    
    #------------------ standard logistic regression --------------------
    fit<- glm(response~., data=training_data, family=binomial)
    coef<- data.frame(coefficient=coef(fit) , odds_ratio=exp(coef(fit)))
    p_value<- summary(fit)$coef[, "Pr(>|z|)"]
    model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
    
    training_obs<- predict(fit, training_data, type="response")
    test_obs<- predict(fit, test_data, type="response")
    training_auc<- auc(training_data$response , training_obs)
    test_auc<- auc(test_data$response , test_obs)
    list_auc_std<- rbind(list_auc_std , c(k , training_auc , test_auc))
    eval(parse(text = paste('list_model_std$k', k, '<- model' , sep='')))
    
    list_predict_prob_training_std<- rbind(list_predict_prob_training_std , training_obs)
    list_predict_prob_test_std<- rbind(list_predict_prob_test_std , test_obs)
    
    #------------------------- step wise regression --------------------
    fit<- glm(response~., data=training_data, family=binomial)
    step_wise<- step(fit , direction="backward")
    
    coef<- data.frame(coefficient=coef(step_wise) , odds_ratio=exp(coef(step_wise)))
    p_value<- summary(step_wise)$coef[, "Pr(>|z|)"]
    model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
    
    training_obs<- predict(step_wise, training_data, type="response")
    test_obs<- predict(step_wise, test_data, type="response")
    training_auc<- auc(training_data$response , training_obs)
    test_auc<- auc(test_data$response , test_obs)
    list_auc_stepwise<- rbind(list_auc_stepwise , c(k , training_auc , test_auc))
    eval(parse(text = paste('list_model_stepwise$k', k, '<- model' , sep='')))
    
    list_predict_prob_training_stepwise<- rbind(list_predict_prob_training_stepwise , training_obs)
    list_predict_prob_test_stepwise<- rbind(list_predict_prob_test_stepwise , test_obs)
    
    #------------- Lasso: based on cross-validation on training data------------------
    k.folds<- 10
    foldid<- nrow(training_data)
    foldid[training_data$response==1]<- sample(rep(1:k.folds, length=length(which(training_data$response==1))))
    foldid[training_data$response==0]<- sample(rep(1:k.folds, length=length(which(training_data$response==0))))
    table(training_data$response , foldid) # QC
    list_lasso_foldid<- rbind(list_lasso_foldid , foldid)
    
    
    #modify this part to create table 4
    training_matrix<- model.matrix(response~., data=training_data)[,-1] # removes intercept term
    test_matrix<- model.matrix(response~., data=test_data)[,-1]
    initial_lambda<- glmnet(x=training_matrix, y=training_data$response, family="binomial", alpha=1, standardize=F)$lambda
    lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500))
    cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
    #ends
    for(i in 1:k.folds){
      cv_training_data<- training_data[foldid!=i,]
      cv_training_matrix<- model.matrix(response~., data=cv_training_data)[,-1]
      cv_test_data<- training_data[foldid==i,]
      cv_test_matrix<- model.matrix(response~., data=cv_test_data)[,-1]
      
      fit_lasso<- glmnet(cv_training_matrix, cv_training_data$response, 
                         lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
      test_pred<- predict(fit_lasso, cv_test_matrix, type="response")
      test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data$response , x)})
      test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) #some small lambda may not be reached
      
      cv_auc[i,]<- test_pred_avg#calculate the AUC based on left-out fold
    }
    total_model<- glmnet(x=training_matrix, y=training_data$response, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
    cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)]))
    optimum_model<- which.max(cv_auc_mean)
    list_lasso_lambda<- rbind(list_lasso_lambda , c(k , lambda_seq[optimum_model]))
    
    # calculate p-value by re-estimate standard logistic model based on non-zero variables
    model_coef<- total_model$beta[,optimum_model]
    odds_ratio<- exp(model_coef)[model_coef != 0]
    non_zero_var<- names(model_coef)[model_coef != 0]
    re_model_var<- c('response', non_zero_var)
    re_fit<- glm(response~., data=training_data[,match(re_model_var , names(training_data))], family=binomial)
    p_value<- summary(re_fit)$coef[, "Pr(>|z|)"]
    model<- data.frame(coefficient=model_coef[model_coef!=0], odds_ratio, p_value=p_value[match(non_zero_var, names(p_value))])
    eval(parse(text = paste('list_model_lasso$k', k, '<- model' , sep='')))
    
    training_obs<- predict(total_model, training_matrix, type="response")[,optimum_model]
    training_auc<- auc(training_data$response , training_obs)
    test_obs<- predict(total_model, test_matrix, type="response")[,optimum_model]
    test_auc<- auc(test_data$response , test_obs)
    list_auc_lasso<- rbind(list_auc_lasso , c(k , training_auc , test_auc))
    
    list_predict_prob_training_lasso<- rbind(list_predict_prob_training_lasso , training_obs)
    list_predict_prob_test_lasso<- rbind(list_predict_prob_test_lasso , test_obs)
    #---------------------------------------------------------------------------------
  }
  
  #----------------- create table 2--------------
  table2_std<- c(mean(list_auc_std[,2]), sd(list_auc_std[,2]) , mean(list_auc_std[,3]), sd(list_auc_std[,3]))
  table2_stepwise<- c(mean(list_auc_stepwise[,2]), sd(list_auc_stepwise[,2]) , mean(list_auc_stepwise[,3]), sd(list_auc_stepwise[,3]))
  
  table2_lasso<- c(mean(list_auc_lasso[,2]), sd(list_auc_lasso[,2]) , mean(list_auc_lasso[,3]), sd(list_auc_lasso[,3]))
  
  table2<- data.frame(rbind(table2_std , table2_stepwise , table2_lasso))
  colnames(table2)<- c('Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
  table2$overfitting<- (table2$Mean_AUC_training - table2$Mean_AUC_test)/table2$Mean_AUC_test
  write.csv(table2,paste('table2_',res_var1,'_',sam_pro1,'.csv'),quote=F)
  
  
  
  #----------------- create table 3--------------
  pvalue_thresh<- 0.05
  table3_temp<- list()
  for(k in 1:100){
    eval(parse(text = paste('model_std<- list_model_std$k', k, sep='')))
    eval(parse(text = paste('model_stepwise<- list_model_stepwise$k', k, sep='')))
    eval(parse(text = paste('model_lasso<- list_model_lasso$k', k, sep='')))
    for(var in setdiff(model_variable , 'response')){
      sign_std<- ifelse(model_std$p_value[rownames(model_std)==var] <= pvalue_thresh , 1 , 0)
      OR_std<- model_std$odds_ratio[rownames(model_std)==var]
      
      retain_stepwise<- ifelse(var %in% rownames(model_stepwise) , 1 , 0)
      if(retain_stepwise == 0){
        sign_stepwise<- NA
        OR_stepwise<- NA
      }
      else{
        sign_stepwise<- ifelse(model_stepwise$p_value[rownames(model_stepwise)==var] <= pvalue_thresh , 1 , 0)
        OR_stepwise<- model_stepwise$odds_ratio[rownames(model_stepwise)==var]
      }
      
      retain_lasso<- ifelse(var %in% rownames(model_lasso) , 1 , 0)
      if(retain_lasso == 0){
        sign_lasso<- NA
        OR_lasso<- NA
      }
      else{
        sign_lasso<- ifelse(model_lasso$p_value[rownames(model_lasso)==var] <= pvalue_thresh , 1 , 0)
        OR_lasso<- model_lasso$odds_ratio[rownames(model_lasso)==var]
      }
      
      eval(parse(text = paste('table3_temp$', var, '<- rbind(table3_temp$', var, ' , c(sign_std, OR_std, 
                              retain_stepwise, sign_stepwise, OR_stepwise, 
                              retain_lasso, sign_lasso, OR_lasso))', sep='')))
    }
    }
  
  table3<- NULL
  for(var in setdiff(model_variable , 'response')){
    eval(parse(text = paste('re_var<- table3_temp$', var, sep='')))
    num_sign_std<- sum(re_var[,1] , na.rm=T)
    mean_OR_std<- mean(re_var[,2] , na.rm=T)
    SD_OR_std<- sd(re_var[,2] , na.rm=T)
    
    num_retain_stepwise<- sum(re_var[,3])
    num_sign_stepwise<- sum(re_var[,4] , na.rm=T)
    mean_OR_stepwise<- mean(re_var[,5] , na.rm=T)
    SD_OR_stepwise<- sd(re_var[,5] , na.rm=T)
    
    num_retain_lasso<- sum(re_var[,6])
    num_sign_lasso<- sum(re_var[,7] , na.rm=T)
    mean_OR_lasso<- mean(re_var[,8] , na.rm=T) # 11)
    SD_OR_lasso<- sd(re_var[,8] , na.rm=T) # 12)
    
    mean_OR_std_lasso<- mean(re_var[which(re_var[,6]==1),2]) # 13) Mean of odds ratio in standard LR when retained by Lasso LR
    dif_OR_14<- mean_OR_lasso - mean_OR_std_lasso	# 14) Difference in odds ratio columns 11 -13
    mean_OR_stepwise_lasso<- mean(re_var[which(re_var[,3]==1 & re_var[,6]==1),5] , na.rm=T) # 15) Mean of odds ratio in stepwise LR when retained by stepwise & Lasso
    dif_OR_16<- mean_OR_lasso - mean_OR_stepwise_lasso	# 16) Difference in odds ratio columns 11 -15
    
    table3<- rbind(table3 , c(num_sign_std, mean_OR_std, SD_OR_std,
                              num_retain_stepwise, num_sign_stepwise, mean_OR_stepwise, SD_OR_stepwise,
                              num_retain_lasso, num_sign_lasso, mean_OR_lasso, SD_OR_lasso,
                              mean_OR_std_lasso, dif_OR_14, mean_OR_stepwise_lasso, dif_OR_16))
  }
  table3<- data.frame(variable=setdiff(model_variable , 'response') , table3)
  colnames(table3)<- c('variable', 'num_sign_std', 'mean_OR_std', 'D_OR_std',
                       'num_retain_stepwise', 'num_sign_stepwise', 'mean_OR_stepwise', 'SD_OR_stepwise',
                       'num_retain_lasso', 'num_sign_lasso', 'mean_OR_lasso', 'SD_OR_lasso',
                       'mean_OR_std_lasso', 'dif_OR_14', 'mean_OR_stepwise_lasso', 'dif_OR_16')
  
  var_description<- read.csv('variable definition.csv')
  table3=cbind(table3 , var_description=var_description$Variable.Definition[match(table3$variable , var_description$Covariate)])
  write.csv(table3, paste('table3_',res_var1,'_',sam_pro1,'.csv'), quote=F, row.names=F)
}
#ends for 20141211

table23('persistent',0.5)
table23('persistent',400/nrow(raw_data))
table23('post_relapse_persist',0.5)
table23('post_relapse_persist',400/nrow(raw_data))
#ends for 20141211


#Start on 20141212
#-------------Create table 4---------------------------
# store results for lrcv

lrcv<-function(res_var, sam_pro){
  #response_variable<- res_var  #value: post_relapse_persist or persistent
  raw_data$response<- raw_data[,names(raw_data)==res_var]
  sam_pro1<-ifelse(sam_pro==400/nrow(raw_data),400,0.5)
  res_var1<-ifelse(res_var=='persistent','persistent','relapse')
  model_variable<- model_variable_v5  # model variable v5
  
  list_training_data<- numeric()
  list_auc_lrcv<- numeric()
  list_lrcv_foldid<- list()
  
  for(k in 1:100){
    # sample training and test data set, proportionally split response outcome into training and test
    #training_sample_proportion<- sample_propotion	# set 0.5 or 400/nrow(raw_data), sample 50% and 200 pairs patients respectively
    training<- numeric(length=nrow(raw_data))
    case<- which(raw_data$response==1)
    control<- which(raw_data$response==0)
    index_case<- sample(case , round(length(case)*sam_pro))
    index_control<- sample(control , round(length(control)*sam_pro))
    training[c(index_case , index_control)]<- 1
    table(training , raw_data$response)
    
    list_training_data<- rbind(list_training_data , training)
    
    training_data<- raw_data[training==1 , match(model_variable, names(raw_data))]
    test_data<- raw_data[training==0 , match(model_variable, names(raw_data))]
    
    #------------------ standard logistic regression --------------------
    # Tenfold CV--------------------------------------------------------
    k.folds<- 10
    foldid<- nrow(training_data)
    foldid[training_data$response==1]<- sample(rep(1:k.folds, length=length(which(training_data$response==1))))
    foldid[training_data$response==0]<- sample(rep(1:k.folds, length=length(which(training_data$response==0))))
    table(training_data$response , foldid) # QC
    list_lrcv_foldid<- rbind(list_lrcv_foldid , foldid)
    
    #training_matrix<- model.matrix(response~., data=training_data)[,-1] # removes intercept term
    #test_matrix<- model.matrix(response~., data=test_data)[,-1]
    #initial_lambda<- glmnet(x=training_matrix, y=training_data$response, family="binomial", alpha=1, standardize=F)$lambda
    #lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500))
    cv_auc <- matrix(nr=k.folds , nc=1)#length(lambda_seq))
  
    for(i in 1:k.folds){
      cv_training_data<- training_data[foldid!=i,]
      #cv_training_matrix<- model.matrix(response~., data=cv_training_data)[,-1]
      cv_test_data<- training_data[foldid==i,]
      #cv_test_matrix<- model.matrix(response~., data=cv_test_data)[,-1]
      
      # Standard logistic regression
      fit<- glm(response~., data=cv_training_data, family=binomial)
      #coef<- data.frame(coefficient=coef(fit) , odds_ratio=exp(coef(fit)))
      #p_value<- summary(fit)$coef[, "Pr(>|z|)"]
      #model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
      
      test_pred<- predict(fit, cv_test_data, type="response")
      test_pred_avg<- auc(cv_test_data$response , test_pred)
      #test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) #some small lambda may not be reached
      
      cv_auc[i,]<- test_pred_avg#calculate the AUC based on left-out fold
    }
      cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)]))
      list_auc_lrcv<-rbind(list_auc_lrcv,c(k,cv_auc_mean))
          
  }
#----------------- create table 4--------------
table4_lrcv<- c(round(mean(list_auc_lrcv[,2]),7), round(sd(list_auc_lrcv[,2]),7) )
table4<-data.frame(rbind(table4_lrcv))
colnames(table4)<- c('Mean_AUC_training', 'SD_AUC_training')
write.csv(table4,paste('table4_',res_var1,'_',sam_pro1,'.csv'),quote=F)
}

lrcv('persistent',0.5)
lrcv('persistent',400/nrow(raw_data))
lrcv('post_relapse_persist',0.5)
lrcv('post_relapse_persist',400/nrow(raw_data))


