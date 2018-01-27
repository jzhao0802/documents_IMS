#==========================================================================================
#    Project: Compare treatment response of Gilenya with Tecfidera

#	Part II: create new vaibles and create table1 - table4

#	Develop time: 12/10/2014 - .

#	Developer: Zhao Jie
#==========================================================================================


###################################################
######### step01:create variable list ######################
#####################################################
library(glmnet)
#Collapse following if small N e.g. <100 obs based on whole sample:
group1_covar<- c(
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
    'idx_prodtype_S' ,   
    'idx_spec_01',
    'idx_spec_02',
    'idx_spec_03',
    'idx_spec_04'
)
group1_covar_distribution<- apply(raw_data[,match(group1_covar , names(raw_data))] , 2 , sum)
group1_covar_retain<- names(group1_covar_distribution[group1_covar_distribution > 100])
group1_covar_drop<- names(group1_covar_distribution[group1_covar_distribution < 100])


#Collapse if N<100 on the whole of the sample:
group2_covar<- c(
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
group2_covar_distribution<- apply(raw_data[,match(group2_covar , names(raw_data))] , 2 , sum)
group2_covar_retain<- names(group2_covar_distribution[group2_covar_distribution > 100])
group2_covar_drop<- names(group2_covar_distribution[group2_covar_distribution < 100])


#Drop if small N for the following e.g. < 100 obs for whole sample:
group3_covar<- c(
    'pre_ampyra1',
    'pre_mri_any',
    'pre_90_cort_any',
    'pre_cort_oral',
    'pre_cort_iv',
    'pre_relapse'
)
group3_covar_distribution<- apply(raw_data[,match(group3_covar , names(raw_data))] , 2 , sum)
group3_covar_retain<- names(group3_covar_distribution[group3_covar_distribution > 100])
group3_covar_drop<- names(group3_covar_distribution[group3_covar_distribution < 100])

#split at median e.g. include binary flag for above median; reference = below median

#raw_data$pchrlson_upmedian<- ifelse(raw_data$pchrlson >= median(raw_data$pchrlson) , 1 , 0)
raw_data$num_pre_meds_upmedian<- ifelse(raw_data$num_pre_meds >= median(raw_data$num_pre_meds) , 1 , 0)
raw_data$num_pre_op_dx_upmedian<- ifelse(raw_data$num_pre_op_dx >= median(raw_data$num_pre_op_dx) , 1 , 0)
raw_data$pre_non_ms_total_allowed_upmedian<- ifelse(raw_data$pre_non_ms_total_allowed >= median(raw_data$pre_non_ms_total_allowed) , 1 , 0)
raw_data$pre_ms_total_allowed_upmedian<- ifelse(raw_data$pre_ms_total_allowed >= median(raw_data$pre_ms_total_allowed) , 1 , 0)
group4_covar <- c('num_pre_meds_upmedian','num_pre_op_dx_upmedian','pre_non_ms_total_allowed_upmedian','pre_ms_total_allowed_upmedian')
raw_data$pchrlson_1 <- ifelse(raw_data$pchrlson ==1,1,0)
raw_data$pchrlson_2 <- ifelse(raw_data$pchrlson >=2,1,0)
raw_data$pchrlson_3 <- ifelse(raw_data$pchrlson ==0,1,0)
pchrlson <- c('pchrlson_2','pchrlson_3')
variable_list <- c('treatment','age','der_sex_female','pre_rx_any',group1_covar_retain,group2_covar_retain,group3_covar_retain,group4_covar,pchrlson)
variable_retain <- raw_data[,as.vector(match(variable_list,names(raw_data)))]

#56
#varible list creating complete################

########variable distribution#####################
OUT<- file('variable_distribution_v1.txt' , 'w')
for(i in variable_list){
    eval(parse(text = paste('x<- raw_data$' , i , sep='')))
    re<- table(x,raw_data$persistent)
    nm_row<- rownames(re)
    nm_col <- colnames(re)
    #pc<- sprintf('%.2f' , re/length(x)*100)
    writeLines(paste(i, 'Frequency', 'Percent' , sep='\t\t') , OUT)
    writeLines(paste(rep('-' , 100) , collapse='') , OUT)
    writeLines(paste(i,'response',sep='\t\t'),OUT)
    pc <- matrix(0,nrow=nrow(re),ncol=ncol(re))
    for(j in 1:nrow(re)){
        for (h in 1:ncol(re)){
            pc[j,h] <- sprintf('%.2f', re[j,h]/sum(re[j,]))
            writeLines(paste(nm_row[j],nm_col[h],re[j,h] , pc[j,h] , sep='\t\t\t\t') , OUT)
        }
        #writeLines(paste(j,re[j,],pc[j],sep='\t\t'),OUT)
        
    }
    writeLines('' , OUT)
}
close(OUT)

#--------------------create table 1 for description statistics---------------------------#
# align variable description to final report

OUT <- file('variable_distribution_v2.txt' , 'w')
#x_freq_matrix <- matrix(0,nrow=length(variable_list),ncol=2)
for(i in variable_list){
    eval(parse(text = paste('x<-raw_data$',i,sep='')))
    
    re_x <- table(x,raw_data$persistent)
    #x_freq_matrix[i,] <-  apply(re_x,2,sum)
    x_freq<-apply(re_x,1,sum)
    response_freq <- apply(re_x,2,sum)
    class_response <- ncol(re_x)
    class_x <- nrow(re_x)
    writeLines(paste(i,'Frequency',sep='\t\t'),OUT)
    writeLines(paste(rep('-' , 100) , collapse='') , OUT)
    #response_rate <- c(NA,NA)
    #freq_rate <- cbind(rownames(re_x),x_freq,response_rate)
    for (j in 1:class_response){
        writeLines(paste(colnames(re_x)[j],response_freq[j],sep='\t\t\t'),OUT)
    }
    
    #for binary variable to calcualte response rate per variable class
    if (nrow(re_x)==2){
        response_rate <-sprintf('%.2f',re_x[,2]/x_freq*100)
        writeLines('',OUT)
        writeLines(paste('response','response rate',sep='\t\t'),OUT)
        
        for (h in 1:class_x){
            writeLines(paste(rownames(re_x)[h],response_rate[h],sep='\t\t\t'),OUT)  
        }
        
    }
    
    #writeLines(freq_rate,sep='\t\t',OUT)
    
    writeLines('',OUT)
}
close(OUT)


#------frequency for variable for 50/50 run------------------#

OUT<- file('variable_distribution_base3348_qc.txt' , 'w')
for(i in model_variable){
    eval(parse(text = paste('x<- raw_data$' , i , sep='')))
    re<- table(x)
    nm_row<- rownames(re)
    #pc<- sprintf('%.2f' , re/length(x)*100)
    if (length(re)==2){
        writeLines(paste(i, 'Frequency' , sep='\t\t') , OUT)
        writeLines(paste(rep('-' , 120) , collapse='') , OUT)
        for (j in 1:length(re)){
            writeLines(paste(names(re)[j],re[j],sep='\t\t'),OUT)
        }
        
    }
    else{
        m <- sprintf('%.2f', mean(x))
        sd <- sprintf('%.2f', sd(x))
        stat <- c(m, sd)
        stat_name <- c('MEAN', 'SD')
        writeLines(paste(i, 'STAT',sep='\t\t'), OUT)
        writeLines(paste(rep('-' , 100) , collapse='') , OUT)
        
        for (h in 1:2){
            writeLines(paste(stat_name[h],stat[h],sep='\t\t'), OUT)
        }
    }
    writeLines('' , OUT)
}
close(OUT)


#------------table1 for the 'Tables for Overfitting 16 December 2014'--------------#
var_description<- read.csv('variable definition.csv')
var_def_list <- as.vector(var_description$Variable.Definition)
AllVarList <- c(variable_list, 'persistent', 'post_relapse_persist')
OUT<- file('variable_distribution_byResponse.txt' , 'w')
for (i in AllVarList){
    eval(parse(text = paste('x<- raw_data$' , i , sep='')))
    re <- table(x)
    var_def <- var_def_list[match(i, var_description$Covariate)]
    
    if (i %in% c('persistent', 'post_relapse_persist')){
        #writeLines('response variables', OUT)
        #writeLines(paste('variable', 'description', 'positive class', 'negative class'), OUT)
        writeLines(paste(i, var_def, re[2], re[1], sep='\t'), OUT)
    }else{
        if (length(re) > 2) {
            writeLines('Continuous Covariates', OUT)
            #writeLines(paste('variable', 'description', 'Mean', 'Sd'), OUT)
            writeLines(paste(i, var_def, sprintf('%.2f', mean(x)), sprintf('%.2f', sd(x)), sep='\t'), OUT)
        }else{
            #writeLines('Binary Covariates', OUT)
            #writeLines(paste('variable', 'description', 'positive class', 'negative class'), OUT)
            writeLines(paste(i, var_def, re[2], re[1], sep='\t'), OUT)
        }
    }
    #writeLines('', OUT)
    
}
close(OUT)

#resp <- 1
#prop <- 1
#run_fourRuns<- function(select_response, proportion, run_index){
#model_variable<- short_model_variable  # or long_model_variable
response_list <- c('persistent','post_relapse_persist')
sample_proportion_list <- c(0.5, 400/nrow(raw_data))
list_table1 <- list()
list_table2 <- list()
list_table3 <- list()
list_table4 <- list()
fold_num <- 10
rep_num <-100
#resp <-1
#prop <-1
for (resp in 1:2){
    for (prop in 1:2){
        response_select <- response_list[resp]
        proportion <- sample_proportion_list[prop]
        index <- (resp-1)*2+prop
        
        
        raw_data$response <- raw_data[,names(raw_data)==response_variable]
        model_variable <- c('response', variable_list) # redefine model_variable
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
        
        
        # store results for different runs (run1 - run4 according to response and sample proportion selection)
        
        for(k in 1:rep_num){
            # sample training and test data set, proportionally split response outcome into training and test
            training_sample_proportion<- proportion    # set 0.5 or 400/nrow(raw_data), sample 50% and 200 pairs patients respectively
            training<- numeric(length=nrow(raw_data))
            case<- which(raw_data$response==1)
            control<- which(raw_data$response==0)
            index_case<- sample(case , round(length(case)*training_sample_proportion))
            index_control<- sample(control , round(length(control)*training_sample_proportion))
            training[c(index_case, index_control)]<- 1
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
            k.folds<- fold_num
            foldid<- nrow(training_data)
            foldid[training_data$response==1]<- sample(rep(1:k.folds, length=length(which(training_data$response==1))))
            foldid[training_data$response==0]<- sample(rep(1:k.folds, length=length(which(training_data$response==0))))
            table(training_data$response , foldid) # QC
            list_lasso_foldid<- rbind(list_lasso_foldid , foldid)
            
            training_matrix<- model.matrix(response~., data=training_data)[,-1] # removes intercept term
            test_matrix<- model.matrix(response~., data=test_data)[,-1]
            initial_lambda<- glmnet(x=training_matrix, y=training_data$response, family="binomial", alpha=1, standardize=F)$lambda
            lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500))
            cv_auc <- matrix(nr=k.folds , nc=length(lambda_seq))
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
                
                cv_auc[i,]<- test_pred_avg
            }
            total_model<- glmnet(x=training_matrix, y=training_data$response, lambda=lambda_seq, family="binomial", alpha=1, standardize=F)
            cv_auc_mean<- apply(cv_auc , 2 , function(x) mean(x[!is.na(x)]))
            optimum_model<- which.max(cv_auc_mean)
            list_lasso_lambda<- rbind(list_lasso_lambda , c(lambda_seq[optimum_model]))
            
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
        
        
        #----------------- create table 1--------------
        table1_std<- data.frame(response_select, proportion, 'standard logistic', mean(list_auc_std[,2]), sd(list_auc_std[,2]) , mean(list_auc_std[,3]), sd(list_auc_std[,3]))
        names(table1_std) <- c('Response', 'Sample proportion', 'Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        table1_stepwise<- data.frame(response_select, proportion, 'stepwise logistic', mean(list_auc_stepwise[,2]), sd(list_auc_stepwise[,2]) , mean(list_auc_stepwise[,3]), sd(list_auc_stepwise[,3]))
        names(table1_stepwise) <- c('Response', 'Sample proportion', 'Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        table1_lasso<- data.frame(response_select, proportion, 'lasso', mean(list_auc_lasso[,2]), sd(list_auc_lasso[,2]) , mean(list_auc_lasso[,3]), sd(list_auc_lasso[,3]))
        names(table1_lasso) <- c('Response', 'Sample proportion', 'Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        table1<- data.frame(rbind(table1_std , table1_stepwise , table1_lasso))
        table1$overfitting<- (table1$Mean_AUC_training - table1$Mean_AUC_test)/table1$Mean_AUC_test
        eval(parse(text=paste('list_table1$run', index, '=table1', sep='' )))
        
        #out_table <- paste('table1_run', index,'.csv',sep='')
        #        }
        #   }
        #write.csv(list_table1, 'out_table1', quote=F, row.names=F)
        
        row1 <- c( 1, 2)
        row2 <- c( 3, 7)
        row <- rbind(row1, row2)
        
        
        #----------------- create table 2--------------
        pvalue_thresh<- 0.05
        table2_temp<- list()
        for(k in 1:rep_num){
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
                }else{
                    sign_stepwise<- ifelse(model_stepwise$p_value[rownames(model_stepwise)==var] <= pvalue_thresh , 1 , 0)
                    OR_stepwise<- model_stepwise$odds_ratio[rownames(model_stepwise)==var]
                }
                
                retain_lasso<- ifelse(var %in% rownames(model_lasso) , 1 , 0)
                if(retain_lasso == 0){
                    sign_lasso<- NA
                    OR_lasso<- NA
                } else{
                    sign_lasso<- ifelse(model_lasso$p_value[rownames(model_lasso)==var] <= pvalue_thresh , 1 , 0)
                    OR_lasso<- model_lasso$odds_ratio[rownames(model_lasso)==var]
                }
                
                    eval(parse(text = paste('table2_temp$', var, '<- rbind(table2_temp$', var, ' , c(sign_std, OR_std, 
                                            retain_stepwise, sign_stepwise, OR_stepwise, 
                                            retain_lasso, sign_lasso, OR_lasso))', sep='')))
            }
            }
        
        table2<- NULL
        for(var in setdiff(model_variable , 'response')){
            eval(parse(text = paste('re_var<- table2_temp$', var, sep='')))
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
            dif_OR_14<- mean_OR_lasso - mean_OR_std_lasso    # 14) Difference in odds ratio columns 11 -13
            mean_OR_stepwise_lasso<- mean(re_var[which(re_var[,3]==1 & re_var[,6]==1),5] , na.rm=T) # 15) Mean of odds ratio in stepwise LR when retained by stepwise & Lasso
            dif_OR_16<- mean_OR_lasso - mean_OR_stepwise_lasso	# 16) Difference in odds ratio columns 11 -15
            
            table2<- rbind(table2 , c(num_sign_std, mean_OR_std, SD_OR_std,
                                      num_retain_stepwise, num_sign_stepwise, mean_OR_stepwise, SD_OR_stepwise,
                                      num_retain_lasso, num_sign_lasso, mean_OR_lasso, SD_OR_lasso,
                                      mean_OR_std_lasso, dif_OR_14, mean_OR_stepwise_lasso, dif_OR_16)) # 15
        }
        table2<- data.frame(variable=setdiff(model_variable , 'response') , table2)
        colnames(table2)<- c('variable', 'num_sign_std', 'mean_OR_std', 'SD_OR_std',
                             'num_retain_stepwise', 'num_sign_stepwise', 'mean_OR_stepwise', 'SD_OR_stepwise',
                             'num_retain_lasso', 'num_sign_lasso', 'mean_OR_lasso', 'SD_OR_lasso',
                             'mean_OR_std_lasso', 'dif_OR_14', 'mean_OR_stepwise_lasso', 'dif_OR_16')
        
        var_description<- read.csv('variable definition.csv')
        table2=data.frame(response=response_select, sample_proportion=proportion, table2 , var_description=var_description$Variable.Definition[match(table2$variable , var_description$Covariate)])
        out_table <- paste('table2_run', index,'.csv',sep='')
        write.csv(table2, out_table, quote=F, row.names=F)
        
        #----------------- create table 3--------------
        #data <- 'training'
        #pred_thresh <- 0.73
        table3_calculate<- function(model , data){
            result<- NULL
            for(k in 1:rep_num){
                eval(parse(text = paste('pred_training<- list_predict_prob_training_', model, '[k,]', sep='')))
                eval(parse(text = paste('pred_data<- list_predict_prob_', data, '_', model, '[k,]', sep='')))
                
                training<- list_training_data[k,]
                training_data<- raw_data[training==1 , match(c('response', 'treatment'), names(raw_data))]
                test_data<- raw_data[training==0 , match(c('response', 'treatment'), names(raw_data))]
                
                # report Classification accuracy on training/test
                eval(parse(text=paste('actual_data<- ', data, '_data', sep='')))
                
                # set cut-off of predictions that predictive outcome rate is the same as actual rate on training data
                pred_thresh<- quantile(pred_training , prob=1 - mean(training_data$response))
                
                num_actual_positive<- sum(actual_data$response) # 3) Number of actual positive cases
                num_actual_negative<- sum(1 - actual_data$response) # 4) Number of actual negative cases
                num_crt_pred_positive<- sum(actual_data$response[pred_data >= pred_thresh]) # 5) No. correct predicted positive
                num_crt_pred_negative<- sum(1 - actual_data$response[pred_data < pred_thresh]) # 6) No. correct predicted negative
                
                true_post_rate<- sum(actual_data$response[pred_data >= pred_thresh]) / num_actual_positive # 7) True positive rate (=sensitivity)
                true_neg_rate<- sum(1 - actual_data$response[pred_data < pred_thresh]) / num_actual_negative # 8) True negative rate (=specificity)
                
                PPV<- sum(actual_data$response[pred_data >= pred_thresh]) / length(which(pred_data >= pred_thresh)) # 9) Precision / PPV
                NPV<- sum(1 - actual_data$response[pred_data < pred_thresh]) / length(which(pred_data < pred_thresh)) # 10) NPV
                rate_accuracy<- (sum(actual_data$response[pred_data >= pred_thresh]) +  sum(1 - actual_data$response[pred_data < pred_thresh])) / nrow(actual_data) # 11) Classification accuracy (proportion of cases predicted correctly)
                
                result<- rbind(result , c(num_actual_positive, num_actual_negative, num_crt_pred_positive, num_crt_pred_negative,
                                          true_post_rate, true_neg_rate, PPV, NPV, rate_accuracy))
            }
            result_avg<- apply(result , 2 , mean)
            final<- c(model, data, result_avg)
            return(final)
        }
        t3_1<- table3_calculate(model='std' , data='training')
        t3_2<- table3_calculate(model='std' , data='test')
        t3_3<- table3_calculate(model='stepwise' , data='training')
        t3_4<- table3_calculate(model='stepwise' , data='test')
        t3_5<- table3_calculate(model='lasso' , data='training')
        t3_6<- table3_calculate(model='lasso' , data='test')
        
        table3<- as.data.frame( rbind(t3_1 , t3_2, t3_3, t3_4, t3_5, t3_6))
        table3<-data.frame(response=response_select, sample_proportion=proportion, table3)
        
        #colnames(table3)<- c('response_variable', 'sample_proportion','model', 'training/test',
        #                    'num_actual_positive', 'num_actual_negative', 'num_crt_pred_positive', 'num_crt_pred_negative',
        #                   'true_post_rate', 'true_neg_rate', 'PPV', 'NPV', 'rate_accuracy')
        #colnames(table3)<- c('model', 'training/test',
        #                    'num_actual_positive', 'num_actual_negative', 'num_pred_positive', 'num_pred_negative',
        #                   'true_post_rate', 'false_post_rate', 'true_neg_rate', 'false_neg_rate',
        #                  'rate_post', 'sensitivity', 'precision', 'rate_accuracy')
        
        rownames(table3)<- NULL
        eval(parse(text=paste('list_table3$run', index, '<- table3', sep='')))
            }
    }
#df_table1 <- as.data.frame(list_table1)
sum_table1 <- as.data.frame(rbind(list_table1$run1,list_table1$run2,list_table1$run3,list_table1$run4))
names(sum_table1) <- c('Response_variable', 'Sample proportion', 'Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
write.csv(sum_table1, 'sum_table1.csv', quote=F, row.names=F)

#sum_table3 <- as.data.frame(list_table3)
sum_table3 <- as.data.frame(rbind(list_table3$run1,list_table3$run2,list_table3$run3,list_table3$run4))
names(sum_table3)<- c('response_variable', 'sample_proportion','model', 'training/test',
                      'num_actual_positive', 'num_actual_negative', 'num_crt_pred_positive', 'num_crt_pred_negative',
                      'true_post_rate', 'true_neg_rate', 'PPV', 'NPV', 'rate_accuracy')
write.csv(sum_table3, 'sum_table3.csv', quote=F, row.names=F)

#run_fourRuns('persistent',400/nrow(raw_data),2)
#run_fourRuns('post_relapse_persist', 0.5, 3)
#run_fourRuns('post_relapse_persist', 400/nrow(raw_data),4)

#------Table 4: Supplemtnary AUC information-----------#
response_select <- 'persistent'
sample_prop <- 0.5
run_index <- 1
AUC_training <- function(response_select,sample_prop,run_index){
    response_variable <- response_select
    raw_data$response <- raw_data[,names(raw_data)==response_variable]
    model_variable <- c('response',variable_list)
    list_training_data<- numeric()
    list_auc_avg <- numeric()
    list_auc_total <- numeric()
    for(k in 1:100){
        # store results for standard logistic regression
        list_model_std<- list()
        list_auc_std<- numeric()
        list_predict_prob_training_std<- numeric()
        list_predict_prob_test_std<- numeric()
        list_std_foldid<- list()
        
        # sample training and test data set, proportionally split response outcome into training and test
        training_sample_proportion<- sample_prop	# set 0.5 or 400/nrow(raw_data), sample 50% and 200 pairs patients respectively
        training<- numeric(length=nrow(raw_data))
        case<- which(raw_data$response==1)
        control<- which(raw_data$response==0)
        index_case<- sample(case , round(length(case)*training_sample_proportion))
        index_control<- sample(control , round(length(control)*training_sample_proportion))
        training[c(index_case , index_control)]<- 1
        table(training , raw_data$response)
        
        list_training_data<- rbind(list_training_data , training)
        
        training_data<- raw_data[training==1 , match(model_variable, names(raw_data))]
        test_data<- raw_data[training==0 , match(model_variable, names(raw_data))]
        
        
        #------------- std logistic: based on cross-validation on training data------------------
        k.folds<- 10
        foldid<- nrow(training_data)
        foldid[training_data$response==1]<- sample(rep(1:k.folds, length=length(which(training_data$response==1))))
        foldid[training_data$response==0]<- sample(rep(1:k.folds, length=length(which(training_data$response==0))))
        table(training_data$response , foldid) # QC
        list_std_foldid<- rbind(list_std_foldid , foldid)
        
        #training_matrix<- model.matrix(response~., data=training_data)[,-1] # removes intercept term
        #test_matrix<- model.matrix(response~., data=test_data)[,-1]
        #initial_lambda<- glmnet(x=training_matrix, y=training_data$response, family="binomial", alpha=1, standardize=F)$lambda
        #lambda_seq<- c(initial_lambda[-length(initial_lambda)] , seq(initial_lambda[length(initial_lambda)] , 0 , length=500))
        
        for(i in 1:k.folds){
            cv_training_data<- training_data[foldid!=i,]
            #cv_training_matrix<- model.matrix(response~., data=cv_training_data)[,-1]
            cv_test_data<- training_data[foldid==i,]
            #cv_test_matrix<- model.matrix(response~., data=cv_test_data)[,-1]
            
            
            fit_std<- glm(response~., data=cv_training_data, family=binomial)
            training_pred<- predict(fit_std, cv_training_data, type="response")
            test_pred<- predict(fit_std, cv_test_data, type="response")
            #test_pred_avg<- apply(test_pred, 2, function(x){auc(cv_test_data$response , x)})
            #test_pred_avg<- c(test_pred_avg , rep(NA , length(lambda_seq) - length(test_pred_avg))) #some small lambda may not be reached
            
            training_auc<- auc(cv_training_data$response , training_pred)
            test_auc<- auc(cv_test_data$response , test_pred)
            list_auc<- rbind(list_auc_std , c(training_auc , test_auc))
            list_auc_total <- rbind(list_auc_total,test_auc)
            #eval(parse(text = paste('list_model_std$k', k, '<- model' , sep='')))
            
            #list_predict_prob_training_std<- rbind(list_predict_prob_training_std , training_pred)
            #list_predict_prob_test_std<- rbind(list_predict_prob_test_std , test_pred)
            
        }
        auc_foldAvg <- apply(list_auc_total, 2, mean)
        list_auc_avg <- rbind(list_auc_avg, auc_foldAvg)
    }
    auc_avg <- apply(list_auc_avg, 2, mean)
    auc_SD <- apply(list_auc_avg,2, sd)
    #auc_avg <- apply(list_auc_total, 2, mean)
    #auc_SD <- apply(list_auc_total,2, sd)
    auc <- cbind(auc_avg,auc_SD)
    eval(parse(text = paste('AUC_model_', run_index,'<<- cbind(auc_avg,auc_SD)',sep='')))
    #auc
    #eval(parse(text = paste('return(AUC_model_', run_index,')',sep='')))
    #return (auc)
}
AUC_training('persistent', 0.5, run_index=1)
AUC_training('persistent', 400/nrow(raw_data), 2)
AUC_training('post_relapse_persist', 0.5, 3)
AUC_training('post_relapse_persist', 400/nrow(raw_data), 4)

AUC_final <- rbind(AUC_model_1,AUC_model_2,AUC_model_3,AUC_model_4)
rownames(AUC_final) <- c("pers_0.5","pers_0.2","rel_0.5","rel_0.2")
write.csv(AUC_final,"table4_AUC_training_stdlog.csv")
rm(auc_model_1)

write.csv(list_table1, 'out_table1', quote=F, row.names=F)
