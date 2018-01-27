#Sampling and modelling
raw_data <- read.table('raw_data_recoded.csv', sep=',', head=T)
dim(raw_data) #5670 129

training <- numeric(nrow(raw_data))
response1_idx <- which(raw_data$response == 1)
training_response1_size <- 3/4*length(response1_idx)
set.seed(12345)
response1_training_idx <- sample(response1_idx, training_response1_size)
response0_idx <- which(raw_data$response == 0)
training_response0_size <- 3/4*length(response0_idx)
response0_training_idx <- sample(response0_idx, size=training_response0_size)
idx_training <- c(response1_training_idx, response0_training_idx)
training[idx_training]<- 1
tb <- table(training, raw_data$response)
prop.table(tb, 1)
prop.table(tb, 2)
training_data <- raw_data[training==1,]
test_data <- raw_data[training==0,]
# training and test data setting over

#---------------Modelling V1 ------------------------#
# modelling without pre_index  and index treatment variables
preIndex_index_treatment_vars <- names(raw_data)[grep('Interf$|Glatir$|none$', names(raw_data), perl=TRUE)] 

summary_vars <- names(raw_data)[grep('any$', names(raw_data), perl=TRUE)]
var_list <- setdiff(names(raw_data), summary_vars)
covar_list <- setdiff(names(raw_data), c(summary_vars, 'response','num_post_relapse_1yr')) #121
covar_list_rmTreatment <- setdiff(names(raw_data), c(preIndex_index_treatment_vars, summary_vars, 'response','num_post_relapse_1yr')) #116

# collapse varibles including class less than 100 in training data
covar_distribution_v1 <- apply(training_data[, match(covar_list_rmTreatment, names(training_data))], 2, sum)
covar_collapse_list_v1 <- names(covar_distribution_v1[covar_distribution_v1 < 100])
covar_retain_temp_v1 <- names(covar_distribution_v1[covar_distribution_v1 >= 100]) #100

covar_distribution_v2 <- apply(training_data[, match(covar_list, names(training_data))], 2, sum)
covar_collapse_list_v2 <- names(covar_distribution_v2[covar_distribution_v2 < 100])
covar_retain_temp_v2 <- names(covar_distribution_v2[covar_distribution_v2 >= 100]) #105

# the combine collapse part is remained to complete for now
idx_paytype_M_R_U_combSmall <- ifelse(sum(training_data[, match(names(training_data)[grep('paytype', names(training_data))], names(training_data))]) > 0, 1, 0)
idx_prodtype_D_U_combSmall <- ifelse(sum(training_data[, match(names(training_data)[grep('prodtype', names(training_data))], names(training_data))]) > 0, 1, 0)
pre_comor_combSmall <- ifelse(sum(training_data[, match(names(training_data)[grep('comor', names(training_data))], names(training_data))]) > 0, 1, 0)
combSmall_vars <- ls()[grep('combSmall$', ls())]
table(as.vector(training_data$pre_comor_combSmall))
table(combSmall_vars[3])

covar_model_v1 <- covar_retain_temp_v1
covar_model_v2 <- covar_retain_temp_v2
covar_model_list <- list()
covar_model_list$v1 <- covar_model_v1
covar_model_list$v2 <- covar_model_v2
response_list <- c('response')
sample_proportion_list <- c(0.75)
list_table1 <- list()
list_table2 <- list()
list_table3 <- list()
list_table4 <- list()
fold_num <- 5
rep_num <-5
resp <-1
prop <-1
for (version in 1:2){
for (resp in 1:length(response_list)){
    for (prop in 1:length(smaple_proportion_list)){
        
        response_select <- response_list[resp]
        proportion <- sample_proportion_list[prop]
        index <- (resp-1)*2+prop
        
        
        raw_data$response <- raw_data[,names(raw_data)==response_select]
        model_variable <- c('response', covar_model_list[[version]])[1:5] # redefine model_variable
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
            
            training_data<- raw_data[training==1 , match(model_variable[1:5], names(raw_data))]
            test_data<- raw_data[training==0 , match(model_variable[1:5], names(raw_data))]
            
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
        
        
        #----------------- create table 1--------------
        table1_std<- data.frame('version', 'standard logistic', mean(list_auc_std[,2]), sd(list_auc_std[,2]) , mean(list_auc_std[,3]), sd(list_auc_std[,3]))
        names(table1_std) <- c('version', 'Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        table1_stepwise<- data.frame(version, 'stepwise logistic', mean(list_auc_stepwise[,2]), sd(list_auc_stepwise[,2]) , mean(list_auc_stepwise[,3]), sd(list_auc_stepwise[,3]))
        names(table1_stepwise) <- c('version', 'Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        table1_lasso<- data.frame(version, 'lasso', mean(list_auc_lasso[,2]), sd(list_auc_lasso[,2]) , mean(list_auc_lasso[,3]), sd(list_auc_lasso[,3]))
        names(table1_lasso) <- c('version', 'Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        table1<- data.frame(rbind(table1_std , table1_stepwise , table1_lasso))
        table1$overfitting<- (table1$Mean_AUC_training - table1$Mean_AUC_test)/table1$Mean_AUC_test
        eval(parse(text=paste('list_table1$run', index, '=table1', sep='' )))
        
        #out_table <- paste('table1_run', index,'.csv',sep='')
        #        }
        #   }
        #write.csv(list_table1, 'out_table1', quote=F, row.names=F)
        
        
        
        #----------------- create table 2--------------
        model_variable <- names(training_data)[1:5]
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
            dif_OR_16<- mean_OR_lasso - mean_OR_stepwise_lasso    # 16) Difference in odds ratio columns 11 -15
            
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
        model <- 'std'
        data <- 'training'
        k <- 1
        table3_calculate<- function(model , data){
            result<- NULL
            for(k in 1:rep_num){
                eval(parse(text = paste('pred_training<- list_predict_prob_training_', model, '[k,]', sep='')))
                eval(parse(text = paste('pred_data<- list_predict_prob_', data, '_', model, '[k,]', sep='')))
                
                training<- list_training_data[k,]
                training_data<- raw_data[training==1 , match(c('response'), names(raw_data))]
                test_data<- raw_data[training==0 , match(c('response'), names(raw_data))]
                
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
