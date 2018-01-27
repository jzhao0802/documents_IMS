response_list <- c('persistent','post_relapse_persist')
sample_proportion_list <- c(0.5, 400/nrow(raw_data))
for (resp in 1:2){
    for (prop in 1:2){
        response_variable <- response_list[resp]
        proportion <- sample_proportion_list[prop]
        index <- (resp-1)*2+prop
        
        raw_data$response <- raw_data[,names(raw_data)==response_variable]
        model_variable <- c('response',variable_list)
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
        list_table1 <- list()
        list_table2 <- list()
        list_table3 <- list()
        list_table4 <- list()
        
        for(k in 1:5){
            # sample training and test data set, proportionally split response outcome into training and test
            training_sample_proportion<- proportion    # set 0.5 or 400/nrow(raw_data), sample 50% and 200 pairs patients respectively
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
            k.folds<- 2
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
        table1_std<- data.frame(response_variable, proportion, 'standard logistic', mean(list_auc_std[,2]), sd(list_auc_std[,2]) , mean(list_auc_std[,3]), sd(list_auc_std[,3]))
        names(table1_std)<- c('response', 'sample proportion','Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        
        table1_stepwise<- data.frame(response_variable, proportion, 'stepwise logistic', mean(list_auc_stepwise[,2]), sd(list_auc_stepwise[,2]) , mean(list_auc_stepwise[,3]), sd(list_auc_stepwise[,3]))
        names(table1_stepwise)<- c('response', 'sample proportion','Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        
        table1_lasso<- data.frame(response_variable, proportion, 'lasso', mean(list_auc_lasso[,2]), sd(list_auc_lasso[,2]) , mean(list_auc_lasso[,3]), sd(list_auc_lasso[,3]))
        names(table1_lasso)<- c('response', 'sample proportion','Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        
        table1<- rbind(table1_std , table1_stepwise , table1_lasso)
        names(table1)<- c('response', 'sample proportion','Model', 'Mean_AUC_training', 'SD_AUC_training', 'Mean_AUC_test', 'SD_AUC_test')
        table1$overfitting <- (table1$Mean_AUC_training - table1$Mean_AUC_test)/table1$Mean_AUC_test
        table1$overfitting=mapply(function(i) {(table1$Mean_AUC_training[i] - table1$Mean_AUC_test[i])/table1$Mean_AUC_test[i]},1:nrow(table1))
        #table1$overfitting <- 0.3
        
        eval(parse(text=paste('list_table1$run', index, '<- table1', sep='' )))
    }
}
write.csv(list_table1,'table1.csv',row.names=F,quote=F)