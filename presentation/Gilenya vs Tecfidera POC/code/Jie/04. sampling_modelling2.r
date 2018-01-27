library(glmnet)
options(digits = 4)

library(e1071)

#1. set up working folder and read in model data
data_path<- 'D:\\working materials\\BRACE\\004 Output'
data_file<- 'model_data_comb.csv';
out_path<-'D:\\working materials\\BRACE\\004 Output'
code_path <- 'D:\\working materials\\BRACE\\003 R Code'
setwd(out_path)


#Sampling and modelling
raw_data <- read.table(data_file, sep=',', head=T)
dim(raw_data) #5483 108

#chech the pos num of each covar, avoid the pos num < 100
preIndex_index_treatment_vars <- names(raw_data)[grep('Interf$|Glatir$|none$', names(raw_data), perl=TRUE)] 

summary_vars <- names(raw_data)[grep('any$', names(raw_data), perl=TRUE)]
var_list <- setdiff(names(raw_data), summary_vars)
num_relapse_1yr_list <- var_list[grep('^num_.*1yr', var_list)]

covar_list <- setdiff(names(raw_data), c(summary_vars, 'response')) #121
covar_list_rmTreatment <- setdiff(names(raw_data), c(preIndex_index_treatment_vars, summary_vars, 'response','num_post_relapse_1yr')) #116

# collapse varibles including class less than 100 in training data
covar_distribution <- apply(training_data[, match(covar_list_rmTreatment, names(training_data))], 2, sum)
covar_collapse_list <- names(covar_distribution_v1[covar_distribution_v1 < 100])
covar_retain_temp <- names(covar_distribution_v1[covar_distribution_v1 >= 100]) #100

covar_distribution <- apply(raw_data[, match(covar_list, names(raw_data))], 2, sum)
covar_collapse_list <- names(covar_distribution_v2[covar_distribution_v2 < 100])
covar_retain_temp <- names(covar_distribution_v2[covar_distribution_v2 >= 100]) #105
covar_distribution[match(names(covar_distribution)[grep('paytype', names(covar_distribution))], names(covar_distribution))]


training_data_seed <- function(seed){
    training <- numeric(nrow(raw_data))
    response1_idx <- which(raw_data$response == 1)
    training_response1_size <- 3/4*length(response1_idx)
    set.seed(seed)
    response1_training_idx <- sample(response1_idx, training_response1_size)
    response0_idx <- which(raw_data$response == 0)
    training_response0_size <- 3/4*length(response0_idx)
    set.seed(seed)
    response0_training_idx <- sample(response0_idx, size=training_response0_size)
    idx_training <- c(response1_training_idx, response0_training_idx)
    training[idx_training]<- 1
    tb <- table(training, raw_data$response)
    prop.table(tb, 1)
    prop.table(tb, 2)
    training_data <- raw_data[training==1,]
    test_data <- raw_data[training==0,]
    dataset <- list(training = training_data, test = test_data)
    return dataset
}
training_data <- training_data_seed(12345)[[1]]
test_data <- training_data_seed(12345)[[2]]

# training and test data setting over


response1_training_idx_list <- numeric()
response0_training_idx_list <- numeric()
#set.seed(12345)
    set.seed(12)
    response1_training_idx <- sample(response1_idx, training_response1_size)
    set.seed(12)
    response0_training_idx <- sample(response0_idx, size=training_response0_size)
    #response01 <- cbind(response1_training_idx, response0_training_idx)
    #response0_training_idx_list <- cbind(response0_training_idx_list, response0_training_idx)
response1_training_idx[1:200]
response0_training_idx[1:200]



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
proportion <- sample_proportion_list <- c(0.75)
rep_num <- 5
fold_num <- 5
coef_odds_pvalue_list <- list()
raw_data <- cbind(raw_data[,1:5], response=raw_data$response)
for(v in 1:length(covar_model_list)){
 #   for(resp in 1:length(response_list)){
    #    for(prop in 1:length(smaple_proportion_list)){
            #response_select <- response_list[resp]
            #proportion <- sample_proportion_list[prop]
            #index <- (resp-1)*2+prop
            
        #for(rep in 1:prep_num)
            raw_data$response <- raw_data[,names(raw_data)==response_select]
            model_variable <- c('response', covar_model_list[[v]])[1:5] # redefine model_variable
            
            training_sample_proportion<- proportion    # set 0.5 or 400/nrow(raw_data), sample 50% and 200 pairs patients respectively
            training <- numeric(nrow(raw_data))
            response1_idx <- which(raw_data$response == 1)
            training_response1_size <- proportion*length(response1_idx)
            set.seed(12345)
            response1_training_idx <- sample(response1_idx, training_response1_size)
            response0_idx <- which(raw_data$response == 0)
            training_response0_size <- proportion*length(response0_idx)
            response0_training_idx <- sample(response0_idx, size=training_response0_size)
            idx_training <- c(response1_training_idx, response0_training_idx)
            training[idx_training]<- 1
            training_data <- raw_data[training==1,]
            test_data <- raw_data[training==0,]
            # training and test data setting over
            #------------------ standard logistic regression --------------------
            fit<- glm(response~., data=training_data, family=binomial)
            coef<- data.frame(coefficient=coef(fit) , odds_ratio=exp(coef(fit)))
            p_value<- summary(fit)$coef[, "Pr(>|z|)"]
            model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
            eval(parse(text=paste('coef_odds_pvalue_list$std_v', v,  '<- model', sep='')))
            
            
            #------------------------- step wise regression --------------------
            fit<- glm(response~., data=training_data, family=binomial)
            step_wise<- step(fit , direction="backward")
            
            coef<- data.frame(coefficient=coef(step_wise) , odds_ratio=exp(coef(step_wise)))
            p_value<- summary(step_wise)$coef[, "Pr(>|z|)"]
            model<- data.frame(coef , p_value=p_value[match(rownames(coef) , names(p_value))])
            eval(parse(text=paste('coef_odds_pvalue_list$stepwise_v', v,  '<- model', sep='')))
            
            
            
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
            eval(parse(text=paste('coef_odds_pvalue_list$lasso_v', v,  '<- model', sep='')))
            
            #---------------------------------------------------------------------------------
            
            
         #}  
            
        #}    
    #}
}