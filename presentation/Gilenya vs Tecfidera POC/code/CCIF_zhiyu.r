#Uplift Random Forest AND CCIF forest
rm(list=ls(all=TRUE))
#data_path<- "D:/Projects/2015/Rebif project"

data_path<-"//bejvfps01/GTO Stats AA/Public/zywang/rebif"

setwd(paste(data_path,"/results",sep=""))
data<- read.table("task1_model_data.csv",header=T, sep=',')
model_var=names(data)
dim(data)
# 3427  101

library(uplift)
library(glmnet)

#CCIF Modeling
res="adherent"

eval(parse(text = paste('data$response=data$',res,sep='')))
final_results_CCIF=data.frame(type="temp",top_p=0,method="temp",ntree=0,mtry=0,depth=0,pvalue=0,minsplit=0,qini_training=0,qini_test=0,auc_training=0,auc_test=0)
#Variable Select using NIV 
set.seed(567)
eval(parse(text=paste("niv_data=data[,setdiff(c(model_var,'response'),'",res,"')]",sep='')))

sub_var=setdiff(names(niv_data),c("response","treatment"))
for (var in sub_var)
{
    eval(parse(text=paste("niv_data$",var,"=as.factor(niv_data$",var,")",sep='')))
}

niv.1 = niv(response ~ trt(treatment)+., data = niv_data,plotit=F,B=10)
niv=niv.1$niv_val[order(-niv.1$niv_val[,3]),]

#top_p in c(0.75,1)
for (top_p in c(1))
{
    proportion=top_p
    niv_varlist=setdiff(rownames(niv),"treatment")
    var_selection=niv_varlist[1:floor(proportion*length(niv_varlist))]    
    CCIF_model_data=niv_data[,c(var_selection,"treatment","response")]
    
    #Cross Validation- 5 folds
    k.folds=5
    foldid=rep(0,length=nrow(CCIF_model_data))
    set.seed(1)
    foldid[CCIF_model_data$response==1]=sample(rep(1:k.folds,length=length(which(CCIF_model_data$response==1))))
    foldid[CCIF_model_data$response==0]=sample(rep(1:k.folds,length=length(which(CCIF_model_data$response==0))))
    table(foldid,CCIF_model_data$response)
    
    #Selection of parameter values
    #all_par=read.csv("parameters_combination_task1_CCIF_v2.csv",sep=",")
    #nrow(all_par)
    minisplit=c(40)
    length(minisplit)
    
    for (j in 1:length(minisplit))
    {
        ccif_method="Int"
        ccif_ntree=500
        ccif_pvalue=0.15
        ccif_mtry=12
        ccif_depth=5
        ccif_minisplit=minisplit[j]
        
        #CV beginning
        temp=as.data.frame(matrix(rep(0,4*k.folds),nr=k.folds,nc=4))
        for (i in 1:k.folds)
        {
            cv_training=CCIF_model_data[foldid!=i,]
            cv_test=CCIF_model_data[foldid==i,]
            set.seed(234)
            x_training_data=cv_training[,var_selection]
            
            fit_CCIF<- ccif(x=x_training_data,
                            y=cv_training$response,
                            ct=cv_training$treatment,
                            mtry=ccif_mtry,
                            ntree=ccif_ntree,
                            pvalue=ccif_pvalue,
                            interaction.depth=ccif_depth,
                            split_method= ccif_method,
                            verbose=F)
            #QINI metrics
            pred_training=predict(fit_CCIF,cv_training)
            training_perf = performance(pred_training[,1],pred_training[,2],cv_training$response,cv_training$treatment,direction=1)
            qini_training = qini(training_perf, direction=1, plotit=F)
            
            pred_test=predict(fit_CCIF,cv_test)
            test_perf = performance(pred_test[,1], pred_test[,2], cv_test$response, cv_test$treatment,direction=1)
            qini_test = qini(test_perf, direction=1, plotit=F)
            temp[i,c(1,2)]=cbind(qini_training[1],qini_test[1])
            
            #AUC
            training_obs=ifelse(cv_training$treatment==1,pred_training[,1],pred_training[,2])
            test_obs=ifelse(cv_test$treatment==1,pred_test[,1],pred_test[,2])
            temp[i,c(3,4)]=cbind(auc(cv_training$response,training_obs),auc(cv_test$response,test_obs))
        }		
        temp_avg=t(apply(temp,2,mean))
        colnames(temp_avg)=c("qini_training","qini_test","auc_training","auc_test")
        
        final_temp=data.frame(type=res,top_p=top_p,method=ccif_method,ntree=ccif_ntree,mtry=ccif_mtry,depth=ccif_depth,pvalue=ccif_pvalue,minsplit=ccif_minisplit,temp_avg)  
        final_results_CCIF=rbind(final_results_CCIF,final_temp)
    }
}
final_results_CCIF=final_results_CCIF[-1,]

eval(parse(text=paste("file.remove('final_results_CCIF_",res,"_4.csv')",sep='')))
eval(parse(text=paste("write.csv(final_results_CCIF,'final_results_CCIF_",res,"_4.csv',row.names=F)",sep='')))




