rm(list=ls())
library('foreach')
library('dplyr')

#Reading Raw data
setwd('C:/Projects/2015/MS/2015-10-29')
data_dir <- './'
# functionfiles <- paste(rootpath, '/functions', sep='') 
datafile <- 'NewFeaturedData.csv'
na_represents <- c('', 'NA', 'unknown', 'ambiguous')
rawdata <- read.csv(paste(data_dir,'data4PythonCode/',datafile, sep=''), 
                    header=T, na.strings = na_represents)
dim(rawdata)
names(rawdata) <- tolower(names(rawdata))
original_var <- paste('f', names(rawdata), sep='')

#New features table
new_features <- read.csv(paste('../results/2015-11-02 09.58.50/new_features_tb.csv', 
                               sep=''), check.names=FALSE) 
names(new_features) <- tolower(names(new_features))
dim(new_features)

#Variables to be dropped in Step 1
dp_tb <- read.csv(paste('../results/2015-11-02 09.58.50/drop_var_step1.csv', 
                        sep=''))
drop_var <- c(as.character(dp_tb$variable))

#Non information variable
nonInfo_file <- read.csv(paste(data_dir,'data4PythonCode/noninformative_vars.csv', 
                               sep=''), check.names=FALSE)
nonInfo_vars <- paste('f',nonInfo_file[,1],sep='')

#All cohort*outcome files list
datafolder <- '../results/20151030_134221'
all_cohort <- c('B2B','B2F', 'B2S', 'continue')
all_outcome <- c("edssprog", "edssconf3", "relapse_fu_any_01", 
                 "progrelapse", "confrelapse")

file_list <- expand.grid(all_cohort, all_outcome)
names(file_list) <- c('cohort', 'outcome')
file_list$filename <- mapply(function(i) {
  paste(file_list$cohort[i], '_', file_list$outcome[i], '.csv', sep='')},
  1:nrow(file_list))

#Create results folder
timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("../results/", timeStamp, sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

#Reading Raw data

#id <- 1 #updated

for (id in 1:nrow(file_list))
{
  datafile <- file_list$filename[id]
  cohort <- as.character(file_list$cohort[id])
  outcome <- as.character(file_list$outcome[id])
  
  sub_folder <- paste(resultDir,'/', cohort, sep='')
  if (file.exists(sub_folder)==F){
    dir.create(sub_folder, showWarnings = TRUE, recursive = TRUE, mode = "0777")
  }
  
  data <- read.csv(paste(datafolder,'/', datafile, sep=''), header=T, check.names=FALSE)
  names(data) <- tolower(names(data))
  dim(data)
  dataWithFeatures <- left_join(data, new_features)
  dim(dataWithFeatures)
  
  #ck
  ncol(data)+ncol(new_features)-1-ncol(dataWithFeatures)
  
  
  if (cohort!='continue'){
    drop_var1 <- c(names(dataWithFeatures)[grepl('fprecont_dayssup', 
                                                 names(dataWithFeatures))==T],
                   'fswitch_rx_dayssup__na')
    dataWithFeatures <- dataWithFeatures[, setdiff(names(dataWithFeatures),
                                                   c(drop_var,drop_var1))]
  } else{
    drop_var1 <- c(names(dataWithFeatures)[grepl('fswitch_rx_dayssu', 
                                                 names(dataWithFeatures))==T],
                   'fprecont_dayssup__na')
    dataWithFeatures <- dataWithFeatures[, setdiff(names(dataWithFeatures),
                                                   c(drop_var,drop_var1))]
  }
  
  
  drop_relapse <- names(dataWithFeatures)[
    which(substr(names(dataWithFeatures),1,nchar('relapse'))=='relapse')]
  
  dataWithFeatures_final <- dataWithFeatures[, setdiff(names(dataWithFeatures),
                                                       c(drop_relapse,'rcdid',nonInfo_vars))]
  dim(dataWithFeatures_final)
  
  #Variable description - origional data
  desc_results <- numeric()
  for (var in names(dataWithFeatures_final)){
    #if (var == "fnum_children_>2")
      #var = var
    eval(parse(text=paste('tempVar <- dataWithFeatures_final$"', var, '"', sep='')))
    nbr_pos <- sum(tempVar)
    nbr_total <- length(tempVar)
    pct <- mean(tempVar)
    pct_result <- cbind(var, nbr_pos, nbr_total, pct)
    desc_results <- rbind(desc_results, pct_result)
  }
  
  desc_results <- as.data.frame(desc_results)
  
  desc_file <- paste(sub_folder,'/description_',cohort,'_', outcome,'.csv', 
                     sep='')
  write.csv(desc_results, desc_file, row.names = F)
  
  #Combine sub-group variables which pct less or equal than 0.1
  flip_var1 <- desc_results[as.numeric(as.character(desc_results$pct))==0 | 
                            as.numeric(as.character(desc_results$pct))==1,'var']
  model_data <- dataWithFeatures_final[,setdiff(names(dataWithFeatures_final),
                                                c(as.character(flip_var1)))]
  
  other_var <- setdiff(names(model_data), original_var)
  var_split <- foreach(i=1:length(other_var),.combine=rbind) %do%
  {
    temp <- strsplit(other_var[i],'__')[[1]]
    l <- length(temp)
    if (l > 2)
    {
      stop("Error! Incorrect variable naming convention. 
           Only one '__' is allowed. \n")
    }
    return(temp)
  }
  
  var_split <- data.frame(var=other_var,var_split)
  var_split$var <- as.character(var_split$var) #added
  desc_results$var <- as.character(desc_results$var) #added
  var_com <- left_join(var_split, desc_results, by="var")
  uniq_org_var <- unique(var_split$X1)
  
  cat("start..\n")
  for (j in 1:length(uniq_org_var)) {
    v <- as.character(uniq_org_var[j])
    if (grepl('birth_region', v) | grepl('init_symptom', v)) {
      t_v1 <- var_com[var_com$X1==v & var_com$X2!='nan',]
      if (nrow(t_v1) == 1)
        next
      t_v_1 <- t_v1[as.numeric(as.character(t_v1$pct)) <= 0.03,]
      pct_1 <- sum(as.numeric(as.character(t_v_1$pct)))
      t_v_2 <- t_v1[as.numeric(as.character(t_v1$pct)) > 0.03,]
      pct_2 <- min(as.numeric(as.character(t_v_2$pct)))
      
      if (nrow(t_v_1)>0) {
        #cat(as.character(uniq_org_var[j]),"\n") #modified
        if (pct_1 < 0.03 & nrow(t_v_2) > 0) {
          formula1.1 <- paste('model_data$"',t_v_1$var, '"', sep='', 
                              collapse = '+')
          formula1.2 <- paste('model_data$"',
                              t_v_2$var[which(as.numeric(as.character(t_v_2$pct))==pct_2)],
                              '"', sep='', collapse = '+')
          formula1 <- paste(formula1.1, '+', formula1.2, sep='')
          dp_var <- c(paste(t_v_1$var, sep=''),
                      t_v_2$var[which(as.numeric(as.character(t_v_2$pct))==pct_2)])
        } else {
          formula1 <- paste('model_data$"',t_v_1$var, '"', sep='', collapse = '+')
          dp_var <- paste(t_v_1$var, sep='')
        }
        formula0 <- paste('model_data$"', v, '__rest"', sep='')
        eval(parse(text=paste(formula0,'<-', formula1, sep='')))
        model_data <- model_data[,setdiff(names(model_data),dp_var)]
      }
    } else  {
      t_v1 <- var_com[var_com$X1==v & var_com$X2!='nan',]
      pct <- as.numeric(as.character(t_v1$pct))
      id <- which(pct <= 0.03)
      if (length(id)==0) {
        next
      } else if (length(id)==nrow(t_v1)) {
        t_v_1 <- t_v1
        formula0 <- paste('model_data$"', v, '__',t_v_1$X2[1],'_',
                          t_v_1$X2[nrow(t_v_1)],'"',
                          sep='')
        formula1 <- paste('model_data$"',t_v1$var,'"', sep='', collapse = '+')
        eval(parse(text=paste(formula0,'<-', formula1, sep='')))
        dp_var <- c(t_v1$var)
        model_data <- model_data[,setdiff(names(model_data),dp_var)]
      } else if (all(diff(id)==1)==TRUE){
        pct1 <- sum(pct[id])
        if (pct1 > 0.03)
        {
          t_v_1 <- t_v1[id,]
        } else if(pct1 <= 0.03 & max(id)==length(pct)) {
          t_v_1 <- t_v1[(min(id)-1):max(id),]
        } else if(pct1 <= 0.03 & max(id)!=length(pct)){
          t_v_1 <- t_v1[min(id):(max(id)+1),]
        }
        formula0 <- paste('model_data$"', v, '__',t_v_1$X2[1],'_',
                          t_v_1$X2[nrow(t_v_1)],'"',
                          sep='')
        formula1 <- paste('model_data$"',t_v_1$var,'"', sep='', collapse = '+')
        eval(parse(text=paste(formula0,'<-', formula1, sep='')))
        dp_var <- c(t_v_1$var)
        model_data <- model_data[,setdiff(names(model_data),dp_var)]
      } else if (length(id)==1){
        if (id!=length(pct)) {
          t_v_1 <- t_v1[id:(id+1),]
          formula0 <- paste('model_data$"', v, '__',t_v_1$X2[1],'_',
                            t_v_1$X2[nrow(t_v_1)],'"',
                            sep='')
          formula1 <- paste('model_data$"',t_v_1$var,'"', sep='', collapse = '+')
        } else {
          t_v_1 <- t_v1[(id-1):id,]
          formula0 <- paste('model_data$"', v, '__>=',t_v_1$X2[1],'"', sep='')
          formula1 <- paste('model_data$"',t_v_1$var,'"', sep='', collapse = '+')
        }
        eval(parse(text=paste(formula0,'<-', formula1, sep='')))
        dp_var <- c(t_v_1$var)
        model_data <- model_data[,setdiff(names(model_data),dp_var)]
      } else {
        n <- 0
        for (i in 1:length(id)) {
          k <- id[i]
          if (k <= n) {
            next
          } else if (i < length(id) & k < length(pct)){
            if(id[i+1]==length(pct) & id[i+1]-id[i]<=2){
              t_v_1 <- t_v1[id[i]:id[i+1],]
              formula0 <- paste('model_data$"', v, '__',t_v_1$X2[1],'_',
                                t_v_1$X2[nrow(t_v_1)],'"',
                                sep='')
              formula1 <- paste('model_data$"',t_v_1$var,'"', sep='', collapse = '+')
              eval(parse(text=paste(formula0,'<-', formula1, sep='')))
              dp_var <- c(t_v_1$var)
              model_data <- model_data[,setdiff(names(model_data),dp_var)]
              n <- id[i+1]
            } else {
              p <- pct[k]
              for (l in (k+1):length(pct)) {
                p <- p + pct[l]
                if (p > 0.03) {
                  break
                }}
              t_v_1 <- t_v1[k:l,]
              formula0 <- paste('model_data$"', v, '__',t_v_1$X2[1],'_',
                                t_v_1$X2[nrow(t_v_1)],'"',
                                sep='')
              formula1 <- paste('model_data$"',t_v_1$var,'"', sep='', collapse = '+')
              eval(parse(text=paste(formula0,'<-', formula1, sep='')))
              dp_var <- c(t_v_1$var)
              model_data <- model_data[,setdiff(names(model_data),dp_var)]
              n <- l + n
            }
          } else if (i == length(id) & k < length(pct)) {
            t_v_1 <- t_v1[id[i]:(id[i]+1),]
            formula0 <- paste('model_data$"', v, '__',t_v_1$X2[1],'_',
                              t_v_1$X2[nrow(t_v_1)],'"',
                              sep='')
            formula1 <- paste('model_data$"',t_v_1$var,'"', sep='', collapse = '+')
            eval(parse(text=paste(formula0,'<-', formula1, sep='')))
            dp_var <- c(t_v_1$var)
            model_data <- model_data[,setdiff(names(model_data),dp_var)]
          } else if (k == length(pct)){
            t_v_1 <- t_v1[(k-1):k,]
            formula0 <- paste('model_data$"', v, '__',t_v_1$X2[1],'_',
                              t_v_1$X2[nrow(t_v_1)],'"',
                              sep='')
            formula1 <- paste('model_data$"',t_v_1$var,'"', sep='', collapse = '+')
            eval(parse(text=paste(formula0,'<-', formula1, sep='')))
            dp_var <- c(t_v_1$var)
            model_data <- model_data[,setdiff(names(model_data),dp_var)]
          }
        }
      }
    }
  }
  cat("end...\n")
  
  #Variable description after combination
  desc_results_com <- numeric()
  for (var in names(model_data)){
    eval(parse(text=paste('tempVar <- model_data$"', var, '"', sep='')))
    nbr_pos <- sum(tempVar)
    nbr_total <- length(tempVar)
    pct <- mean(tempVar)
    pct_result <- cbind(var, nbr_pos, nbr_total, pct)
    desc_results_com <- rbind(desc_results_com, pct_result)
  }
  
  desc_results_com <- as.data.frame(desc_results_com)
  desc_file_com <- paste(sub_folder,'/description_',cohort,'_', outcome,'_combine.csv', sep='')
  write.csv(desc_results_com, desc_file_com, row.names = F)
  
  #Drop variables of all less than or equal to 0 and more than or equal to 0.99
  flip_var <- desc_results_com[as.numeric(as.character(desc_results_com$pct))<=0.01 | 
                                 as.numeric(as.character(desc_results_com$pct))>=0.99,
                               'var']
  model_data_final <- model_data[,setdiff(names(model_data),c(as.character(flip_var)))]
  
  final_datafile <- paste(sub_folder,'/',cohort,'_', outcome,'_final.csv', sep='')
  write.csv(model_data_final, final_datafile, row.names = F)
}

