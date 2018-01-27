library(caret)
library(doParallel)
library(readr)
library(randomForest)
library(dplyr)

registerDoParallel(cores=4)

datafile <- './B2B4Model.csv'
data <- readr::read_csv(datafile)
head(data)

vars2rm <- c("B2Fir", "B2Sec", "record_num", "B2B"
             , grep("^edss|^relapse.+(prog|conf)$", names(data), value = T))
data <- data %>% select(-one_of(vars2rm)) %>%
      mutate(response=relapse_fu_any_01) %>%
      select(-relapse_fu_any_01) %>%
      # mutate(response=as.factor(response))
      mutate(response=as.factor(ifelse(response==1, "Yes", "No")))

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")

fullRF <- getModelInfo(model="rf", regex=FALSE)

prm <- data.frame(parameter=c('ntree', 'mtry', 'nodesize')
                  , class= rep('numeric', 3)
                  , label= c('ntree', 'mtry', 'nodesize')
)

fullRF$rf$parameters <- prm

RF_fit <- function(x, y, wts, param, lev, last, classProbs, ...)
      randomForest(x=x
                   , y=y
                   , ntree=param$ntree
                   , mtry=param$mtry
                   , nodesize=param$nodesize
                   , ...)


fullRF$rf$fit <- RF_fit

ctrl <- trainControl(method='cv'
                     , number=3
                     , classProbs = TRUE
                     , summaryFunction = twoClassSummary)

grid <- expand.grid(ntree=c(10, 50)
                    ,mtry=c(5,6)
                    , nodesize=c(50, 80))

tm1 <- Sys.time()
rf_refit <- caret::train(response~.,
                         data=data
                         , method=fullRF$rf
                         , tuneGrid=grid
                         , trControl=ctrl
                         , metric='ROC')
print(Sys.time()-tm1)



twoClassSummary <- 
function (data, lev = NULL, model = NULL) 
{
      lvls <- levels(data$obs)
      if (length(lvls) > 2) 
            stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
      requireNamespaceQuietStop("ModelMetrics")
      if (!all(levels(data[, "pred"]) == lvls)) 
            stop("levels of observed and predicted data do not match")
      data$y = as.numeric(data$obs == lvls[2])
      rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 
                                         1), data[, lvls[1]])
      out <- c(rocAUC, sensitivity(data[, "pred"], data[, "obs"], 
                                   lev[1]), specificity(data[, "pred"], data[, "obs"], lev[2]))
      names(out) <- c("ROC", "Sens", "Spec")
      out
}

ppvAtGivenRecall <- 
      function (data, lev = NULL, model = NULL) 
      {
            lvls <- levels(data$obs)
            if (length(lvls) > 2) 
                  stop(paste("Your outcome has", length(lvls), "levels. The ppvAtGivenRecall() function isn't appropriate."))
            # requireNamespaceQuietStop("ModelMetrics")
            if (!all(levels(data[, "pred"]) == lvls)) 
                  stop("levels of observed and predicted data do not match")
            data$y = as.numeric(data$obs == lvls[2])
#             rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 
#                                                1), data[, lvls[1]])
            aucObj <- ROCR::prediction(data[, lvls[1]], ifelse(data$obs==lev[2], 0, 1))
            ppvRec <- ROCR::performance(aucObj, 'ppv', 'sens')
            
            selRec <- ppvRec@x.values[[1]][which.min(abs(ppvRec@x.values[[1]]-0.2))]
            tarPPV <- ppvRec@y.values[[1]][which.min(abs(ppvRec@x.values[[1]]-0.2))]
            # out <- c(rocAUC, sensitivity(data[, "pred"], data[, "obs"], 
                                         # lev[1]), specificity(data[, "pred"], data[, "obs"], lev[2]))
            # names(out) <- c("ROC", "Sens", "Spec")
            out <- c(tarPPV, selRec, 0.2)
            names(out) <- c("PPVAtGivenRecall", "SelectedRecall", "GivenRecall")
            
            out
      }

ctrl <- trainControl(method = 'cv'
                     , number=3
                     , classProbs = T
                     , summaryFunction = ppvAtGivenRecall
                     )

grid <- expand.grid(nrounds=c(100, 200)
                    , max_depth= c(5,6)
                    , eta = c(0.1, 0.5)
                    , gamma = 0.5
                    , colsample_bytree=1
                    , min_child_weight=1
                    , subsample=1
                    )
library(xgboost)
tm1 <- Sys.time()
xbg_re <- caret::train(response~., data=data, method="xgbTree"
                       ,tuneGrid=grid
                       , verbose=T
                       , trControl=ctrl
                       , metric='PPVAtGivenRecall'
                       )
print(Sys.time()-tm1)
