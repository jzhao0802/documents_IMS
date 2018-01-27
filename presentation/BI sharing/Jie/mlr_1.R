library(mlr)
library(readr)
library(dplyr)
library(parallel)
library(ROCR)
library(parallelMap)

timeStamp <- as.character(Sys.time())
timeStamp <- gsub(":", ".", timeStamp)  # replace ":" by "."
resultDir <- paste("./Results/", timeStamp, "/", sep = '')
dir.create(resultDir, showWarnings = TRUE, recursive = TRUE, mode = "0777")


datafile <- './B2B4Model.csv'
data <- readr::read_csv(datafile)
head(data)

vars2rm <- c("B2Fir", "B2Sec", "record_num", "B2B"
             , grep("^edss|^relapse.+(prog|conf)$", names(data), value = T))
data <- data %>% dplyr::select(-one_of(vars2rm)) %>%
      dplyr::mutate(response=relapse_fu_any_01) %>%
      dplyr::select(-relapse_fu_any_01) %>%
      sapply(., as.numeric) %>%
      as.data.frame() %>%
      mutate(response=as.factor(response))
#       mutate(response=as.factor(ifelse(response==1, "Yes", "No")))

dataset <- makeClassifTask(id='xgb_mlr_eg', data=data, target='response', positive=1)

getTaskDescription(dataset)
getTaskDesc(dataset)

norm_task <- normalizeFeatures(dataset, method='range')
summary(getTaskData(norm_task))

lrn <- makeLearner('classif.xgboost', predict.type='prob')
lrn$par.vals = list(
      nrounds=100
      , verbose=F
      , objective='binary:logistic'
)

lrn

lrn$par.set
getHyperPars(lrn)
getParamSet(lrn)
getParamSet('classif.xgboost')
listLearners()

ps <- makeParamSet(
      makeNumericParam("eta", lower=0.15, upper=0.4)
      , makeIntegerParam("max_depth", lower=3, upper=8)
      , makeNumericParam("alpha", lower=0, upper=1)
      , makeNumericParam("lambda", lower=0.25, upper=3)
      , makeIntegerParam("min_child_weight", lower=2, upper = 6)
      , makeNumericParam("colsample_bytree", lower=.3, upper=.7)
      , makeNumericParam("subsample", lower = 0.65, upper=0.95)
)

ps

ctrl <- makeTuneControlRandom(maxit = 30)
ctrl

cv <- makeResampleDesc("CV", iters=3)
measures_lst <- list(auc, acc)
parallelGetRegisteredLevels()

random_seed <- 123
set.seed(random_seed, "L'Ecuyer")

num_cores<- 4
parallelStartSocket(num_cores, level="mlr.turnParams")

res <- tuneParams(lrn, dataset, resampling=cv, par.set = ps, control=ctrl, show.info = T, measures=measures_lst)
parallelStop()

res$x
res$y

lrn_opt <- setHyperPars(lrn, par.vals = res$x)
lrn_opt


