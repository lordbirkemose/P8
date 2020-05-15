### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
  library(randomForest)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
data <- funGetDataHAR()
dataLog <- funGetDataHAR(log = TRUE)

dataBase <- data %>%
  dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction))

dataLogBase <- dataLog %>%
  dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction))

### Functions ----------------------------------------------------------------
funCrossValidation <- function(dat, date, ntree, mtry, maxnodes) {
  train <- dat %>%
    dplyr::filter(Start < date)
  
  vali <- dat %>%
    dplyr::filter(
      Start > date,
      Start <= date + lubridate::weeks(1)
    )
  
  mod <- randomForest::randomForest(
    RV.1.Ahead ~ . - Start,
    data = train,
    ntree = ntree,
    mtry = mtry,
    maxnodes = maxnodes
  )
  
  pred <- predict(mod, vali)
  
  oosError <- sqrt(mean((vali$RV.1.Ahead - pred)^2))
  
  print(paste("Done:", date))
  return(list("oosError" = oosError))
}

### Grid ---------------------------------------------------------------------
hyperParamGrid <- expand.grid(
  ntree = 2^(5:12),
  mtry = 1:4,
  maxnodes = seq(10, 130, 10)
)

### Tuning -------------------------------------------------------------------
mc.cores <- parallel::detectCores()/2

paramTuningRF <- parallel::mcmapply(
  function(ntree, mtry, maxnodes, check) {
    errorRolling <- data$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = data,
        ntree = ntree,
        mtry = mtry,
        maxnodes = maxnodes
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    print(paste("Ext - Done:", check,"/",nrow(hyperParamGrid)))
    
    return(
      list(
        "ntree" = ntree,
        "mtry" = mtry,
        "maxnodes" = maxnodes,
        "oosError" = errorRolling$oosError
      )
    )
  },
  ntree = hyperParamGrid$ntree,
  mtry = hyperParamGrid$mtry,
  maxnodes = hyperParamGrid$maxnodes,
  check = 1:nrow(hyperParamGrid),
  mc.cores = mc.cores
)
save(
  paramTuningRF,
  file = "./Rdata/paramTuningRF.RData"
)

paramTuningRFLog <- parallel::mcmapply(
  function(ntree, mtry, maxnodes, check) {
    errorRolling <- dataLog$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = dataLog,
        ntree = ntree,
        mtry = mtry,
        maxnodes = maxnodes
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    print(paste("Ext Log - Done:", check,"/",nrow(hyperParamGrid)))
    
    return(
      list(
        "ntree" = ntree,
        "mtry" = mtry,
        "maxnodes" = maxnodes,
        "oosError" = errorRolling$oosError
      )
    )
  },
  ntree = hyperParamGrid$ntree,
  mtry = hyperParamGrid$mtry,
  maxnodes = hyperParamGrid$maxnodes,
  check = 1:nrow(hyperParamGrid),
  mc.cores = mc.cores
)
save(
  paramTuningRFLog,
  file = "./Rdata/paramTuningRFLog.RData"
)

paramTuningRFBase <- parallel::mcmapply(
  function(ntree, mtry, maxnodes, check) {
    errorRolling <- dataBase$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = dataBase,
        ntree = ntree,
        mtry = mtry,
        maxnodes = maxnodes
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    print(paste("Base - Done:", check,"/",nrow(hyperParamGrid)))
    
    return(
      list(
        "ntree" = ntree,
        "mtry" = mtry,
        "maxnodes" = maxnodes,
        "oosError" = errorRolling$oosError
      )
    )
  },
  ntree = hyperParamGrid$ntree,
  mtry = hyperParamGrid$mtry,
  maxnodes = hyperParamGrid$maxnodes,
  check = 1:nrow(hyperParamGrid),
  mc.cores = mc.cores
)
save(
  paramTuningRFBase,
  file = "./Rdata/paramTuningRFBase.RData"
)

paramTuningRFBaseLog <- parallel::mcmapply(
  function(ntree, mtry, maxnodes, check) {
    errorRolling <- dataLogBase$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = dataLogBase,
        ntree = ntree,
        mtry = mtry,
        maxnodes = maxnodes
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    print(paste("Base Log - Done:", check,"/",nrow(hyperParamGrid)))
    
    return(
      list(
        "ntree" = ntree,
        "mtry" = mtry,
        "maxnodes" = maxnodes,
        "oosError" = errorRolling$oosError
      )
    )
  },
  ntree = hyperParamGrid$ntree,
  mtry = hyperParamGrid$mtry,
  maxnodes = hyperParamGrid$maxnodes,
  check = 1:nrow(hyperParamGrid),
  mc.cores = mc.cores
)
save(
  paramTuningRFBaseLog,
  file = "./Rdata/paramTuningRFBaseLog.RData"
)