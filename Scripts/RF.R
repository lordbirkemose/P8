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
funCrossValidation <- function(
  dat, date, ntree, mtry, maxnodes
) {
  train <- dat %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(- c(Start, RV.1.Ahead)) %>%
    as.matrix()
  
  trainLabel <- dat %>%
    dplyr::select(Start, RV.1.Ahead) %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  vali <- dat %>%
    dplyr::filter(
      Start > date,
      Start <= date + lubridate::weeks(1)
    ) %>%
    dplyr::select(- c(Start, RV.1.Ahead)) %>%
    as.matrix()
  
  valiLabel <- dat %>%
    dplyr::select(Start, RV.1.Ahead) %>%
    dplyr::filter(
      Start > date,
      Start <= date + lubridate::weeks(1)
    ) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  mod <- randomForest::randomForest(
    x = train,
    y = trainLabel,
    data = train,
    ntree = ntree,
    mtry = mtry,
    maxnodes = maxnodes
  )
  
  pred <- predict(mod, vali)
  
  # print(paste("Done:", date))
  oosError <- sqrt(mean((valiLabel - pred)^2))
  
  return(list("oosError" = oosError))
}

### Grid ---------------------------------------------------------------------

hyperParamGrid <- expand.grid(
  ntree = 2^(0:10),
  mtry = 1:4,
  maxnodes = 1:10 
)

### Tuning -------------------------------------------------------------------

paramTuningRF <- mapply(
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
  check = 1:nrow(hyperParamGrid)
)


paramTuningRFLog <- mapply(
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
  check = 1:nrow(hyperParamGrid)
)


paramTuningRFBase <- mapply(
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
  check = 1:nrow(hyperParamGrid)
)


paramTuningRFBaseLog <- mapply(
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
  check = 1:nrow(hyperParamGrid)
)

