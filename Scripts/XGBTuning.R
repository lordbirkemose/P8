### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
data <- funGetDataHAR(model = "extended")
dataLog <- funGetDataHAR(model = "extendedLog")

dataBase <- data %>%
  dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction))

dataLogBase <- dataLog %>%
  dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction))

### Functions ----------------------------------------------------------------
funCrossValidation <- function(
  dat, date, nrounds, eta, max_depth, early_stop_round
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

  mod <- xgboost::xgboost(
    data = train,
    label = trainLabel,
    params = list(
      booster = "gbtree",
      objective = "reg:linear",
      eta = eta,
      max_depth = max_depth,
      gamma = 0
    ),
    nrounds = nrounds,
    early_stop_round = early_stop_round,
    verbose = 0,
    eval_metric = "rmse"
  )
  
  pred <- predict(mod, vali)
  
  print(paste("Done:", date))
  oosError <- sqrt(mean((valiLabel - pred)^2))
  
  return(list("oosError" = oosError))
}

### Grid ---------------------------------------------------------------------
hyperParamGrid <- expand.grid(
  nrounds = seq(100, 500, 100),
  eta = seq(0.001, 0.1, 0.002),
  max_depth = seq(1, 10, 1),
  early_stop_round = 20
)

### Tuning -------------------------------------------------------------------
paramTuningXGB <- mapply(
  function(nrounds, eta, max_depth, early_stop_round, check) {
    errorRolling <- data$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = data,
        nrounds = nrounds,
        eta = eta,
        max_depth = max_depth,
        early_stop_round = early_stop_round
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    print(paste("Done:", check))
    
    return(
      list(
        "nrounds" = nrounds,
        "eta" = eta,
        "max_depth" = max_depth,
        "early_stop_round" = early_stop_round,
        "oosError" = errorRolling$oosError
      )
    )
  },
  nrounds = hyperParamGrid$nrounds,
  eta = hyperParamGrid$eta,
  max_depth = hyperParamGrid$max_depth,
  early_stop_round = hyperParamGrid$early_stop_round,
  check = 1:nrow(hyperParamGrid)
)

paramTuningXGBLog <- mapply(
  function(nrounds, eta, max_depth, early_stop_round, check) {
    errorRolling <- dataLog$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = dataLog,
        nrounds = nrounds,
        eta = eta,
        max_depth = max_depth,
        early_stop_round = early_stop_round
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    print(paste("Done:", check))
    
    return(
      list(
        "nrounds" = nrounds,
        "eta" = eta,
        "max_depth" = max_depth,
        "early_stop_round" = early_stop_round,
        "oosError" = errorRolling$oosError
      )
    )
  },
  nrounds = hyperParamGrid$nrounds,
  eta = hyperParamGrid$eta,
  max_depth = hyperParamGrid$max_depth,
  early_stop_round = hyperParamGrid$early_stop_round,
  check = 1:nrow(hyperParamGrid)
)

paramTuningXGBBase <- mapply(
  function(nrounds, eta, max_depth, early_stop_round, check) {
    errorRolling <- dataBase$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = dataBase,
        nrounds = nrounds,
        eta = eta,
        max_depth = max_depth,
        early_stop_round = early_stop_round
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    print(paste("Done:", check))
    
    return(
      list(
        "nrounds" = nrounds,
        "eta" = eta,
        "max_depth" = max_depth,
        "early_stop_round" = early_stop_round,
        "oosError" = errorRolling$oosError
      )
    )
  },
  nrounds = hyperParamGrid$nrounds,
  eta = hyperParamGrid$eta,
  max_depth = hyperParamGrid$max_depth,
  early_stop_round = hyperParamGrid$early_stop_round,
  check = 1:nrow(hyperParamGrid)
)

paramTuningXGBLogBase <- mapply(
  function(nrounds, eta, max_depth, early_stop_round, check) {
    errorRolling <- dataLogBase$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = dataLogBase,
        nrounds = nrounds,
        eta = eta,
        max_depth = max_depth,
        early_stop_round = early_stop_round
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    print(paste("Done:", check))
    
    return(
      list(
        "nrounds" = nrounds,
        "eta" = eta,
        "max_depth" = max_depth,
        "early_stop_round" = early_stop_round,
        "oosError" = errorRolling$oosError
      )
    )
  },
  nrounds = hyperParamGrid$nrounds,
  eta = hyperParamGrid$eta,
  max_depth = hyperParamGrid$max_depth,
  early_stop_round = hyperParamGrid$early_stop_round,
  check = 1:nrow(hyperParamGrid)
)

### Save ---------------------------------------------------------------------
save(
  paramTuningXGB, paramTuningXGBLog,
  paramTuningXGBBase, paramTuningXGBLogBase,
  file = "./Data/paramTuningXGBExtended.RData"
)
