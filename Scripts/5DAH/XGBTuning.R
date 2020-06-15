### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
  library(randomForest)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
data <- funGetDataHAR_v2(DAH = 5, model = "extended", test = FALSE)
dataLog <- funGetDataHAR_v2(DAH = 5, model = "extendedLog")

dataBase <- data %>%
  dplyr::select(-c(Jump, Pos.Return, Neg.Return, RV.Direction))

dataLogBase <- dataLog %>%
  dplyr::select(-c(Jump, Pos.Return, Neg.Return, RV.Direction))

### Functions ----------------------------------------------------------------
funCrossValidation <- function(
  dat, date, nrounds, eta, max_depth, early_stop_round
) {
  train <- dat %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(- c(Start, Target)) %>%
    as.matrix()
  
  trainLabel <- dat %>%
    dplyr::select(Start, Target) %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  vali <- dat %>%
    dplyr::filter(
      Start > date,
      Start <= date + lubridate::weeks(1)
    ) %>%
    dplyr::select(- c(Start, Target)) %>%
    as.matrix()
  
  valiLabel <- dat %>%
    dplyr::select(Start, Target) %>%
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
    eval_metric = "rmse",
    nthread = 1
  )
  
  pred <- predict(mod, vali)
  
  oosError <- sqrt(mean((valiLabel - pred)^2))
  
  return(list("oosError" = oosError))
}

funRoll <- function(nrounds, eta, max_depth, early_stop_round, data) {
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
  
  return(
    list(
      "nrounds" = nrounds,
      "eta" = eta,
      "max_depth" = max_depth,
      "early_stop_round" = early_stop_round,
      "oosError" = errorRolling$oosError
    )
  )
}

### Grid ---------------------------------------------------------------------
hyperParamGrid <- expand.grid(
  nrounds = seq(100, 500, 150),
  eta = seq(0.001, 0.1, 0.003),
  max_depth = seq(1, 8, 1),
  early_stop_round = 20
)

### Tuning -------------------------------------------------------------------
mc.cores <- parallel::detectCores()/2

# Extended
paramTuningXGB <- parallel::mcmapply(
  funRoll,
  nrounds = hyperParamGrid$nrounds,
  eta = hyperParamGrid$eta,
  max_depth = hyperParamGrid$max_depth,
  early_stop_round = hyperParamGrid$early_stop_round,
  MoreArgs = list(data = data),
  mc.cores = mc.cores
)

save(paramTuningXGB, file = "./Rdata/5DAH/paramTuningXGB.RData")

# Extended Log
paramTuningXGBLog <- parallel::mcmapply(
  funRoll,
  nrounds = hyperParamGrid$nrounds,
  eta = hyperParamGrid$eta,
  max_depth = hyperParamGrid$max_depth,
  early_stop_round = hyperParamGrid$early_stop_round,
  MoreArgs = list(data = dataLog),
  mc.cores = mc.cores
)

save(paramTuningXGBLog, file = "./Rdata/5DAH/paramTuningXGBLog.RData")

# Base
paramTuningXGBBase <- parallel::mcmapply(
  funRoll,
  nrounds = hyperParamGrid$nrounds,
  eta = hyperParamGrid$eta,
  max_depth = hyperParamGrid$max_depth,
  early_stop_round = hyperParamGrid$early_stop_round,
  MoreArgs = list(data = dataBase),
  mc.cores = mc.cores
)

save(paramTuningXGBBase, file = "./Rdata/5DAH/paramTuningXGBBase.RData")

paramTuningXGBBaseLog <- parallel::mcmapply(
  funRoll,
  nrounds = hyperParamGrid$nrounds,
  eta = hyperParamGrid$eta,
  max_depth = hyperParamGrid$max_depth,
  early_stop_round = hyperParamGrid$early_stop_round,
  MoreArgs = list(data = dataLogBase),
  mc.cores = mc.cores
)

save(paramTuningXGBBaseLog, file = "./Rdata/5DAH/paramTuningXGBBaseLog.RData")