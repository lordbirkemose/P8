### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
  library(xgboost)
})

source("./Scripts/Functions.R", echo = FALSE)

### Data ---------------------------------------------------------------------
data <- funGetDataHAR_v2(DAH = 5, model = "extended", test = FALSE)
dataLog <- funGetDataHAR_v2(DAH = 5, model = "extendedLog")

dataBase <- data %>%
  dplyr::select(-c(Jump, Pos.Return, Neg.Return, RV.Direction))

dataLogBase <- dataLog %>%
  dplyr::select(-c(Jump, Pos.Return, Neg.Return, RV.Direction))

### Functions ----------------------------------------------------------------

funCrossValidation <- function(dat, date, nrounds, eta, max_depth, gamma=0) {
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
      gamma = gamma
    ),
    nrounds = nrounds,
    early_stop_round = 20,
    verbose = 0,
    eval_metric = "rmse",
    nthread = 1
  )
  
  pred <- predict(mod, vali)
  
  oosError <- sqrt(mean((valiLabel - pred)^2))
  
  return(list("oosError" = oosError))
}

funRoll <- function(data, nrounds, eta, max_depth, gamma) {
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
      gamma = gamma
    ) %>%
    dplyr::mutate(
      weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
    ) %>%
    dplyr::summarise(oosError = sum(weightedOosError))
  
  return(
    list(
      'gamma' = gamma,
      'oosError' = errorRolling$oosError
    )
  )
}

### Tuning of gamma ----------------------------------------------------------
mc.cores <- parallel::detectCores()

gammaTuningXGB <- parallel::mclapply(
  seq(from = 0, to = 3, by = 0.2),
  FUN = funRoll,
  data = data,
  nrounds = 400,
  eta = 0.088,
  max_depth = 1,
  mc.cores = mc.cores
)

gammaTuningXGB %<>% do.call(rbind, .)

save(
  gammaTuningXGB, 
  file = './Rdata/5DAH/gammaTuningXGB.RData'
)

gammaTuningXGBLog <- parallel::mclapply(
  seq(from = 0, to = 3, by = 0.2),
  FUN = funRoll,
  data = dataLog,
  nrounds = 400,
  eta = 0.055,
  max_depth = 1,
  mc.cores = mc.cores
)

gammaTuningXGBLog %<>% do.call(rbind, .)

save(
  gammaTuningXGBLog, 
  file = './Rdata/5DAH/gammaTuningXGBLog.RData'
)

gammaTuningXGBBase <- parallel::mclapply(
  seq(from = 0, to = 3, by = 0.2),
  FUN = funRoll,
  data = dataBase,
  nrounds = 400,
  eta = 0.1,
  max_depth = 1,
  mc.cores = mc.cores
)

gammaTuningXGBBase %<>% do.call(rbind, .)

save(
  gammaTuningXGBBase, 
  file = './Rdata/5DAH/gammaTuningXGBBase.RData'
)

gammaTuningXGBBaseLog <- parallel::mclapply(
  seq(from = 0, to = 3, by = 0.2),
  FUN = funRoll,
  data = dataBase,
  nrounds = 100,
  eta = 0.094,
  max_depth = 1,
  mc.cores = mc.cores
)

gammaTuningXGBBaseLog %<>% do.call(rbind, .)

save(
  gammaTuningXGBBaseLog, 
  file = './Rdata/5DAH/gammaTuningXGBBaseLog.RData'
)