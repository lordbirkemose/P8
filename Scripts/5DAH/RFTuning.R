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
funCrossValidation <- function(dat, date, ntree, mtry, maxnodes) {
  train <- dat %>%
    dplyr::filter(Start < date)
  
  vali <- dat %>%
    dplyr::filter(
      Start > date,
      Start <= date + lubridate::weeks(1)
    )
  
  mod <- randomForest::randomForest(
    Target ~ . - Start,
    data = train,
    ntree = ntree,
    mtry = mtry,
    maxnodes = maxnodes
  )
  
  pred <- predict(mod, vali)
  
  oosError <- sqrt(mean((vali$Target - pred)^2))
  
  return(list("oosError" = oosError))
}

funRoll <- function(ntree, mtry, maxnodes, data) {
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
  
  return(
    list(
      "ntree" = ntree,
      "mtry" = mtry,
      "maxnodes" = maxnodes,
      "oosError" = errorRolling$oosError
    )
  )
}

### Grid ---------------------------------------------------------------------
hyperParamGrid <- expand.grid(
  ntree = 2^(5:12),
  mtry = 1:4,
  maxnodes = seq(10, 130, 10)
)

### Tuning -------------------------------------------------------------------
mc.cores <- parallel::detectCores()/2

# Extended
paramTuningRF <- parallel::mcmapply(
  funRoll,
  ntree = hyperParamGrid$ntree,
  mtry = hyperParamGrid$mtry,
  maxnodes = hyperParamGrid$maxnodes,
  MoreArgs = list(data = data),
  mc.cores = mc.cores
)

save(
  paramTuningRF,
  file = "./Rdata/5DAH/paramTuningRF.RData"
)

# Extended log
paramTuningRFLog <- parallel::mcmapply(
  funRoll,
  ntree = hyperParamGrid$ntree,
  mtry = hyperParamGrid$mtry,
  maxnodes = hyperParamGrid$maxnodes,
  MoreArgs = list(data = dataLog),
  mc.cores = mc.cores
)

save(
  paramTuningRFLog,
  file = "./Rdata/5DAH/paramTuningRFLog.RData"
)

# Base
paramTuningRFBase <- parallel::mcmapply(
  funRoll,
  ntree = hyperParamGrid$ntree,
  mtry = hyperParamGrid$mtry,
  maxnodes = hyperParamGrid$maxnodes,
  MoreArgs = list(data = dataBase),
  mc.cores = mc.cores
)

save(
  paramTuningRFBase,
  file = "./Rdata/5DAH/paramTuningRFBase.RData"
)

# Base log
paramTuningRFBaseLog <- parallel::mcmapply(
  funRoll,
  ntree = hyperParamGrid$ntree,
  mtry = hyperParamGrid$mtry,
  maxnodes = hyperParamGrid$maxnodes,
  MoreArgs = list(data = dataLogBase),
  mc.cores = mc.cores
)

save(
  paramTuningRFBaseLog,
  file = "./Rdata/5DAH/paramTuningRFBaseLog.RData"
)
