### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
  library(xgboost)
})

source("./Scripts/Functions.R", echo = FALSE)

### Data ---------------------------------------------------------------------
dataRaw <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %T")) %>%
  dplyr::filter(Start <= "2007-09-30")

data <- dataRaw %$% left_join_multi(
  funDirectionalVolatility(., lag = -1),
  funAverageTrueRange(.),
  funStochasticOscillator(., k = 1),
  funOpenCloseToDailyRange(.),
  funVolatilityRatio(., k = 20),
  funAbsolutDailyReturn(.),
  funRealizedVolatilityCyclicty(.),
  by = "Start"
) %>%
  stats::na.omit() %>%
  dplyr::mutate(
    RVDirection = RVDirection,
    Start = as.POSIXct(Start)
  ) %>%
  dplyr::select(-pctD)

### Functions ----------------------------------------------------------------
funCrossValidationXGB <- function(
  dat, date, nrounds, eta, max_depth, early_stop_round
) {
  train <- dat %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix()
  
  trainLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  vali <- dat %>%
    dplyr::filter(
      Start > date,
      Start <= date + lubridate::weeks(1)
    ) %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix()
  
  valiLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
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
      objective = "binary:logistic",
      eta = eta,
      max_depth = max_depth,
      gamma = 0
    ),
    nrounds = nrounds,
    early_stop_round = early_stop_round,
    verbose = 0,
    eval_metric = "error"
  )
  
  pred <- predict(mod, vali)
  pred <- ifelse(pred > 0.5, 1, 0)
  
  # print(paste("Done:", date))
  oosError <- 1 - mean(pred == valiLabel)
  
  return(list("oosError" = oosError))
}

funCrossValidationGammaXGB <- function(dat, date, gamma) {
  train <- dat %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix()
  
  trainLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  vali <- dat %>%
    dplyr::filter(
      Start > date, 
      # Start <= date %+m% months(1)
      Start <= date + lubridate::weeks(1)
    ) %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix()
  
  valiLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
    dplyr::filter(
      Start > date, 
      # Start <= date %+m% months(1)
      Start <= date + lubridate::weeks(1)
    ) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  mod <- xgboost::xgboost(
    data = train,
    label = trainLabel,
    params = list(
      booster = "gbtree",
      objective = "binary:logistic",
      eta = 0.1,
      max_depth = 1,
      gamma = gamma
    ),
    nrounds = 200,
    early_stop_round = 20,
    verbose = 0,
    eval_metric = "error"
  )
  
  pred <- predict(mod, vali)
  pred <- ifelse(pred > 0.5, 1, 0)
  
  # print(paste("Done:", date))
  oosError <- 1 - mean(pred == valiLabel)
  
  return(list("oosError" = oosError))
}

### Grid tuning --------------------------------------------------------------
hyperParamGrid <- expand.grid(
  nrounds = seq(100, 500, 100),
  eta = seq(0.001, 0.9, 0.002),
  max_depth = seq(1, 10, 1),
  early_stop_round = seq(20, 100, 30)
)

paramTuningXGB <- mapply(
  function(nrounds, eta, max_depth, early_stop_round, check) {
    errorRolling <- data$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidationXGB,
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

### Tuning gamma -------------------------------------------------------------
paramTuningGammaXGB <- mapply(
  function(gamma, check) {
    errorRolling <- data$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidationGammaXGB,
        dat = data,
        gamma = gamma
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    print(paste("Done:", check, "of 200"))
    
    return("oosError" = errorRolling$oosError)
  },
  gamma = seq(from = 0, to = 10, by = 0.1),
  check = 1:101
)

### Saving -------------------------------------------------------------------
save(
  paramTuningXGB,
  file = "./Rdata/paramTuningXGB.RData"
)

save(
  paramTuningGammaXGB,
  file = "./Rdata/paramTuningGammaXGB.RData"
)
