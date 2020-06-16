### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
  library(xgboost)
})

source("./Scripts/Functions.R", echo = FALSE)

### Data ---------------------------------------------------------------------
indicators <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %T")) %>%
  dplyr::filter(Start <= "2007-09-30") %$%
  left_join_multi(
    funDirectionalVolatility(., lag = -1),
    funAverageTrueRange(.),
    funStochasticOscillator(., k = 1),
    funOpenCloseToDailyRange(.),
    funVolatilityRatio(., k = 20),
    funAbsolutDailyReturn(.),
    funRealizedVolatilityCyclicty(.),
    funFibonacciRatioRV(.),
    funExpMARV(.),
    by = "Start"
  ) %>%
  stats::na.omit() %>%
  dplyr::mutate(
    Start = as.POSIXct(Start),
    RVDirection = as.numeric(RVDirection)
  ) %>%
  dplyr::select(-c(pctD, TR, FR1U, FR2U, FR1L, FR2L, FR3L))

### Functions ----------------------------------------------------------------
# Tuning
funCrossValidation <- function(dat, date, nrounds, eta, max_depth) {
  train <- dat %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(-c(Start, RVDirection)) %>%
    as.matrix()
  
  trainLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  vali <- dat %>%
    dplyr::filter(Start > date, Start <= date + lubridate::weeks(1)) %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix()
  
  valiLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
    dplyr::filter(Start > date, Start <= date + lubridate::weeks(1)) %>%
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
    early_stop_round = 20,
    verbose = 0,
    eval_metric = "error",
    nthread = 1
  )
  
  pred <- predict(mod, vali)
  pred <- as.numeric(pred > 0.5)
  
  oosError <- 1 - mean(pred == valiLabel)

  return(list("oosError" = oosError))
}

funRoll <- function(nrounds, eta, max_depth, data) {
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
      max_depth = max_depth
    ) %>%
    dplyr::mutate(
      weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
    ) %>%
    dplyr::summarise(oosError = sum(weightedOosError))
  
  return(
    list(
      'nrounds' = nrounds,
      'eta' = eta,
      'max_depth' = max_depth,
      'oosError' = errorRolling$oosError
    )
  )
}

### Tuning -------------------------------------------------------------------
hyperParamGrid <- expand.grid(
  nrounds = seq(100, 500, 100),
  eta = seq(0.001, 0.1, 0.004),
  max_depth = seq(1, 15, 2)
)

mc.cores <- parallel::detectCores()/2

paramTuningXGB <- parallel::mcmapply(
  funRoll,
  nrounds = hyperParamGrid$nrounds,
  eta = hyperParamGrid$eta,
  max_depth = hyperParamGrid$max_depth,
  MoreArgs = list(data = indicators),
  mc.cores = mc.cores
)

paramTuningXGB <- matrix(
  unlist(paramTuningXGB),
  ncol = 4, byrow = TRUE
)
colnames(paramTuningXGB) <- c(
  'ntree', 'mtry', 'maxnodes', 'oosError'
)

save(
  paramTuningXGB,
  file = "./Rdata/5DAH/DirectionalVolatilityParamTuningXGB.RData"
)
