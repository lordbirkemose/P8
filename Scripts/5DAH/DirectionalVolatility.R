### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
  library(xgboost)
})

source("./Scripts/Functions.R", echo = FALSE)

### Data ---------------------------------------------------------------------
data <- read.csv("./Data/SpyCleaned.gz") %>%
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

dataTrain <- data %>%
  dplyr::filter(Start <= "2007-09-30")

dataTest <- data %>%
  dplyr::filter(Start >= "2006-08-31")

### Functions ----------------------------------------------------------------
funCrossValidation <- function(dat, date, nrounds, eta, max_depth, gamma) {
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
    dplyr::filter(Start > date, Start <= date + lubridate::weeks(1))
  
  Start <- vali[,1]
  
  vali %<>% dplyr::select(- c(Start, RVDirection)) %>%
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
      gamma = gamma
    ),
    nrounds = nrounds,
    early_stop_round = 20,
    verbose = 0,
    eval_metric = "error",
    nthread = 1
  )
  
  pred <- predict(mod, vali)
  pred <- as.numeric(pred > 0.5)
  
  print(date)

  return(
    list(
      'Start' = Start,
      'RVDirectionPred' = pred
    )
  )
}

funRoll <- function(data, nrounds, eta, max_depth, gamma) {
  rolling <- data$Start %>%
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
    )
  
  return(rolling)
}

### Predict on test set ------------------------------------------------------
predDirectionTest <- funRoll(
  data = dataTest,
  nrounds = 400,
  eta = 0.089,
  max_depth = 1,
  gamma = 0
)

write.csv(
  predDirectionTest,
  file = './Data/5DAH/predDirectionTest.csv'
)
