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
    funDirectionalVolatility(., lag = -22),
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
  
  oosError <- 1 - mean(pred == valiLabel)
  
  print(date)
  
  return(list("oosError" = oosError))
}

### Tuning of gamma ----------------------------------------------------------
mc.cores <- parallel::detectCores()/2

gammaTuningXGB <- parallel::mclapply(
  seq(from = 0, to = 3, by = 0.3),
  FUN = function(gamma, data) {
    errorRolling <- data$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = data,
        nrounds = 400,
        eta = 0.089,
        max_depth = 1,
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
  },
  data = indicators,
  mc.cores = mc.cores
)

gammaTuningXGB %<>% do.call(rbind, .)

save(
  gammaTuningXGB, 
  file = './Rdata/22DAH/DirectionalVolatilityGammaTuningXGB.RData'
)