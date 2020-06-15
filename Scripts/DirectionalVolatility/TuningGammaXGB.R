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
      eta = 0.07,
      max_depth = 1,
      gamma = gamma
    ),
    nrounds = 400,
    early_stop_round = 20,
    verbose = 0,
    eval_metric = "error",
    nthread = 10
  )
  
  pred <- predict(mod, vali)
  pred <- ifelse(pred > 0.5, 1, 0)
  
  # print(paste("Done:", date))
  oosError <- 1 - mean(pred == valiLabel)
  
  return(list("oosError" = oosError))
}

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
  gamma = seq(from = 0, to = 5, by = 0.1),
  check = 1:51
)

### Saving -------------------------------------------------------------------
save(
  paramTuningGammaXGB,
  file = "./Rdata/paramTuningGammaXGB.RData"
)
