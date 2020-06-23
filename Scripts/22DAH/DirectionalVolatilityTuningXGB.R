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
    funStochasticOscillator(., k = 1),
    funStochasticOscillator(., k = 10),
    funOpenCloseToDailyRange(.),
    funVolatilityRatio(., k = 20),
    funAbsolutDailyReturn(.),
    funBollingerBands(.),
    funRelativeVigorIndex(., k = 10),
    funRelativeStrengthIndexRV(., k = 10),
    by = "Start"
  ) %>%
  stats::na.omit() %>%
  dplyr::mutate(
    RVDirection = as.factor(RVDirection),
    Start = as.POSIXct(Start)
  ) %>%
  dplyr::rename(
    pctK1 = pctK.x, 
    pctK10 = pctK.y, 
    pctD1 = pctD.x, 
    pctD10 = pctD.y
  ) %>%
  dplyr::select(-c(UBB, LBB, RVI, RV))

### Functions ----------------------------------------------------------------

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
  nrounds = seq(500, 800, 100),
  eta = seq(0.09, 0.2, 0.008),
  max_depth = seq(2, 4, 1)
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
  'nrounds', 'eta', 'max_depth', 'oosError'
)

save(
  paramTuningXGB,
  file = "./Rdata/22DAH/DirectionalVolatilityParamTuningXGB.RData"
)