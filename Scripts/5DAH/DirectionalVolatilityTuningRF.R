### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(randomForest)
  library(lubridate)
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
    RVDirection = as.factor(RVDirection),
    Start = as.POSIXct(Start)
  ) %>%
  dplyr::select(-c(pctD, TR, FR1U, FR2U, FR1L, FR2L, FR3L))

### Functions ----------------------------------------------------------------
# Tuning
funCrossValidation <- function(dat, date, ntree, mtry, maxnodes) {
  train <- dat %>%
    dplyr::filter(Start < date)
  
  vali <- dat %>%
    dplyr::filter(Start > date, Start <= date + lubridate::weeks(1))
  
  mod <- randomForest::randomForest(
    RVDirection ~ . - Start,
    data = train,
    ntree = ntree,
    mtry = mtry,
    maxnodes = maxnodes
  )
  
  pred <- stats::predict(mod, vali, type = "class")
  oosError <- mean(pred == vali$RVDirection)

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

### Tuning -------------------------------------------------------------------
hyperParamGrid <- expand.grid(
  ntree = 2^(8:13),
  mtry = 1:7,
  maxnodes = seq(40, 100, 10)
)

mc.cores <- parallel::detectCores()/2

tic <- Sys.time()

paramTuningRF <- parallel::mcmapply(
  funRoll,
  ntree = hyperParamGrid$ntree,
  mtry = hyperParamGrid$mtry,
  maxnodes = hyperParamGrid$maxnodes,
  MoreArgs = list(data = indicators),
  mc.cores = mc.cores
)

toc <- Sys.time()
toc - tic

paramTuningRF <- matrix(
  unlist(paramTuningRF),
  ncol = 4, byrow = TRUE
)
colnames(paramTuningRF) <- c(
  'ntree', 'mtry', 'maxnodes', 'oosError'
)

save(
  paramTuningRF,
  file = "./Rdata/5DAH/DirectionalVolatilityParamTuningRF.RData"
)
