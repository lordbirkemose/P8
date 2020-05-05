### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(randomForest)
  library(lubridate)
})

source("./Scripts/Functions.R", echo = FALSE)

### Data ---------------------------------------------------------------------
data <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %T")) %>%
  dplyr::filter(Start <= "2007-09-30")

data <- left_join_multi(
  funDirectionalVolatility(data, lag = -1),
  funAverageTrueRange(data),
  funStochasticOscillator(data, k = 1),
  funOpenCloseToDailyRange(data),
  funVolatilityRatio(data, k = 20),
  funAbsolutDailyReturn(data),
  funRealizedVolatilityCyclicty(data),
  by = "Start"
) %>%
  stats::na.omit() %>%
  dplyr::mutate(
    RVDirection = as.factor(RVDirection),
    Start = as.POSIXct(Start)
  ) %>%
  dplyr::select(-pctD)

### Functions ----------------------------------------------------------------
funCrossValidation <- function(date, dat, m){
  train <- dat %>%
    dplyr::filter(Start < date)
  
  vali <- dat %>%
    dplyr::filter(
      Start > date, 
      Start <= date %m+% months(1)
    )

  mod <- randomForest::randomForest(
    RVDirection ~ . -Start,
    data = train,
    ntree = 5000,
    mtry = m,
    maxnodes = 32
  )

  pred <- stats::predict(mod, vali, type = "class")

  oosError <- 1 - mean(pred == vali$RVDirection)

  return(list("oosError" = oosError))
}

funRollingModel <- function(date, dat, nDay){
  train <- dat %>%
    dplyr::filter(
      Start < date
    )
  
  vali <- dat %>%
    dplyr::filter(
      Start > date, 
      Start <= date + lubridate::days(nDay)
    )
  
  mod <- randomForest::randomForest(
    RVDirection ~ . -Start,
    data = train,
    ntree = 5000,
    mtry = 6,
    maxnodes = 32
  )
  
  pred <- stats::predict(mod, vali, type = "class")
  
  oosError <- 1 - mean(pred == vali$RVDirection)
  
  return(oosError)
}

### Tuning m -----------------------------------------------------------------
mc.cores <- 3

tuningM <- parallel::mclapply(
  1:8,
  function(m) {
    crossValidationErrors <- unique(
      lubridate::floor_date(data$Start, unit = "month")
    ) %>%
      .[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidation,
        dat = data,
        m = m
      ) %>%
      dplyr::mutate(
        weightedOosError = oosError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(oosError = sum(weightedOosError))
    
    return(
      list("m" = m, "oosError" = crossValidationErrors$oosError)
    )
  },
  mc.cores = mc.cores
) %>%
  do.call(rbind, .)

### Tuning retraining frequency ----------------------------------------------
mc.cores <- 3

set.seed(2020)
tuningRetrainFreq <- pbapply::pblapply(
  c(1, 5, 10, 15, 20),
  function(nDay) {
    errorRolling <- data$Start %>%
      .[seq(1, length(.), nDay)] %>%
      .[-(1:10)] %>%
      purrr::map_dfr(., funRollingModel, dat = data, nDay = nDay) %>%
      dplyr::summarise(
        hitRate = 1 - mean(oosError)
      )
    return(
      list("nDay" = nDay, "oosError" = errorRolling$essError)
    )
  }
) %>%
  do.call(rbind, .)

### Saving -------------------------------------------------------------------
save(
  tuningM,
  file = "./Rdata/TuningM.Rdata"
)
