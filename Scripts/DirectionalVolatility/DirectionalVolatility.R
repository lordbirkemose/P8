### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(randomForest)
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
    by = "Start"
  ) %>%
  stats::na.omit() %>%
  dplyr::mutate(Start = as.POSIXct(Start)) %>%
  dplyr::select(-pctD)

dataTest <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %T")) %>%
  dplyr::filter(Start >= "2006-08-31") %$% 
  left_join_multi(
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
  dplyr::mutate(Start = as.POSIXct(Start)) %>%
  dplyr::select(-pctD)

### Functions ----------------------------------------------------------------
funCrossValidationRF <- function(date, dat){
  train <- dat %>%
    dplyr::filter(
      Start < date,
      Start > date %m-% lubridate::years(1)
    )
  
  vali <- dat %>%
    dplyr::filter(
      Start > date, 
      Start <= date + lubridate::weeks(1)
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
  
  print(paste("Done:", date))
  
  return(list("oosError" = oosError, "Date" = date))
}

funCrossValidationXGB <- function(dat, date) {
  train <- dat %>%
    dplyr::filter(
      Start < date,
      Start >= date %m-% lubridate::years(1)
    ) %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix()
  
  trainLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
    dplyr::filter(
      Start < date,
      Start >= date %m-% lubridate::years(1)
    ) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  vali <- dat %>%
    dplyr::filter(
      Start > date, 
      # Start <= date %m+% months(1)
      Start <= date + lubridate::weeks(1)
    ) %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix()

  valiLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
    dplyr::filter(
      Start > date, 
      # Start <= date %m+% months(1)
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
      gamma = 3.1
    ),
    nrounds = 200,
    early_stop_round = 20,
    verbose = 0,
    eval_metric = "error"
  )

  pred <- predict(mod, vali)
  pred <- ifelse(pred > 0.5, 1, 0)
  oosError <- 1 - mean(pred == valiLabel)

  print(paste("Done:", date))
  
  return(list("oosError" = oosError, "Date" = date))
}

funRollingXGB <- function(date, dat){
# browser()
  train <- dat %>%
    dplyr::filter(
      Start < date,
      Start >= date %m-% lubridate::years(1)
    ) %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix()
  
  trainLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
    dplyr::filter(
      Start < date,
      Start >= date %m-% lubridate::years(1)
    ) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  vali <- dat %>%
    dplyr::filter(
      Start > date, 
      # Start <= date %m+% months(1)
      Start <= date + lubridate::weeks(1)
    )
  
  Start <- vali[,1]
  
  vali <- vali %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix()
  
  valiLabel <- dat %>%
    dplyr::select(Start, RVDirection) %>%
    dplyr::filter(
      Start > date, 
      # Start <= date %m+% months(1)
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
      gamma = 3.1
    ),
    nrounds = 200,
    early_stop_round = 20,
    verbose = 0,
    eval_metric = "error"
  )
  
  pred <- predict(mod, vali)
  pred <- ifelse(pred > 0.5, 1, 0)

  print(paste("Done:", date))
  
  return(data.frame("Start" = Start, "RVDirectionPred" = pred))
}

### Random Forest ------------------------------------------------------------
set.seed(2020)
errorRollingRF <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:10)] %>%
  purrr::map_dfr(
    .,
    funCrossValidationRF,
    dat = data %>% dplyr::mutate(RVDirection = as.factor(RVDirection))
  )
1 - mean(errorRollingRF$oosError)

### Extreme General Boosting -------------------------------------------------
set.seed(2020)
errorRollingXGB <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:10)] %>%
  purrr::map_dfr(., funRollingXGB, dat = data)
1- mean(errorRollingXGB$oosError)

### Test period prediction ---------------------------------------------------
set.seed(2020)
predDirectionTestXGB <- dataTest$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:53)] %>%
  purrr::map_dfr(., funRollingXGB, dat = dataTest)
  
### Saving data --------------------------------------------------------------
write.csv(
  predDirectionTestXGB,
  file = "./Data/predDirectionTestXGB.csv",
  row.names = FALSE
)
