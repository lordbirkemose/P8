### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
data <- funGetDataHAR(model = "extended", test = TRUE)
dataLog <- funGetDataHAR(model = "extendedLog", test = TRUE)

### Functions ----------------------------------------------------------------
funCrossValidation <- function(
  dat, date, nrounds, eta, max_depth, early_stop_round
) {
  train <- dat %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(- c(Start, RV.1.Ahead)) %>%
    as.matrix()
  
  trainLabel <- dat %>%
    dplyr::select(Start, RV.1.Ahead) %>%
    dplyr::filter(Start < date) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  vali <- dat %>%
    dplyr::filter(
      Start > date,
      Start <= date + lubridate::weeks(1)
    ) %>%
    dplyr::select(- c(Start, RV.1.Ahead)) %>%
    as.matrix()
  
  valiLabel <- dat %>%
    dplyr::select(Start, RV.1.Ahead) %>%
    dplyr::filter(
      Start > date,
      Start <= date + lubridate::weeks(1)
    ) %>%
    dplyr::select(-Start) %>%
    as.matrix()
  
  dates <- dat %>%
    dplyr::filter(
      Start > date,
      Start <= date + lubridate::weeks(1)
    )

  mod <- xgboost::xgboost(
    data = train,
    label = trainLabel,
    params = list(
      booster = "gbtree",
      objective = "reg:linear",
      eta = eta,
      max_depth = max_depth,
      gamma = 0
    ),
    nrounds = nrounds,
    early_stop_round = early_stop_round,
    verbose = 0,
    eval_metric = "rmse"
  )
  
  pred <- stats::predict(mod, vali)
  
  print(paste("Done:", date))
  
  return(
    data.frame("Start" = dates$Start, "RV" = valiLabel[,1], "RVPred" = pred)
  )
}

### Predict ------------------------------------------------------------------
XGBbase <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:10)] %>%
  purrr::map_dfr(
    .,
    funCrossValidation,
    dat = data %>%
      dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction)),
    nrounds = 200,
    eta = 0.093,
    max_depth = 1,
    early_stop_round = 20
  )

XGBBaseLog <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:10)] %>%
  purrr::map_dfr(
    .,
    funCrossValidation,
    dat = dataLog %>%
      dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction)),
    nrounds = 200,
    eta = 0.053,
    max_depth = 1,
    early_stop_round = 20
  )

XGBExtended <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:10)] %>%
  purrr::map_dfr(
    .,
    funCrossValidation,
    dat = data,
    nrounds = 200,
    eta = 0.097,
    max_depth = 1,
    early_stop_round = 20
  )

XGBExtendedLog <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:10)] %>%
  purrr::map_dfr(
    .,
    funCrossValidation,
    dat = dataLog,
    nrounds = 200,
    eta = 0.061,
    max_depth = 2,
    early_stop_round = 20
  )

### Error measurements -------------------------------------------------------
XGBExtended %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

XGBbase %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

XGBExtendedLog %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

XGBBaseLog %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

### Save ---------------------------------------------------------------------
dataToSave <- c("XGBExtended", "XGBbase", "XGBExtendedLog", "XGBBaseLog")

dataLong <- purrr::map_df(dataToSave, funGatherToLongFormat)

write.csv(
  dataLong,
  file = "./Data/resultsXGB.csv",
  row.names = FALSE
)