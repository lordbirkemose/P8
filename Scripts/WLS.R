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
funRolling <- function(date, dat){
  train <- dat %>%
    dplyr::filter(
      Start <= date,
      Start >= date %m-% lubridate::years(1)
    )
  
  vali <- dat %>%
    dplyr::filter(
      Start > date, 
      Start <= date + lubridate::weeks(1)
    )
  
  modOLS <- lm(
    RV.1.Ahead ~ . -Start,
    data = train
  )
  
  modWLS <- lm(
    RV.1.Ahead ~ . -Start,
    data = train,
    weights = 1/abs(fitted(modOLS))
  )
  
  pred <- stats::predict(modWLS, vali)
  
  print(paste("Done:", date))
  
  return(
    data.frame("Start" = vali$Start, "RV" = vali$RV.1.Ahead, "RVPred" = pred)
  )
}

### Rolling ------------------------------------------------------------------
extendedWLS <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:53)] %>%
  purrr::map_dfr(., funRolling, dat = data)

baseWLS <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:53)] %>%
  purrr::map_dfr(
    ., 
    funRolling, 
    dat = data %>%
      dplyr::select(Start, RV.1.Ahead, RV.Daily, RV.Weekly, RV.Monthly)
  )

extendedLogWLS <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:53)] %>%
  purrr::map_dfr(., funRolling, dat = dataLog)

baseLogWLS <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:53)] %>%
  purrr::map_dfr(
    ., 
    funRolling, 
    dat = dataLog %>%
      dplyr::select(Start, RV.1.Ahead, RV.Daily, RV.Weekly, RV.Monthly)
  )

### Error measurements -------------------------------------------------------
extendedWLS %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

baseWLS %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

extendedLogWLS %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

baseLogWLS %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

### Save ---------------------------------------------------------------------
dataToSave <- c("extendedWLS", "baseWLS", "extendedLogWLS", "baseLogWLS")

dataLong <- purrr::map_df(dataToSave, funGatherToLongFormat)

write.csv(
  dataLong,
  file = "./Data/resultsWLS.csv",
  row.names = FALSE
)
