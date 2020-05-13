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

  mod <- lm(
    RV.1.Ahead ~ . -Start,
    data = train
  )

  pred <- stats::predict(mod, vali)
  
  print(paste("Done:", date))
  
  return(
    data.frame("Start" = vali$Start, "RV" = vali$RV.1.Ahead, "RVPred" = pred)
  )
}

### Rolling ------------------------------------------------------------------
extendedOLS <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:53)] %>%
  purrr::map_dfr(., funRolling, dat = data)

baseOLS <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:53)] %>%
  purrr::map_dfr(
    ., 
    funRolling, 
    dat = data %>%
      dplyr::select(Start, RV.1.Ahead, RV.Daily, RV.Weekly, RV.Monthly)
  )

extendedLogOLS <- data$Start %>%
  lubridate::floor_date(., unit = "week") %>%
  unique() %>%
  .[-(1:53)] %>%
  purrr::map_dfr(., funRolling, dat = dataLog)

baseLogOLS <- data$Start %>%
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
extendedOLS %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

baseOLS %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

extendedLogOLS %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

baseLogOLS %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

### Save ---------------------------------------------------------------------
dataToSave <- c("extendedOLS", "baseOLS", "extendedLogOLS", "baseLogOLS")

dataLong <- purrr::map_df(dataToSave, funGatherToLongFormat)

write.csv(
  dataLong,
  file = "./Data/resultsOLS.csv",
  row.names = FALSE
)
