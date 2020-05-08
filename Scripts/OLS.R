### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
predDirectionTestXGB <- read.csv("./Data/predDirectionTestXGB.csv") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    Start = as.POSIXct(Start, format = "%F"),
  )

data <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    Start = as.POSIXct(Start, format = "%F"),
    Log.Price = log(Price),
    Abs.Diff.Price = abs(c(diff(Log.Price), 0)),
    Abs.Diff.Price.Lag = dplyr::lag(Abs.Diff.Price)
  ) %>%
  dplyr::group_by(Start) %>%
  dplyr::summarise(
    RV.Daily = sum(diff(Log.Price)^2),
    Return = diff(Log.Price, lag = 390),
    Pos.Return = max(c(Return, 0)),
    Neg.Return = min(c(Return, 0)),
    N = dplyr::n(),
    BV.Daily = pi*N/(2*N-2)*sum(Abs.Diff.Price*Abs.Diff.Price.Lag)
  ) %>%
  dplyr::mutate(
    RV.Weekly = zoo::rollmeanr(RV.Daily, k = 5, fill = NA),
    RV.Monthly = zoo::rollmeanr(RV.Daily, k = 22, fill = NA),
    RV.1.Ahead = dplyr::lead(RV.Daily),
    RV.Direction = as.numeric(RV.Daily/dplyr::lag(RV.Daily) > 1),
    Pos.Return.Weekly = zoo::rollmeanr(Pos.Return, k = 5, fill = NA),
    Neg.Return.Weekly = zoo::rollmeanr(Neg.Return, k = 5, fill = NA),
    Jump.Daily = pmax(RV.Daily - BV.Daily, 0)
  ) %>%
  dplyr::left_join(., predDirectionTestXGB, by = "Start") %>%
  dplyr::mutate(
    RV.Direction = ifelse(
      is.na(RVDirectionPred), RV.Direction, RVDirectionPred
    )
  ) %>%
  dplyr::select(
    Start, RV.1.Ahead, RV.Daily, RV.Weekly, RV.Monthly, Jump.Daily,
    Pos.Return.Weekly, Neg.Return.Weekly, RV.Direction
  ) %>%
  tidyr::drop_na()

dataLog <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    Start = as.POSIXct(Start, format = "%F"),
    Log.Price = log(Price),
    Abs.Diff.Price = abs(c(diff(Log.Price), 0)),
    Abs.Diff.Price.Lag = dplyr::lag(Abs.Diff.Price)
  ) %>%
  dplyr::group_by(Start) %>%
  dplyr::summarise(
    RV.Daily = sum(diff(Log.Price)^2),
    Return = diff(Log.Price, lag = 390),
    Pos.Return = max(c(Return, 0)),
    Neg.Return = min(c(Return, 0)),
    N = dplyr::n(),
    BV.Daily = pi*N/(2*N-2)*sum(Abs.Diff.Price*Abs.Diff.Price.Lag)
  ) %>%
  dplyr::mutate(
    RV.Weekly = zoo::rollmeanr(RV.Daily, k = 5, fill = NA),
    RV.Monthly = zoo::rollmeanr(RV.Daily, k = 22, fill = NA),
    RV.1.Ahead = dplyr::lead(RV.Daily),
    RV.Direction = as.numeric(RV.Daily/dplyr::lag(RV.Daily) > 1),
    Pos.Return.Weekly = zoo::rollmeanr(Pos.Return, k = 5, fill = NA),
    Neg.Return.Weekly = zoo::rollmeanr(Neg.Return, k = 5, fill = NA),
    Jump.Daily = pmax(RV.Daily - BV.Daily, 0)
  ) %>%
  dplyr::left_join(., predDirectionTestXGB, by = "Start") %>%
  dplyr::mutate(
    RV.Direction = ifelse(
      is.na(RVDirectionPred), RV.Direction, RVDirectionPred
    )
  ) %>%
  dplyr::select(
    Start, RV.1.Ahead, RV.Daily, RV.Weekly, RV.Monthly, Jump.Daily,
    Pos.Return.Weekly, Neg.Return.Weekly, RV.Direction
  ) %>%
  dplyr::mutate(
    RV.1.Ahead = log(RV.1.Ahead),
    RV.Daily = log(RV.Daily),
    RV.Mothly = log(RV.Monthly),
    Jump.Daily = log(1 + Jump.Daily),
    Pos.Return.Weekly = log(1 + Pos.Return.Weekly),
    Neg.Return.Weekly = log(1 + abs(Neg.Return.Weekly))
  ) %>%
  tidyr::drop_na()

### Functions ----------------------------------------------------------------
funRolling <- function(date, dat){
  train <- dat %>%
    dplyr::filter(
      Start < date,
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
extendedOLS <- extendedOLS %>%
  dplyr::filter(Start > "2007-10-01") %>%
  dplyr::mutate(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

baseOLS <- baseOLS %>%
  dplyr::filter(Start > "2007-10-01") %>%
  dplyr::mutate(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

extendedLogOLS <- extendedLogOLS %>%
  dplyr::filter(Start > "2007-10-01") %>%
  dplyr::mutate(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

baseLogOLS <- baseLogOLS %>%
  dplyr::filter(Start > "2007-10-01") %>%
  dplyr::mutate(
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
