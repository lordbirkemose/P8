### Packages -----------------------------------------------------------------
library(randomForest)
library(magrittr)

### Data ---------------------------------------------------------------------
data <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %T", tz = "CET"))

dataModel <- data %>%
  dplyr::mutate(
    Log.Price = log(Price),
    Start = as.Date(Start),
    Start = as.POSIXct(Start)
  ) %>%
  dplyr::group_by(Start) %>%
  dplyr::summarise(
    RV.Daily = sum(diff(Log.Price)^2),
    Return = diff(Log.Price, lag = 390),
    Positive.Return = max(c(Return, 0)),
    Negative.Return = min(c(Return, 0))
  ) %>%
  dplyr::mutate(
    RV.Weekly = zoo::rollmean(RV.Daily, k = 5,  align = "right", fill = NA),
    RV.Monthly = zoo::rollmean(RV.Daily, k = 22, align = "right", fill = NA),
    RV.1.Ahead = dplyr::lead(RV.Daily)
  ) %>%
  dplyr::select(
    Start, RV.1.Ahead, RV.Daily, RV.Weekly, RV.Monthly, Positive.Return, 
    Negative.Return
  ) %>%
  tidyr::drop_na()

# Rolling the model ----------------------------------------------------------
funCrossValidation <- function(date, dat){
  train <- dat %>%
    dplyr::filter(Start < date)
  
  vali <- dat %>%
    dplyr::filter(
      Start > date, 
      Start <= date + lubridate::weeks(1)
    )
  
  mod <- randomForest::randomForest(
    RVDirection ~ . - Start,
    data = train,
    ntree = 5000,
    mtry = m,
    # maxnodes = 20,
    nodesize = 7
  )
  
  pred <- predict(mod, newdata = vali, type = "class")
  
  oosError <- mean(pred == vali$RVDirection)
  
  print(paste('Done:', date))
  
  return(oosError)
}

crossValidationErrors <- unique(
  lubridate::floor_date(data$Start, unit = 'week')
)[-(1:10)] %>%
  purrr::map_dfr(., funCrossValidation, dat = indicators)
