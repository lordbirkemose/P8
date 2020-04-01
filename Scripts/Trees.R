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

listDay <- lubridate::floor_date(dataModel$Start, unit = 'day') %>%
  unique()

# Rolling the model ----------------------------------------------------------
rollFunction <- function(date, dat, target){
  train <- dat %>%
    dplyr::filter(Start < date)
  test <- dat %>%
    dplyr::filter(
      Start <= date, 
      Start <= date + lubridate::weeks(1)
    )

  # model <- randomForest(
  #   x = dplyr::select(train, -dplyr::one_of(target), -Start),
  #   y = as.factor(x = train[[target]]),
  #   ntree = 1000,
  #   mtry = 14,
  #   nodesize = 5,
  #   importance = TRUE,
  #   keep.inbag = TRUE,
  #   corr.bias = TRUE,
  #   type =  ''
  # )

  # result <- predict(model, newdata = test, type = "prob")

  # test$result_up <- result[,2]
  # test$result_down <- result[,1]

  model <- lm(
    RV.1.Ahead ~ RV.Daily + RV.Weekly + RV.Monthly + Positive.Return +
      Negative.Return,
    data = train
  )

  test$result <- predict(model, newdata = test)

  print(paste('Done:', date))
  
  return(test)
}

rolling <- unique(lubridate::floor_date(listDay, unit = 'week'))[-1] %>% 
  # .[.>=(max(.)-lubridate::days(2*365))] %>% 
  purrr::map(., rollFunction, dat=dataModel, target = 'RV.1.Ahead') %>%
  do.call(rbind, .)
