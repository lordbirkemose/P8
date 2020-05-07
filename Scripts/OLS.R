### Packages -----------------------------------------------------------------
library(magrittr)

### Data ---------------------------------------------------------------------
data <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %T", tz = "CET")) %>%
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

plot(data$RV.Daily ~ data$Start, type = "l")
