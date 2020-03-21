library(tidyverse)
library(magrittr)
data <- as_tibble(read.csv("./Data/SpyCleaned.gz"))

data %<>% mutate(Log.Price = log(Price),
                 Start = as.POSIXct(Start, format = "%F") ) %>%
  group_by(Start) %>%
  summarise(RV.Daily = sum(diff(Log.Price)^2),
            Return = diff(Log.Price, lag = 390),
            Positive.Return = max(c(Return, 0)),
            Negative.Return = min(c(Return, 0)) ) %>%
  mutate(RV.Weekly  = zoo::rollmean(RV.Daily, k = 5,  align = "right", fill = NA),
         RV.Monthly = zoo::rollmean(RV.Daily, k = 22, align = "right", fill = NA),
         RV.1.Ahead = dplyr::lead(RV.Daily), 
         Direction  = as.numeric( RV.Daily/dplyr::lag(RV.Daily) > 1) ) %>%
  select(Start, RV.1.Ahead, RV.Daily, RV.Weekly, RV.Monthly, 
         Positive.Return, Negative.Return, Direction) %>%
  drop_na(); data

Base.HAR <- data %$% lm(RV.1.Ahead ~ RV.Daily + 
                                     RV.Weekly + 
                                     RV.Monthly)

WLS.HAR.L.D <- data %$% lm(RV.1.Ahead ~ RV.Daily +
                                        RV.Weekly +
                                        RV.Monthly +
                                        Positive.Return +
                                        Negative.Return +
                                        Direction , weights = 1/predict(Base.HAR))  