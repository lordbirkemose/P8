library(tidyverse)
library(magrittr)
data <- as_tibble(read.csv("./SpyCleaned.csv"))

data %<>% mutate(Log.Price = log(Price),
                Start = as.POSIXct(Start, format = "%F")) %>%
  group_by(Start) %>%
  summarise(RV.Daily = mean(diff(Log.Price)^2),
            Mean.Price = mean(Log.Price),
            Return = mean(diff(Log.Price)),
            Positive.Return = max(c(Return, 0)),
            Negative.Return = min(c(Return, 0)) ) %>%
  mutate(RV.Weekly  = zoo::rollmean(RV.Daily, k = 5, align = "right", fill = NA),
         RV.Monthly = zoo::rollmean(RV.Daily, k = 22, align = "right", fill = NA),
         RV.1.Ahead = dplyr::lag(RV.Daily)) %>%
  drop_na(); data

Base.HAR <- data %$% lm(RV.1.Ahead ~ RV.Daily + 
                                     RV.Weekly + 
                                     RV.Monthly)
summary(Base.HAR)
