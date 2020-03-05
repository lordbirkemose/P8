library(tidyverse)
library(magrittr)
data <- as_tibble(read.csv("./SpyCleaned.csv"))

real_vol <- function(x){
  mean(diff(x)^2)
}

data %<>% mutate(Log.Price = log(Price),
                Start = as.POSIXct(Start, format = "%F")) %>%
  group_by(Start) %>%
  summarise(RV.Daily = real_vol(Log.Price)) %>%
  mutate(RV.Weekly  = zoo::rollmean(RV.Daily, k = 5, align = "right", fill = NA),
         RV.Monthly = zoo::rollmean(RV.Daily, k = 22, align = "right", fill = NA),
         RV.1.Ahead = dplyr::lag(RV.Daily)) %>%
  drop_na(); data

Base.HAR <- data %$% lm(RV.1.Ahead ~ RV.Daily + 
                                     RV.Weekly + 
                                     RV.Monthly)
summary(Base.HAR)