library(tidyverse)
library(magrittr)
data <- as_tibble(read.csv("./Data/SpyCleaned.gz"))

aggr_return <- function(price, sgn){
  
}


data %<>% mutate(Log.Price = log(Price),
                 Start = as.POSIXct(Start, format = "%F") ) %>%
  group_by(Start) %>%
  summarise(RV.Daily = sum(diff(Log.Price)^2),
            Return = diff(Log.Price, lag = 390),
            Positive.Return = max(c(Return, 0)),
            Negative.Return = min(c(Return, 0)) ) %>%
  mutate(RV.Weekly  = zoo::rollmean(RV.Daily, k = 5,  align = "right", fill = NA),
         RV.Monthly = zoo::rollmean(RV.Daily, k = 22, align = "right", fill = NA),
         RV.1.Ahead = dplyr::lag(RV.Daily)) %>%
  # select(-Return) %>%
  drop_na(); data

Base.HAR <- data %$% lm(RV.1.Ahead ~ RV.Daily + 
                                     RV.Weekly + 
                                     RV.Monthly)
summary(Base.HAR)

# The residual vs fitted plot has a megaphone shape indicating
# we should regress |eps| ~ RV.1.Ahead to obtain w_{ii}'s as inverses
# of fitted values
# w <- data %$% lm( abs( residuals(Base.HAR) ) ~ RV.1.Ahead) %>%
#   {1/predict(.)}
# 
# WLS.HAR <- data %$% lm(RV.1.Ahead ~ RV.Daily + 
#                                     RV.Weekly + 
#                                     RV.Monthly, weights = w)
#
# w <- 1/abs(predict(Base.HAR)) # Vi predicter en negativ observation 
# WLS.RV.HAR <- data %$% lm(RV.1.Ahead ~ RV.Daily + 
#                                     RV.Weekly + 
#                                     RV.Monthly, weights = w)
# 
AR.HAR <- data %$% lm(RV.1.Ahead ~ RV.Daily +
                                   RV.Weekly +
                                   RV.Monthly +
                                   Positive.Return +
                                   Negative.Return)

