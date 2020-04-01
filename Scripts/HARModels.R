library(tidyverse)
library(magrittr)
data <- as_tibble(read.csv("./Data/SpyCleaned.gz"))

bi_pow <- function(X){
  n <- length(X)
  pi*n/(2*n-2) * sum( abs(X[3:n] - X[2:(n-1)]) * abs(X[2:(n-1)] - X[1:(n-2)]) )
}

rlmean <- function(vec, k){
  zoo::rollmean(vec, k = k, align = "right", fill = NA)
}

data %<>% mutate(Log.Price = log(Price),
                 Start = as.POSIXct(Start, format = "%F") ) %>%
  group_by(Start) %>%
  summarise(RV.Daily = sum(diff(Log.Price)^2),
            BV.Daily = bi_pow(Log.Price),
            Return = diff(Log.Price, lag = 390),
            Positive.Return = max(c(Return, 0)),
            Negative.Return = min(c(Return, 0)) 
            ) %>%
  mutate(RV.Weekly  = rlmean(RV.Daily, k = 5),
         RV.Monthly = rlmean(RV.Daily, k = 22),
         RV.1.Ahead = dplyr::lag(RV.Daily),
         Direction  = as.numeric( RV.Daily/dplyr::lag(RV.Daily) > 1),
         J.Daily   = max(RV.Daily - BV.Daily, 0)
         # J.Weekly  = max(RV.Weekly  - rlmean(BV.Daily,  k = 5),  0), Creates
         # J.Monthly = max(RV.Monthly - rlmean(BV.Daily , k = 22), 0)  NAS
         ) %>%
  select(-Return) %>%
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