library(magrittr)

# methods <- c("OLS", "WLS", "RF", "XGB", "ARFIMA")
methods <- c("OLS")
models <- c("base", "baseLog", "extended", "extendedLog")
trainFreqs <- c("daily", "weekly")
Loss <- modelnames <- NULL

for (method in methods) {
  for (trainFreq in trainFreqs) {
    data <- read.csv(paste0("./Data/results", method, trainFreq,".csv"))
    for (model in models) {
      Loss <- data %>%
        dplyr::filter(Type == paste0(model)) %>%
        tidyr::spread(key = Var, value = Value) %>% 
        dplyr::mutate(Start = as.POSIXct(Start)) %>%  
        dplyr::filter(Start >= "2007-10-01") %$%
        MCS::LossVol(RV, RVPred) %>%
        cbind(Loss, .)
      modelnames %<>%
        cbind(., paste0(model, method, trainFreq))
    }
  }
}
colnames(Loss) <- modelnames

modelSet <- MCS::MCSprocedure(Loss = Loss, 
                  alpha = 0.05, 
                  B = 5000, 
                  statistic = "TR")
