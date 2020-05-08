library(magrittr)
library(MCS)

# methods <- c("OLS", "WLS", "RF", "XGB", "ARFIMA")
methods <- c("OLS", "WLS")
models <- c("base", "baseLog", "extended", "extendedLog")
Loss <- modelnames <- c()

for(method in methods){
  data <- read.csv(paste0("./Data/results", method, ".csv"))
  for (model in models) {
    Loss <- data %>%
    dplyr::filter(Type == paste0(model, method)) %>%
    tidyr::spread(key = Var, value = Value) %>%
    dplyr::mutate(Start = as.POSIXct(Start)) %$%
    LossVol(RV, RVPred) %>%
    cbind(Loss, .)
    modelnames %<>%
      cbind(., paste0(model, method))
  }
}
colnames(Loss) <- modelnames

MCSprocedure(Loss = Loss, alpha = 0.05, B = 5000, statistic = "TR")
