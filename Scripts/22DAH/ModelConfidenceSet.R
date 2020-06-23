library(magrittr)

methods <- c("OLS", "WLS", "RF", "ARFIMA")
models <- c("base", "extended", "baseLog", "extendedLog")
Loss <- modelnames <- NULL

for (method in methods) {
  data <- read.csv(paste0("./Data/22DAH/results", method,".csv"))
  for (model in models) {
    Loss <- data %>%
      dplyr::filter(Type == paste0(model)) %>%
      tidyr::spread(key = Var, value = Value) %>% 
      dplyr::mutate(Start = as.POSIXct(Start)) %>%  
      dplyr::filter(Start >= "2007-10-01") %$%
      MCS::LossVol(RV, RVPred, which = "SE1") %>%
      cbind(Loss, .)
    modelnames %<>%
      cbind(., paste0(model, method))
  }
}
colnames(Loss) <- modelnames

MCSmodels <- c(
  "baseOLS", "extendedOLS", "baseLogOLS", "extendedLogOLS",
  "baseWLS", "extendedWLS", "baseLogWLS", "extendedLogWLS",
  "baseRF", "extendedRF", "baseLogRF", "extendedLogRF",
  "baseARFIMA", "baseLogARFIMA"
)

set.seed(2020)
modelSet <- MCS::MCSprocedure(
  Loss = Loss[, MCSmodels], 
  alpha = 0.05, 
  B = 5000, 
  statistic = "TR"
)
