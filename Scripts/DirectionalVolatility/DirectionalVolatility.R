### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(randomForest)
  library(lubridate)
  library(xgboost)
})

source("./Scripts/Functions.R", echo = FALSE)

### Data ---------------------------------------------------------------------
dataRaw <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %T")) %>%
  dplyr::filter(Start <= "2007-09-30")

data <- dataRaw %$% left_join_multi(
  funDirectionalVolatility(., lag = -1),
  funAverageTrueRange(.),
  funStochasticOscillator(., k = 1),
  funOpenCloseToDailyRange(.),
  funVolatilityRatio(., k = 20),
  funAbsolutDailyReturn(.),
  funRealizedVolatilityCyclicty(.),
  by = "Start"
) %>%
  stats::na.omit() %>%
  dplyr::mutate(
    RVDirection = as.factor(RVDirection),
    Start = as.POSIXct(Start)
  ) %>%
  dplyr::select(-pctD)

### Random Forest ------------------------------------------------------------
set.seed(2020)
modRF <- randomForest::randomForest(
  RVDirection ~ . -Start,
  data = data,
  ntree = 5000,
  mtry = 6,
  maxnodes = 32,
  do.trace = 500
)

### Extreme General Boosting -------------------------------------------------
dataTrainXGB <- list(
  train = data %>%
    dplyr::select(- c(Start, RVDirection)) %>%
    as.matrix(),
  label = data %>%
    dplyr::select(RVDirection) %>%
    dplyr::mutate(RVDirection = as.numeric(RVDirection) - 1) %>%
    as.matrix()
)

set.seed(2020)
modXGB <- xgboost::xgboost(
  data = dataTrainXGB$train,
  label = dataTrainXGB$label,
  params = list(
    booster = "gbtree",
    objective = "binary:logistic",
    eta = 0.3,
    max_depth = 6,
    gamma = 0
  ),
  nrounds = 100,
  early_stop_round = 20,
  print_every_n = 10
)

### Saving data --------------------------------------------------------------
save(
  modDirectionalRV,
  file = "./Rdata/modDirectionalRV.Rdata"
)