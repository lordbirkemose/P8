### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(randomForest)
})

source("./Scripts/Functions.R", echo = FALSE)

### Data ---------------------------------------------------------------------
data <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %T")) %>%
  dplyr::filter(Start <= "2007-09-30")

### Determine k --------------------------------------------------------------
left_join_multi(
  funDirectionalVolatility(data, lag = -1),
  funVolatilityRatio(data, k = 5),
  funVolatilityRatio(data, k = 10),
  funVolatilityRatio(data, k = 20),
  funRelativeVigorIndex(data, k = 5),
  funRelativeVigorIndex(data, k = 10),
  funRelativeVigorIndex(data, k = 20),
  funRelativeStrengthIndexRV(data, k = 5),
  funRelativeStrengthIndexRV(data, k = 10),
  funRelativeStrengthIndexRV(data, k = 20),
  by = "Start"
) %>%
  dplyr::filter(Start <= "2005-06-30 16:00:00") %>%
  stats::na.omit() %>%
  dplyr::select(-Start, -RV) %>%
  stats::cor() %>%
  as.data.frame() %>%
  round(., 5) %>%
  dplyr::select(RVDirection)

### Indicators ---------------------------------------------------------------
indicators <- left_join_multi(
  funDirectionalVolatility(data, lag = -1),
  funAverageTrueRange(data),
  funStochasticOscillator(data, k = 1),
  funStochasticOscillator(data, k = 10),
  funOpenCloseToDailyRange(data),
  funVolatilityRatio(data, k = 20),
  funAbsolutDailyReturn(data),
  funRealizedVolatilityCyclicty(data),
  funBollingerBands(data),
  funFibonacciRatioRV(data),
  funExpMARV(data),
  funMovingAverageConvergenceDivergence(data),
  funRelativeVigorIndex(data, k = 10),
  funRelativeStrengthIndexRV(data, k = 10),
  funCommodityChannelIndex(data),
  by = "Start"
) %>%
  stats::na.omit() %>%
  dplyr::mutate(RVDirection = as.factor(RVDirection)) %>%
  dplyr::rename(
    pctK1 = pctK.x, 
    pctK10 = pctK.y, 
    pctD1 = pctD.x, 
    pctD10 = pctD.y
  )

# indicators2 <- list(
#   funDirectionalVolatility, 
#   funAverageTrueRange
# ) %>%
#   purrr::invoke_map_dfc(data = data)

### Correlation --------------------------------------------------------------
corData <- indicators %>%
  dplyr::filter(Start <= "2005-06-30 16:00:00") %>%
  dplyr::mutate(RVDirection = as.numeric(RVDirection)) %>%
  dplyr::select(-Start) %>%
  stats::cor() %>%
  as.data.frame() %>%
  round(., 2) %>%
  replace(., upper.tri(.), "")

### Feature selection --------------------------------------------------------
dataTrain <- indicators %>%
  dplyr::filter(Start <= "2005-06-30 16:00:00") %>%
  dplyr::select(-Start, -FR1U, -FR2U, -FR1L, -FR2L, -FR3L)

dataVali <- indicators %>%
  dplyr::filter(Start > "2005-06-30") %>%
  dplyr::select(-Start, -FR1U, -FR2U, -FR1L, -FR2L, -FR3L)

m <- sqrt(length(dataTrain) - 1)

# Feature selection
set.seed(2020)
modRFFeatureSelection <- randomForest::randomForest(
  RVDirection ~ .,
  data = dataTrain,
  ntree = 5000,
  mtry = m,
  importance = TRUE,
  # maxnodes = 20,
  nodesize = 7,
  do.trace = 500
)

importanceFeatureSelection <- randomForest::importance(modRFFeatureSelection)
colMeans(importanceFeatureSelection[,3:4])

# Model selection

predTrain <- stats::predict(modRFFeatureSelection, dataTrain, type = "class")
mean(predTrain == dataTrain$RVDirection)
table(predTrain, dataTrain$RVDirection)

predVali <- stats::predict(modRFFeatureSelection, dataVali, type = "class")
mean(predVali == dataVali$RVDirection)
table(predVali, dataVali$RVDirection)
### Saving data --------------------------------------------------------------
save(
  importanceFeatureSelection,
  file = "./Rdata/importanceFeatureSelection.Rdata"
)
