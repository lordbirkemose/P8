### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(randomForest)
  library(lubridate)
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
  dplyr::mutate(
    RVDirection = as.factor(RVDirection),
    Start = as.POSIXct(Start)
  ) %>%
  dplyr::rename(
    pctK1 = pctK.x, 
    pctK10 = pctK.y, 
    pctD1 = pctD.x, 
    pctD10 = pctD.y
  )

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

# RF for feature selection
# Tuning in 'TuningBeforeFeatureImportance.R'
set.seed(2020)
modRFFeatureSelection <- randomForest::randomForest(
  RVDirection ~ .,
  data = dataTrain,
  ntree = 5000,
  mtry = m,
  importance = TRUE,
  maxnodes = 17,
  # nodesize = 39,
  do.trace = 500
)

# Selecting features
importanceFeatureSelection <- randomForest::importance(
  modRFFeatureSelection
) %>%
  data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::select(
    Indicator = rowname,
    MDA = MeanDecreaseAccuracy,
    MDG = MeanDecreaseGini
  ) %>%
  dplyr::mutate(
    AverageMDA = mean(MDA),
    AverageMDG = mean(MDG)
  )

indicatorsMDA <- importanceFeatureSelection %>%
  dplyr::filter(MDA > AverageMDA) %>%
  dplyr::select(Indicator)

dataMDA <- indicators %>%
  dplyr::select(
    Start,
    RVDirection,
    tidyr::one_of(indicatorsMDA$Indicator)
  )

indicatorsMDG <- importanceFeatureSelection %>%
  dplyr::filter(MDG > AverageMDG) %>%
  dplyr::select(Indicator)

dataMDG <- indicators %>%
  dplyr::select(
    Start,
    RVDirection,
    tidyr::one_of(indicatorsMDG$Indicator)
  )

# Cross validation for MDA and MDG
# Tuning in 'TuningImportanceCriteria'
funCrossValidation <- function(date, dat){
  train <- dat %>%
    dplyr::filter(Start < date)

  vali <- dat %>%
    dplyr::filter(
      Start > date, 
      Start <= date %m+% months(1)
    )

  mod <- randomForest::randomForest(
    RVDirection ~ . -Start,
    data = train,
    ntree = 5000,
    mtry = m,
    maxnodes = 17
  )

  pred <- stats::predict(mod, vali, type = "class")

  oosError <- 1 - mean(pred == vali$RVDirection)

  print(paste('Done:', date))
  
  return(list("ossError" = oosError))
}

crossValidationErrorsMDA <- unique(
  lubridate::floor_date(dataMDA$Start, unit = "month")
)[-(1:10)] %>%
  purrr::map_dfr(., funCrossValidation, dat = dataMDA) %>%
  dplyr::mutate(
    weightedOosError = ossError*funWeight(1:dplyr::n(), lambda = 0.01)
  ) %>%
  dplyr::summarise(
    oosError = mean(weightedOosError)
  )



# Final model
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
