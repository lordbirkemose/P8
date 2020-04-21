### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(randomForest)
  library(caret)
  library(e1071)
})

source("./Scripts/Functions.R", echo = FALSE)

### Indicator functions -------------------------------------------------------
## Average True Range
funAverageTrueRange <- function(data, k = 10) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      High = max(Price),
      Low = min(Price),
      Close = dplyr::last(Price)
    ) %>%
    dplyr::mutate(
      Close = dplyr::lag(Close), 
      TR = pmax(High - Low, abs(High - Close), abs(Low - Close)),
      ATR = zoo::rollmeanr(TR, k = k, fill = NA)
    ) %>%
    dplyr::select(Start, ATR, TR)

  return(dat)
}

## Stochastic Oscillator
funStochasticOscillator = function(data, k) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      High = max(Price),
      Low = min(Price),
      Close = dplyr::last(Price)
    ) %>%
    dplyr::mutate(
      LowK = -zoo::rollmaxr(-Low, k = k, fill = NA),
      HighK = zoo::rollmaxr(High, k = k, fill = NA),
      pctK = (Close - LowK)/(HighK - LowK),
      pctD = zoo::rollmeanr(pctK, k = 3, fill = NA)
    ) %>%
    dplyr::select(Start, pctK , pctD)

  return(dat)
}

## Open-Close To Daily Range
funOpenCloseToDailyRange <- function(data) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      High = max(Price),
      Low = min(Price),
      Close = dplyr::last(Price),
      Open = dplyr::first(Price)
    ) %>%
    dplyr::mutate(OCTFR = (Open - Close)/(High - Low)) %>%
    dplyr::select(Start, OCTFR)

  return(dat)
}

## Volatility Ratio
funVolatilityRatio <- function(data, k) {
  dat <- data %>%
    funAverageTrueRange(., k = k) %>%
    dplyr::mutate(VR = TR/ATR) %>%
    dplyr::select(Start, VR)

  return(dat)
}

## Absolut Daily Return
funAbsolutDailyReturn <- function(data) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(logClose = log(dplyr::last(Price))) %>%
    dplyr::mutate(ADR = c(NA, abs(diff(logClose)))) %>%
    dplyr::select(Start, ADR)

  return(dat)
}

## Realized Vollatility Cyclicty
funRealizedVolatilityCyclicty <- function(data) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      High = max(Price),
      Low = min(Price),
      RV = sum(diff(log(Price))^2)
    ) %>%
    dplyr::mutate(RVC = (RV - High)/(RV - Low)) %>%
    dplyr::select(Start, RVC)
}

## Bollinger Bands
funBollingerBands <- function(data, k = 20) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      Close = dplyr::last(Price),
      RV = sum(diff(log(Price))^2),
      meanPrice = mean(Price)
    ) %>%
    dplyr::mutate(
      MA = zoo::rollmeanr(meanPrice, k = k, fill = NA),
      RVK = zoo::rollmeanr(RV, k = k, fill = NA),
      UBB = MA + 2*RVK,
      LBB = MA - 2*RVK,
      pctB = (Close - LBB)/(UBB - LBB)
    ) %>%
    dplyr::select(Start, UBB, LBB, pctB)

  return(dat)
}

## Fibonacci Ratio Realized Volatility
funFibonacciRatioRV <- function(data, k = 20) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      RV = sum(diff(log(Price))^2),
      High = max(Price),
      Low = min(Price),
      Close = dplyr::last(Price)
    ) %>%
    dplyr::mutate(
      RVK = zoo::rollmeanr(RV, k = k, fill = NA),
      Close = dplyr::lag(Close), 
      TR = pmax(High - Low, abs(High - Close), abs(Low - Close)),
      ATR = zoo::rollmeanr(TR, k = k, fill = NA),
      FR1U = ATR + 1.618*RVK,
      FR2U = ATR + 2.618*RVK,
      FR3U = ATR + 4.236*RVK,
      FR1L = ATR - 1.618*RVK,
      FR2L = ATR - 2.618*RVK,
      FR3L = ATR - 4.236*RVK
    ) %>%
    dplyr::select(Start, FR1U, FR2U, FR3U, FR1L, FR2L, FR3L)

  return(dat)
}

## Exponential Moving Average of Realised Volatility
funExpMARV <- function(data, k = 25, lambda = 0.05) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(RV = sum(diff(log(Price))^2)) %>%
    dplyr::mutate(
      EMARV = zoo::rollapplyr(
        data = .$RV,
        width = k,
        FUN = stats::weighted.mean,
        w = exp(lambda*1:k)/sum(exp(lambda*1:k)),
        fill = NA
      )
    ) %>%
    dplyr::select(Start, EMARV)

  return(dat)
}

## Moving Average Convergence Divergence
funMovingAverageConvergenceDivergence <- function(data, k = 12, h = 26) {
  dat <- data %>%
    funExpMARV(., k = k) %>%
    dplyr::mutate(
      EMARV_h = funExpMARV(data, k = h)$EMARV,
      MACD = EMARV - EMARV_h
    ) %>%
    dplyr::select(Start, MACD)

  return(dat)
}

## Relative Vigor Index
funRelativeVigorIndex <- function(data, k) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      High = max(Price),
      Low = min(Price),
      Close = dplyr::last(Price),
      Open = dplyr::first(Price)
    ) %>%
    dplyr::mutate(
      CO0 = Close - Open,
      CO1 = Close - dplyr::lag(Open, n = 1),
      CO2 = Close - dplyr::lag(Open, n = 2),
      CO3 = Close - dplyr::lag(Open, n = 3),
      HL0 = High - Low,
      HL1 = High - dplyr::lag(Low, n = 1),
      HL2 = High - dplyr::lag(Low, n = 2),
      HL3 = High - dplyr::lag(Low, n = 3),
      V = (CO0 + 2*(CO1 + CO2) + CO3)/
        (HL0 + 2*(HL1 + HL2) + HL3 + .Machine$double.eps),
      RVI = zoo::rollmeanr(V, k = k, fill = NA)
    ) %>%
    dplyr::select(Start, V, RVI)

  return(dat)
}

## Relative Strength Index for Realized Volatility
funRelativeStrengthIndexRV <- function(data, k) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(RV = sum(diff(log(Price))^2)) %>%
    dplyr::mutate(
      RVChange = c(NA, diff(RV)),
      RVInc = ifelse(RVChange > 0, RVChange, NA),
      RVDec = ifelse(RVChange < 0, RVChange, NA),
      RVIncMean = zoo::rollapplyr(
        RVInc,
        width = k,
        FUN = mean,
        na.rm = TRUE,
        fill = NA
      ),
      RVDecMean = zoo::rollapplyr(
        RVDec,
        width = k,
        FUN = mean,
        na.rm = TRUE,
        fill = NA
      ),
      RSIRV = 1 - 1/(1 + RVIncMean/RVDecMean)
    ) %>%
    dplyr::select(Start, RSIRV)

  return(dat)
}

## Commodity Channel Index
funCommodityChannelIndex <- function(data, k = 20) {
  dat <- data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      High = max(Price),
      Low = min(Price),
      Close = dplyr::last(Price)
    ) %>%
    dplyr::mutate(
      TP = (Close + High + Low)/3,
      MATP = zoo::rollmeanr(TP, k = k, fill = NA),
      MD = TTR::runMAD(TP, n = k, center = MATP, stat = "mean"),
      CCI = (TP - MATP)/(0.015*MD)
    ) %>%
    dplyr::select(Start, CCI)

  return(dat)
}

## Realized volatility
funDirectionalVolatility <- function(data, lag) {
  dat <-  data %>%
    dplyr::mutate(Start = format(Start, "%F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(RV = sum(diff(log(Price))^2)) %>%
    dplyr::mutate(RVDirection = as.numeric(RV/dplyr::lag(RV) > 1)) %>%
    dplyr::select(Start, RVDirection, RV)

  if(missing(lag)) {
    return(dat)
  } else if(lag < 0) {
    dat %<>% dplyr::mutate(RVDirection = dplyr::lead(RVDirection, n = -lag))
  } else {
    dat %<>% dplyr::mutate(RVDirection = dplyr::lag(RVDirection, n = lag))
  }
}

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

### Random Forest ------------------------------------------------------------
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
  ntree = 500,
  mtry = m,
  importance = TRUE
)

importanceFeatureSelection <- randomForest::importance(modRFFeatureSelection)

# Model selection

predVali <- stats::predict(modRFFeatureSelection, dataVali, type = "class")
mean(predVali == dataVali$RVDirection)
table(predVali, dataVali$RVDirection)

### Saving data --------------------------------------------------------------
save(
  importanceFeatureSelection,
  file = "./Rdata/importanceFeatureSelection.Rdata"
)
