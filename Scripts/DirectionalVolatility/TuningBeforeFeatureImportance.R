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
  ) %>%
  dplyr::select(-FR1U, -FR2U, -FR1L, -FR2L, -FR3L)

dataTrain <- indicators %>%
  dplyr::filter(Start <= "2005-06-30 16:00:00") %>%
  dplyr::select(-Start)

dataVali <- indicators %>%
  dplyr::filter(Start > "2005-06-30") %>%
  dplyr::select(-Start)

### Tuning -------------------------------------------------------------------
m <- sqrt(length(dataTrain) - 1)

# Tuning
tuningMaxnodes <- purrr::map_dfr(
  2:500, 
  function(maxnodes) {
    set.seed(2020)
    mod <- randomForest::randomForest(
      RVDirection ~ .,
      data = dataTrain,
      ntree = 5000,
      mtry = m,
      maxnodes = maxnodes
    )
    
    pred <- stats::predict(mod, dataVali, type = "class")
    oosError <- mean(pred == dataVali$RVDirection)
    
    return(
      list("maxnodes" = maxnodes, "ossError" = oosError)
    )
  }
)

tuningNodesize <- purrr::map_dfr(
  1:500, 
  function(nodesize) {
    set.seed(2020)
    mod <- randomForest::randomForest(
      RVDirection ~ .,
      data = dataTrain,
      ntree = 5000,
      mtry = m,
      nodesize = nodesize
    )
    
    pred <- stats::predict(mod, dataVali, type = "class")
    oosError <- mean(pred == dataVali$RVDirection)
    
    return(
      list("nodesize" = nodesize, "ossError" = oosError)
    )
  }
)

### Saving -------------------------------------------------------------------
save(
  tuningMaxnodes, tuningNodesize,
  file = "./Rdata/TuningBeforeFeatureImportance.Rdata"
)