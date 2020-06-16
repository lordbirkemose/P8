### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(randomForest)
  library(lubridate)
  library(xgboost)
})

source("./Scripts/Functions.R", echo = FALSE)

### Data ---------------------------------------------------------------------
indicators <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %T")) %>%
  dplyr::filter(Start <= "2007-09-30") %$%
  left_join_multi(
  funDirectionalVolatility(., lag = -1),
  funAverageTrueRange(.),
  funStochasticOscillator(., k = 1),
  funStochasticOscillator(., k = 10),
  funOpenCloseToDailyRange(.),
  funVolatilityRatio(., k = 20),
  funAbsolutDailyReturn(.),
  funRealizedVolatilityCyclicty(.),
  funBollingerBands(.),
  funFibonacciRatioRV(.),
  funExpMARV(.),
  funMovingAverageConvergenceDivergence(.),
  funRelativeVigorIndex(., k = 10),
  funRelativeStrengthIndexRV(., k = 10),
  funCommodityChannelIndex(.),
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
  )%>%
  dplyr::select(-c(FR1U, FR2U, FR1L, FR2L, FR3L))

### Tunning before feature selection -----------------------------------------
# gridPreSelection <- expand.grid(
#   ntree = 2^(8:13),
#   mtry = 3:12,
#   maxnodes = seq(10, 100, 10)
# )

gridPreSelection <- expand.grid(
  ntree = 2^(10:13),
  mtry = 10:18,
  maxnodes = seq(60, 100, 5)
)

mc.cores <- parallel::detectCores() - 1

tic <- Sys.time()
paramTuningPreSelection <- parallel::mcmapply(
  function(ntree, mtry, maxnodes) {
    set.seed(2020)
    mod <- randomForest::randomForest(
      RVDirection ~ . -Start,
      data = indicators,
      ntree = ntree,
      mtry = mtry,
      maxnodes = maxnodes
    )
    
    return(
      list(
        'ntree' = ntree,
        'mtry' = mtry,
        'maxnodes' = maxnodes,
        'oobError' = mean(mod$err.rate[,1])
      )
    )
  },
  ntree = gridPreSelection$ntree,
  mtry = gridPreSelection$mtry,
  maxnodes = gridPreSelection$maxnodes,
  mc.cores = mc.cores
)
toc <- Sys.time()

(time <- toc - tic)

### Random Forest feature selection ------------------------------------------
set.seed(2020)
modRFFeatureSelection <- randomForest::randomForest(
  RVDirection ~ . - Start,
  data = indicators,
  ntree = 4096,
  mtry = 16,
  maxnodes = 70,
  importance = TRUE,
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
  dplyr::arrange(MDA) %>%
  dplyr::mutate(MDAScore = 1:21) %>%
  dplyr::arrange(MDG) %>%
  dplyr::mutate(MDGScore = 1:21) %>%
  dplyr::mutate(Score = MDAScore + MDGScore) %>%
  dplyr::arrange(Score) %>%
  dplyr::mutate(AverageScore = mean(Score))

### Saving data --------------------------------------------------------------
save(
  importanceFeatureSelection,
  file = "./Rdata/5DAH/importanceFeatureSelection.RData"
)
