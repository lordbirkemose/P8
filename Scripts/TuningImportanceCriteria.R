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

dataMDA <- left_join_multi(
  funDirectionalVolatility(data, lag = -1),
  funAverageTrueRange(data),
  funStochasticOscillator(data, k = 1),
  funOpenCloseToDailyRange(data),
  funVolatilityRatio(data, k = 20),
  funAbsolutDailyReturn(data),
  funRealizedVolatilityCyclicty(data),
  by = "Start"
) %>%
  stats::na.omit() %>%
  dplyr::mutate(
    RVDirection = as.factor(RVDirection),
    Start = as.POSIXct(Start)
  ) %>%
  dplyr::select(-pctD)

dataMDG <- dataMDA %>%
  dplyr::select(-ATR, -pctK)

### Tuning -------------------------------------------------------------------
mc.cores <- 40
m <- sqrt(length(dataMDG) - 2)

# Maxnodes
funCrossValidationMaxnodes <- function(date, dat, maxnodes){
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
    maxnodes = maxnodes
  )
  
  pred <- stats::predict(mod, vali, type = "class")
  
  oosError <- 1 - mean(pred == vali$RVDirection)
  
  # print(paste('Done:', date))
  
  return(list("ossError" = oosError))
}

tuningMaxnodesMDA <- parallel::mclapply(
  2:500,
  function(maxnodes) {
    crossValidationErrors <- unique(
      lubridate::floor_date(dataMDA$Start, unit = "month")
    )[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidationMaxnodes,
        dat = dataMDA,
        maxnodes = maxnodes
      ) %>%
      dplyr::mutate(
        weightedOosError = ossError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(
        oosError = mean(weightedOosError)
      )

    print(paste("Max nodes:", maxnodes))

    return(
      list("maxnodes" = maxnodes, "ossError" = crossValidationErrors$oosError)
    )
  },
  mc.cores = mc.cores
) %>%
  Reduce(rbind, .)

tuningMaxnodesMDG <- parallel::mclapply(
  2:500,
  function(maxnodes) {
    crossValidationErrors <- unique(
      lubridate::floor_date(dataMDG$Start, unit = "month")
    )[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidationMaxnodes,
        dat = dataMDG,
        maxnodes = maxnodes
      ) %>%
      dplyr::mutate(
        weightedOosError = ossError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(
        oosError = mean(weightedOosError)
      )
    
    print(paste("Max nodes:", maxnodes))
    
    return(
      list("maxnodes" = maxnodes, "ossError" = crossValidationErrors$oosError)
    )
  },
  mc.cores = mc.cores
) %>%
  Reduce(rbind, .)

# Nodesize
funCrossValidationNodesize <- function(date, dat, nodesize){
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
    nodesize = nodesize
  )
  
  pred <- stats::predict(mod, vali, type = "class")
  
  oosError <- 1 - mean(pred == vali$RVDirection)
  
  # print(paste('Done:', date))
  
  return(list("ossError" = oosError))
}

tuningNodesizeMDA <- parallel::mclapply(
  1:500,
  function(nodesize) {
    crossValidationErrors <- unique(
      lubridate::floor_date(dataMDA$Start, unit = "month")
    )[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidationNodesize,
        dat = dataMDA,
        nodesize = nodesize
      ) %>%
      dplyr::mutate(
        weightedOosError = ossError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(
        oosError = mean(weightedOosError)
      )
    
    print(paste("Node size:", nodesize))
    
    return(
      list("nodesize" = nodesize, "ossError" = crossValidationErrors$oosError)
    )
  },
  mc.cores = mc.cores
) %>%
  Reduce(rbind, .)

tuningNodesizeMDG <- parallel::mclapply(
  1:500,
  function(nodesize) {
    crossValidationErrors <- unique(
      lubridate::floor_date(dataMDG$Start, unit = "month")
    )[-(1:10)] %>%
      purrr::map_dfr(
        .,
        funCrossValidationNodesize,
        dat = dataMDG,
        nodesize = nodesize
      ) %>%
      dplyr::mutate(
        weightedOosError = ossError*funWeight(1:dplyr::n(), lambda = 0.01)
      ) %>%
      dplyr::summarise(
        oosError = mean(weightedOosError)
      )
    
    print(paste("Node size:", nodesize))
    
    return(
      list("nodesize" = nodesize, "ossError" = crossValidationErrors$oosError)
    )
  },
  mc.cores = mc.cores
) %>%
  Reduce(rbind, .)

### Saving -------------------------------------------------------------------
save(
  tuningMaxnodesMGA, tuningMaxnodesMDG,
  tuningNodesizeMGA, tuningNodesizeMDG,
  file = "./Rdata/TuningImportanceCriteria.Rdata"
)