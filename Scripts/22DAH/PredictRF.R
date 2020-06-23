### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
dataextended <- funGetDataHAR_v2(DAH = 22, model = "extended", test = TRUE)
dataextendedLog <- funGetDataHAR_v2(
  DAH = 22,
  model = "extendedLog",
  test = TRUE
)

database <- dataextended %>%
  dplyr::select(-c(Jump, Pos.Return, Neg.Return, RV.Direction))

databaseLog <- dataextendedLog %>%
  dplyr::select(-c(Jump, Pos.Return, Neg.Return, RV.Direction))

### Functions ----------------------------------------------------------------
funRolling <- function(date, dat, method, param, model) {
  
  train <- dat %>%
    dplyr::filter(
      Start <  date
    )
  
  test <- dat %>%
    dplyr::filter(Start >= date, Start <= date + lubridate::weeks(1))
  
  theseDates <- test %>%
    dplyr::select(Start)
  
  set.seed(2020)
  mod <- randomForest::randomForest(
    Target ~ . -Start,
    data = train,
    ntree = param$ntree,
    mtry = param$mtry,
    maxnodes = param$maxnodes
  )
  
  pred <- stats::predict(mod, test)
  
  print(paste(paste(method, model, sep = "-"), "Done:", date))
  
  if (model %in% c("baseLog", "extendedLog")) {
    return(
      data.frame(
        "Start" = theseDates,
        "RV" = exp(test$Target),
        "RVPred" = exp(pred)
      )
    )
  }
  else {
    return(
      data.frame(
        "Start" = theseDates,
        "RV" = test$Target,
        "RVPred" = pred
      )
    )
  }
}

funRollingPred <- function(method) {
  dates <- read.csv("./Data/dates.csv") %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(Start = as.POSIXct(Start, format = "%F")) %>%
    dplyr::select(Start)
  
  data <- list()
  for (model in c("extended", "base", "extendedLog", "baseLog")) {
    data[[model]] <- lubridate::floor_date(dates$Start, unit = "week") %>%
      .[-(1:53)] %>%
      unique() %>% 
      purrr::map_dfr(
        .,
        funRolling,
        dat = get(paste0("data", model)),
        method = method,
        param = get(paste0("param", method, model)),
        model = model
      )
    
    errors <- data[[model]] %>%
      dplyr::filter(Start >= "2007-10-01") %>%
      dplyr::summarise(
        RMSE = sqrt(mean((RV - RVPred)^2)),
        MAPE = mean(abs((RV - RVPred)/RV))*100
      ) %>%
      dplyr::mutate(
        model = paste(method, model, sep = "-")
      ) %>%
      dplyr::bind_rows(errors, .)
  }
  
  return(
    list(
      data = data,
      errors = errors
    )
  )
}

### Parameters ---------------------------------------------------------------

paramRFbase <- list(
  ntree = 64,
  mtry = 1,
  maxnodes = 80 
)
paramRFbaseLog <- list(
  ntree = 128,
  mtry = 1,
  maxnodes = 30 
)
paramRFextended <- list(
  ntree = 64,
  mtry = 2,
  maxnodes = 100 
)
paramRFextendedLog <- list(
  ntree = 256,
  mtry = 4,
  maxnodes = 130 
)

### Prediction ---------------------------------------------------------------

models <- c("extended", "base", "extendedLog", "baseLog")
errorTable <- data.frame()
errors <- list()

rollPred <- funRollingPred(method = 'RF')

dataLong <- purrr::map_df(
  models,
  funGatherToLongFormat2,
  data = rollPred$data
)

write.csv(
  dataLong,
  file = './Data/22DAH/resultsRF.csv',
  row.names = FALSE
)

errorTable %<>%
  dplyr::bind_rows(., rollPred$errors)

write.table(
  errorTable,
  file = "./Data/22DAH/modelErrorsRF.txt"
)
