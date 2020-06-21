### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
dataextended <- funGetDataHAR_v2(
  DAH = 5,
  model = "extended",
  test = TRUE
)
dataextendedLog <- funGetDataHAR_v2(
  DAH = 5,
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
      Start <  date,
      Start >= date %m-% lubridate::years(1)
    )
  
  test <- dat %>%
    dplyr::filter(Start >= date, Start <= date + lubridate::weeks(1))

  theseDates <- test %>%
    dplyr::select(Start)
  
  if (method == "OLS") {
    mod <- lm(
      Target ~ . -Start,
      data = train
    )
  }
  
  if (method == "WLS") {
    modOLS <- lm(
      Target ~ . -Start,
      data = train
    )
    
    mod <- lm(
      Target ~ . -Start,
      data = train,
      weights = 1/abs(fitted(modOLS))
    )
  }
  
  if (method == "RF") {
    set.seed(2020)
    mod <- randomForest::randomForest(
      Target ~ . -Start,
      data = train,
      ntree = param$ntree,
      mtry = param$mtry,
      maxnodes = param$maxnodes
    )
  }
  
  if (method == "ARFIMA") {
    
    train %<>%
      dplyr::select(Target) %>%
      as.matrix()
    
    mod <- forecast::arfima(
      y = train 
    )
    
    pred <- forecast(object = mod, h = length(test))[["mean"]][1:length(test)]
  }
  
  if (method == "XGB") {
    trainResponse <- train %>%
      dplyr::select(Target) %>%
      as.matrix()
    
    train %<>%
      dplyr::select(- c(Start, Target)) %>%
      as.matrix()
    
    testResponse <- test %>%
      dplyr::select(Target) %>%
      as.matrix()
    
    test %<>%
      dplyr::select(- c(Start, Target)) %>%
      as.matrix()
    
    mod <- xgboost::xgboost(
      data = train,
      label = trainResponse,
      params = param$params,
      nrounds = param$nrounds,
      early_stop_round = param$early_stop_round,
      verbose = 0,
      eval_metric = "rmse"
    )
  }
  
  if (method != "ARFIMA") {
    pred <- stats::predict(mod, test)
  }
  
  print(
    paste(
      paste(method, model, sep = "-"), "Done:", date)
  )
  
  if (model %in% c("baseLog", "extendedLog")) {
    return(
      data.frame(
        "Start" = theseDates,
        "RV" = if (method == "XGB") as.double(exp(testResponse))
        else exp(test$Target),
        "RVPred" = exp(pred)
      )
    )
  }
  else {
    return(
      data.frame(
        "Start" = theseDates,
        "RV" = if (method == "XGB") as.double(testResponse) 
        else test$Target,
        "RVPred" = pred
      )
    )
  }
}


funRollingPred <- function(method) {
  if (!is.element(method, c("OLS","WLS","RF","XGB", "ARFIMA") )) {
    stop("The specified method is not a function option ",
         "(OLS, WLS, RF, XGB)")
  }

  dates <- read.csv("./Data/dates.csv") %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(Start = as.POSIXct(Start, format = "%F")) %>%
    dplyr::select(Start)
  
  data <- list()
  for (model in c("extended", "base", "extendedLog", "baseLog")) {
    data[[model]] <- lubridate::floor_date(dates$Start, unit = "week") %>%
      .[-(1:53)]
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
  maxnodes = 130 
)
paramRFbaseLog <- list(
  ntree = 32,
  mtry = 1,
  maxnodes = 30 
)
paramRFextended <- list(
  ntree = 64,
  mtry = 2,
  maxnodes = 130 
)
paramRFextendedLog <- list(
  ntree = 4096,
  mtry = 4,
  maxnodes = 130 
)

paramXGBbase <- list(
  nrounds = 400,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .1,
    max_depth = 1,
    gamma = 0
  )
)
paramXGBbaseLog <- list(
  nrounds = 100,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .094,
    max_depth = 1,
    gamma = 0
  )
)
paramXGBextended <- list(
  nrounds = 400,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .088,
    max_depth = 1,
    gamma = 0
  )
)
paramXGBextendedLog <- list(
  nrounds = 400,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .055,
    max_depth = 1,
    gamma = 0
  )
)

### Prediction ---------------------------------------------------------------

methods <- c("ARFIMA", "OLS", "WLS", "RF", "XGB")
models <- c("extended", "base", "extendedLog", "baseLog")
errorTable <- data.frame()
errors <- list()

for (method in methods) {
  rollPred <- funRollingPred(
    method = method
  )
  dataLong <- purrr::map_df(
    models,
    funGatherToLongFormat2,
    data = rollPred$data
  )
  errorTable %<>%
    dplyr::bind_rows(., rollPred$errors)
  write.csv(
    dataLong,
    file = paste0("./Data/5DAH/results", method,".csv"),
    row.names = FALSE
  )
}

write.table(
  errorTable,
  file = "./Data/5DAH/modelErrors.txt"
)
