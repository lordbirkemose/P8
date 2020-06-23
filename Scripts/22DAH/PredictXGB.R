### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
dataextended <- funGetDataHAR_v2(DAH = 5, model = "extended", test = TRUE)
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
      Start <  date
    )
  
  test <- dat %>%
    dplyr::filter(Start >= date, Start <= date + lubridate::weeks(1))
  
  theseDates <- test %>%
    dplyr::select(Start)
  
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
    eval_metric = "rmse",
    ntread = 1
  )
  
  pred <- stats::predict(mod, test)
  
  print(paste(paste(method, model, sep = "-"), "Done:", date))
  
  if (model %in% c("baseLog", "extendedLog")) {
    return(
      data.frame(
        "Start" = theseDates,
        "RV" = as.double(exp(testResponse)),
        "RVPred" = exp(pred)
      )
    )
  }
  else {
    return(
      data.frame(
        "Start" = theseDates,
        "RV" = as.double(testResponse),
        "RVPred" = pred
      )
    )
  }
}

funRollingPred <- function(method) {
  data <- list()
  for (model in c("extended", "base", "extendedLog", "baseLog")) {
    data[[model]] <- get(paste0("data", model))$Start %>%
      lubridate::floor_date(., unit = "week") %>%
      unique() %>%
      .[-(1:53)] %>%
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
  nrounds = 300,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .05,
    max_depth = 1,
    gamma = 0
  )
)
paramXGBextended <- list(
  nrounds = 200,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .071,
    max_depth = 1,
    gamma = 0
  )
)
paramXGBextendedLog <- list(
  nrounds = 500,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .02,
    max_depth = 1,
    gamma = 0
  )
)

### Prediction ---------------------------------------------------------------

models <- c("extended", "base", "extendedLog", "baseLog")
errorTable <- data.frame()
errors <- list()

rollPred <- funRollingPred(method = 'XGB')

dataLong <- purrr::map_df(
  models,
  funGatherToLongFormat2,
  data = rollPred$data
)

write.csv(
  dataLong,
  file = './Data/22DAH/resultsXGB.csv',
  row.names = FALSE
)

errorTable %<>%
  dplyr::bind_rows(., rollPred$errors)

write.table(
  errorTable,
  file = "./Data/22DAH/modelErrorsXGB.txt"
)
