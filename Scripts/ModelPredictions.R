### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
dataextended <- funGetDataHAR(
  model = "extended",
  test = TRUE
  )
dataextendedLog <- funGetDataHAR(
  model = "extendedLog",
  test = TRUE
)

database <- dataextended %>%
  dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction))

databaseLog <- dataextendedLog %>%
  dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction))


### Functions ----------------------------------------------------------------
funRolling <- function(
  date, dat, method, trainFreq, param
) {
  
  train <- dat %>%
    dplyr::filter(
      Start <  date,
      Start >= date %m-% lubridate::years(1)
    )
  
  
  test <- dat %>%
    dplyr::filter(
      Start >= date
    ) %>% 
    {
      if (trainFreq == "weekly") {
        dplyr::filter(., Start <= date %m+% lubridate::weeks(1))
      }
      else {
        dplyr::filter(., Start == date)
      }
    }
  theseDates <- test %>%
    dplyr::select(Start)
  
  if (method == "OLS") {
    mod <- lm(
      RV.1.Ahead ~ . -Start,
      data = train
    )
  }
  
  if (method == "WLS") {
    modOLS <- lm(
      RV.1.Ahead ~ . -Start,
      data = train
    )
    
    mod <- lm(
      RV.1.Ahead ~ . -Start,
      data = train,
      weights = 1/abs(fitted(modOLS))
    )
  }
  
  if (method == "RF") {
    set.seed(2020)
    mod <- randomForest::randomForest(
      RV.1.Ahead ~ . -Start,
      data = train,
      ntree = param$ntree,
      mtry = param$mtry,
      maxnodes = param$maxnodes
    )
  }
  
  if (method == "XGB") {
    trainResponse <- train %>%
      dplyr::select(RV.1.Ahead) %>%
      as.matrix()
    
    train %<>%
      dplyr::select(- c(Start, RV.1.Ahead)) %>%
      as.matrix()
    
    testResponse <- test %>%
      dplyr::select(RV.1.Ahead) %>%
      as.matrix()
    
    test %<>%
      dplyr::select(- c(Start, RV.1.Ahead)) %>%
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

  pred <- stats::predict(mod, test)
  
  # print(paste("Done:", date))
  return(
    data.frame(
      "Start" = theseDates,
      "RV" = if (method == "XGB") as.double(testResponse) 
             else test$RV.1.Ahead,
      "RVPred" = pred)
  )
  
}


funRollingPred <- function(
  method, trainFreq
) {
  if (!is.element(method, c("OLS","WLS","RF","XGB", "ARFIMA") )) {
    stop("The specified method is not a function option ",
         "(OLS, WLS, RF, XGB)")
  }
  if (!is.element(trainFreq, c("daily", "weekly") )) {
    stop("The specified trainFreq is not a function option ",
         "(daily, weekly)")
  }
  dates <- read.csv("./Data/dates.csv") %>%
    tidyr::as_tibble() %>%
    dplyr::mutate(Start = as.POSIXct(Start, format = "%F")) %>%
    dplyr::select(Start)
  
  data <- list()
  for (model in c("extended", "base", "extendedLog", "baseLog")) {
    data[[model]] <- dates %>%
      dplyr::filter( Start >= as.POSIXct("2007-10-01") %m+% 
                      lubridate::years(1)
      ) %>%
      {
        if (trainFreq == "weekly") {
          lubridate::floor_date(.$Start, unit = "week") %>%
          unique()
        } else .$Start
      } %>% 
      purrr::map_dfr(
        .,
        funRolling,
        dat = get( paste0("data", model) ), 
        trainFreq = trainFreq,
        method = method,
        param = get( paste0("param", method, model))
      ) %>%
      dplyr::filter(Start >= "2008-10-01")
    
    print(
      paste(method, model, trainFreq)
    )
    data[[model]] %>%
      dplyr::filter(Start >= "2007-10-01") %>%
      dplyr::summarise(
        RMSE = sqrt(mean((RV - RVPred)^2)),
        MAPE = mean(abs((RV - RVPred)/RV))*100
      ) %>%
      print()
  }
  
  return(data)
}


### Parameters ---------------------------------------------------------------

paramRFbase <- list(
  ntree = 4,
  mtry = 2,
  maxnodes = 6 
)
paramRFbaseLog <- list(
  ntree = 1024,
  mtry = 2,
  maxnodes = 10 
)
paramRFextended <- list(
  ntree = 2,
  mtry = 3,
  maxnodes = 9 
)
paramRFextendedLog <- list(
  ntree = 256,
  mtry = 3,
  maxnodes = 10 
)

paramXGBbase <- list(
  nrounds = 200,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .093,
    max_depth = 1,
    gamma = 0
  )
)
paramXGBbaseLog <- list(
  nrounds = 200,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .053,
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
    eta = .097,
    max_depth = 1,
    gamma = 0
  )
)
paramXGBextendedLog <- list(
  nrounds = 200,
  early_stop_round = 20,
  params = list(
    booster = "gbtree",
    objective = "reg:linear",
    eta = .061,
    max_depth = 2,
    gamma = 0
  )
)





### Prediction ---------------------------------------------------------------

methods <- c("OLS", "WLS","RF", "XGB")
models <- c("extended", "base", "extendedLog", "baseLog")
trainFreqs <- c("daily", "weekly")


for (method in methods) {
  for (trainFreq in trainFreqs) {
    data <- funRollingPred(
      method = method,
      trainFreq = trainFreq
    )
    dataLong <- purrr::map_df(
      models,
      funGatherToLongFormat2,
      data = data
    )
    write.csv(
      dataLong,
      file = paste0("./Data/results", method, trainFreq,".csv"),
      row.names = FALSE
    )
  }
}