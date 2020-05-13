### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
})

source("./Scripts/Functions.R")

### Data (temp afsnit) -------------------------------------------------------
data <- funGetDataHAR(test = TRUE)
dataLog <- funGetDataHAR(model = "extendedLog")

dataBase <- data %>%
  dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction))

dataLogBase <- dataLog %>%
  dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction))
### Functions ----------------------------------------------------------------
funRolling <- function(
  date, dat, trainFreq, method, param
) {
  train <- dat %>%
    dplyr::filter(
      Start <= date,
      Start >= date %m-% lubridate::years(1)
    )
  
  test <- dat %>%
    dplyr::filter(
      Start > date, 
      Start <= date + ifelse(trainFreq == "weekly", lubridate::weeks(1), 0)
    )
  
  if (method == "OLS") {
    mod <- lm(
      RV.1.Ahead ~ . -Start,
      data = train
    )
  }
  
  if (method = "WLS") {
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
    mod <- randomForest::randomForest(
      RV.1.Ahead ~ .,
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
    
    # mod <- xgboost::xgboost(
    #   data = train,
    #   label = trainResponse,
    #   params = list(
    #     booster = "gbtree",
    #     objective = "reg:linear",
    #     eta = eta,
    #     max_depth = max_depth,
    #     gamma = 0
    #   ),
    #   nrounds = nrounds,
    #   early_stop_round = early_stop_round,
    #   verbose = 0,
    #   eval_metric = "rmse"
    # )
  }
  
  pred <- stats::predict(mod, test)
  
  print(paste("Done:", date))
  
  return(
    data.frame(
      "Start" = date, 
      "RV" = ifelse(method == "XGB", testResponse,
                                     test$RV.1.Ahead), 
      "RVPred" = pred)
  )
  
}


funRollingPred <- function(
  dat, trainFreq, method, param
) {
  if (!is.element(method, c("OLS","WLS","RF","XGB") )) {
    stop("The specified method is not a function option ",
         "(OLS, WLS, RF, XGB)")
  }
  if (!is.element(trainFreq, c("daily", "weekly") )) {
    stop("The specified trainFreq is not a function option ",
         "(daily, weekly)")
  }
  dates <- read.csv("./Data/dates.csv") %>%
    tidyr::as_tibble() %>%
    dplyr::select(Start)
  
  for (model in c("extended", "base", "extendedLog", "baseLog")) {
    data <- dates %>%
      filter(Start >= "2007-10-01" %m+% lubridate::years(1)) %>%
      {
        if (trainFreq == "daily") {
          lubridate::floor_date(., unit = "day")
        }
        else {
          lubridate::floor_date(., unit = "week")
        }
      } %>%
      purr::map_dfr(
        .,
        funRolling,
        dat = funGetDataHAR(model = model, test = TRUE), 
        trainFreq = trainFreq,
        method = method,
        param = param
      )
    
    paste(method, model, "- RMSE and MAPE")
    data %>%
      dplyr::filter(Start >= "2007-10-01") %>%
      dplyr::summarise(
        RMSE = sqrt(mean((RV - RVPred)^2)),
        MAPE = mean(abs((RV - RVPred)/RV))*100
      ) %>%
      print()
  }
}


### Prediction ---------------------------------------------------------------

### Error measurements -------------------------------------------------------
extendedRF %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

baseRF %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

extendedLogRF %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

baseLogRF %>%
  dplyr::filter(Start >= "2007-10-01") %>%
  dplyr::summarise(
    RMSE = sqrt(mean((RV - RVPred)^2)),
    MAPE = mean(abs((RV - RVPred)/RV))*100
  )

### Save ---------------------------------------------------------------------
dataToSave <- c("extendedRF", "baseRF", "extendedLogRF", "baseLogRF")

# dataLong <- purrr::map_df(dataToSave, funGatherToLongFormat)

# write.csv(
#   dataLong,
#   file = "./Data/resultsOLS.csv",
#   row.names = FALSE
# )
paramTuningRF <- load("./Rdata/paramTuningRF.RData")
matrix(unlist(paramTuningRF), ncol = 4, byrow = TRUE) 
