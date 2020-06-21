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
    dplyr::filter(Start <  date) %>%
    dplyr::select(Target) %>%
    as.matrix()
  
  test <- dat %>%
    dplyr::filter(Start >= date, Start <= date + lubridate::weeks(1))
  
  theseDates <- test %>%
    dplyr::select(Start)

  mod <- forecast::arfima(y = train)
  
  pred <- forecast(object = mod, h = nrow(test))[["mean"]][1:nrow(test)]
  
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
# browser()
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

### Prediction ---------------------------------------------------------------

models <- c("extended", "base", "extendedLog", "baseLog")
errorTable <- data.frame()
errors <- list()

rollPred <- funRollingPred(method = 'ARFIMA')

dataLong <- purrr::map_df(
  models,
  funGatherToLongFormat2,
  data = rollPred$data
)

write.csv(
  dataLong,
  file = './Data/5DAH/resultsARFIMA.csv',
  row.names = FALSE
)

errorTable %<>%
  dplyr::bind_rows(., rollPred$errors)

write.table(
  errorTable,
  file = "./Data/5DAH/modelErrorsARFIMA.txt"
)
