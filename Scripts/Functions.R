### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
})

### Load data ----------------------------------------------------------------
funReadCsvFolder <- function(path, nCores) {
  fileNames <- list.files(path = path, pattern = "*.csv")

  dataReturn <- parallel::mclapply(
    fileNames,
    function(x) {
      read.csv(paste(path, x, sep = "/"), stringsAsFactors = FALSE) %>%
        dplyr::mutate(
          Start = gsub(".*_|.csv.*", "",  x),
          Start = as.POSIXct(paste0(Start, utcsec), format="%F %T")
        ) %>%
       dplyr::select(-utcsec)
    },
    mc.cores = nCores
  ) %>%
    do.call(rbind, .)

  return(dataReturn)
}

### Preprocessing ------------------------------------------------------------
funPreprocessing <- function(path, nCores) {
  dat <- funReadCsvFolder(path, nCores) %>%
    dplyr::filter(
      dplyr::between(as.numeric(format(Start, "%H%M")), 0930, 1600),
      price != 0,
      corr %in% c(NA, 0)) %>%
    dplyr::mutate(cond = gsub(" ", "", .$cond)) %>%
    dplyr::filter(cond %in% c(NA, "", "E", "F")) %>%
    dplyr::mutate(mad = zoo::rollmean(price, 51, fill = NA)) %>%
    dplyr::filter(abs(price - 10*mad) < 10*mad) %>%
    dplyr::select(Start, Price = price, Volume = volume)
  
  return(dat)
}

### Synchronizing ------------------------------------------------------------
funSync <- function(data) {
  dates <- data %>%
    dplyr::mutate(Start = as.character(format(Start, "%F"))) %>%
    dplyr::select(Start) %>%
    unique()
  
  dat <- data %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(Price = sum(Volume*Price)/sum(Volume)) %>%
    dplyr::mutate(Start = format(Start, "%F %H:%M")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(Price = dplyr::last(Price)) %>%
    dplyr::mutate(Start = as.POSIXct(Start)) %>%
    tidyr::complete(
      Start = seq(
        from = (lubridate::floor_date(min(Start), "day") +
                  lubridate::minutes(9*60+30)),
        to = max(Start),
        by = "1 min"
      )
    ) %>%
    tidyr::fill(Price, .direction = "downup") %>%
    dplyr::filter(
      dplyr::between(as.numeric(format(Start, "%H:%M")), 0930, 1600)
    ) %>%
    dplyr::mutate(Date = as.character(format(Start, "%F"))) %>%
    dplyr::filter(Date %in% dates$Start) %>%
    dplyr::select(-Date)
  
  return(dat)
}

### Synchronizing with X sec period ------------------------------------------
funXSecSync <- function(x, file) {
  dat <- read.csv(paste0("./Data/", file)) %>%
    dplyr::mutate(
      Start = gsub(".*_|.csv.*", "",  file),
      Start = as.POSIXct(paste0(Start, utcsec), format="%Y%m%d %H:%M:%S")
    ) %>%
    dplyr::select(-utcsec) %>%
    dplyr::filter(
      dplyr::between(as.numeric(format(Start, "%H%M")), 0930, 1600),
      price != 0,
      corr %in% c(NA, 0)) %>%
    dplyr::mutate(cond = gsub(" ", "", .$cond)) %>%
    dplyr::filter(cond %in% c(NA, "", "E", "F")) %>%
    dplyr::mutate(mad = zoo::rollmean(price, 51, fill = NA)) %>%
    dplyr::filter(abs(price - 10*mad) < 10*mad) %>%
    dplyr::select(Start, Price = price, Volume = volume) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(Price = sum(Volume*Price)/sum(Volume)) %>%
    dplyr::mutate(Start = format(Start, "%F %H:%M:%S")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(Price = dplyr::last(Price)) %>%
    dplyr::mutate(Start = as.POSIXct(Start)) %>%
    tidyr::complete(
      Start = seq(
        from = (lubridate::floor_date(Start, "day") +
                  lubridate::minutes(9*60+30)),
        to = max(Start),
        by = "1 sec"
      )
    ) %>%
    tidyr::fill(Price, .direction = "downup") %>%
    dplyr::filter(
      dplyr::between(as.numeric(format(Start, "%H%M")), 0930, 1600)
    ) %>%
    group_by(Start = cut(Start, breaks = paste(x, "secs"))) %>%
    summarise(Price = dplyr::last(Price))
  
  return(dat)
}

### Multiple left_join -------------------------------------------------------
left_join_multi <- function(..., by) {
  list(...) %>%
    purrr::reduce(dplyr::left_join, by = by)
}
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
    dplyr::mutate(OCTDR = (Open - Close)/(High - Low)) %>%
    dplyr::select(Start, OCTDR)
  
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

### Cross validation weight --------------------------------------------------
funWeight <- function(n, lambda) {
  exp(n*lambda)/sum(exp(n*lambda))
}

### Gather to long format ----------------------------------------------------
funGatherToLongFormat <- function(dataName) {
  data <- get(dataName)
  
  data %<>% tidyr::gather(key = "Var", Value, -Start) %>%
    dplyr::mutate(Type = dataName)
  
  return(data)
}

funGatherToLongFormat2 <- function(data, name) {
  dat <- data[[name]]
  
  dat %<>% tidyr::gather(key = "Var", Value, -Start) %>%
    dplyr::mutate(Type = name)
  
  return(dat)
}

### Get data for HAR ---------------------------------------------------------
funGetDataHAR <- function(model = "extended", test = FALSE) {
  if (!is.element(model, c("base", "baseLog", "extended", "extendedLog")) ) {
    stop("The specified model is not a function option ",
         "(base, baseLog, extended, extendedLog)")
  }
  if (test) {
    predDirectionTestXGB <- read.csv("./Data/predDirectionTestXGB.csv") %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Start = as.POSIXct(Start, format = "%F")) 
  }
  
  data <- read.csv("./Data/SpyCleaned.gz") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      Start = as.POSIXct(Start, format = "%F"),
      Log.Price = log(Price),
      Abs.Diff.Price = abs(c(diff(Log.Price), 0)),
      Abs.Diff.Price.Lag = dplyr::lag(Abs.Diff.Price)
    ) %>%
    {
      if (!test) dplyr::filter(., Start <= "2007-09-30") else .
    } %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      RV.Daily = sum(diff(Log.Price)^2),
      Return = diff(Log.Price, lag = 390),
      Pos.Return = max(c(Return, 0)),
      Neg.Return = min(c(Return, 0)),
      N = dplyr::n(),
      BV.Daily = pi*N/(2*N-2)*sum(Abs.Diff.Price*Abs.Diff.Price.Lag)
    ) %>%
    dplyr::mutate(
      RV.Weekly = zoo::rollmeanr(RV.Daily, k = 5, fill = NA),
      RV.Monthly = zoo::rollmeanr(RV.Daily, k = 22, fill = NA),
      RV.1.Ahead = dplyr::lead(RV.Daily),
      RV.Direction = as.numeric(RV.Daily/dplyr::lag(RV.Daily) > 1),
      Jump.Daily = pmax(RV.Daily - BV.Daily, 0)
    ) %>%
    {
      if (test) {
        dplyr::left_join(., predDirectionTestXGB, by = "Start") %>%
        dplyr::mutate(
          RV.Direction = ifelse(
            is.na(RVDirectionPred), RV.Direction, RVDirectionPred
          )
        )
      } else .
    } %>%
    dplyr::select(
      Start, RV.1.Ahead, RV.Daily, RV.Weekly, RV.Monthly, Jump.Daily,
      Pos.Return, Neg.Return, RV.Direction
    )
  
  if(model %in% c("baseLog", "extendedLog")) {
    data %<>%
    dplyr::mutate(
      RV.1.Ahead = log(RV.1.Ahead),
      RV.Daily = log(RV.Daily),
      RV.Monthly = log(RV.Monthly),
      Jump.Daily = log(1 + Jump.Daily),
      Pos.Return = log(1 + Pos.Return),
      Neg.Return = log(1 + abs(Neg.Return))
    )
  }
  
  if (model %in% c("base", "baseLog")) {
    data %<>%  
      dplyr::select(-c(Jump.Daily, Pos.Return, Neg.Return, RV.Direction))
  }

  data %>% 
    tidyr::drop_na() %>%
    return()
}

funGetDataHAR_v2 <- function(DAH, model = "extended", test = FALSE) {
  if (!is.element(model, c("base", "baseLog", "extended", "extendedLog")) ) {
    stop("The specified model is not a function option ",
         "(base, baseLog, extended, extendedLog)")
  }
  if (test) {
    predDirectionTest <- read.csv(
      paste0('./Data/', DAH ,'DAH/predDirectionTest.csv')
    ) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(Start = as.POSIXct(Start, format = "%F")) 
  }
  
  data <- read.csv("./Data/SpyCleaned.gz") %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      Start = as.POSIXct(Start, format = "%F"),
      Log.Price = log(Price),
      Abs.Diff.Price = abs(c(diff(Log.Price), 0)),
      Abs.Diff.Price.Lag = dplyr::lag(Abs.Diff.Price)
    ) %>%
    {
      if (!test) dplyr::filter(., Start <= "2007-09-30") else .
    } %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(
      RV.Daily = sum(diff(Log.Price)^2),
      Return = diff(Log.Price, lag = 390),
      N = dplyr::n(),
      BV.Daily = pi*N/(2*N-2)*sum(Abs.Diff.Price*Abs.Diff.Price.Lag)
    ) %>%
    dplyr::mutate(
      RV.Weekly = zoo::rollmeanr(RV.Daily, k = 5, fill = NA),
      RV.Monthly = zoo::rollmeanr(RV.Daily, k = 22, fill = NA),
      Target = dplyr::lead(RV.Daily, n = DAH),
      RV.Direction = as.numeric(Target/RV.Daily > 1),
      Pos.Return.Temp = zoo::rollmeanr(Return, k = DAH, fill = NA),
      Pos.Return = ifelse(Pos.Return.Temp > 0, Pos.Return.Temp, 0),
      Neg.Return.Temp = zoo::rollmeanr(Return, k = DAH, fill = NA),
      Neg.Return = ifelse(Neg.Return.Temp < 0, Neg.Return.Temp, 0),
      RV.Roll = zoo::rollmeanr(RV.Daily, k = DAH, fill = NA),
      BV.Roll = zoo::rollmeanr(BV.Daily, k = DAH, fill = NA),
      Jump = pmax(RV.Roll - BV.Roll, 0)
    ) %>%
    {
      if (test) {
        dplyr::left_join(., predDirectionTest, by = "Start") %>%
          dplyr::mutate(
            RV.Direction = ifelse(
              is.na(RVDirectionPred), RV.Direction, RVDirectionPred
            )
          )
      } else .
    } %>%
    dplyr::select(
      Start, Target, RV.Daily, RV.Weekly, RV.Monthly, Jump,
      Pos.Return, Neg.Return, RV.Direction
    )
  
  if(model %in% c("baseLog", "extendedLog")) {
    data %<>%
      dplyr::mutate(
        Target = log(Target),
        RV.Daily = log(RV.Daily),
        RV.Monthly = log(RV.Monthly),
        Jump = log(1 + Jump),
        Pos.Return = log(1 + Pos.Return),
        Neg.Return = log(1 + abs(Neg.Return))
      ) %>%
      tidyr::drop_na()
    
    return(data)
  }
  
  if (model %in% c("base", "baseLog")) {
    data %<>%  
      dplyr::select(-c(Jump, Pos.Return, Neg.Return, RV.Direction)) %>%
      tidyr::drop_na()
    
    return(data)
  }
  
  data %>% 
    tidyr::drop_na() %>%
    return()
}
