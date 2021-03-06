### Preliminary --------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(Cairo)
  library(forecast)
})

source("./Scripts/Functions.R", echo = FALSE)

# Colors
# colors <- scales::hue_pal()(4)
colors <- c("#fc8d62", "#779ecc", "#66c2a5", "#007e89", "#aec6cf")
# scales::show_col(colors) # Run to see colors

# Themes
theme <- list(
  ggplot2::theme_minimal(),
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(), 
    panel.grid.minor = ggplot2::element_line(),
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black")
  )
)
themeLegend <- list(
  ggplot2::theme_minimal(),
  ggplot2::theme(
    panel.grid.major = ggplot2::element_line(), 
    panel.grid.minor = ggplot2::element_line(),
    panel.background = ggplot2::element_blank(), 
    axis.line = ggplot2::element_line(colour = "black"),
    legend.position = "top" , 
    legend.justification = "left" , 
    legend.direction = "horizontal", 
    legend.background = ggplot2::element_blank()
  )
)

# ACF plot
funAcfPlot <- function(data, lag.max = 50, include.lag.zero = FALSE) {
  acfData <- stats::acf(data, lag.max = lag.max, plot = FALSE)
  
  acfData <- tibble::tibble(
    lag = acfData$lag,
    acf = acfData$acf
  )
  
  if(!include.lag.zero) acfData %<>% dplyr::slice(-1)
  
  confInt <- qnorm((1 + 0.95)/2)/sqrt(length(data))
  
  plot <- ggplot(data = acfData, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0) +
    geom_segment(ggplot2::aes(xend = lag, yend = 0)) +
    geom_hline(yintercept = confInt, linetype = 2, color = colors[2]) +
    geom_hline(yintercept = -confInt, linetype = 2, color = colors[2]) +
    labs(x = "Lag", y = "ACF") +
    list(
      theme_minimal(),
      theme(
        panel.grid.major = element_line(), 
        panel.grid.minor = element_line(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")
      )
    )
  
  return(plot)
}

# Results plot
funResultsPlots <- function(data) {
  
  data %<>% dplyr::filter(Start >= as.Date("2006-01-01"))
  
  textData <- tibble::tibble(
    label = c(rep("In-sample", 4), rep("Out-of-sample", 4)),
    x = c(rep(as.Date("2006-10-01"), 4), rep(as.Date("2008-11-01"), 4))
  )
  
  labelsData <- c(
    base = "Base HAR", baseLog = "Base HAR Log",
    extended = "Extended HAR", extendedLog = "Extended HAR Log"
  )

  p <- ggplot(data = data) +
    geom_line(aes(x = Start, y = Value, color = Var), size = 0.3) +
    labs(
      # title = "OLS",
      x = "Time",
      y = "Value"
    ) +
    scale_colour_manual(
      name = "",
      labels = c(
        "Actual Realized Volatility", "Predicted Realized Volatility"
      ),
      values = colors[2:3]
    ) +
    scale_x_date(
      date_labels = "%Y",
      breaks = pretty(data$Start, n = 5)
    ) +
    scale_y_continuous(
      expand = expand_scale(mult = c(.15, .1))
    ) +
    themeLegend +
    theme(strip.text.x = element_text(size = 10)) +
    facet_wrap(
      ~Type,
      scales = "free",
      labeller = labeller(Type = labelsData),
      ncol = 1
    ) +
    annotate(
      geom = "segment",
      y = -Inf, yend = -Inf,
      x = as.Date("2006-01-01"), xend = as.Date("2009-12-31"),
      color = "grey",
      size = 11
    ) +
    geom_text(
      data = textData,
      mapping = aes(x = x, y = -Inf, label = label),
      vjust = -.5,
      size = 3
    ) +
    geom_vline(
      xintercept = as.Date("2007-10-01"),
      linetype = "longdash",
      color = "black"
    )
  
  return(p)
}

### Data comparison (WARNING: Needs raw data) --------------------------------
path <- "~/Desktop/P8/SPY2000-2001" # Path to raw data

from <- as.POSIXct("2001-12-10 00:00:00")
to <- as.POSIXct("2001-12-10 23:59:00")
  
dataRaw <- funReadCsvFolder(path, 2) %>%
  dplyr::select(Start, price) %>%
  dplyr::filter(Start >= from, Start <= to)

dataPreprocessed <- read.csv("./Data/SpyPreprocessed2000-2001.csv") %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F%T")) %>%
  dplyr::filter(Start >= from, Start <= to)

dataSync <- read.csv("./Data/SpyCleaned.gz") %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F %H:%M")) %>%
  dplyr::filter(Start >= from, Start <= to)

ggplot(data = dataRaw) +
  geom_line(
    aes(x = Start, y = price, colour = "Raw")
  ) +
  geom_line(
    data = dataPreprocessed, 
    aes(x = Start, y = Price, colour = "Preprocessed")
  ) +
  geom_line(
    data = dataSync, 
    aes(x = Start, y = Price, colour = "Synchronized")
    ) +
  scale_colour_manual(
    "",
    breaks = c("Raw", "Preprocessed", "Synchronized"),
    values = c(
      "Raw" = colors[3], 
      "Preprocessed" = colors[2], 
      "Synchronized" = colors[1]
    )
  ) +
  scale_x_datetime(breaks = "1 hour", date_labels = "%H:%M") +
  xlab("Time") +
  ylab("Price") +
  themeLegend

ggsave(
  file = paste0("./Plots/","dataComparison",".eps"),
  width =  9, height = 3.5 , device = cairo_ps , dpi = 600
)
### Volatility signature plot ------------------------------------------------
file <- "SPY_20081120.csv"
# period <- c(1, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300)
period <- c(1:10, seq(30, 600, by = 30))

VolSigPlotData <- parallel::mclapply(
  period,
  function(x) {
    funXSecSync(x, file) %>%
      dplyr::mutate(logPrice = log(Price)) %>%
      dplyr::group_by() %>%
      dplyr::summarise(
        RV = sum(diff(logPrice)^2),
        period = x
      )
  },
  mc.cores = 2
) %>%
  do.call(rbind, .)

ggplot(data = VolSigPlotData) +
  geom_point(aes(x = period/60, y = RV), color = colors[2]) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  xlab("Sampling Period (min)") +
  ylab("Realized Volatility") +
  theme

ggsave(
  file = paste0("./Plots/","volSigPlot",".eps"),
  width =  9, height = 3 , device = cairo_ps , dpi = 600
)

### Bid-Ask plot -------------------------------------------------------------
from <- as.POSIXct("2020-03-02 13:00:00")
to <- as.POSIXct("2020-03-02 13:30:00")
SPY <- as_tibble(read.csv("./Data/IVEtickbidask.txt")) %>%
  mutate(Time = as.POSIXct(paste(Date,Time), format = "%d/%m/%Y %H:%M:%S")) %>%
  filter(between(Time,from,to)) %>%
  select(-c(Date, Size))

ggplot(data = SPY) +
  geom_step(
    aes(x = Time, y = Bid, colour = "Bid")
  ) +
  geom_step(
    data = SPY,
    aes(x = Time, y = Ask, colour = "Ask")
  ) +
  geom_point(
    data = SPY,
    aes(x = Time, y = Price, colour = "Price"),
    size = 0.5
  ) +
  scale_colour_manual(
    "",
    breaks = c("Bid", "Ask", "Price"),
    values = c(
      "Bid" = colors[3], 
      "Ask" = colors[2], 
      "Price" = colors[1]
    ),
    guide = guide_legend(
      override.aes = list(
        linetype = c(1, 1, NA),
        shape = c(NA, NA, 20),
        size = c(.5, .5, 1.5)
      )
    )
  ) +
  ylab("Price") +
  themeLegend

ggsave(
  file = paste0("./Plots/","bidAskBounce",".eps"),
  width =  9, height = 3.5 , device = cairo_ps , dpi = 600
)

### Feature selection --------------------------------------------------------
load("./Rdata/DirectionalVolatility/importanceFeatureSelection.Rdata")

importanceFeatureSelectionTidy <- importanceFeatureSelection %>%
  dplyr::select(
    Indicator,
    `Mean Decrease Accuracy` = MDA,
    `Mean Decrease Impurity` = MDG
  ) %>%
  tidyr::gather(key = "var", value = "Value", -Indicator)
  
ggplot(data = importanceFeatureSelectionTidy) +
  geom_bar(
    aes(x = Indicator, y = Value, fill = var),
    stat = "identity", position = position_dodge()
  ) +
  geom_hline(
    aes(
    yintercept = importanceFeatureSelection$AverageMDA[1],
    linetype = "Average Mean Decrease Accuracy"
    ),
    color = colors[2]
  ) +
  geom_hline(
    aes(
      yintercept = importanceFeatureSelection$AverageMDG[1],
      linetype = "Average Mean Decrease Impurity"
    ),
    color = colors[3]
  ) +
  coord_flip() +
  scale_fill_manual(
    name = "",
    values = c(
      colors[2],
      colors[3]
    )
  ) +
  scale_linetype_manual(
    name = "",
    values = c(2, 2),
    guide = guide_legend(override.aes = list(color = c(colors[2], colors[3])))
  ) +
  themeLegend

# ggplot(data = importanceFeatureSelection) +
#   geom_point(
#     aes(
#       x = MDA,
#       y = Indicator, 
#       color = "Mean Decrease Acuracy"
#     )
#   ) +
#   geom_point(
#     aes(
#       x = MDG,
#       y = Indicator,
#       color = "Mean Decrease Impurity"
#     )
#   ) +
#   scale_colour_manual(
#     "",
#     values = c(
#       colors[2],
#       colors[3]
#     )
#   ) +
#   xlab("Value") +
#   themeLegend

ggsave(
  file = paste0("./Plots/","featureSelection",".eps"),
  width =  9, height = 3.5 , device = cairo_ps , dpi = 600
)

### Tuning importance criteria -----------------------------------------------
load("./Rdata/TuningImportanceCriteria.Rdata")

ggplot() +
  geom_line(
    data = tuningMaxnodesMDA,
    aes(
      x = maxnodes,
      y = oosError,
      color = "Mean Decrease Accuracy (max nodes)"
    )
  ) +
  geom_line(
    data = tuningMaxnodesMDG,
    aes(
      x = maxnodes, 
      y = oosError,
      color = "Mean Decrease Impurity (max nodes)"
    )
  ) +
  geom_line(
    data = tuningNodesizeMDA,
    aes(
      x = nodesize, 
      y = oosError,
      color = "Mean Decrease Accuracy (node size)"
    )
  ) +
  geom_line(
    data = tuningNodesizeMDG,
    aes(
      x = nodesize, 
      y = oosError,
      color = "Mean Decrease Impurity (node size)"
    )
  ) +
  ylab("Validation Error") +
  xlab("Value") +
  scale_colour_manual(
    "",
    values = c(
      colors[1],
      colors[2],
      colors[3],
      colors[4]
    )
  ) +
  guides(colour = guide_legend(nrow = 2)) +
  themeLegend

ggsave(
  file = paste0("./Plots/","tuningImportanceCriteria",".eps"),
  width =  9, height = 3.8 , device = cairo_ps , dpi = 600
)


### RV ACF ------------------------------------------------------------------
data <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    Start = as.POSIXct(Start, format = "%F"),
    Log.Price = log(Price)
  ) %>%
  dplyr::group_by(Start) %>%
  dplyr::summarise( RV.Daily = sum(diff(Log.Price)^2) ) %>% 
  dplyr::filter(., Start <= "2007-09-30")

ggAcf(
  data$RV.Daily,
  lag.max = 100,
  type = c("correlation", "covariance", "partial"),
  plot = TRUE,
  main = NULL,
) + 
  theme

# ggsave(
#   file = paste0("./Plots/","RV_ACF",".eps"),
#   width =  9, height = 3 , device = cairo_ps , dpi = 600
# )

### Results ------------------------------------------------------------------
dataOLS <- read.csv("./Data/resultsOLSdaily.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  mutate(
    Type = factor(
      Type,
      levels = c('base', 'baseLog', 'extended', 'extendedLog')
    )
  )

funResultsPlots(dataOLS)

# ggsave(
#   file = paste0("./Plots/","OLS",".eps"),
#   device = cairo_ps , dpi = 600,
#   height = 230, width = 150, units = "mm"
# )

## WLS
dataWLSdaily <- read.csv("./Data/resultsWLSdaily.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  filter(Type %in% c("base", "extended"))
dataWLSweekly <- read.csv("./Data/resultsWLSweekly.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  filter(Type %in% c("baseLog", "extendedLog"))
dataWLS <- rbind(dataWLSdaily, dataWLSweekly) %>%
  mutate(
    Type = factor(
      Type,
      levels = c('base', 'baseLog', 'extended', 'extendedLog')
    )
  )

funResultsPlots(dataWLS)

ggsave(
  file = paste0("./Plots/","WLS",".eps"),
  device = cairo_ps , dpi = 600,
  height = 230, width = 150, units = "mm"
)

## RF
dataRF <- read.csv("./Data/resultsRFdaily.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start))

funResultsPlots(dataRF)

# ggsave(
#   file = paste0("./Plots/","RF",".eps"),
#   device = cairo_ps , dpi = 600,
#   height = 230, width = 150, units = "mm"
# )

## XGB
dataXGBdaily <- read.csv("./Data/resultsXGBdaily.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  filter(Type %in% c('extended'))
dataXGBweekly <- read.csv("./Data/resultsXGBweekly.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  filter(Type %in% c("base", "baseLog", "extendedLog"))
dataXGB <- rbind(dataXGBdaily, dataXGBweekly) %>%
  mutate(
    Type = factor(
      Type,
      levels = c('base', 'baseLog', 'extended', 'extendedLog')
    )
  )

funResultsPlots(dataXGB)

# ggsave(
#   file = paste0("./Plots/","XGB",".eps"),
#   device = cairo_ps , dpi = 600,
#   height = 230, width = 150, units = "mm"
# )

## ARFIMA
dataARFIMA <- read.csv("./Data/resultsARFIMAdaily.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  filter(Type %in% c("base", "baseLog")) %>%
  mutate(
    Type = factor(
      Type,
      levels = c('base', 'baseLog')
    )
  ) %>%
  dplyr::filter(Start >= as.Date("2006-01-01"))

textData <- tibble::tibble(
  label = c(rep("In-sample", 4), rep("Out-of-sample", 4)),
  x = c(rep(as.Date("2006-10-01"), 4), rep(as.Date("2008-11-01"), 4))
)

labelsData <- c(
  base = "RV", baseLog = "Log RV"
)

ggplot(data = dataARFIMA) +
  geom_line(aes(x = Start, y = Value, color = Var), size = 0.3) +
  labs(
    # title = "OLS",
    x = "Time",
    y = "Value"
  ) +
  scale_colour_manual(
    name = "",
    labels = c(
      "Actual Realized Volatility", "Predicted Realized Volatility"
    ),
    values = colors[2:3]
  ) +
  scale_x_date(
    date_labels = "%Y",
    breaks = pretty(dataARFIMA$Start, n = 5)
  ) +
  scale_y_continuous(
    expand = expand_scale(mult = c(.15, .1))
  ) +
  themeLegend +
  theme(strip.text.x = element_text(size = 10)) +
  facet_wrap(
    ~Type,
    scales = "free",
    labeller = labeller(Type = labelsData),
    ncol = 1
  ) +
  annotate(
    geom = "segment",
    y = -Inf, yend = -Inf,
    x = as.Date("2006-01-01"), xend = as.Date("2009-12-31"),
    color = "grey",
    size = 11
  ) +
  geom_text(
    data = textData,
    mapping = aes(x = x, y = -Inf, label = label),
    vjust = -.5,
    size = 3
  ) +
  geom_vline(
    xintercept = as.Date("2007-10-01"),
    linetype = "longdash",
    color = "black"
  )

# ggsave(
#   file = paste0("./Plots/","ARFIMA",".eps"),
#   device = cairo_ps , dpi = 600,
#   height = 120, width = 150, units = "mm"
# )

## MAPE plot
dataMAPE <- left_join_multi(
  dataOLS %>% spread(key = Var, value = Value) %>%
    mutate(OLS = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  dataWLS %>% spread(key = Var, value = Value) %>%
    mutate(WLS = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  dataRF %>% spread(key = Var, value = Value) %>%
    mutate(`Random Forest` = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  dataXGB %>% spread(key = Var, value = Value) %>%
    mutate(`Extreme Gradient Boosting` = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  read.csv("./Data/resultsARFIMAdaily.csv") %>%
    as_tibble() %>%
    mutate(Start = as.Date(Start)) %>%
    spread(key = Var, value = Value) %>%
    mutate(ARFIMA = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  by = c("Start", "Type")
) %>%
  gather(key = "Var", value = "Value", -c(Start, Type)) %>%
  filter(Start >= "2006-01-01") %>%
  mutate(
    Var = factor(
      Var,
      levels = c(
        'ARFIMA', 'OLS', 'WLS', 'Random Forest', 'Extreme Gradient Boosting'
      )
    )
  )

textData <- tibble::tibble(
  label = c(rep("In-sample", 4), rep("Out-of-sample", 4)),
  x = c(rep(as.Date("2006-10-01"), 4), rep(as.Date("2008-11-01"), 4))
)

labelsData <- c(
  base = "Base HAR", baseLog = "Base HAR Log",
  extended = "Extended HAR", extendedLog = "Extended HAR Log"
)

ggplot(data = dataMAPE) +
  geom_line(aes(x = Start, y = Value, color = Var), size = 0.3) +
  labs(
    # title = "OLS",
    x = "Time",
    y = "Absolute Percentage Error"
  ) +
  scale_colour_manual(
    name = "",
    values = colors[c(5, 1, 4, 2, 3)]
  ) +
  scale_x_date(
    date_labels = "%Y",
    breaks = pretty(dataMAPE$Start, n = 5)
  ) +
  scale_y_continuous(
    expand = expand_scale(mult = c(.15, .1))
  ) +
  themeLegend +
  theme(strip.text.x = element_text(size = 10)) +
  facet_wrap(
    ~Type,
    # scales = "free",
    scales = "free_x",
    labeller = labeller(Type = labelsData),
    ncol = 1
  ) +
  annotate(
    geom = "segment",
    y = -Inf, yend = -Inf,
    x = as.Date("2006-01-01"), xend = as.Date("2009-12-31"),
    color = "grey",
    size = 11
  ) +
  geom_text(
    data = textData,
    mapping = aes(x = x, y = -Inf, label = label),
    vjust = -.5,
    size = 3
  ) +
  geom_vline(
    xintercept = as.Date("2007-10-01"),
    linetype = "longdash",
    color = "black"
  )
  # guides(colour = guide_legend(override.aes = list(alpha = 1)))

# ggsave(
#   file = paste0("./Plots/","MAPE",".eps"),
#   device = cairo_ps , dpi = 600,
#   height = 230, width = 150, units = "mm"
# )

### Compare to Benchmark -----------------------------------------------------
dataRMSEBenchmark <- left_join_multi(
  dataOLS %>% spread(key = Var, value = Value) %>%
    mutate(OLS = abs(RV - RVPred)) %>%
    select(-RV, -RVPred),
  dataWLS %>% spread(key = Var, value = Value) %>%
    mutate(WLS = abs(RV - RVPred)) %>%
    select(-RV, -RVPred),
  dataRF %>% spread(key = Var, value = Value) %>%
    mutate(`Random Forest` = abs(RV - RVPred)) %>%
    select(-RV, -RVPred),
  dataXGB %>% spread(key = Var, value = Value) %>%
    mutate(`Extreme Gradient Boosting` = abs(RV - RVPred)) %>%
    select(-RV, -RVPred),
  read.csv("./Data/resultsARFIMAdaily.csv") %>%
    as_tibble() %>%
    mutate(Start = as.Date(Start)) %>%
    spread(key = Var, value = Value) %>%
    mutate(ARFIMA = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  by = c("Start", "Type")
) %>%
  na.omit() %>%
  mutate(
    OLS = cumsum(ARFIMA - OLS),
    WLS = cumsum(ARFIMA - WLS),
    `Random Forest` = cumsum(ARFIMA - `Random Forest`),
    `Extreme Gradient Boosting` = cumsum(`Extreme Gradient Boosting`)
  ) %>%
  select(-ARFIMA) %>%
  gather(key = "Var", value = "Value", -c(Start, Type)) %>%
  filter(Start >= "2006-01-01") %>%
  mutate(
    Var = factor(
      Var,
      levels = c(
        'OLS', 'WLS', 'Random Forest', 'Extreme Gradient Boosting'
      )
    )
  )

textData <- tibble::tibble(
  label = c(rep("In-sample", 4), rep("Out-of-sample", 4)),
  x = c(rep(as.Date("2006-10-01"), 4), rep(as.Date("2008-11-01"), 4))
)

labelsData <- c(
  base = "Base HAR", baseLog = "Base HAR Log",
  extended = "Extended HAR", extendedLog = "Extended HAR Log"
)

ggplot(data = dataRMSEBenchmark) +
  geom_line(aes(x = Start, y = Value, color = Var), size = 0.3) +
  labs(
    # title = "OLS",
    x = "Time",
    y = "Improvement"
  ) +
  scale_colour_manual(
    name = "",
    values = scales::hue_pal()(4)
  ) +
  scale_x_date(
    date_labels = "%Y",
    breaks = pretty(dataRMSEBenchmark$Start, n = 5)
  ) +
  scale_y_continuous(
    expand = expand_scale(mult = c(.15, .1))
  ) +
  themeLegend +
  theme(strip.text.x = element_text(size = 10)) +
  facet_wrap(
    ~Type,
    # scales = "free",
    scales = "free",
    labeller = labeller(Type = labelsData),
    ncol = 1
  ) +
  annotate(
    geom = "segment",
    y = -Inf, yend = -Inf,
    x = as.Date("2006-01-01"), xend = as.Date("2009-12-31"),
    color = "grey",
    size = 11
  ) +
  geom_text(
    data = textData,
    mapping = aes(x = x, y = -Inf, label = label),
    vjust = -.5,
    size = 3
  ) +
  geom_vline(
    xintercept = as.Date("2007-10-01"),
    linetype = "longdash",
    color = "black"
  )
# guides(colour = guide_legend(override.aes = list(alpha = 1)))

ggsave(
  file = './Plots/RMSEBenchmark.eps',
  device = cairo_ps , dpi = 600,
  height = 230, width = 150, units = "mm"
)


### Front page ---------------------------------------------------------------
dataFrontPage <- left_join_multi(
  dataOLS %>% spread(key = Var, value = Value) %>%
    mutate(OLS = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  dataWLS %>% spread(key = Var, value = Value) %>%
    mutate(WLS = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  dataRF %>% spread(key = Var, value = Value) %>%
    mutate(`Random Forest` = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  dataXGB %>% spread(key = Var, value = Value) %>%
    mutate(`Extreme Gradient Boosting` = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  read.csv("./Data/resultsARFIMAdaily.csv") %>%
    as_tibble() %>%
    mutate(Start = as.Date(Start)) %>%
    spread(key = Var, value = Value) %>%
    mutate(ARFIMA = abs((RV - RVPred)/RV)*100) %>%
    select(-RV, -RVPred),
  by = c("Start", "Type")
) %>%
  gather(key = "Var", value = "Value", -c(Start, Type)) %>%
  filter(Start >= "2009-01-01", Type == "extendedLog")

ggplot(data = dataFrontPage) +
  geom_line(aes(x = Start, y = Value, color = Var)) +
  scale_colour_manual(
    name = "",
    values = colors[c(5, 1, 4, 2, 3)]
  ) +
  labs(x = "", y = "") +
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = ggplot2::element_blank(), 
      legend.position = "none" ,
      legend.background = ggplot2::element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1)
    )
  )

# ggsave(
#   file = paste0("./Plots/","frontPage",".eps"),
#   width =  9, height = 5 , device = cairo_ps , dpi = 600
# )


dataFrontPage2 <- read.csv("./Data/SpyCleaned.gz") %>%
  tibble::as_tibble() %>%
  dplyr::mutate(
    Start = as.POSIXct(Start, format = "%F %T"),
    Start = format(Start, "%F")
  ) %>%
  dplyr::group_by(Start) %>%
  dplyr::summarise(
    High = max(Price),
    Low = min(Price),
    Close = dplyr::last(Price),
    Open = dplyr::first(Price)
  ) %>%
  mutate(
    fill = ifelse(Open > Close, "green", "red"),
    candleUpper = ifelse(Open > Close, Open, Close),
    candleLower = ifelse(Open < Close, Open, Close),
    candleMiddle = candleLower
  ) %>%
  filter(Start < "1999-06-01")

ggplot(
  data = dataFrontPage2,
) +  
  geom_boxplot(
    stat = 'identity',
    aes(
      x = 1:nrow(dataFrontPage2),
      lower = candleLower,
      middle = candleMiddle,
      upper = candleUpper,
      ymin = Low,
      ymax = High,
      group = Start,
      fill = fill
    ),
    fatten = NULL
  ) +
  # geom_line(data = dataFrontPage, aes(x = Start, y = Value, color = Var)) +
  labs(x = "", y = "") +
  list(
    ggplot2::theme_minimal(),
    ggplot2::theme(
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.background = ggplot2::element_blank(), 
      legend.position = "none" ,
      legend.background = ggplot2::element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, size=1)
    )
  )
