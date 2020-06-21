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

### Importance Feature Selection ---------------------------------------------
load("./Rdata/5DAH/importanceFeatureSelection.RData")

ggplot(data = importanceFeatureSelection) +
  geom_bar(
    aes(x = Indicator, y = Score, fill = 'Score'),
    stat = 'identity'
  ) +
  geom_hline(
    aes(
      yintercept = importanceFeatureSelection$AverageScore[1],
      linetype = 'Average Score'
    ),
    color = 'black'
  ) +
  coord_flip() +
  scale_fill_manual(
    name = "",
    values = colors[2]
  ) +
  scale_linetype_manual(
    name = "",
    values = 2,
  ) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(order = 2)
  ) +
  themeLegend

ggsave(
  file = './Plots/5DAH/featureSelection.eps',
  width =  9, height = 3.5 , device = cairo_ps , dpi = 600
)

### MAPE plot ----------------------------------------------------------------
dataOLS <- read.csv("./Data/5DAH/resultsOLS.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  mutate(
    Type = factor(
      Type,
      levels = c('base', 'baseLog', 'extended', 'extendedLog')
    )
  )

dataWLS <- read.csv("./Data/5DAH/resultsWLS.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  mutate(
    Type = factor(
      Type,
      levels = c('base', 'baseLog', 'extended', 'extendedLog')
    )
  )

dataRF <- read.csv("./Data/5DAH/resultsRF.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  mutate(
    Type = factor(
      Type,
      levels = c('base', 'baseLog', 'extended', 'extendedLog')
    )
  )

dataXGB <- read.csv("./Data/5DAH/resultsXGB.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  mutate(
    Type = factor(
      Type,
      levels = c('base', 'baseLog', 'extended', 'extendedLog')
    )
  )

dataARFIMA <- read.csv("./Data/5DAH/resultsARFIMA.csv") %>%
  as_tibble() %>%
  mutate(Start = as.Date(Start)) %>%
  mutate(
    Type = factor(
      Type,
      levels = c('base', 'baseLog', 'extended', 'extendedLog')
    )
  )

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
  dataARFIMA %>% spread(key = Var, value = Value) %>%
    mutate(ARFIMA = abs(RV - RVPred)) %>%
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
  file = './Plots/5DAH/RMSEBenchmark.eps',
  device = cairo_ps , dpi = 600,
  height = 230, width = 150, units = "mm"
)
