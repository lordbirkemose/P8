### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(Cairo)
})

source("./Scripts/Functions.R", echo = FALSE)

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
  scale_x_datetime(breaks = date_breaks("1 hour"), date_labels = "%H:%M") +
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
load("./Rdata/importanceFeatureSelection.Rdata")

ggplot(data = importanceFeatureSelection) +
  geom_point(
    aes(
      x = MDA,
      y = Indicator, 
      color = "Mean Decrease Acuracy"
    )
  ) +
  geom_point(
    aes(
      x = MDG,
      y = Indicator,
      color = "Mean Decrease Impurity"
    )
  ) +
  scale_colour_manual(
    "",
    values = c(
      colors[2],
      colors[3]
    )
  ) +
  xlab("Value") +
  themeLegend

ggsave(
  file = paste0("./Plots/","featureSelection",".eps"),
  width =  9, height = 3.5 , device = cairo_ps , dpi = 600
)
