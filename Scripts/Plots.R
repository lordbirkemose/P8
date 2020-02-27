### Packages -----------------------------------------------------------------
library(tidyverse)
library(Cairo)

source("./Scripts/Functions.R", echo = FALSE)

### Data comparison ----------------------------------------------------------
path <- "~/Desktop/P8/SPY2000-2001"
# path2 <- "/Volumes/Samsung_T5/highFrequencyData/Cleaned/"

from <- as.POSIXct("2001-12-10 09:30:00")
to <- as.POSIXct("2001-12-10 16:00:00")
  
dataRaw <- funReadCsvFolder(path) %>%
  dplyr::select(Start, price) %>%
  dplyr::filter(Start >= from, Start <= to)

dataPreprocessed <- read.csv("~/Desktop/P8/SpyPreprocessed.csv") %>%
  dplyr::mutate(Start = as.POSIXct(Start, format = "%F%T")) %>%
  dplyr::filter(Start >= from, Start <= to)

dataCleaned <- read.csv("~/Desktop/P8/SpyCleaned.csv") %>%
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
    data = dataCleaned, 
    aes(x = Start, y = Price, colour = "Cleaned")
    ) +
  scale_colour_manual(
    "", 
    values = alpha(
      c("Raw" = colors[1], "Preprocessed" = colors[2], "Cleaned" = colors[3]), 
      1
    )
  ) +
  scale_x_datetime(breaks = date_breaks("1 hour"),date_labels = "%H:%M") +
  xlab("Time") +
  ylab("Price") +
  themeLegend

ggsave(
  file = paste0("./Plots/","dataComparison",".eps"),
  width =  9, height = 3 , device = cairo_ps , dpi = 600
)
