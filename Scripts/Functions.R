### Packages -----------------------------------------------------------------
library(tidyverse)
library(scales)

### Load data ----------------------------------------------------------------
funReadCsvFolder <- function(path, nCores) {
  fileNames <- list.files(path = path, pattern = "*.csv")

  dataReturn <- parallel::mclapply(
    fileNames,
    function(x) {
      read.csv(paste(path, x, sep = "/"), stringsAsFactors = FALSE) %>%
        dplyr::mutate(
          Start = gsub(".*_|.csv.*", "",  x),
          Start = as.POSIXct(paste0(Start, utcsec), format="%Y%m%d %H:%M:%S")
        ) %>%
       dplyr::select(-utcsec)
    },
    mc.cores = nCores
  )
  dataReturn <- do.call(rbind, dataReturn)

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

### Cleaning -----------------------------------------------------------------
funCleaning <- function(data) {
  dat <- data %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(Price = sum(Volume*Price)/sum(Volume)) %>%
    dplyr::mutate(Start = format(Start, "%F %H:%M")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(Price = dplyr::last(Price)) %>%
    dplyr::mutate(Start = as.POSIXct(Start)) %>%
    tidyr::complete(Start = seq(min(Start), max(Start), by = "1 min")) %>%
    tidyr::fill(Price, .direction = "down")
  
  return(dat)
}

### Plots --------------------------------------------------------------------
# Colors
# colors <- hue_pal()(4)
colors <- c("#fc8d62", "#779ecc", "#66c2a5", "#007e89", "#aec6cf")
# show_col(colors) # Run to see colors
DEr
# Themes
theme <- list(
  theme_minimal(),
  theme(
    panel.grid.major = element_line(), 
    panel.grid.minor = element_line(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black")
  )
)
themeLegend <- list(
  theme_minimal(),
  theme(
    panel.grid.major = element_line(), 
    panel.grid.minor = element_line(),
    panel.background = element_blank(), 
    axis.line = element_line(colour = "black"),
    legend.position = "top" , 
    legend.justification = "left" , 
    legend.direction = "horizontal", 
    legend.background = element_blank()
  )
)

