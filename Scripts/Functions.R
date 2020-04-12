### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
})

### Plots --------------------------------------------------------------------
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