### Packages -----------------------------------------------------------------
library(tidyverse)

### Load data ----------------------------------------------------------------
funReadCsvFolder <- function(path) {
  fileNames <- list.files(path = path, pattern = "*.csv")

  dataReturn <- lapply(
    fileNames,
    function(x) {
      read.csv(paste(path, x, sep = "/"), stringsAsFactors = FALSE) %>%
        dplyr::mutate(
          Start = gsub(".*_|.csv.*", "",  x),
          Start = as.POSIXct(
            paste0(Start, utcsec), 
            format = "%Y%m%d %H:%M:%S",
            tz = "EST"
          )
        ) %>%
       dplyr:: select(-utcsec)
    }
  )

  dataReturn <- do.call(rbind, dataReturn)

  return(dataReturn)
}

### Preprocessing ------------------------------------------------------------

funPreprocessing <- function(path) {
  dat <- funReadCsvFolder(path) %>%
    dplyr::filter(
      dplyr::between(as.numeric(format(Start, "%H%M")), 0930, 1600),
      price != 0,
      corr %in% c(NA, 0)) %>%
    dplyr::mutate(cond = gsub(" ", "", .$cond)) %>%
    dplyr::filter(cond %in% c(NA, "", "E", "F")) %>%
    dplyr::group_by(Start) %>%
    dplyr::summarise(price = sum(volume*price)/sum(volume)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mad = zoo::rollmean(price, 51, fill = NA)) %>%
    dplyr::filter(abs(price - 10*mad) < 10*mad) %>%
    dplyr::select(Start, Price = price)
  
  return(dat)
}

### Cleaning -----------------------------------------------------------------
