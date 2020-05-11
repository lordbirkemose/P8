### Packages -----------------------------------------------------------------
suppressPackageStartupMessages({
  library(magrittr)
  library(lubridate)
})

source("./Scripts/Functions.R")

### Data ---------------------------------------------------------------------
data <- funGetDataHAR(); data
dataTest <- funGetDataHAR(test = TRUE); dataTest 

