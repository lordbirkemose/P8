### Packages -----------------------------------------------------------------
library(tidyverse)
source("./Scripts/Functions.R")

### Preprocessing ------------------------------------------------------------
# path <- "/Volumes/Samsung_T5/highFrequencyData/SPY"
path <- "~/Desktop/P8/SPY2000-2001"

dataPreprocessed <- funPreprocessing(path)

### Write csv ----------------------------------------------------------------
write.csv(
  dataPreprocessed, 
  paste(path, "SpyPreprocessed.csv", "/"), 
  row.names = FALSE
)