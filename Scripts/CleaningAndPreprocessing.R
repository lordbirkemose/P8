### Packages -----------------------------------------------------------------
library(tidyverse)
source("./Scripts/Functions.R", echo = FALSE)

### Preprocessing ------------------------------------------------------------
path <- "/Volumes/Samsung_T5/highFrequencyData/SPY"

dataPreprocessed <- funPreprocessing(path)

write.csv(
  dataPreprocessed,
  "/Volumes/Samsung_T5/highFrequencyData/Cleaned/SpyPreprocessed.csv",
  row.names = FALSE
)

### Cleaning -----------------------------------------------------------------
dataCleaned <- funCleaning(dataPreprocessed)

write.csv(
  dataCleaned,
  "/Volumes/Samsung_T5/highFrequencyData/Cleaned/SpyCleaned.csv",
  row.names = FALSE
)
