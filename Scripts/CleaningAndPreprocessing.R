### Packages -----------------------------------------------------------------
library(tidyverse)
source("./Scripts/Functions.R", echo = FALSE)

### Preprocessing ------------------------------------------------------------
# path <- "/Volumes/Samsung_T5/highFrequencyData/SPY"
path <- "~/Desktop/P8/SPY2000-2001"

dataPreprocessed <- funPreprocessing(path)

write.csv(
  dataPreprocessed,
  "~/Desktop/P8/SpyPreprocessed.csv",
  row.names = FALSE
)

### Cleaning -----------------------------------------------------------------
dataCleaned <- funCleaning(dataPreprocessed)

write.csv(
  dataCleaned,
  "~/Desktop/P8/SpyCleaned.csv",
  row.names = FALSE
)
