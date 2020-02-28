### Packages -----------------------------------------------------------------
library(tidyverse)
source("./Scripts/Functions.R", echo = FALSE)

### Preprocessing ------------------------------------------------------------
path <- "~/SPY"

dataPreprocessed <- funPreprocessing(path, 2)

write.csv(
  dataPreprocessed,
  "~/Cleaned/SpyPreprocessed.csv",
  row.names = FALSE
)

### Cleaning -----------------------------------------------------------------
dataCleaned <- funCleaning(dataPreprocessed)

write.csv(
  dataCleaned,
  "~/Cleaned/SpyCleaned.csv",
  row.names = FALSE
)
