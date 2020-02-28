### Packages -----------------------------------------------------------------
library(tidyverse)
source("./Scripts/Functions.R", echo = FALSE)

### Preprocessing ------------------------------------------------------------
# WARNING need 12 GB RAM
path <- "~/SPY"

dataPreprocessed <- funPreprocessing(path, 2)

write.csv(
  dataPreprocessed,
  "~/Cleaned/SpyPreprocessed.csv",
  row.names = FALSE
)

### Cleaning -----------------------------------------------------------------
dataSync <- funSync(dataPreprocessed)

write.csv(
  dataCleaned,
  "~/Cleaned/SpySync.csv",
  row.names = FALSE
)
