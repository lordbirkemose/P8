### Packages -----------------------------------------------------------------
library(tidyverse)
source("./Scripts/Functions.R", echo = FALSE)

### Preprocessing ------------------------------------------------------------
# WARNING needs 12 GB RAM
path <- "~/SPY"

dataPreprocessed <- funPreprocessing(path, 2)

### Cleaning -----------------------------------------------------------------
dataSync <- funSync(dataPreprocessed)

write.csv(
  dataSync,
  gzfile("./Data/SpyCleaned.gz"),
  row.names = FALSE
)