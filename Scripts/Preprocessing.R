### Packages -----------------------------------------------------------------
library(tidyverse)
source("./Scripts/Functions.R")

### Preprocessing ------------------------------------------------------------
# path <- "/Volumes/Samsung_T5/highFrequencyData/SPY"
path <- "/Volumes/Samsung_T5/SPY2000-2001"

dataPreprocessed <- funPreprocessing(path)

### Write csv ----------------------------------------------------------------
write.csv(dataPreprocessed, )