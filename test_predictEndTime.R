
library(RWeka)
library(stringi)  # faster string substitution
library(hash)
library(dplyr)
# ---
source("BF_util.R")  # my personal utilities 

# ---- Constants ----
endTime = 3600 # 1 hour
startTime = Sys.time()
consoleOut ("1 hour from now:", as.POSIXct(as.POSIXlt(startTime) + endTime))