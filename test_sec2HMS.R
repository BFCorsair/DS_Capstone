
library(RWeka)
library(stringi)  # faster string substitution
library(hash)
library(dplyr)
# ---
source("BF_util.R")  # my personal utilities 

# ---- Constants ----

inVct = c(1,9, 59, 60, 61, 3600, 3530, 3540, 3541, 3599, 3601, 3659, 3660, 3661)

for(input in inVct) 	consoleOut("Input:", input, "Sec2HMS:", sec2HMS(input))
