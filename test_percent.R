library(RWeka)
library(stringi)  # faster string substitution
library(hash)
library(dplyr)
# ---
source("BF_util.R")  # my personal utilities 

df <- read.csv("../Data/Blog/gramCount.csv", stringsAsFactors=FALSE)
results <- percentiles(df)
print(results)