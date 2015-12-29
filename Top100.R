# Plot Distributions


library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)



sourceList = c("News", "Blog",  "Twitter")

# Prints a collection of variables on a single line
consoleOut <- function(...) { print(paste(..., sep=" "))}

# ---
# Prints the run times (Sys and Proc) from the times given as inputs
print_runtime <- function(sysStart, procStart) {
	run_time <- Sys.time() - sysStart
	proc_time <- proc.time() - procStart
	consoleOut("Time: ", Sys.time(), " - Run time: ", run_time)
	print("Proc time: ")
	print(proc_time)	
}

size = 100
max = 5000

# --- Main

i <- 0
for (source in sourceList) {
	gramFile <- paste0("../Data/",source,"/gramCountDistri.csv")
	# consoleOut("Reading file:", gramFile)
	df <- read.csv(gramFile, stringsAsFactors=FALSE)
	# consoleOut("Nb rows:", nrow(df))
	if (i == 0) {
		ref <- df[1:size,1:2]
		colnames(ref) <- c("News", "Word")
		aggDF <- data.frame(ref[, c("Word","News")])
	} else {
		# Truncate
		df <- df[1:max,1:2]
		index <- c()
		for (word in ref$Word) {
			res <- df[df$value==word,]
			# consoleOut(word, res[1,1])
			if (nrow(res) > 0 ) { # we found a match
				index <- c(index, res[1,1])
			} else {
				index <- c(index, max) # round out to max
			}
		}
		aggDF <- cbind(aggDF,index)
	}
	i <- i+1
}
colnames(aggDF) <- c("Word", sourceList)
# Get rid of the "Word" column
aggDF <- aggDF[,-1]
result <- apply(aggDF, 2, max)
print("Index of the top 100 News words, in other sources")
print(result)

