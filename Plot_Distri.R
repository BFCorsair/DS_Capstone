# Plot Distributions


library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)



sourceList = c("Blog", "News", "Twitter")

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

# --- Main

firstTime <- TRUE
for (source in sourceList) {
	gramFile <- paste0("../Data/",source,"/gramCountDistri.csv")
	consoleOut("Reading file:", gramFile)
	df <- read.csv(gramFile, stringsAsFactors=FALSE)
	consoleOut("Nb rows:", nrow(df))
	if (firstTime) {
		firstTime <- FALSE
		aggDF <- data.frame(df[,"pct"])
	} else {
		# Truncate rows to the smallest
		delta <- nrow(aggDF) - nrow(df)
		if (nrow(aggDF) > nrow(df)) {
			aggDF <- aggDF[1:nrow(df),]
		} else {
			df <- df[1:nrow(aggDF), ]
		}
		aggDF <- cbind(aggDF, df[,"pct"])
	}
}
colnames(aggDF) <- sourceList
index <- c(1:nrow(aggDF))
aggDF <- cbind(index, aggDF)
colnames(aggDF) <- c("index", sourceList)
# longDF <- melt(aggDF, id = index)
# colnames(longDF) <- c("Index", "Source", "Percent")

# Truncate the first 10,000 rows
df <- data.frame(aggDF[1:10000,])
g <- ggplot(df, aes(index, ymin=0, ymax=100))
g <- g + geom_line(aes(y=Blog, colour="Blog"))
g <- g + geom_line(aes(y=News, colour="News"))
g <- g + geom_line(aes(y=Twitter, colour="Twitter"))
g <- g + ylab("Percent Coverage") + labs(title="Distribution of Word Counts", colour="Sources")
g

png(filename="Word_Distribution.png", width=480,height = 480)
print(g)
dev.off()




i <- 0
for (source in sourceList) {
	gramFile <- paste0("../Data/",source,"/biGramCountDistri.csv")
	consoleOut("Reading file:", gramFile)
	df <- read.csv(gramFile, stringsAsFactors=FALSE)
	consoleOut("Nb rows:", nrow(df))
	if (i == 0) {
		aggDF <- data.frame(df[,"pct"])
	} else {
		# Truncate rows to the smallest
		delta <- nrow(aggDF) - nrow(df)
		if (nrow(aggDF) > nrow(df)) {
			aggDF <- aggDF[1:nrow(df),]
		} else {
			df <- df[1:nrow(aggDF), ]
		}
		aggDF <- cbind(aggDF, df[,"pct"])
	}
	i <- i+1
}
colnames(aggDF) <- sourceList
index <- c(1:nrow(aggDF))
aggDF <- cbind(index, aggDF)
colnames(aggDF) <- c("index", sourceList)
# longDF <- melt(aggDF, id = index)
# colnames(longDF) <- c("Index", "Source", "Percent")

# Truncate the first 10,000 rows
df <- data.frame(aggDF[1:10000,])
q <- ggplot(df, aes(index, ymin=0, ymax=100))
q <- q + geom_line(aes(y=Blog, colour="Blog"))
q <- q + geom_line(aes(y=News, colour="News"))
q <- q + geom_line(aes(y=Twitter, colour="Twitter"))
q <- q + ylab("Percent Coverage") + labs(title="Distribution of Bigram Counts", colour="Sources")
q
png(filename="Bigram_Distribution.png", width=480,height = 480)
print(g)
dev.off()


grid.arrange(g, q, ncol = 2)
# consoleOut("Completed at: ", Sys.time())
