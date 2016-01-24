# Plot Distributions
source("BF_util.R")  # my personal utilities 
library(ggplot2)
library(reshape2)
library(gridExtra)

progName =  "Plot_Final"
source = "All"
sourceList = c("Blog", "News", "Twitter")
dataDir = './'
maxN = 50000

# ---- Constants ----


wordFile = paste0(dataDir, 'aggregateTokenSet.csv')
biGramFile = paste0(dataDir, 'aggregateBigrams.csv')
triGramFile = paste0(dataDir, 'aggregateTrigrams.csv')


# --- Main
wDF <- read.csv(wordFile, stringsAsFactors=FALSE)
biDF <- read.csv(biGramFile, stringsAsFactors=FALSE)
triDF <- read.csv(triGramFile, stringsAsFactors=FALSE)
consoleOut("Total number of words: ", nrow(wDF))
consoleOut("Total number of biGrams: ", nrow(biDF))
consoleOut("Total number of  triGrams: ", nrow(triDF))

# Only keep the first maxN rows
index <- c(1:maxN)
aggDF <- data.frame(cbind(index,wDF$pct[1:maxN], biDF$pct[1:maxN], triDF$pct[1:maxN]))
colnames(aggDF) <- c("index", "OneWord", "TwoWord", "ThreeWord")


# Sub-sample the data set to make plotting faster
idx <- seq(1,maxN,100)
df <- aggDF[idx,]
g <- ggplot(df, aes(index, ymin=0, ymax=100))
g <- g + geom_line(aes(y=OneWord, colour="1-Word"),size=1.5)
g <- g + geom_line(aes(y=TwoWord, colour="2-Word"),size=1.5)
g <- g + geom_line(aes(y=ThreeWord, colour="3-Word"),size=1.5)
g <- g + ylab("Percent Coverage") + labs(title="Cumulative Distribution of Token Counts", colour="Token Length")
g

# png(filename="Token_Distribution.png", width=480,height = 480)
# print(g)
# dev.off()



# # Truncate the first 10,000 rows
# df <- data.frame(triDF[1:maxN,])
# q <- ggplot(df, aes(index, ymin=0, ymax=100))
# q <- q + geom_line(aes(y=Blog, colour="Blog"))
# q <- q + geom_line(aes(y=News, colour="News"))
# q <- q + geom_line(aes(y=Twitter, colour="Twitter"))
# q <- q + ylab("Percent Coverage") + labs(title="Distribution of Bigram Counts", colour="Sources")
# q
# # png(filename="Bigram_Distribution.png", width=480,height = 480)
# # print(g)
# # dev.off()


# grid.arrange(g, q, ncol = 2)
# # consoleOut("Completed at: ", Sys.time())
