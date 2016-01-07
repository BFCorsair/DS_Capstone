
library(RWeka)
library(stringi)  # faster string substitution
library(SnowballC)  # for word stemming
library(doParallel)
library(hash)
# ---
source("BF_util.R")  # my personal utilities 


# Source can be Blog, News or Twitter
source <- "News"

if (source == "Blog") {
	dataDir = '../Data/Blog/'  # note the '/' at the end
	inFile = paste0(dataDir,'en_US.blogs_clean.txt')
} else if (source == "News") {
	dataDir = '../Data/News/'
	inFile = paste0(dataDir,'en_US.news_clean.txt')
} else if (source == "Twitter") {
	dataDir = '../Data/Twitter/'
	inFile = paste0(dataDir,'en_US.twitter_clean.txt')
} else {
	consoleOut("incorrect source:", source)
	stop(1)
}
tokenFile = paste0(dataDir,'tokenSet.txt')

DEBUG = TRUE
if (DEBUG) {
	inFile = '../Data/News/news_1000.txt'
}



sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
consoleOut("Starting at: ", Sys.time())
 
sentences <- readLines(inFile)
consoleOut("Input File: ", inFile, "Read:", length(sentences), "lines")
refTokenSet <- readLines(tokenFile)
consoleOut("Number of Reference tokens:", length(refTokenSet))
# Create hashTable
refHashTbl <- hash(refTokenSet, 1:length(refTokenSet))

wordList <-list()
listSize <- 0
print_runtime(sysStart, procStart)
loopTime <- Sys.time()
for (line in sentences) {
	words <- unlist(strsplit(line, " +")) # to handle consecutive spaces
	words <- words[unlist(lapply(words, function(w) {nchar(w) >0}))]
	listSize <- listSize +1 
	wordList[[listSize]] <- words
}
consoleOut("Time to break into words:", Sys.time()-loopTime)
loopTime <- Sys.time()

outList <- list()
for (i in 1:listSize) {
	words <- wordList[[i]]
	outList[[i]] <- words[unlist(lapply(words, function(w){! is.null(refHashTbl[[w]])}))]

}
consoleOut("Time to check against refTokenSet with Hash:", Sys.time()-loopTime)
loopTime <- Sys.time()
print_runtime(sysStart, procStart)


outList <- list()
print_runtime(sysStart, procStart)

for (i in 1:listSize) {
	words <- wordList[[i]]
	outList[[i]] <- words[unlist(lapply(words, function(w){w %in% refTokenSet}))]
}
consoleOut("Time to check against refTokenSet:", Sys.time()-loopTime)
loopTime <- Sys.time()
print_runtime(sysStart, procStart)



for (i in 1:listSize) {
	words <- outList[[i]]
	paste(words, sep = " ", collapse = " ")
}
consoleOut("Time to paste words:", Sys.time()-loopTime)
loopTime <- Sys.time()
print_runtime(sysStart, procStart)


