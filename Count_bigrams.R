# Reads the file
# Applies the following pipepline, by processing groups of N lines 
# - Reads N lines
# - Replaces all punctuations by spaces
# - Converts all characters to lowercase
# - Anything between spaces is a token
# - Replaces any "bad word" token with a space (and increments bad-word count)
# - Add each token to the overall token set
# - Outputs the token set when all lines have been consumed
# Later
# - creates 2-grams and 3-grams with counts so that we can create probabilities

# Punctuation characters: ! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~.

# Bad words list from: https://www.cs.cmu.edu/~biglou/resources/
# ----------


# ---
source("BF_util.R")  # my personal utilities 


# Source can be Blog, News or Twitter
source <- getSource()
consoleOut("Count_bigrams - source is: ", source)

# ---- Constants ----

if (source == "Blog") {
	dataDir = '../Data/Blog/'  # note the '/' at the end
} else if (source == "News") {
	dataDir = '../Data/News/'
} else if (source == "Twitter") {
	dataDir = '../Data/Twitter/'
} else {
	consoleOut("incorrect source:", source)
	stop(1)
}


# Use the 90% - aggregated across 3 sources - token set
biGramFile = './biGramSet.txt'

outFile = paste0(dataDir,'biGramCount.csv')
pngFile = paste0(dataDir,'biGramCount.png')
tmpFile = paste0(dataDir,'biGramCount_tmp.csv')

# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word

pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
DEBUG = TRUE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 100
	statusFreq = 10 # Frequency, in seconds, of status output
	inFile <- '../Data/Blog/blog_clean_1000.txt'
} else {
	bufSize = 5000
	statusFreq = 60 # Frequency, in seconds, of status output
	inFile = paste0(dataDir,'tokenizedText.txt')
}




# ---- Main ----
consoleOut("Starting at: ", Sys.time())
consoleOut("Source:", source, "- File: ", inFile)
linesToProcess <- getLineCount(inFile)
consoleOut("Lines to Process:", linesToProcess)


sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output


# create a vector containing all possible 2-word combinations of keeper grams
allBigram <- readLines(biGramFile)
consoleOut("Count of ALL Bigrams: ", prettyNum(length(allBigram),big.mark = ","))


# gramCount is a vector indexed by the hash of each token (bigram)
# hashTable stores the hash values
# allBigram is the list of bigrams
# Store 1 entry (of count 0) - so that the tables are not empty
hashTable <- hash(allBigram, 1:length(allBigram))
gramCount <- seq(0,0,len=length(allBigram))

con <- file(inFile, open="rt")
totalRead <- 0
loopTime <- Sys.time()
repeat {
	lines <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(lines) == 0) break   # EOF

	totalRead <- totalRead + length(lines)
	# Aggregate all the lines into a single character buffer
	gramCount <- countGramHash(lines, gramCount, hashTable)


	# print status once in a while
	if (difftime(Sys.time(), lastStatus,  units="secs") > statusFreq) {  # Show sign of life 
		lastStatus <- Sys.time()
		consoleOut("Lines read: ", prettyNum(totalRead,big.mark = ","))
		consoleOut("Loop time: ", sec2HMS(difftime(Sys.time(),loopTime,units="secs")))
		consoleOut("Predicted completion time", predictEndTime(sysStart, linesToProcess,totalRead))
		print_runtime(sysStart, procStart)
	}

	loopTime <- Sys.time()
	if (DEBUG) break 	# DEBUG - stop after 1 iteration
}
close(con)
# Create a data frame with 2 columns: The bigrams, and their respective counts
biGramCount <- data.frame(cbind(allBigram,gramCount), stringsAsFactors = FALSE)
colnames(biGramCount) <- c("Bigram", "Count")
rm(hashList,gramCount,allBigram)
gc()

# Only keep biGrams that we see 2x or more
biGramCount <- biGramCount[biGramCount$Count >=2,]
consoleOut("Lines read: ", totalRead)
consoleOut("Final Number of  Bigrams: ", nrow(biGramCount))
print_runtime(sysStart, procStart)
write.csv(biGramCount, file=tmpFile, row.names = FALSE)

# --- Compute Distribution

gramDistri <- computeDistri(biGramCount)

# Save results and print statistics
write.csv(gramDistri, file=outFile, row.names = FALSE)
print(percentiles(gramDistri))

# Plot the cumul percentages
png(filename=pngFile)
title <- paste0("Bi-gram cumulative distribution\nSource: ", source)
plot(gramDistri$pct,type="l",main=title, ylab="Cumulative Distribution")
dev.off()


consoleOut("Completed at: ", Sys.time())

# ---
