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
inFile = paste0(dataDir,'tokenizedText.txt')
gramFile = './keepTokenSet.txt'

outFile = paste0(dataDir,'biGramCount.csv')
pngFile = paste0(dataDir,'biGramCount.png')
tmpFile = paste0(dataDir,'biGramCount_tmp.csv')
nb2Keep = 3  # only keep the biGrams we see at least these many times

pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
DEBUG = FALSE
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

# ---- Functions ----
# ---
# Create a vector of biGrams "word1 word2" from a vector of words (in sequence)
makeBiGram <- function(lines, keepersHashTbl) {
# Create 2-word strings from consecutive words in tokenSet
# i.e paste tokenSet with a shifted-by-1 version of itself

	# Eliminate blanks at start and end of sentences to avoid empty strings after strsplit
	# More efficient than post-processing code commented out below
	lines <- gsub("^ +| +$", "", lines)
	biGramVct <- foreach(i=1:length(lines),.combine=c) %dopar% {
		words <- unlist(strsplit(lines[i], " +")) # Account for multiple blanks
		# Eliminate empty strings
		# words <- words[unlist(lapply(words, function(w) {nchar(w) >0}))]
		# only keep the words that are in the reference - i.e. whose hash is not null
		words <- words[unlist(lapply(words, function(w){! is.null(keepersHashTbl[[w]])}))]
		# Create a set of 1st words and second words - then paste them to create a bigram
		if (length(words) < 2 ) {
			outVct <- c() # Sentence has shrunk to 0 or 1 keeper words
		} else {
			wordOne <- words[1:(length(words)-1)]
			wordTwo <- words[2:length(words)]
			outVct <- paste(wordOne, wordTwo, sep=" ") # create 1 string with the 2 words
		}
		outVct
	}
	biGramVct  # return the vector of bigrams
}


# ---

# Assume tokenSet is large, and thus contains repeats
# Use Run Length Encoding to count the repeats inside tokenSet
countWithNewHash <- function(tokenSet, hashList) {
	# Diassemble the list
	gramVct <- hashList[[1]]
	gramCnt <- hashList[[2]]
	hashTbl <- hashList[[3]]

	# Sort orders all the tokens, and thus the repeats are one after the other
	# RLE then counts them
	tokenRLE <- rle(sort(tokenSet))
	# Identify the tokens in this batch which are not in the hash table
	theseToken <- tokenRLE$values  # unique by construction
	# newTokens are the members of theseToken whose hash is null
	newToken <- theseToken[ unlist(lapply(theseToken, function(w) {is.null(hashTbl[[w]])})) ]
	nbHash <- length(hashTbl)
	nbNew <- length(newToken)
	if (nbNew >0 ) { # we have new tokens
		# add the new tokens to the hash table
		hashTbl[newToken] <- (nbHash+1):(nbHash+nbNew)
		# and to the list of grams
		gramVct <- c(gramVct, newToken)
		# Initialize their count to 0
		gramCnt <- c(gramCnt, seq(0,0, length.out=nbNew))
	}
	rm(theseToken,newToken)
	gc()
	# tokenRLE has 2 columns: values (i.e. the words) and lengths (i.e. counts)
	for (i in 1:length(tokenRLE$values)) {  # weird way to get the size of RLE
		token <- tokenRLE$values[i]
		count <- tokenRLE$lengths[i]
		# For each token, determine if it is already in the table and 
		# increment the gramCount for its hash value
		gramCnt[hashTbl[[token]]] <- gramCnt[hashTbl[[token]]] + count
	}
	list(gramVct, gramCnt, hashTbl)   # return the reconstituted list
}




# ---- Main ----
consoleOut("Starting at: ", Sys.time())
consoleOut("Source:", source, "- File: ", inFile)
linesToProcess <- getLineCount(inFile)
consoleOut("Lines to Process:", linesToProcess)


sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Initialize the dataframe that will hold the count for each token
# Read the tokens identified in previous path
keepGram <- readLines(gramFile)
keepHash <- hash(keepGram, 1:length(keepGram))
consoleOut("Keeping  # tokens: ", prettyNum(length(keepGram),big.mark = ","))
rm(keepGram)
gc()

# gramCount is a vector indexed by the hash of each token (bigram)
# hashTable stores the hash values
# it holds the number of occurences of each bigram in gramVector
# Store 1 entry (of count 0) - so that the tables are not empty
hashTable <- hash("of the",1)
gramVector <- c("of the")
gramCount <- c(0)
hashList <- list(gramVector, gramCount, hashTable)

con <- file(inFile, open="rt")
totalRead <- 0
loopTime <- Sys.time()
repeat {
	lines <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(lines) == 0) break   # EOF

	totalRead <- totalRead + length(lines)
	# Aggregate all the lines into a single character buffer
	biGramVct <- makeBiGram(lines, keepHash)
	hashList <- countWithNewHash(biGramVct, hashList)


	# print status once in a while
	if (difftime(Sys.time(), lastStatus,  units="secs") > statusFreq) {  # Show sign of life 
		lastStatus <- Sys.time()
		consoleOut("Lines read: ", prettyNum(totalRead,big.mark = ","), ' - ', round(100*totalRead/linesToProcess,2), "% Complete")
		consoleOut("Number of Bigrams: ", prettyNum(length(hashList[[1]]),big.mark = ","))
		consoleOut("Loop time: ", sec2HMS(difftime(Sys.time(),loopTime,units="secs")))
		consoleOut("Predicted completion time", predictEndTime(sysStart, linesToProcess,totalRead))
		print_runtime(sysStart, procStart)
	}

	loopTime <- Sys.time()
	if (DEBUG) break 	# DEBUG - stop after 1 iteration
}

close(con)
# Create a data frame with 2 columns: The bigrams, and their respective counts
biGramCount <- data.frame(cbind(hashList[[1]], hashList[[2]]), stringsAsFactors = FALSE)
colnames(biGramCount) <- c("Bigram", "Count")
rm(hashList)
gc()

# Only keep biGrams that we see 2x or more
biGramCount <- biGramCount[biGramCount$Count >=nb2Keep,]
consoleOut("Lines read: ", totalRead)
consoleOut("Keeping: ", prettyNum(nrow(biGramCount),big.mark = ","))
consoleOut("Minimum occurences:", nb2Keep)
print_runtime(sysStart, procStart)
write.csv(biGramCount, file=tmpFile, row.names = FALSE)

# --- Compute Distribution

gramDistri <- computeDistri(biGramCount)

# Save results and print statistics
write.csv(gramDistri, file=outFile, row.names = FALSE)
print(percentiles(gramDistri))

# Plot the cumul percentages
title <- paste0("Bi-gram cumulative distribution\nSource: ", source, " - With least ", nb2Keep, " occurences")
plot(gramDistri$pct,type="l",main=title, ylab="Cumulative Distribution",yaxp  = c(0,100,20))
grid(lwd=2)
# Plot the cumul percentages
png(filename=pngFile)
plot(gramDistri$pct,type="l",main=title, ylab="Cumulative Distribution",yaxp  = c(0,100,20))
grid(lwd=2)
dev.off()

consoleOut("Completed at: ", Sys.time())

# ---
