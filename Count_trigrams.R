
# ---
source("BF_util.R")  # my personal utilities 


# Source can be Blog, News or Twitter
source <- getSource()
consoleOut("Count_trigrams - source is: ", source)

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
trigramFile = './triGramSet.txt'
inFile = paste0(dataDir,'tokenizedText.txt')
gramFile = './keepTokenSet.txt'
biGramFile = './keepBiGrams.txt'

outFile = paste0(dataDir,'trigramCount.csv')
pngFile = paste0(dataDir,'trigramCount.png')
tmpFile = paste0(dataDir,'trigramCount_tmp.csv')
nb2Keep = 3  # only keep the trigrams we see at least these many times

DEBUG = FALSE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 500
	statusFreq = 10 # Frequency, in seconds, of status output
	inFile <- '../Data/Blog/blog_clean_1000.txt'
	nb2Keep = 0  # only keep the trigrams we see at least these many times
	pctThreshold = 0 # We only keep the tokens whose cumulative frequency is under this threshold
} else {
	bufSize = 5000
	statusFreq = 60 # Frequency, in seconds, of status output
	inFile = paste0(dataDir,'tokenizedText.txt')
	nb2Keep = 3  # only keep the trigrams we see at least these many times
	pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
}


makeTriGram <- function(lines, tokenHash, bigramHash) {
# Create 3-word strings from consecutive words in tokenSet
# i.e paste tokenSet with a shifted-by-1 and shifted-by 2 versions of itself
# In the process, use the hash tables of 1-gram and bi-grams to make sure that we 
# limit to only the grams and bigrams that we want to keep

	# Eliminate blanks at start and end of sentences to avoid empty strings after strsplit
	# More efficient than post-processing code commented out below
	lines <- gsub("^ +| +$", "", lines)
	triGramVct <- foreach(i=1:length(lines),.combine=c) %dopar% {
		words <- unlist(strsplit(lines[i], " +")) # Account for multiple blanks
		# Eliminate empty strings
		# words <- words[unlist(lapply(words, function(w) {nchar(w) >0}))]
		# only keep the words that are in the reference - i.e. whose hash is not null
		# Note: we do this here because we know that bigrams are made of only keeper words
		# otherwise we'd have to do it on wordThree
		# For some reason, sapply generates error: task 29 failed - "invalid subscript type 'list'"
		words <- words[unlist(lapply(words, function(w){! is.null(tokenHash[[w]])}))]
		# Create a set of bigrams and third words - then paste them to create a bigram
		if (length(words) < 3 ) {
			outVct <- c() # Sentence has shrunk to 0 or 1 keeper words
		} else {
			wordOne <- words[1:(length(words)-2)]
			wordTwo <- words[2:(length(words)-1)]
			wordThree <- words[3:length(words)]
			biVct <- paste(wordOne, wordTwo, sep=" ") # create bigrams with the first 2 words
			# Get the index of the biGrams that are Keepers
			idx <- sapply(biVct, function(w){! is.null(bigramHash[[w]])})
			# Use this index to build the vector of triGrams
			outVct <- paste(wordOne[idx], wordTwo[idx], wordThree[idx], sep=" ")
		}
		outVct
	}
	triGramVct  # return the vector of trigrams
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
consoleOut("Source:", source)

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output
linesToProcess <- getLineCount(inFile)

# Read the tokens identified in previous path
tokenSet <- readLines(gramFile)
bigramSet <- readLines(biGramFile)


consoleOut("Total number of  tokens: ", prettyNum(length(tokenSet),big.mark = ","))
consoleOut("Total number of  tokens: ", prettyNum(length(bigramSet),big.mark = ","))
# Create hash tables for each
tokenHash <- hash(tokenSet, 1:length(tokenSet))
bigramHash <- hash(bigramSet,1:length(bigramSet))
rm(tokenSet,bigramSet)
gc()

# gramCountHash is a vector indexed by the hash of each token
# it holds the number of occurences of each token 
# Store 1 entry (of count 0) - so that the tables are not empty
initialTrigram <- "love endures delay"
hashTable <- hash(initialTrigram,1)
gramVector <- c(initialTrigram)
gramCount <- c(0)

con <- file(inFile, open="rt")
totalRead <- 0
loopTime <- Sys.time()
repeat {
	lines <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(lines) == 0) break   # EOF

	totalRead <- totalRead + length(lines)
	# Aggregate all the lines into a single character buffer
	triGramVct <- makeTriGram(lines, tokenHash, bigramHash)

	# Count the instances of each trigram
	for (triG in triGramVct) {
		if(is.null(hashTable[[triG]])) { # not in the table, need to add it
			# Create new hash entry
			hashTable[[triG]] <- 1+length(hashTable)
			# Add triGram to list
			gramVector <- c(gramVector, triG)
			# Add a new entry to the count equal to 1
			gramCount <- c(gramCount, 1)
		} else { # increment the count
			hh <- hashTable[[triG]]
			gramCount[hh] <- 1 + gramCount[hh]
		}
	}

	# print status once in a while
	if (difftime(Sys.time(), lastStatus,  units="secs") > statusFreq) {  # Show sign of life 
		lastStatus <- Sys.time()
		consoleOut("Lines read: ", prettyNum(totalRead,big.mark = ","), ' - ', round(100*totalRead/linesToProcess,2), "% Complete")
		consoleOut("Number of Trigrams: ", prettyNum(length(gramVector),big.mark = ","))
		consoleOut("Loop time: ", sec2HMS(difftime(Sys.time(),loopTime,units="secs")))
		consoleOut("Predicted completion time", predictEndTime(sysStart, linesToProcess,totalRead))
		print_runtime(sysStart, procStart)
	}
	loopTime <- Sys.time()	

	if (DEBUG) break 	# DEBUG - stop after 1 iteration
}
close(con)

# Create a data frame with 2 columns: The bigrams, and their respective counts
triGramCount <- data.frame(gramVector, gramCount, stringsAsFactors = FALSE)
colnames(triGramCount) <- c("Trigram", "Count")
gc()
# Only keep triGrams that we see 2x or more
triGramCount <- triGramCount[triGramCount$Count >=nb2Keep,]
consoleOut("Lines read: ", totalRead)
consoleOut("Keeping: ", prettyNum(nrow(triGramCount),big.mark = ","))
consoleOut("Minimum occurences:", nb2Keep)

write.csv(triGramCount, file=tmpFile, row.names = FALSE)
print_runtime(sysStart, procStart)


# --- Compute Distribution

gramDistri <- computeDistri(triGramCount)

# Save results and print statistics
write.csv(gramDistri, file=outFile, row.names = FALSE)
print(percentiles(gramDistri))

# Plot the cumul percentages
title <- paste0("Tri-gram cumulative distribution\nSource: ", source, " - With least ", nb2Keep, " occurences")
plot(gramDistri$pct,type="l",main=title, ylab="Cumulative Distribution",yaxp  = c(0,100,20))
grid(lwd=2)
# Plot the cumul percentages
png(filename=pngFile)
plot(gramDistri$pct,type="l",main=title, ylab="Cumulative Distribution",yaxp  = c(0,100,20))
grid(lwd=2)
dev.off()

print_runtime(sysStart, procStart)

