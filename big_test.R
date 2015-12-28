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

library(RWeka)
library(stringi)  # faster string substitution
library(hash)

# ---- Constants ----

statusFreq = 15 # Frequency, in seconds, of status output
inFile = './en_US.blogs_tokenized.txt'
# NOTE: output file is same, regardless of input => collision potential
tokenFile = './tokenSet.txt'
gramFile = './gramCountDistri.csv'
outBiFile = './big_test.csv'
# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word
pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
DEBUG = TRUE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 100000
} else {
	bufSize = 250000
}
# ---

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

# ---
# Create a vector of biGrams "word1 word2" from a vector of words (in sequence)
makeBiGram <- function(tokenSet, keepers) {
# Create 2-word strings from consecutive words in tokenSet
# i.e paste tokenSet with a shifted-by-1 version of itself
	wordOne <- tokenSet[1:(length(tokenSet)-1)]
	wordTwo <- tokenSet[2:length(tokenSet)]
	# Only keep pairs for which the 1st word is in "keepers"
	# keepVct is a vector of boolean - TRUE is workdOne is in keepers
	keepVct <- unlist(lapply(wordOne, function(w){w %in% keepers}))
	# Weed out the pairs we don't want
	wordOne <- wordOne[keepVct]
	wordTwo <- wordTwo[keepVct]
	# Only keep pairs for which the 2nd word is in "keepers"
	# keepVct is a vector of boolean - TRUE is workdOne is in keepers
	keepVct <- unlist(lapply(wordTwo, function(w){w %in% keepers}))
	# Weed out the pairs we don't want
	wordOne <- wordOne[keepVct]
	wordTwo <- wordTwo[keepVct]	
	paste(wordOne, wordTwo, sep=" ") # create 1 string with the 2 words
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


# Assume tokenSet is large, and thus contains repeats
# Use Run Length Encoding to count the repeats inside tokenSet
countWithNewHashOld <- function(tokenSet, hashList) {
	# Diassemble the list
	gramVct <- hashList[[1]]
	gramCnt <- hashList[[2]]
	hashTbl <- hashList[[3]]

	# Sort orders all the tokens, and thus the repeats are one after the other
	# RLE then counts them
	tokenRLE <- rle(sort(tokenSet))
	# tokenRLE has 2 columns: values (i.e. the words) and lengths (i.e. counts)
	for (i in 1:length(tokenRLE$values)) {  # weird way to get the size of RLE
		token <- tokenRLE$values[i]
		count <- tokenRLE$lengths[i]
		# For each token, determine if it is already in the table and 
		# increment the gramCount for its hash value
		if (! is.null(hashTbl[[token]])) {
			gramCnt[hashTbl[[token]]] <- gramCnt[hashTbl[[token]]] + count
		} else { # 1st time we see it
			# add the new token to the hash table
			hashTbl[[token]] <- 1+ length(gramVct)
			# Add the new token and its count to the respective vectors
			gramVct <- c(gramVct, token)
			gramCnt <- c(gramCnt, count)
		}
	}
	list(gramVct, gramCnt, hashTbl)   # return the reconstituted list
}



# ---- Main ----
consoleOut("Starting at: ", Sys.time())

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Initialize the dataframe that will hold the count for each token
# Read the tokens identified in previous path
tokenSet <- readLines(tokenFile)
gramDF <- read.csv(gramFile, stringsAsFactors=FALSE)
gramDF <- gramDF[,-1] # get rid of the first column: indices
# Keep only the grams that make up the cumulative 90% - Grams are in column 1
keepGram <- filter(gramDF, pct<=pctThreshold)[,1]  
rm(gramDF)
gc()
consoleOut("Total number of  tokens: ", length(tokenSet))
consoleOut("Keeping  # tokens: ", length(keepGram))

# gramCountHash is a vector indexed by the hash of each token
# it holds the number of occurences of each token 
# Hash of each token
# Store 1 entry (of count 0) - so that the tables are not empty
hashTable <- hash("of the",1)
gramVector <- c("of the")
gramCount <- c(0)
hashList <- list(gramVector, gramCount, hashTable)

con <- file(inFile, open="rt")
totalRead <- 0
repeat {
	tokenized <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(tokenized) == 0) break   # EOF

	totalRead <- totalRead + length(tokenized)
	# Aggregate all the lines into a single character buffer
	biGramVct <- makeBiGram(tokenized, keepGram)
	hashList <- countWithNewHashOld(biGramVct, hashList)

	consoleOut("Lines read: ", totalRead)
	consoleOut("Number of Bigrams: ", length(hashList[[1]]))
	print_runtime(sysStart, procStart)

	# print status once in a while
	# if (Sys.time() - lastStatus > statusFreq) {  # Show sign of life 
	# 	lastStatus <- Sys.time()
	# 	consoleOut("Lines read: ", totalRead)
	# 	consoleOut("Number of new tokens: ", length(thisTokenSet))
	# 	print_runtime(sysStart, procStart)
	# }

	if (DEBUG) break 	# DEBUG - stop after 1 iteration
}
close(con)
# Create a data frame with 2 columns: The bigrams, and their respective counts
biGramCount <- data.frame(cbind(hashList[[1]], hashList[[2]]), stringsAsFactors = FALSE)
colnames(biGramCount) <- c("Bigram", "Count")
rm(hashList)
gc()


consoleOut("Lines read: ", totalRead)
consoleOut("Final Number of  Bigrams: ", nrow(biGramCount))
print_runtime(sysStart, procStart)
write.csv(biGramCount, file=outBiFile)
consoleOut("Completed at: ", Sys.time())

# ---
