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

# Source can be Blog, News or Twitter
source <- "Twitter"

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

statusFreq = 15 # Frequency, in seconds, of status output
inFile = paste0(dataDir,'tokenizedText.txt')
tokenFile = paste0(dataDir,'tokenSet.txt')
gramFile = paste0(dataDir,'gramCountDistri.csv')
outBiFile = paste0(dataDir,'biGramCount.csv')


gramBiFile = paste0(dataDir, 'biGramCountDistri.csv')
outTriFile = paste0(dataDir, 'triGramCount.csv')

# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word
pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
biPctThreshold = 50 # only keep the biGrams whose cumulative frequency is under this threshold
DEBUG = FALSE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 1000
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
# Create a vector of triGrams "word1 word2" from a vector of words (in sequence)
makeTriGram <- function(tokenSet, keepers, keepersBi) {
# Create 2-word strings from consecutive words in tokenSet
# i.e paste tokenSet with a shifted-by-1 version of itself
	wordOne <- tokenSet[1:(length(tokenSet)-2)]
	wordTwo <- tokenSet[2:(length(tokenSet)-1)]
	wordThree <- tokenSet[3:length(tokenSet)]
	biGram <- paste(wordOne, wordTwo, sep=" ") # create 1 string with the 2 words

	# Only keep pairs for which the bigrams is in "keepersBi"
	# keepVct is a vector of boolean - TRUE is workdOne is in keepers
	keepVct <- unlist(lapply(biGram, function(w){w %in% keepersBi}))
	# Weed out the pairs we don't want
	biGram <- biGram[keepVct]
	wordThree <- wordThree[keepVct]
	# Only keep pairs for which the 2nd word is in "keepers"
	# keepVct is a vector of boolean - TRUE is workdOne is in keepers
	keepVct <- unlist(lapply(wordThree, function(w){w %in% keepers}))
	# Weed out the pairs we don't want
	biGram <- biGram[keepVct]
	wordThree <- wordThree[keepVct]

	paste(biGram, wordThree, sep=" ") # create 1 string with the 3 words
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

# Initialize the dataframe that will hold the count for each token
triGramCount <- data.frame(Gram=character(),Count=integer(),stringsAsFactors=FALSE)
# Read the tokens identified in previous path
tokenSet <- readLines(tokenFile)
gramDF <- read.csv(gramFile, stringsAsFactors=FALSE)
gramDF <- gramDF[,-1] # get rid of the first column: indices
# Keep only the grams that make up the cumulative 90% - Grams are in column 1
keepGram <- filter(gramDF, pct<=pctThreshold)[,1]

consoleOut("Total number of  tokens: ", length(tokenSet))
consoleOut("Keeping  # tokens: ", length(keepGram), " - Threshold (%): ", pctThreshold)

biGramDF <- read.csv(gramBiFile, stringsAsFactors=FALSE)
biGramDF <- biGramDF[,-1] # get rid of the first column: indices
# Keep only the biGrams that make up the cumulative biPctThreshold % - Grams are in column 1
keepBiGram <- filter(biGramDF, pct<=biPctThreshold)[,1]
consoleOut("Total number of  biGrams: ", nrow(biGramDF))
consoleOut("Keeping  # biGrams: ", length(keepBiGram), " - Threshold (%): ", biPctThreshold)
rm(gramDF,biGramDF)
gc()

# gramCountHash is a vector indexed by the hash of each token
# it holds the number of occurences of each token 
# Hash of each token
# Store 1 entry (of count 0) - so that the tables are not empty
hashTable <- hash("need to do",1)
gramVector <- c("need to do")
gramCount <- c(0)
hashList <- list(gramVector, gramCount, hashTable)


con <- file(inFile, open="rt")
totalRead <- 0
repeat {
	tokenized <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(tokenized) == 0) break   # EOF

	totalRead <- totalRead + length(tokenized)
	# Aggregate all the lines into a single character buffer
	triGramVct <- makeTriGram(tokenized, keepGram, keepBiGram)
	hashList <- countWithNewHash(triGramVct, hashList)

	consoleOut("Lines read: ", totalRead)
	consoleOut("Number of Trigrams: ", length(hashList[[1]]))
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
triGramCount <- data.frame(cbind(hashList[[1]], hashList[[2]]), stringsAsFactors = FALSE)
colnames(triGramCount) <- c("Trigram", "Count")
rm(hashList)
gc()


consoleOut("Lines read: ", totalRead)
consoleOut("Final Number of Trigrams: ", nrow(triGramCount))
write.csv(triGramCount, file=outTriFile)
print_runtime(sysStart, procStart)
consoleOut("Ended at: ", Sys.time())

# ---
