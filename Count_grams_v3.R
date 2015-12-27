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

# ---- Constants ----

bufSize = 5000  # number of lines to read per iteration
statusFreq = 15 # Frequency, in seconds, of status output
inFile = './final/en_US/en_US.blogs.txt'
# NOTE: output file is same, regardless of input => collision potential
tokenFile = './tokenSet.txt'
outFile = './gramCount.csv'
outBiFile = './biGramCount.csv'
# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word
sentenceStart = "S_o_S"
sentenceEnd = "E_o_S"
DEBUG = FALSE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 500
} else {
	bufSize = 10000
}
# ---

# Prints a collection of variables on a single line
consoleOut <- function(...) { print(paste0(...))}

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

## Creates a set of words by reading a text file containing these words
readWordSet <- function(fileName, nbLines) {
	wordSet = c()  # global set
	con <- file(fileName, open="rt")
	repeat {
		charvct <- readLines(con=con, n=nbLines) # Vector of length n
		if (length(charvct) == 0) break   # EOF

		# Aggregate all the lines into a single character buffer
		wordvct <- unlist(strsplit(charvct, " "))  # vector of single words
		# sort and make unique
		tmpwordset <- sort(unique(wordvct))
		# Merge with global set
		wordSet <- sort(union(wordSet,tmpwordset))
	}
	close(con)
	wordSet  # returns the set of words
}

# ---
cleanSentence <- function(inString, refTokenSet) {
	# Get rid of all non-printable characters
	# charbuf <- gsub("[^[:alpha:] ]"," ",inString)
	# faster
	charbuf <- stri_replace_all_regex(inString,"[^[:alpha:] ]"," ")
	# Convert to lowercase
	charbuf <-tolower(charbuf)

	# Tokensize
	thisTokenSet <- WordTokenizer(charbuf)
	# More clean-up
	# Only keep the words that are in the approved corpus
	newTokenSet <- thisTokenSet[thisTokenSet %in% refTokenSet]
	newTokenSet # Return the result
}

# ---

# Assume tokenSet is large, and thus contains repeats
# Use Run Length Encoding to count the repeats inside tokenSet
countGram <- function(tokenSet, gramCount) {

	# Sort orders all the tokens, and thus the repeats are one after the other
	# RLE then counts them
	tokenRLE <- rle(sort(tokenSet))
	# tokenRLE has 2 columns: values (i.e. the words) and lengths (i.e. counts)
	for (i in 1:length(tokenRLE$values)) {  # weird way to get the size of RLE
		token <- tokenRLE$values[i]
		count <- tokenRLE$lengths[i]
		indx <- match(token, gramCount[,'Gram'],nomatch = 0)
		if (indx > 0) { # We've seen it before
			gramCount$Count[indx] <- count + gramCount$Count[indx]
		} else { # 1st time
			# Append the new token w/ a count of 1
			# gramCount[nrow(gramCount)+1,] <-data.frame(c(token, 1))
			gramCount <-rbind(gramCount, data.frame("Gram"=token, "Count"=count))
		}
	}
	gramCount  # return gramCount
}

find_50_90 <- function(gramCount) {
	# Compute total Count
	totalCount <- sum(gramCount$Count)
	# Find the 50% and 90% cut
	runningTotal <- 0
	fiftyMark <- as.integer(round(totalCount*0.5,0))
	ninetyMark <- as.integer(round(totalCount*0.9,0))
	fiftyIndx <- 0
	ninetyIndx <- 0
	# Assumes gramCount is sorted in Desc order
	for (i in 1:nrow(gramCount)) {
		runningTotal <- runningTotal + gramCount$Count[i]
		if(fiftyIndx == 0 & runningTotal >= fiftyMark) fiftyIndx <- i
		else if (ninetyIndx == 0 & runningTotal >= ninetyMark) ninetyIndx <- i
	}
	# Return results
	data.frame("Token Count" = nrow(gramCount), "Instances Count"=totalCount, "Index50"=fiftyIndx, "Index90"=ninetyIndx)
}

# ---
# Create a vector of biGrams "word1 word2" from a vector of words (in sequence)
makeBiGram <- function(tokenSet) {
# Create 2-word strings from consecutive words in tokenSet
# i.e paste tokenSet with a shifted-by-1 version of itself
	paste(tokenSet[1:length(tokenSet)-1], tokenSet[2:length(tokenSet)], sep=" ")
}

# ----
sortByCount <- function(gramCount) {
# Sort by Count & re-number the rows
	gramCount <- gramCount[order(-gramCount$Count),]
	rownames(gramCount) <- seq(length=nrow(gramCount)) 
	gramCount
}


# ---- Main ----

# Initialize the dataframe that will hold the count for each token
gramCount <- data.frame(Gram=character(),Count=integer(),stringsAsFactors=FALSE)
biGramCount <- data.frame(Gram=character(),Count=integer(),stringsAsFactors=FALSE)

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Read the tokens identified in previous path
tokenSet <- readLines(tokenFile)
consoleOut("Starting at: ", sysStart)
consoleOut("Total number of  tokens: ", length(tokenSet))
con <- file(inFile, open="rt")
totalRead <- 0
repeat {
	charvct <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(charvct) == 0) break   # EOF

	totalRead <- totalRead + length(charvct)
	# Aggregate all the lines into a single character buffer
	charbuf <- paste(charvct, sep = " ", collapse = " ")
	tokenized <- cleanSentence(charbuf, tokenSet)
	gramCount <- countGram(tokenized, gramCount)
	# biGramVct <- makeBiGram(tokenized)
	# biGramCount <- countGram(biGramVct, biGramCount)

	consoleOut("Lines read: ", totalRead)
	consoleOut("Number of grams: ", nrow(gramCount))
	consoleOut("Number of biGrams: ", nrow(biGramCount))
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

# Find indices for 50% and 90% coverage for both gram and bigram
# Sort by Count & re-number the rows
gramCount <- sortByCount(gramCount)
gramResult <- find_50_90(gramCount)
print(gramResult)
# biGramCount <- sortByCount(biGramCount)
# biGramResult <- find_50_90(biGramCount)
# print(biGramResult)

consoleOut("Lines read: ", totalRead)


print_runtime(sysStart, procStart)
write.csv(gramCount, file=outFile)
write.csv(biGramCount, file=outBiFile)
consoleOut("Ended at: ", Sys.time())

# ---
