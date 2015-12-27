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

statusFreq = 15 # Frequency, in seconds, of status output
inFile = './final/en_US/en_US.blogs.txt'
# NOTE: output file is same, regardless of input => collision potential
tokenFile = './tokenSet.txt'
gramFile = './gramCountDistri.csv'
outBiFile = './biGramCount.csv'
# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word
sentenceStart = "S_o_S"
sentenceEnd = "E_o_S"
pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
DEBUG = TRUE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 2500
} else {
	bufSize = 10000
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

# ---- Main ----
consoleOut("Starting at: ", Sys.time())

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Initialize the dataframe that will hold the count for each token
biGramCount <- data.frame(Gram=character(),Count=integer(),stringsAsFactors=FALSE)
# Read the tokens identified in previous path
tokenSet <- readLines(tokenFile)
gramDF <- read.csv(gramFile, stringsAsFactors=FALSE)
gramDF <- gramDF[,-1] # get rid of the first column: indices
# Keep only the grams that make up the cumulative 90% - Grams are in column 1
keepGram <- filter(gramDF, pct<=pctThreshold)[,1]  
consoleOut("Total number of  tokens: ", length(tokenSet))
consoleOut("Keeping  # tokens: ", length(keepGram))
con <- file(inFile, open="rt")
totalRead <- 0
repeat {
	charvct <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(charvct) == 0) break   # EOF

	totalRead <- totalRead + length(charvct)
	# Aggregate all the lines into a single character buffer
	charbuf <- paste(charvct, sep = " ", collapse = " ")
	tokenized <- cleanSentence(charbuf, tokenSet)
	biGramVct <- makeBiGram(tokenized, keepGram)

	biGramCount <- countGram(biGramVct, biGramCount)

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


consoleOut("Lines read: ", totalRead)
print_runtime(sysStart, procStart)
write.csv(biGramCount, file=outBiFile)
consoleOut("Completed at: ", Sys.time())

# ---
