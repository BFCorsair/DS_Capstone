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

# Input file is assumed to have cleanup content: only acceptble tokens
# ----------

library(RWeka)
library(stringi)  # faster string substitution
library(hash)

# ---
# [1] "Lines read: 32886043"
# [1] "Time: 2015-12-25 13:23:43 - Run time: 2.56232758495543"
# [1] "Proc time: "
#     user   system  elapsed 
# 8426.439  778.680 9224.380 
# [1] "Completed at: 2015-12-25 13:23:43"

# ---- Constants ----

statusFreq = 15 # Frequency, in seconds, of status output
inFile = './en_US.blogs_tokenized.txt'
# NOTE: output file is same, regardless of input => collision potential
tokenFile = './tokenSet.txt'
outFile = './gramCount.csv'
# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word
sentenceStart = "S_o_S"
sentenceEnd = "E_o_S"
DEBUG = FALSE
# number of lines to read per iteration  (1 token per line)
if (DEBUG) {
	bufSize = 500
} else {
	bufSize = 100000
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


# Assume tokenSet is large, and thus contains repeats
# Use Run Length Encoding to count the repeats inside tokenSet
countGramHash <- function(tokenSet, gramCountHash,hashTable) {

	# Sort orders all the tokens, and thus the repeats are one after the other
	# RLE then counts them
	tokenRLE <- rle(sort(tokenSet))
	# tokenRLE has 2 columns: values (i.e. the words) and lengths (i.e. counts)
	for (i in 1:length(tokenRLE$values)) {  # weird way to get the size of RLE
		token <- tokenRLE$values[i]
		count <- tokenRLE$lengths[i]
		# For each token, increment the gramCount for its hash value
		gramCountHash[hashTable[[token]]] <- gramCountHash[hashTable[[token]]] + count
	}
	gramCountHash  # return gramCount
}



# ---- Main ----
consoleOut("Starting at: ", Sys.time())
sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Read the tokens identified in previous path
tokenSet <- readLines(tokenFile)
consoleOut("Total number of  tokens: ", length(tokenSet))

# gramCountHash is a vector indexed by the hash of each token
# it holds the number of occurences of each token 
gramCountHash <- seq(0,0,len=length(tokenSet))
# Hash of each token
hashTable <- hash(tokenSet, 1:length(tokenSet))

con <- file(inFile, open="rt")
totalRead <- 0
repeat {
	tokenized <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(tokenized) == 0) break   # EOF

	totalRead <- totalRead + length(tokenized)
	gramCountHash <- countGramHash(tokenized, gramCountHash, hashTable)
	# biGramVct <- makeBiGram(tokenized)
	# biGramCount <- countGram(biGramVct, biGramCount)

	consoleOut("Lines read: ", totalRead)
	consoleOut("Number of grams: ", nrow(gramCountHash))
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
rm(tokenized, hashTable)
gc()
gramCount<-data.frame(cbind(tokenSet,gramCountHash), stringsAsFactors = FALSE)
colnames(gramCount) <- c("Gram", "Count")
rm(gramCountHash)
gc()

# --- Wrap-up
consoleOut("Lines read: ", totalRead)
write.csv(gramCount, file=outFile)
print_runtime(sysStart, procStart)
consoleOut("Completed at: ", Sys.time())
rm(gramCount)
gc()
# ---
