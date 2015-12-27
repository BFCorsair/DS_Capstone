library(microbenchmark)
library(stringi)

library(RWeka)

# Results
# Unit: seconds
#                   expr      min       lq     mean   median       uq      max neval
#   tokenizeFile(inFile) 40.82562 41.64358 44.37700 44.39968 46.81576 51.08436   100
#  tokenizeFile2(inFile) 27.16003 27.82201 29.46331 28.53552 30.99633 33.81229   100

# ---- Constants ----

bufSize = 5000  # number of lines to read per iteration
statusFreq = 15 # Frequency, in seconds, of status output
inFile = './final/en_US/en_US.blogs.txt'
# NOTE: output file is same, regardless of input => collision potential
tokenFile = './tokenSet.txt'
outFile = './gramCount.csv'
# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word
sentenceStart = "S_o_S"
sentenceEnd = "E_o_S"
DEBUG = FALSE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 100
} else {
	bufSize = 5000
}


# Prints the run times (Sys and Proc) from the times given as inputs
print_runtime <- function(sysStart, procStart) {
	run_time <- Sys.time() - sysStart
	proc_time <- proc.time() - procStart
	print(paste0("Run time: ", run_time))
	print("Proc time: ")
	print(proc_time)	
}
# ---

# Prints a collection of variables on a single line
consoleOut <- function(...) { print(paste0(...))}

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
cleanSentence <- function(inString) {
	# Get rid of all non-printable characters
	charbuf <- gsub("[^[:alpha:] ]"," ",inString)
}

cleanSentence2 <- function(inString) {
	# Get rid of all non-printable characters
	charbuf <- stri_replace_all_regex(inString,"[^[:alpha:] ]"," ")
}

tokenizeFile <- function (inFile) {
	con <- file(inFile, open="rt")
	totalRead <- 0
	repeat {
		charvct <- readLines(con=con, n=bufSize) # Vector of length n
		if (length(charvct) == 0) break   # EOF

		totalRead <- totalRead + length(charvct)
		# Aggregate all the lines into a single character buffer
		charbuf <- paste(charvct, sep = " ", collapse = " ")
		tokenized <- cleanSentence(charbuf)

		# if (DEBUG) 	consoleOut("Lines read: ", totalRead)

		# consoleOut("Lines read: ", totalRead)
		# consoleOut("Number of new tokens: ", length(thisTokenSet))
		# print_runtime(sysStart, procStart)

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
}


tokenizeFile2 <- function (inFile) {
	con <- file(inFile, open="rt")
	totalRead <- 0
	repeat {
		charvct <- readLines(con=con, n=bufSize) # Vector of length n
		if (length(charvct) == 0) break   # EOF

		totalRead <- totalRead + length(charvct)
		# Aggregate all the lines into a single character buffer
		charbuf <- paste(charvct, sep = " ", collapse = " ")
		tokenized <- cleanSentence2(charbuf)

		# if (DEBUG) 	consoleOut("Lines read: ", totalRead)

		# consoleOut("Lines read: ", totalRead)
		# consoleOut("Number of new tokens: ", length(thisTokenSet))
		# print_runtime(sysStart, procStart)

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
consoleOut("Total number of  tokens: ", length(tokenSet))
con <- file(inFile, open="rt")
totalRead <- 0


results <- microbenchmark(tokenizeFile(inFile), tokenizeFile2(inFile))
print(results)



