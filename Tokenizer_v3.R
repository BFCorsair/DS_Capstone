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
# ---


DEBUG = FALSE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 500
} else {
	bufSize = 5000
}
statusFreq = 15 # Frequency, in seconds, of status output
maxLines = 20000 # Lines to read in corpus to limit execution time
inFile = './final/en_US/en_US.blogs.txt'
badWordFile = "./bad_words.txt"
# NOTE: output file is same, regardless of input => collision potential
rawFile = './en_US.blogs_raw.txt'
outFile = './en_US.blogs_tokenized.txt'
tokenFile = './tokenSet.txt'
dictFile = './dict/web2'
notIndictFile = './notindict.txt'

# ---
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
cleanSentence <- function(inString) {
	# Get rid of all non-printable characters
	# charbuf <- gsub("[^[:alpha:] ]"," ",inString)
	# faster
	charbuf <- stri_replace_all_regex(inString,"[^[:alpha:] ]"," ")
	# Convert to lowercase
	charbuf <-tolower(charbuf)

	# Tokensize and return
	WordTokenizer(charbuf)
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

# ---- Main ----
consoleOut("Starting at: ", Sys.time())


sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
tokenSet <- c()
lastStatus <- Sys.time() # Time of last status output
totalRead <- 0
inCon <- file(inFile, open="rt")
rawCon <- file(rawFile, open="wt")
repeat {
	charvct <- readLines(con=inCon, n=bufSize) # Vector of length n
	if (length(charvct) == 0) break   # EOF

	nbRead <- length(charvct)
	totalRead <- totalRead + nbRead
	# Aggregate all the lines into a single character buffer
	charbuf <- paste(charvct, sep = " ", collapse = " ")
	thisTokenSet <- cleanSentence(charbuf)
	write(thisTokenSet,rawCon,append=TRUE)

	# order and only keep uniques
	thisTokenSet <- sort(unique(thisTokenSet))
	# Merge with the aggregate tokenSet
	tokenSet <- sort(union(tokenSet,thisTokenSet))

	if (Sys.time() - lastStatus > statusFreq) {  # Show sign of life 
		lastStatus <- Sys.time()
		consoleOut("Lines read: ", totalRead)
		consoleOut("Number of new tokens: ", length(thisTokenSet))
		consoleOut("Total number of  tokens: ", length(tokenSet))
		print_runtime(sysStart, procStart)
	}
	if (DEBUG) break
}
close(inCon)
close(rawCon)

print_runtime(sysStart, procStart)
consoleOut("Lines read: ", totalRead)
consoleOut("Total number of  tokens: ", length(tokenSet))


# Remove bad words
badWordSet <- readLines(badWordFile)  # small enough file
consoleOut("Bad Word Count: ", length(badWordSet))
tokenSet <- setdiff(tokenSet, badWordSet)
consoleOut("Tokens without bad words: ", length(tokenSet))

# Find words that are not in the Unix dictionary
dictSet <- readWordSet(dictFile, bufSize)
consoleOut("Dictionary Word Count: ", length(dictSet))
notInDict <- setdiff(tokenSet, dictSet)
consoleOut("Tokens that are not words: ", length(notInDict))
write(notInDict, file=notIndictFile, sep='\n')
# Get rid of the non-words
refTokenSet <- setdiff(tokenSet, notInDict)
consoleOut("Reference Tokens  count: ", length(refTokenSet))
write(refTokenSet, file=tokenFile, sep='\n')
consoleOut("Completed at: ", Sys.time())


# Only keep the good words and write out to output file
# rawfile has one token per line
inCon <- file(rawFile, open="rt")
outCon <- file(outFile, open="wt")
repeat {
	charvct <- readLines(con=inCon, n=bufSize) # Vector of length n
	if (length(charvct) == 0) break   # EOF

	# Only keep words in the reference tokenSet
	thisTokenSet <- charvct[charvct %in% refTokenSet]
	write(thisTokenSet,outCon,append=TRUE)
}
close(inCon)
close(outCon)


print_runtime(sysStart, procStart)
consoleOut("Lines read: ", totalRead)
consoleOut("Total number of  tokens: ", length(tokenSet))
consoleOut("Completed at: ", Sys.time())

# --- End
