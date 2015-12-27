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

# ---- Main ----

bufSize = 5000  # number of lines to read per iteration
statusFreq = 15 # Frequency, in seconds, of status output
maxLines = 20000 # Lines to read in corpus to limit execution time
inFile = './final/en_US/en_US.blogs.txt'
badWordFile = "./bad_words.txt"
# NOTE: output file is same, regardless of input => collision potential
outRawFile = './tokenSetRaw.txt'
outFile = './tokenSet.txt'
dictFile = './dict/web2'
notIndictFile = './notindict.txt'
sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
tokenSet <- c()
lastStatus <- Sys.time() # Time of last status output
totalRead <- 0
con <- file(inFile, open="rt")
repeat {
	charvct <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(charvct) == 0) break   # EOF

	nbRead <- length(charvct)
	totalRead <- totalRead + nbRead
	# Aggregate all the lines into a single character buffer
	charbuf <- paste(charvct, sep = " ", collapse = " ")
	# consoleOut("Size raw charbuf: ", nchar(charbuf))
	# Eliminate all non-alphanumeric or punctuation characters
	charbuf <- gsub("[^[:alnum:][:punct:] ]"," ",charbuf)
	# consoleOut("Size  charbuf after non-char: ", nchar(charbuf))
	# Replace punctuation by blanks, before the tokenization
	# ToDo: deal w/ email: @ .
	# ToDo: deal w/ Tweeter syntax: @, #
	# ToDo: deal with ' and "
	#
	# In charbuf, each word is separated by spaces - except 1st and last workds
	# ToDo: apply the cleanup to first and last words as well
	# Eliminate all leading and trailing punctuation
	charbuf <- gsub(" [[:punct:]]*|[[:punct:]]* "," ", charbuf)
	# print(charbuf)
	# Replace dash or sub-dash by " " (by now they should only occur inside word)
	charbuf <- gsub("-|_", " ", charbuf)
	# Eliminate numbers
	charbuf <- gsub(" [0-9]* ", " ", charbuf)
	# Replace some punctuation characters inside words by spaces
	charbuf <- gsub('[.!,;:?&+\"\`#()*<=>\\^~]'," ",charbuf)
	# Convert to lowercase
	charbuf <-tolower(charbuf)	
	# consoleOut("Size  charbuf after punctuation: ", nchar(charbuf))

	# Tokensize
	thisTokenSet <- sort(unique(WordTokenizer(charbuf)))
	# consoleOut("Size of first THIS token set: ", length(thisTokenSet))

	# # Eliminate all leading punctuation
	# thisTokenSet <- gsub("^[:punct:]*","", thisTokenSet)
	# Eliminate non-characters at start or end of word
	thisTokenSet <- gsub("^[^A-Za-z]*|[^A-Za-z]*$","",thisTokenSet)
	# # Replace dash or sub-dash by " " (by now they should only occur inside word)
	# Eliminate numbers
	thisTokenSet <- gsub("^[0-9]*$","", thisTokenSet)
	# Eliminate apostrophy in I'm, I'd, parent's, etc
	# need to use [[:punct:]] because neither ' or \' works
	thisTokenSet <- gsub("[[:punct:]][a-z]$","",thisTokenSet)

	tokenSet <- sort(union(tokenSet,thisTokenSet))

	if (Sys.time() - lastStatus > statusFreq) {  # Show sign of life 
		lastStatus <- Sys.time()
		consoleOut("Lines read: ", totalRead)
		consoleOut("Number of new tokens: ", length(thisTokenSet))
		consoleOut("Total number of  tokens: ", length(tokenSet))
		print_runtime(sysStart, procStart)
	}
	# if (totalRead >= maxLines) break  # Keep it small initially
}
close(con)

print_runtime(sysStart, procStart)
consoleOut("Lines read: ", totalRead)
consoleOut("Total number of  tokens: ", length(tokenSet))


# Clean up
# Replace with nothing the following patterns
# '^-'
tokenSet <- sub('^-', "", tokenSet)

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
finalTokenSet <- setdiff(tokenSet, notInDict)
consoleOut("Tokens final count: ", length(finalTokenSet))
write(finalTokenSet, file=outFile, sep='\n')

# --- End
