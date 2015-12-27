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

library(RWeka)

print_runtime <- function(sysStart, procStart, totalRead) {
	run_time <- Sys.time() - sysStart
	proc_time <- proc.time() - procStart
	print(paste0("Lines read: ", totalRead))
	print(paste0("Run time: ", run_time))
	print("Proc time: ")
	print(proc_time)	
}

# ---

consoleOut <- function(text, value) { print(paste0(text,value))}

# --- 

# Creates a set of words by reading a text file containing these words
readWordSet <- function(fileName, nbLines) {
	wordSet = c()  # global set
	con <- file(fileName, open="rt")
	repeat {
		charvct <- readLines(con=con, n=nbLines) # Vector of length n
		if (length(charvct) == 0) break   # EOF

		# Aggregate all the lines into a single character buffer
		charbuf <- paste(charvct, sep = " ", collapse = " ")
		# sort and make unique
		thisTokenSet <- sort(unique(charbuf))
		# Merge with global set
		wordSet <- sort(union(wordSet,thisTokenSet))
	}
	close(con)
	wordSet  # returns the set of words
}

# Bad words list from: https://www.cs.cmu.edu/~biglou/resources/

nbLines = 5000
statusFreq = 3*60 # Frequency, in seconds, of status output
inFile = './final/en_US/en_US.blogs.txt'
# NOTE: output file is same, regardless of input => collision potential
outFile = './tokenSet.txt'
con <- file('./final/en_US/en_US.blogs.txt', open="rt")
totalRead <- 0
sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
tokenSet <- c()
lastStatus <- Sys.time() # Time of last status output
nbRead <- nbLines # to make sure the loop starts
while (nbRead > 0 ) {
	charvct <- readLines(con=con, n=nbLines) # Vector of length n
	nbRead <- length(charvct)
	totalRead <- totalRead + nbRead
	# Aggregate all the lines into a single character buffer
	charbuf <- paste(charvct, sep = " ", collapse = " ")
	thisTokenSet <- sort(unique(WordTokenizer(charbuf)))
	tokenSet <- sort(union(tokenSet,thisTokenSet))

	if (Sys.time() - lastStatus > statusFreq) {  # Show sign of life 
		lastStatus <- Sys.time()
		print(paste0("Lines read: ", totalRead))
		print(paste0("Number of new tokens: ", length(thisTokenSet)))
		print(paste0("Total number of  tokens: ", length(tokenSet)))
		print_runtime(sysStart, procStart, totalRead)
	}
}

print_runtime(sysStart, procStart, totalRead)
write(tokenSet, file=outFile, sep='\n')

# Remove bad words
badWordSet = c()
con2 <- file('./final/en_US/en_US.blogs.txt', open="rt")
totalBad <- 0
nbRead <- nbLines
while (nbRead > 0 ) {
	charvct <- readLines(con=con2, n=nbLines) # Vector of length n
	nbRead <- length(charvct)
	totalBad <- totalBad + nbRead
	# Aggregate all the lines into a single character buffer
	charbuf <- paste(charvct, sep = " ", collapse = " ")
	thisTokenSet <- sort(unique(charbuf))
	badWordSet <- sort(union(badWordSet,thisTokenSet))
}
close(con2)
consoleOut("Bad Word Count: ", length(badWordSet))

# Find words that are not in the Unix dictionary
dictSet = c()

# ---
close(con)