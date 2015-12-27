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

# Prints the run times (Sys and Proc) from the times given as inputs
print_runtime <- function(sysStart, procStart) {
	run_time <- Sys.time() - sysStart
	proc_time <- proc.time() - procStart
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
		wordvct <- unlist(strsplit(charvct, " "))  # vector of single words
		# sort and make unique
		tmpwordset <- sort(unique(wordvct))
		# Merge with global set
		wordSet <- sort(union(wordSet,tmpwordset))
	}
	close(con)
	wordSet  # returns the set of words
}

# Bad words list from: https://www.cs.cmu.edu/~biglou/resources/

nbLines = 5000
statusFreq = 3*60 # Frequency, in seconds, of status output
inFile = './final/en_US/en_US.blogs.txt'
badWordFile = "./bad_words.txt"
# NOTE: output file is same, regardless of input => collision potential
outFile = './tokenSet.txt'
dictFile = './dict/web2'
outFile = '.dict.txt'

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
dictSet = readWordSet(inFile, nbLines)
print_runtime(sysStart, procStart, totalRead)
print(length(dictSet))
write(dictSet, file=outFile, sep='\n')
