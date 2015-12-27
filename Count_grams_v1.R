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

# ---
extractGram <- function(inString, tokenSet, gramCount) {
	# Get rid of all non-printable characters
	charbuf <- gsub("[^[:alnum:][:punct:] ]"," ",inString)
	# Eliminate all leading and trailing punctuation
	charbuf <- gsub(" [[:punct:]]*|[[:punct:]]* "," ", charbuf)
	# Replace dash or sub-dash by " " (by now they should only occur inside word)
	charbuf <- gsub("-|_", " ", charbuf)
	# Replace some punctuation characters inside words by spaces
	charbuf <- gsub('[.!,;:?&+\"\`#()*<=>\\^~]'," ",charbuf)
	# Eliminate numbers
	charbuf <- gsub(" [0-9]* ", " ", charbuf)

	 # Convert to lowercase
	charbuf <-tolower(charbuf)

	# Tokensize
	thisTokenSet <- WordTokenizer(charbuf)
	# More clean-up
	# Eliminate non-characters at start or end of word
	thisTokenSet <- gsub("^[^A-Za-z]*|[^A-Za-z]*$","",thisTokenSet)
	# Eliminate apostrophy in I'm, I'd, parent's, etc
	# need to use [[:punct:]] because neither ' or \' works
	thisTokenSet <- gsub("[[:punct:]][a-z]$","",thisTokenSet)
	for (token in thisTokenSet) {
		# Skip tokens that are not in our approved corpus
		if (! token %in% tokenSet) next
		# we have a good token - increment count in the table
		indx <- match(token, gramCount[,'Gram'],nomatch = 0)
		if (indx > 0) { # We've seen it before
			gramCount$Count[indx] <- 1 + gramCount$Count[indx]
		} else { # 1st time
			# Append the new token w/ a count of 1
			# gramCount[nrow(gramCount)+1,] <-data.frame(c(token, 1))
			gramCount <-rbind(gramCount, data.frame("Gram"=token, "Count"=1))
		}
	}
	gramCount  # return gramCount
}

# ---- Main ----

bufSize = 5000  # number of lines to read per iteration
statusFreq = 15 # Frequency, in seconds, of status output
inFile = './final/en_US/en_US.blogs.txt'
# NOTE: output file is same, regardless of input => collision potential
tokenFile = './tokenSet.txt'
outFile = './gramCount.csv'

# Initialize the dataframe that will hold the count for each token
gramCount <- data.frame(Gram=character(),Count=integer(),stringsAsFactors=FALSE)

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Read the tokens identified in previous path
tokenSet <- readLines(tokenFile)
consoleOut("Total number of  tokens: ", length(tokenSet))
con <- file(inFile, open="rt")
totalRead <- 0
repeat {
	charvct <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(charvct) == 0) break   # EOF

	totalRead <- totalRead + length(charvct)
	# Aggregate all the lines into a single character buffer
	for (sentence in charvct) {  # charvct is a vector of sentences
		# Extract & count the grams
		gramCount <- extractGram(sentence, tokenSet, gramCount)
	}

	# print status once in a while
	if (Sys.time() - lastStatus > statusFreq) {  # Show sign of life 
		lastStatus <- Sys.time()
		consoleOut("Lines read: ", totalRead)
		consoleOut("Number of new tokens: ", length(thisTokenSet))
		print_runtime(sysStart, procStart)
	}

	# DEBUG - stop after 1 iteration
	# break

}
close(con)
# Sort by Count & renumber the rows
gramCount <- gramCount[order(-gramCount$Count),]
rownames(gramCount) <- seq(length=nrow(gramCount)) 
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
consoleOut("Lines read: ", totalRead)
consoleOut('#Tokens: ', nrow(gramCount), " - #Instances: ", totalCount)
consoleOut('50% Index: ', fiftyIndx, " - 90% Index: ", ninetyIndx)

print_runtime(sysStart, procStart)
write.csv(gramCount, file=outFile)

# Clean up




# ---
