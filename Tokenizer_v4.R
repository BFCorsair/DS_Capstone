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

# --- 
# 1/4/2016: using sed & tr to remove non-alpha characters and transforming to lower case

library(RWeka)
library(stringi)  # faster string substitution
library(SnowballC)  # for word stemming
# ---
source("BF_util.R")  # my personal utilities 


# Source can be Blog, News or Twitter
source <- "Blog"

if (source == "Blog") {
	dataDir = '../Data/Blog/'  # note the '/' at the end
	inFile = paste0(dataDir,'en_US.blogs_clean.txt')
} else if (source == "News") {
	dataDir = '../Data/News/'
	inFile = paste0(dataDir,'en_US.news_clean.txt')
} else if (source == "Twitter") {
	dataDir = '../Data/Twitter/'
	inFile = paste0(dataDir,'en_US.twitter_clean.txt')
} else {
	consoleOut("incorrect source:", source)
	stop(1)
}

DEBUG = FALSE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 500
	inFile = '../Data/Blog/blog_clean_1000.txt'
} else {
	bufSize = 1000
}
statusFreq = 30 # Frequency, in seconds, of status output


# dictFile = '../dict/web2'
dictFile = '../bf_dict_clean.txt'
stopFile = '../stopwords.txt'

badWordFile = "../bad_words.txt"
# NOTE: output file is same, regardless of input => collision potential
rawFile = paste0(dataDir,'rawText.txt')
outFile = paste0(dataDir,'tokenizedText.txt')
tokenFile = paste0(dataDir,'tokenSet.txt')
notIndictFile = paste0(dataDir,'notindict.txt')


# ---- Main ----
consoleOut("Starting at: ", Sys.time())
consoleOut("Source:", source)


sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
tokenSet <- c()
lastStatus <- Sys.time() # Time of last status output
totalRead <- 0
inCon <- file(inFile, open="rt")
# rawCon <- file(rawFile, open="wt")
# write("",rawCon,append=FALSE) # to clear the file
repeat {
	charvct <- readLines(con=inCon, n=bufSize) # Vector of length n
	if (length(charvct) == 0) break   # EOF

	nbRead <- length(charvct)
	totalRead <- totalRead + nbRead
	# Aggregate all the lines into a single character buffer
	charbuf <- paste(charvct, sep = " ", collapse = " ")
	thisTokenSet <- WordTokenizer(charbuf)
	# write(thisTokenSet,rawCon,append=TRUE)

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
# close(rawCon)

# clean up big objects no longer used
rm(thisTokenSet,charbuf)
gc()

print_runtime(sysStart, procStart)
consoleOut("Lines read: ", totalRead)
consoleOut("Total number of  tokens: ", length(tokenSet))


# Remove bad words
badWordSet <- readLines(badWordFile)  # small enough file
consoleOut("Bad Word Count: ", length(badWordSet))
tokenSet <- setdiff(tokenSet, badWordSet)
consoleOut("Tokens without bad words: ", length(tokenSet))
rm(badWordSet)
gc()

# Find words that are not in the Unix dictionary
# dictSet <- readWordSet(dictFile, bufSize)
dictSet <- readLines(dictFile)
consoleOut("Dictionary Word Count: ", length(dictSet))
notInDict <- setdiff(tokenSet, dictSet)
consoleOut("Tokens that are not words: ", length(notInDict))
write(notInDict, file=notIndictFile, sep='\n')


# Get rid of the non-words
refTokenSet <- setdiff(tokenSet, notInDict)
# Get rid of stop-words and single-letter words
stopWords <- readLines(stopFile)
# Also add single letters to list of stopwords
stopWords <- sort(unique(c(stopWords, letters[1:26])))
refTokenSet <- setdiff(refTokenSet, stopWords)
consoleOut("Reference Tokens  count: ", length(refTokenSet))
# Reduce the token set by finding the stems of  each word
# refTokenSet <- wordStem(refTokenSet)
# Re-sort and unique after this
# refTokenSet <- sort(unique(refTokenSet))
# consoleOut("Reference Tokens  count - After stemming: ", length(refTokenSet))
write(refTokenSet, file=tokenFile, sep='\n')
rm(notInDict, stopWords, tokenSet)
gc()
print_runtime(sysStart, procStart)

# Only keep the good words and write out to output file
# rawfile has one token per line
print("Cleaning up tokens")
inCon <- file(inFile, open="rt")
outCon <- file(outFile, open="wt")
write("", outCon, append=FALSE, sep='') # set file to empty
lastLoopTime <- Sys.time()
totalRead <- 0
repeat {
	sentences <- readLines(con=inCon, n=bufSize) # Vector of length n
	# Only keep words in the reference tokenSet
	if (length(sentences) == 0) break   # EOF
	# Sentences is a vector of sentences - each sentence is a line of input
	# We rewrite each sentence/line by stripping out any words that are not in the reference token set
	sentences <- unlist(lapply(sentences, function(w) {rewriteLine(w,refTokenSet)}))
	write(sentences, outCon, append=TRUE, sep='\n')
	totalRead <- totalRead + length(sentences)
	consoleOut("Processed: ", length(sentences), " - Total:", totalRead)
	print_runtime(sysStart, procStart)
	consoleOut("Loop execution time", Sys.time()-lastLoopTime)
	lastLoopTime <- Sys.time()
}
close(inCon)
close(outCon)
rm(sentences,refTokenSet)
gc()


# sentences <- readLines(inFile) 
# # Only keep words in the reference tokenSet
# consoleOut("Read:", length(sentences))
# sentences <- unlist(lapply(sentences, function(w) {rewriteLine(w,refTokenSet)}))
# write(sentences,outFile, sep='\n')
# rm(sentences,refTokenSet)
# gc()

print_runtime(sysStart, procStart)
consoleOut("Completed at: ", Sys.time())

# --- End

