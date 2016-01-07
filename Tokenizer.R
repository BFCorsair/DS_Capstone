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
library(doParallel)
library(hash)
# ---
source("BF_util.R")  # my personal utilities 


# Source can be Blog, News or Twitter
repeat {
	cat("Enter source - one of: Blog, News, Twitter","\n") # prompt
	source <- scan(what=character(),nlines=1)
	# ToDo: Clean the sentence
	if(length(source) == 0) stop("Aborted")
	if (source %in% c("Blog", "News", "Twitter")) {
		break 
	} else {
		print("Source must be one of: Blog, News, Twitter")
		print("Enter <CR> to abort")
	}

}
consoleOut("Tokenizer - source is: ", source)


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
	bufSize1 = 5000
	bufSize2 = 100
	inFile = '../Data/Blog/blog_clean_1000.txt'
} else {
	bufSize1 = 50000
	bufSize2 = 10000
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
registerDoParallel()


sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution

refTokenSet <- readLines(tokenFile)
consoleOut("Number of Reference tokens:", length(refTokenSet))
# Create hashTable
refHashTbl <- hash(refTokenSet, 1:length(refTokenSet))
# Only keep the good words and write out to output file
# rawfile has one token per line
print("Rewriting source corpus")
inCon <- file(inFile, open="rt")
outCon <- file(outFile, open="wt")
write("", outCon, append=FALSE, sep='') # set file to empty
lastLoopTime <- Sys.time()
	# Sentences is a vector of sentences - each sentence is a line of input
totalRead <- 0
repeat {
	sentences <- readLines(con=inCon, n=bufSize2) # Vector of length n
	# Only keep words in the reference tokenSet
	if (length(sentences) == 0) break   # EOF# Only keep words in the reference tokenSet
	# We rewrite each sentence/line by stripping out any words that are not in the reference token set
	outLines <- foreach(i=1:length(sentences),.combine=c) %dopar%
		# rewriteLine(sentences[i], refTokenSet)
		rewriteLineHash(sentences[i], refHashTbl)
	write(outLines, outCon, append=TRUE, sep='\n')
	flush(outCon)

	totalRead <- totalRead + length(outLines)
	consoleOut("Processed: ", length(outLines), " - Total:", totalRead)
	print_runtime(sysStart, procStart)
	consoleOut("Loop execution time:", Sys.time()-lastLoopTime)
	lastLoopTime <- Sys.time()
}
close(outCon)



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

