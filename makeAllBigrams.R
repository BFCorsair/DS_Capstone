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
source("BF_util.R")  # my personal utilities 



# ---- Constants ----

# if (source == "Blog") {
# 	dataDir = '../Data/Blog/'  # note the '/' at the end
# } else if (source == "News") {
# 	dataDir = '../Data/News/'
# } else if (source == "Twitter") {
# 	dataDir = '../Data/Twitter/'
# } else {
# 	consoleOut("incorrect source:", source)
# 	stop(1)
# }

# inFile = paste0(dataDir,'tokenizedText.txt')
inFile <- '../Data/Blog/blog_clean_1000.txt'

# Use the 90% - aggregated across 3 sources - token set
gramFile = './keepTokenSet.txt'
biGramFile = './biGramSet.txt'

# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word

pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
DEBUG = FALSE
# number of lines to read per iteration
if (DEBUG) {
	bufSize = 100
	statusFreq = 10 # Frequency, in seconds, of status output
} else {
	bufSize = 5000
	statusFreq = 60 # Frequency, in seconds, of status output
}


makeAllBigrams <- function(tokens) {
	# Create a vector containing all possible 2-word combinations based on the input vector
	start <- Sys.time()
	status <- Sys.time()
	loop <- Sys.time()
	size <- length(tokens)
	biGramVct <- foreach(i=1:size,.combine=c) %dopar% {
		vct <- paste(tokens[i], tokens, sep = " ")

		# print status once in a while
		if (i %% 100 == 0) {  # Show sign of life 
			lastStatus <- Sys.time()
			consoleOut("Loop time: ", sec2HMS(difftime(Sys.time(),loop,units="secs")))
			consoleOut("Predicted completion time", predictEndTime(start, size,i))
			print_runtime(sysStart, procStart)
			loop <- Sys.time()
		}
		vct
	}
	biGramVct  # return the vector of bigrams
}




# ---- Main ----
consoleOut("Starting at: ", Sys.time())

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Read the tokens identified in previous path
df <- read.csv("aggregateTokenSet.csv", stringsAsFactors = FALSE)
# Keep only about 5,000 grams - they represent roughly 80% of the word occurences
df <- df[df$count>=1500,]
consoleOut("Keeping: ", prettyNum(nrow(df),big.mark = ","), "Tokens - cumulative % is: ", df[nrow(df),"pct"])
keepGram <- df$value  # vector of words that we keep
rm(df)
gc()

# create a vector containing all possible 2-word combinations of keeper grams
allBigram <- makeAllBigrams(keepGram)
consoleOut("Count of ALL Bigrams: ", length(allBigram))
gc()
write(allBigram, file=biGramFile, sep='\n')

consoleOut("Completed at: ", Sys.time())

# ---
