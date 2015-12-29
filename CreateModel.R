
library(RWeka)
library(stringi)  # faster string substitution
library(hash)
library(dplyr)

# ---- Constants ----


# Source can be Blog, News or Twitter
source <- "Blog"

if (source == "Blog") {
	dataDir = '../Data/Blog/'  # note the '/' at the end
} else if (source == "News") {
	dataDir = '../Data/News/'
} else if (source == "Twitter") {
	dataDir = '../Data/Twitter/'
} else {
	consoleOut("incorrect source:", source)
	stop(1)
}

statusFreq = 15 # Frequency, in seconds, of status output
inFile = paste0(dataDir,'tokenizedText.txt')
tokenFile = paste0(dataDir, 'tokenSet.txt')
gramFile = paste0(dataDir, 'gramCountDistri.csv')
biGramFile = paste0(dataDir, 'biGramCountDistri.csv')
triGramFile = paste0(dataDir, 'triGramCountDistri.csv')
biGramModelFile = paste0(dataDir, 'biGramModel.csv')
triGramModelFile = paste0(dataDir, 'triGramModel.csv')

# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word
pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
DEBUG = FALSE

# ---
# Note that at this point, the model may have multiple rows in case some {word1, word2} pairs are equally 
# Likely for the same word1
# ---

# Prints a collection of variables on a single line
consoleOut <- function(...) { print(paste(..., sep=" "))}

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
predictGram <- function(model, gram) {
	result <- filter(model, word1 == gram)
	if (nrow(result) == 1) {
		predict <- result[1,2]
		consoleOut("predictGram:", gram, predict)
	} else if (nrow(result) == 0) {
		predict <- ""
	} else {
		consoleOut("PredictGram has more than 1 result for:", gram)
		print(result)
		stop(1)
	}
	predict # return the prediction
}

# # vct is assumed to be a vector of strings
# # firstWord returns a vector containing the 1st word of each string
# # Words are separated by blanks
# firstWord <- function(vct) {
# 	unlist(lapply(strsplit(vct, " "),function(x) x[1]))
# }
# # Similartly secondWord returns the 2nd word
# secondWord <- function(vct) {
# 	unlist(lapply(strsplit(vct, " "),function(x) x[2]))
# }
# # Similartly thirdWord returns the 3rd word
# thirdWord <- function(vct) {
# 	unlist(lapply(strsplit(vct, " "),function(x) x[3]))
# }
# ---- Main ----
consoleOut("Starting at: ", Sys.time())
consoleOut("Source:", source)

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Model for biGrams
biGramDF <- read.csv(biGramFile, stringsAsFactors=FALSE)
biGramDF <- biGramDF[,-1] # get rid of the first column: indices
# Keep only the biGrams that make up the cumulative 90% - Grams are in column 1
consoleOut("Total number of  biGrams: ", nrow(biGramDF))
biGramDF <- filter(biGramDF, pct<=pctThreshold)
consoleOut("Keeping  # biGrams: ", nrow(biGramDF))
biGramDF <- biGramDF[,c("value","count")] # Only keep these 2 columns
# Split the biGram into 2 words - and add these to the dataframe
biGramDF <- cbind(biGramDF, read.table(textConnection(biGramDF$value)))
colnames(biGramDF) <- c("value", "count", "word1", "word2")

# Figure out the max counts for each word1
agg <- aggregate(count ~ word1, biGramDF, FUN = "max")
# Merge back with biGramDF so that we can find word2
biModelDF <-  merge(agg, biGramDF)
# Only keep the 2 word columns
biModelDF <- biModelDF[,c("word1", "word2")]
rm(biGramDF, agg)
gc()

consoleOut("biGram model size: ", nrow(biModelDF))
write.csv(biModelDF, file=biGramModelFile)
print_runtime(sysStart, procStart)


# Model for triGrams
triGramDF <- read.csv(triGramFile, stringsAsFactors=FALSE)
triGramDF <- triGramDF[,-1] # get rid of the first column: indices
# Keep only the triGrams that make up the cumulative 90% - Grams are in column 1
consoleOut("Total number of  triGrams: ", nrow(triGramDF))
triGramDF <- filter(triGramDF, pct<=pctThreshold)
consoleOut("Keeping  # triGrams: ", nrow(triGramDF))
triGramDF <- triGramDF[,c("value","count")] # Only keep these 2 columns
# Split the triGram into 3 words - and add these to the dataframe
triGramDF <- cbind(triGramDF, read.table(textConnection(triGramDF$value)))
colnames(triGramDF) <- c("value", "count", "word1", "word2", "word3")
triGramDF <- mutate(triGramDF, biGram=paste(word1,word2, sep=" "))

# Figure out the max counts for each word1
agg <- aggregate(count ~ biGram, triGramDF, FUN = "max")
# Merge back with triGramDF so that we can find word3
# Only keep the columns we need
triModelDF <-  merge(agg, triGramDF[,c("count", "biGram", "word3")])
# Only keep the 2 word columns
triModelDF <- triModelDF[,c("biGram", "word3")]
colnames(triModelDF) <- c("word1", "word2")  # to be consistent w/ biGram
rm(triGramDF, agg)
gc()


consoleOut("triGram model size: ", nrow(triModelDF))
write.csv(triModelDF, file=triGramModelFile)
print_runtime(sysStart, procStart)
consoleOut("Ended at: ", Sys.time())


if(DEBUG) {
	# test 

	repeat {
		cat("Enter start of Phrase","\n") # prompt
		sentence <- scan(what=character(),nlines=1)
		# ToDo: Clean the sentence
		if(sentence == "") break

		prediction <- predictGram(biModelDF, sentence)
		consoleOut(sentence, "->", prediction)
	}
}

