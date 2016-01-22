
source("BF_util.R")  # my personal utilities 

progName =  "CreateModel"
source = "All"
sourceList = c("Blog", "News", "Twitter")
dataDir = './'

# ---- Constants ----


biGramFile = paste0(dataDir, 'aggregateBigrams.csv')
triGramFile = paste0(dataDir, 'aggregateTrigrams.csv')
biGramModelFile = paste0(dataDir, 'biGramModel.csv')
triGramModelFile = paste0(dataDir, 'triGramModel.csv')

DEBUG = FALSE

if (DEBUG) {
	statusFreq = 10 # Frequency, in seconds, of status output
	nb2Keep = 0  # only keep the trigrams we see at least these many times
	pctThreshold = 0 # We only keep the tokens whose cumulative frequency is under this threshold
} else {
	statusFreq = 60 # Frequency, in seconds, of status output
	nb2Keep = 3  # only keep the trigrams we see at least these many times
	pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
}

# ---
# Note that at this point, the model may have multiple rows in case some {word1, word2} pairs are equally 
# Likely for the same word1
# ---

# ---
predictGram <- function(model, gram) {
	result <- model[,model$word1 == gram]
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

# ---- Main ----
consoleOut("Starting at: ", Sys.time())

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Model for biGrams
biGramDF <- read.csv(biGramFile, stringsAsFactors=FALSE)
# Keep only the biGrams that make up the cumulative 90% - Grams are in column 1
consoleOut("Total number of  biGrams: ", nrow(biGramDF))
biGramDF <- biGramDF[biGramDF$pct<=pctThreshold,]
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
write.csv(biModelDF, file=biGramModelFile, row.names = FALSE)
print_runtime(sysStart, procStart)


# Model for triGrams
triGramDF <- read.csv(triGramFile, stringsAsFactors=FALSE)
# Keep only the triGrams that make up the cumulative 90% - Grams are in column 1
consoleOut("Total number of  triGrams: ", nrow(triGramDF))
triGramDF <- triGramDF[triGramDF$pct<=pctThreshold, ]
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
write.csv(triModelDF, file=triGramModelFile, row.names = FALSE)
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

