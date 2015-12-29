
library(RWeka)
library(stringi)  # faster string substitution
library(hash)

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
biGramModelFile = paste0(dataDir, 'biGramModel.csv')
triGramModelFile = paste0(dataDir, 'triGramModel.csv')
# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word
pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
DEBUG = TRUE

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
# Note that at this point, the model may have multiple rows in case some {word1, word2} pairs are equally 
# Likely for the same word1
predictGram <- function(model, gram) {
	result <- filter(model, word1 == gram)
	if (nrow(result) >= 1) { 
		predict <- result[1,2] # i.e the 2nd word
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
consoleOut("Source:", source)

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Read the biGram and triGram models
biModelDF <- read.csv(biGramModelFile, stringsAsFactors=FALSE)
biModelDF <- biModelDF[,-1] # get rid of the 1st column
consoleOut("Total number of biGrams in Model: ", nrow(biModelDF))
triModelDF <- read.csv(triGramModelFile, stringsAsFactors=FALSE)
triModelDF <- triModelDF[,-1] # get rid of the 1st column
consoleOut("Total number of triGrams in Model: ", nrow(triModelDF))

repeat {
	cat("\n","Enter start of Phrase","\n") # prompt
	sentence <- scan(what=character(),nlines=1)
	# ToDo: Clean the sentence


	prediction <- ""
	if (length(sentence) == 0) break

	nbWords <- length(sentence)
	if (nbWords >= 2) {
		# Use the last 2 words to predict
		biGram <- paste(sentence[nbWords-1],sentence[nbWords])
		prediction <- predictGram(triModelDF, biGram)
		if (DEBUG & prediction != "") consoleOut("Predicted with 3-gram")
	}

	# If we don't have enough words or triGram prediction failed
	# try biGram prediction
	if (prediction == "" | nbWords == 1) {
		consoleOut("Using biGrams with: ", sentence[nbWords])
		prediction <- predictGram(biModelDF, sentence[nbWords])
		if (DEBUG & prediction != "") consoleOut("Predicted with 2-gram")
	} 
	# If previous predictions failed
	if (prediction == "") {
		prediction <- "the"
		if (DEBUG & prediction != "") consoleOut("Predicted with 1-gram")
	}
	consoleOut( paste(sentence, sep = " ", collapse = " "), "->", prediction)
}