
source("BF_util.R")  # my personal utilities 

progName =  "Predictor"

# ---- Constants ----

DEBUG = FALSE

dataDir = './'
biGramModelFile = paste0(dataDir, 'biGramModel.csv')
triGramModelFile = paste0(dataDir, 'triGramModel.csv')
gramFile = paste0(dataDir, 'keepTokenSet.txt')

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

cleanLine <- function(line, tokenHash) {
# Clean input line by only keeping the tokens that we use in our predictor

	# Eliminate blanks at start and end of sentences to avoid empty strings after strsplit
	# More efficient than post-processing code commented out below
	line <- gsub("^ +| +$", "", line)
	words <- unlist(strsplit(line, " +")) # Account for multiple blanks
	# Return a vector of words, where all the tokens are keepers
	words[unlist(lapply(words, function(w){has.key(w,tokenHash)}))]
}

predictor <- function (sentence, tokenHash, biModelDF, triModelDF) {
	# ToDo: Clean the sentence
	words <- cleanLine(sentence,tokenHash)
	consoleOut("Input:", sentence)
	consoleOut("Cleaned Input:", paste(words, sep= " ", collapse = " "))


	prediction <- ""
	nbWords <- length(words)
	if (nbWords == 0) break

	if (nbWords >= 2) {
		# Use the last 2 words to predict
		biGram <- paste(words[nbWords-1],words[nbWords])
		consoleOut("Using triGrams with: ", biGram)
		prediction <- predictGram(triModelDF, biGram)
		if (DEBUG & prediction != "") consoleOut("Predicted with 3-gram")
	}

	# If we don't have enough words or triGram prediction failed
	# try biGram prediction
	if (prediction == "" | nbWords == 1) {
		consoleOut("Using biGrams with: ", words[nbWords])
		prediction <- predictGram(biModelDF, words[nbWords])
		if (DEBUG & prediction != "") consoleOut("Predicted with 2-gram")
	} 
	# If previous predictions failed
	if (prediction == "") {
		prediction <- "the"
		if (DEBUG & prediction != "") consoleOut("Predicted with 1-gram")
	}
	prediction  # return
}

# ---- Main ----
consoleOut("Starting at: ", Sys.time())
consoleOut("Source:", source)

if (dataDir == './') consoleOut("HEADS UP: Using data files from local (rather than SwiftKey) Directory")

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

tokenSet <- readLines(gramFile)
consoleOut("Total number of tokens in Model: ", length(tokenSet))
# Read the biGram and triGram models
biModelDF <- read.csv(biGramModelFile, stringsAsFactors=FALSE)
consoleOut("Total number of biGrams in Model: ", nrow(biModelDF))
triModelDF <- read.csv(triGramModelFile, stringsAsFactors=FALSE)
consoleOut("Total number of triGrams in Model: ", nrow(triModelDF))

bigramSet <- paste(biModelDF$word1, biModelDF$word2)
# Create hash tables for each
tokenHash <- hash(tokenSet, 1:length(tokenSet))
bigramHash <- hash(bigramSet,1:length(bigramSet))
rm(tokenSet,bigramSet)
gc()

repeat {
	sentence <- readline(prompt= paste("Enter start of Phrase: ")) # prompt
	# ToDo: Clean the sentence
	prediction <- predictor(sentence, tokenHash, biModelDF, triModelDF) 
	consoleOut( sentence, "->", prediction)
}