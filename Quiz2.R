
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

gramFile = paste0(dataDir, 'gramCount.csv')
biGramFile = paste0(dataDir, 'biGramCount.csv')
triGramFile = paste0(dataDir, 'triGramCount.csv')
# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word
pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
DEBUG = TRUE

sentence1 = "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
options1 = c("cheese", "pretzels", "soda", "beer")
sentence2 = "You're the reason why I smile everyday. Can you follow me please? It would mean the"
options2 = c("universe", "world", "most", "best")
sentence3 = "Hey sunshine, can you follow me and make me the"
options3 = c("saddest", "bluest", "happiest", "smelliest")
sentence4 = "Very early observations on the Bills game: Offense still struggling but the"
options4 = c("referees", "players", "defense", "crowd")
sentence5 = "Go on a romantic date at the"
options5 = c("mall", "beach", "grocery", "movies")
sentence6 = "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
options6 = c("motorcycle", "way", "phone", "horse")
sentence7 = "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
options7 = c("time", "years", "weeks", "thing")
sentence8 = "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
options8 = c("toes", "ears", "eyes", "fingers")
sentence9 = "Be grateful for the good times and keep the faith during the"
options9 = c("bad", "sad", "worse", "hard")
sentence10 = "If this isn't the cutest thing you've ever seen, then you must be"
options10 = c("insane", "callous", "asleep", "insensitive")


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
Answer <- function(gDF, biDF, triDF, sentence, options) {
	sentenceTokens <- unlist(strsplit(sentence, " "))
	prediction <- ""
	nbWords <- length(sentenceTokens)
	# try Trigrams

	maxCount <- 0
	winner <- ""
	for (word in options) {
		trigram <- paste(sentenceTokens[nbWords-1],sentenceTokens[nbWords], word)
		consoleOut("Trying trigram:", trigram)
		res <- filter(triDF, Trigram == trigram)
		if (nrow(res) >0) {
			count <- res[1,"Count"]
			print(res)
			if (count > maxCount) {
				maxCount <- count
				winner <- word
			}
		}
	}
	if (maxCount ==  0) {  # Try biGrams
		consoleOut("Trying biGram") 
		for (word in options) {
			bigram <- paste(sentenceTokens[nbWords], word)
			consoleOut("Trying bigram:", bigram)
			res <- filter(biDF, Bigram == bigram)
			if (nrow(res) >0) {
				count <- res[1,"Count"]
				print(res)
				if (count > maxCount) {
					maxCount <- count
					winner <- word
				}
			}
		}
	}
	if (maxCount == 0) { # Try single words
		consoleOut("Trying Grams") 
		for (word in options) {
			consoleOut("Trying gram:", word)
			res <- filter(gDF, Gram == word)
			if (nrow(res) >0) {
				count <- res[1,"Count"]
				print(res)
				if (count > maxCount) {
					maxCount <- count
					winner <- word
				}
			}
		}
	}
	winner  # return the result
}

# ---- Main ----
consoleOut("Starting at: ", Sys.time())
consoleOut("Source:", source)

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Read the gram, biGram and triGram counts
gramDF  <- read.csv(gramFile, stringsAsFactors=FALSE)
gramDF <- gramDF[,-1] # get rid of the 1st column
consoleOut("Total number of Grams: ", nrow(gramDF))
biGramDF  <- read.csv(biGramFile, stringsAsFactors=FALSE)
biGramDF <- biGramDF[,-1] # get rid of the 1st column
consoleOut("Total number of BiGrams: ", nrow(biGramDF))
triGramDF  <- read.csv(triGramFile, stringsAsFactors=FALSE)
triGramDF <- triGramDF[,-1] # get rid of the 1st column
consoleOut("Total number of Trigrams: ", nrow(triGramDF))

winner1 <- Answer(gramDF, biGramDF, triGramDF, sentence1, options1) 
consoleOut("Answer 1:", winner1)
winner2 <- Answer(gramDF, biGramDF, triGramDF, sentence2, options2) 
consoleOut("Answer 2:", winner2)
winner3 <- Answer(gramDF, biGramDF, triGramDF, sentence3, options3) 
consoleOut("Answer 3:", winner3)
winner4 <- Answer(gramDF, biGramDF, triGramDF, sentence4, options4) 
consoleOut("Answer 4:", winner4)
winner5 <- Answer(gramDF, biGramDF, triGramDF, sentence5, options5) 
consoleOut("Answer 5:", winner5)
winner6 <- Answer(gramDF, biGramDF, triGramDF, sentence6, options6) 
consoleOut("Answer 6:", winner6)
winner7 <- Answer(gramDF, biGramDF, triGramDF, sentence7, options7) 
consoleOut("Answer 7:", winner7)
winner8 <- Answer(gramDF, biGramDF, triGramDF, sentence8, options8) 
consoleOut("Answer 8:", winner8)
winner9 <- Answer(gramDF, biGramDF, triGramDF, sentence9, options9) 
consoleOut("Answer 9:", winner9)
winner10 <- Answer(gramDF, biGramDF, triGramDF, sentence10, options10) 
consoleOut("Completed at: ", Sys.time())

# Summary
consoleOut("Answer 10:", winner10)
consoleOut("Answer 1:", winner1)
consoleOut("Answer 2:", winner2)
consoleOut("Answer 3:", winner3)
consoleOut("Answer 4:", winner4)
consoleOut("Answer 5:", winner5)
consoleOut("Answer 6:", winner6)
consoleOut("Answer 7:", winner7)
consoleOut("Answer 8:", winner8)
consoleOut("Answer 9:", winner9)
consoleOut("Answer 10:", winner10)





	
