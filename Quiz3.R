
library(RWeka)
library(stringi)  # faster string substitution
library(hash)
# ---
source("BF_util.R")  # my personal utilities 


# Source can be Blog, News or Twitter
source <- "Twitter"

if (source == "Blog") {
	dataDir = '../Data_2016_12_29/Blog/'  # note the '/' at the end
} else if (source == "News") {
	dataDir = '../Data_2016_12_29/News/'
} else if (source == "Twitter") {
	dataDir = '../Data_2016_12_29/Twitter/'
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

# ---
cleanSentence <- function(inString) {
	# Get rid of all non-printable characters - returns a string
	stri_replace_all_regex(inString,"[^[:alpha:] ]"," ")
}

# ---
# Note that at this point, the model may have multiple rows in case some {word1, word2} pairs are equally 
# Likely for the same word1
Answer <- function(gDF, biDF, triDF, sentence, options) {
	sentenceTokens <- unlist(strsplit(cleanSentence(sentence), " "))
	prediction <- ""
	nbWords <- length(sentenceTokens)
	# try Trigrams

	maxCount <- 0
	winner <- ""
	for (word in options) {
		trigram <- paste(sentenceTokens[nbWords-1],sentenceTokens[nbWords], word)
		consoleOut("Trying trigram:", trigram)
		res <- triDF[triDF$Trigram == trigram, ]
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
			res <- biDF[biDF$Bigram == bigram, ]
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
			res <- gDF[gDF$Gram == word, ]
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

sentence1 = "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
options1 = c("give", "sleep", "eat", "die")
sentence2 = "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
options2 = c( "financial", "horticultural", "marital", "spiritual")
sentence3 = "I'd give anything to see arctic monkeys this"
options3 = c( "weekend", "morning", "month", "decade")
sentence4 = "Talking to your mom has the same effect as a hug and helps reduce your"
options4 = c( "sleepiness", "stress", "happiness", "hunger")
sentence5 = "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
options5 = c( "minute", "picture", "walk", "look")
sentence6 = "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
options6 = c( "account", "incident", "case", "matter")
sentence7 = "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
options7 = c( "arm", "toe", "hand", "finger")
sentence8 = "Every inch of you is perfect from the bottom to the"
options8 = c( "middle", "center", "top", "side")
sentence9 = "I’m thankful my childhood was filled with imagination and bruises from playing"
options9 = c("outside", "weekly", "daily", "inside")
sentence10 = "I like how the same people are in almost all of Adam Sandler's"
options10 = c("pictures", "movies", "stories", "novels")



# ---- Main ----
consoleOut("Starting at: ", Sys.time())
consoleOut("Source:", source)

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Read the gram, biGram and triGram counts
gramDF  <- read.csv(gramFile, stringsAsFactors=FALSE)
consoleOut("Total number of Grams: ", nrow(gramDF))
biGramDF  <- read.csv(biGramFile, stringsAsFactors=FALSE)
consoleOut("Total number of BiGrams: ", nrow(biGramDF))
triGramDF  <- read.csv(triGramFile, stringsAsFactors=FALSE)
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



