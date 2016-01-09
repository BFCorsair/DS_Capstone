# Code no longer used

# ---
cleanSentence <- function(inString) {
	# Get rid of all non-printable characters
	# charbuf <- gsub("[^[:alpha:] ]"," ",inString)
	# faster
	charbuf <- stri_replace_all_regex(inString,"[^[:alpha:] ]"," ")
	# Convert to lowercase
	charbuf <-tolower(charbuf)

	# Tokensize and return
	WordTokenizer(charbuf)
}

--- 

## Creates a set of words by reading a text file containing these words
readWordSet <- function(fileName, nbLines) {
	wordSet = c()  # global set
	con <- file(fileName, open="rt")
	repeat {
		charvct <- readLines(con=con, n=nbLines, skipNul=TRUE) # Vector of length n
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
# Create a vector of biGrams "word1 word2" from a vector of words (in sequence)
countBiGram <- function(lines, countHash, keepersHashTbl) {
# Create 2-word strings from consecutive words in tokenSet
# i.e paste tokenSet with a shifted-by-1 version of itself

	# Eliminate blanks at start and end of sentences to avoid empty strings after strsplit
	# More efficient than post-processing code commented out below
	lines <- gsub("^ +| +$", "", lines)
	foreach(i=1:length(lines)) %dopar% {
		words <- unlist(strsplit(lines[i], " +")) # Account for multiple blanks
		# Eliminate empty strings
		# words <- words[unlist(lapply(words, function(w) {nchar(w) >0}))]
		# only keep the words that are in the reference - i.e. whose hash is not null
		words <- words[sapply(words, function(w){! is.null(keepersHashTbl[[w]])})]
		# Create a set of 1st words and second words - then paste them to create a bigram
		if (length(words) < 2 ) {
			outVct <- c() # Sentence has shrunk to 0 or 1 keeper words
		} else {
			wordOne <- words[1:(length(words)-1)]
			wordTwo <- words[2:length(words)]
			outVct <- paste(wordOne, wordTwo, sep=" ") # create 1 string with the 2 words
			for (token in outVct) {
				countHash[keepersHashTbl[[token]]] <- countHash[keepersHashTbl[[token]]] + 1
			}
		}
	}
	countHash  # return the counts
}

# ---

# Assume tokenSet is large, and thus contains repeats
# Use Run Length Encoding to count the repeats inside tokenSet
countWithNewHash <- function(tokenSet, hashList) {
	# Diassemble the list
	gramVct <- hashList[[1]]
	gramCnt <- hashList[[2]]
	hashTbl <- hashList[[3]]

	# Sort orders all the tokens, and thus the repeats are one after the other
	# RLE then counts them
	tokenRLE <- rle(sort(tokenSet))
	# Identify the tokens in this batch which are not in the hash table
	theseToken <- tokenRLE$values  # unique by construction
	# newTokens are the members of theseToken whose hash is null
	newToken <- theseToken[ sapply(theseToken, function(w) {is.null(hashTbl[[w]])}) ]
	nbHash <- length(hashTbl)
	nbNew <- length(newToken)
	if (nbNew >0 ) { # we have new tokens
		# add the new tokens to the hash table
		hashTbl[newToken] <- (nbHash+1):(nbHash+nbNew)
		# and to the list of grams
		gramVct <- c(gramVct, newToken)
		# Initialize their count to 0
		gramCnt <- c(gramCnt, seq(0,0, length.out=nbNew))
	}
	rm(theseToken,newToken)
	gc()
	# tokenRLE has 2 columns: values (i.e. the words) and lengths (i.e. counts)
	for (i in 1:length(tokenRLE$values)) {  # weird way to get the size of RLE
		token <- tokenRLE$values[i]
		count <- tokenRLE$lengths[i]
		# For each token, determine if it is already in the table and 
		# increment the gramCount for its hash value
		gramCnt[hashTbl[[token]]] <- gramCnt[hashTbl[[token]]] + count
	}
	list(gramVct, gramCnt, hashTbl)   # return the reconstituted list
}
