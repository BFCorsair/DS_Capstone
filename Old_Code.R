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