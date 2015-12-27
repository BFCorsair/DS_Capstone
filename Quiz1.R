# Quiz 1

# Question 3
# What is the length of the longest line seen in any of the three en_US data sets?
# Over 11 thousand in the news data set
# Over 40 thousand in the blogs data set
# Over 11 thousand in the blogs data set
# Over 40 thousand in the news data set


# Question 4
# In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
# 4
# 2
# 0.25
# 0.5


# Question 5
# The one tweet in the en_US twitter data set that matches the word "biostats" says what?
# They just enrolled in a biostat program
# It's a tweet about Jeff Leek from one of his students in class
# They haven't studied for their biostats exam
# They need biostats help on their project


# ----------


consoleOut <- function(text, value) { print(paste0(text,value))}

# --- 

## Creates a set of words by reading a text file containing these words
readWordSet <- function(fileName, nbLines) {
	wordSet = c()  # global set
	con <- file(fileName, open="rt")
	repeat {
		charvct <- readLines(con=con, n=nbLines) # Vector of length n
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

# ---- Main ----

bufSize = 5000  # number of lines to read per iteration
statusFreq = 3*60 # Frequency, in seconds, of status output
maxLines = 20000 # Lines to read in corpus to limit execution time
blogFile = './final/en_US/en_US.blogs.txt'
newsFile = './final/en_US/en_US.news.txt'
tweetFile = './final/en_US/en_US.twitter.txt'
fileList = c(blogFile, newsFile, tweetFile)



#
# Question 4 & 5
#
loveCnt <- 0
hateCnt <- 0
con <- file(tweetFile, open="rt")
repeat {
	charvct <- readLines(con=con, n=1) # read each line
	if (length(charvct) == 0) break   # EOF
	charbuf <- paste(charvct, sep = " ", collapse = " ")
	if (length(grep('love', charbuf)) > 0) loveCnt <- loveCnt +1
	if (length(grep('hate', charbuf)) > 0) hateCnt <- hateCnt +1
	# Question 5
	if (length(grep('biostats', charbuf)) > 0) print(charvct)

}
close(con)

consoleOut("love: ", loveCnt)
consoleOut("hate: ", hateCnt)
consoleOut("ratio: ", loveCnt / hateCnt)


# [1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"
# [1] "love: 90956"
# [1] "hate: 22138"
# [1] "ratio: 4.10859156202006"


#
# Question 3
#
for (filename in fileList) {
	con <- file(filename, open="rt")
	max <- 0
	repeat {
		charvct <- readLines(con=con, n=1, skipNul = TRUE) # read each line
		if (length(charvct) == 0) break   # EOF
		if (nchar(charvct[1]) > max) max <- nchar(charvct[1])
	}
	close(con)
	print(paste0(filename, ": ", max))
}
# Results
# [1] "./final/en_US/en_US.blogs.txt: 40833"
# [1] "./final/en_US/en_US.news.txt: 11384"
# [1] "./final/en_US/en_US.twitter.txt: 140"



# ---
