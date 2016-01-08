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

# Input file is assumed to have cleanup content: only acceptble tokens
# ----------


source("BF_util.R")  # my personal utilities 

# ---- Constants ----


# Source can be Blog, News or Twitter
repeat {
	cat("Enter source - one of: Blog, News, Twitter","\n") # prompt
	source <- scan(what=character(),nlines=1)
	# ToDo: Clean the sentence
	if(length(source) == 0) stop("Aborted")
	if (source %in% c("Blog", "News", "Twitter")) {
		break 
	} else {
		print("Source must be one of: Blog, News, Twitter")
		print("Enter <CR> to abort")
	}

}
consoleOut("Count_grams - source is: ", source)



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


statusFreq = 60 # Frequency, in seconds, of status output
inFile = paste0(dataDir,'tokenizedText.txt')
# NOTE: output file is same, regardless of input => collision potential
tokenFile = paste0(dataDir,'tokenSet.txt')
outFile = paste0(dataDir,'gramCount.csv')
pngFile = paste0(dataDir,'gramCount.png')

# Strings to indicate start or end of sentence
# Use "_" to guarantee that they won't collide with a legit word

DEBUG = FALSE
# number of lines to read per iteration  (1 token per line)
if (DEBUG) {
	bufSize = 500
} else {
	bufSize = 5000
}

# ---- Main ----
consoleOut("Starting at: ", Sys.time())
consoleOut("Source:", source)

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Read the tokens identified in previous path
tokenSet <- readLines(tokenFile)
consoleOut("Total number of  tokens: ", length(tokenSet))

# gramCountHash is a vector indexed by the hash of each token
# it holds the number of occurences of each token 
gramCountHash <- seq(0,0,len=length(tokenSet))
# Hash of each token
hashTable <- hash(tokenSet, 1:length(tokenSet))

con <- file(inFile, open="rt")
totalRead <- 0
repeat {
	lines <- readLines(con=con, n=bufSize) # Vector of length n
	if (length(lines) == 0) break   # EOF

	totalRead <- totalRead + length(lines)
	gramCountHash <- countGramHash(lines, gramCountHash, hashTable)

	# print status once in a while
	if (difftime(Sys.time(), lastStatus, units="secs") > statusFreq) {  # Show sign of life 
		lastStatus <- Sys.time()
		consoleOut("Lines read: ", totalRead)
		print_runtime(sysStart, procStart)
	}
	if (DEBUG) break 	# DEBUG - stop after 1 iteration
}
close(con)
rm(lines, hashTable)
gc()
gramCount<-data.frame(cbind(tokenSet,gramCountHash), stringsAsFactors = FALSE)
colnames(gramCount) <- c("Gram", "Count")
rm(gramCountHash)
gc()

# --- Wrap-up
consoleOut("Lines read: ", totalRead)
# write.csv(gramCount, file=outFile, row.names = FALSE)
print_runtime(sysStart, procStart)

# --- Compute Distribution

gramDistri <- computeDistri(gramCount)

# Save results and print statistics
write.csv(gramDistri, file=outFile, row.names = FALSE)
print(percentiles(gramDistri))

# Plot the cumul percentages
png(filename=pngFile)
title <- paste0("1-gram cumulative distribution\nSource: ", source)
plot(gramDistri$pct,type="l",main=title, ylab="Cumulative Distribution")
dev.off()


consoleOut("Completed at: ", Sys.time())
# ---

# Results for BLOG
#             Entity    Value
# 1      Token Count    60675
# 2  Instances Count 16272697
# 3              50%      923
# 4              55%     1210
# 5              60%     1576
# 6              65%     2062
# 7              70%     2721
# 8              75%     3628
# 9              80%     4936
# 10             85%     6923
# 11             90%    10267
# 12             95%    17057
# 13            100%    60675
