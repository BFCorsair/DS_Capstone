# Merge the token sets from the 3 sources

library(hash)
library(dplyr)

sourceList = c("Blog", "News", "Twitter")
dataDir = '../Data/'
fileName = 'tokenSet.txt'
distriName = 'gramCountDistri.csv'
outFile = './aggregateTokenSet.csv'
pngFile = './aggregateTokenSet.png'
stopFile = '../stopwords.txt'

pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold

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

# ----
sortByCount <- function(df) {
# Sort by count & re-number the rows
	# df <- df[order(-df$count),]  # For some reason, this does not work
	df <- mutate(df, count=as.integer(count))
	df <- df[with(df, order(count, decreasing=TRUE)), ]	
	# renumber the rows 
	rownames(df) <- seq(length=nrow(df)) 
	df
}

# ---
find_50_90 <- function(df) {
# Assume that df has 4 columns: value, count, cumsum (cumul count), pct (cumul %)
# pct are 0-100
	fiftyIndx <- 0
	ninetyIndx <- 0
	for (i in 1:nrow(df)) {
		if(fiftyIndx == 0 & df$pct[i] >= 50) fiftyIndx <- i
		else if (ninetyIndx == 0 & df$pct[i] >= 90) ninetyIndx <- i
	}
	# Return results
	data.frame("Token Count" = nrow(df), "Instances Count"=sum(df$count), "Index50"=fiftyIndx, "Index90"=ninetyIndx)
}

# ---- Main ----

consoleOut("Starting at: ", Sys.time())
sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output


firstTime <- TRUE
for (source in sourceList) {
	tokenFile <- paste0(dataDir,source,'/',fileName)

	# Read the tokens identified in previous path
	tokenSet <- readLines(tokenFile)
	consoleOut("Source:", source, "Total number of  tokens: ", length(tokenSet))
	if (firstTime) {
		firstTime <- FALSE
		finalSet <- tokenSet
	} else {
		finalSet <- sort(union(finalSet,tokenSet))
	}

}
consoleOut("Aggregate number of  tokens: ", length(finalSet))



aggSize <- length(finalSet)
for (source in sourceList) {
	tokenFile <- paste0(dataDir,source,'/',fileName)

	# Read the tokens identified in previous path
	tokenSet <- readLines(tokenFile)
	size <- length(tokenSet)
	inter <- length(intersect(finalSet, tokenSet))
	consoleOut("Source:", source, "Number of tokens in aggregate set:", inter, "-", round(100.0 * inter/aggSize, 2), "%")


}


# --- Aggregate the count over the 3 corpora
# gramCountHash is a vector indexed by the hash of each token
# it holds the number of occurences of each token 
gramCountHash <- seq(0,0,len=length(finalSet))
# Hash of each token
hashTable <- hash(finalSet, 1:length(finalSet))


# for each source, for each word, add its count to the aggregate
for (source in sourceList) {
	distriFile <- paste0(dataDir,source,'/', distriName)
	gramDF <- read.csv(distriFile, stringsAsFactors=FALSE)
	gramDF <- gramDF[,c("value", "count")] # just keep the words and the counts
	for (i in 1:nrow(gramDF)) {
		gramCountHash[hashTable[[gramDF[i,"value"]]]] <- gramCountHash[hashTable[[gramDF[i,"value"]]]] + gramDF[i,"count"]
	}
}
# Create a data frame with the counts
df <-data.frame(cbind(finalSet,gramCountHash), stringsAsFactors = FALSE)
colnames(df) <- c("value", "count")
# Sort by descending order
df <- sortByCount(df)
# Get the grand-total
total <- sum(df$count)
# Accumulate the counts & Compute the pct coverage
df <- mutate(df, cumsum = cumsum(count), pct = round(100*cumsum/total,2))
# Save results and print statistics
write.csv(df, file=outFile)
print(find_50_90(df))

# Plot the cumul percentages
png(filename=pngFile)
plot(df$pct)
dev.off()



# Keep only the grams that make up the cumulative 90% - Grams are in column 1
keepSet <- filter(df, pct<=pctThreshold)[,1] 
consoleOut("Keeping:", length(keepSet), "for", pctThreshold,"% threshold")

for (source in sourceList) {
	tokenFile <- paste0(dataDir,source,'/',fileName)

	# Read the tokens identified in previous path
	tokenSet <- readLines(tokenFile)
	size <- length(tokenSet)
	inter <- length(intersect(keepSet, tokenSet))
	consoleOut("Source:", source, "Number of tokens in KEEPER set:", inter, "-", round(100.0 * inter/size, 2), "%")
}


consoleOut("\n--- Removing Stop Words ---\n")
# Remove Stop words
stopWords <- readLines(stopFile)
# Also add single letters to list of stopwords
stopWords <- sort(unique(c(stopWords, letters[1:26])))
# Filter out the stopwords and only keep value and count columns
keepDF <- filter(df, ! value %in% stopWords)[,1:2]
# Sort by descending order
df <- sortByCount(df)
# Get the grand-total
total <- sum(keepDF$count)
# Accumulate the counts & Compute the pct coverage
keepDF <- mutate(keepDF, cumsum = cumsum(count), pct = round(100*cumsum/total,2))
# Save results and print statistics
print(find_50_90(keepDF))

keepSet <- filter(keepDF, pct<=pctThreshold)[,1] 
consoleOut("Keeping:", length(keepSet), "for", pctThreshold,"% threshold")

for (source in sourceList) {
	# read data frame with counts and percent
	distriFile <- paste0(dataDir,source,'/', distriName)
	gramDF <- read.csv(distriFile, stringsAsFactors=FALSE)
	# Filter out the stopwords and only keep value and count columns (2 & 3)
	keepDF <- filter(gramDF, ! value %in% stopWords)[,c("value", "count")]	
	# Sort by descending order
	df <- sortByCount(df)
	# Get the grand-total
	total <- sum(keepDF$count)
	# Accumulate the counts & Compute the pct coverage
	keepDF <- mutate(keepDF, cumsum = cumsum(count), pct = round(100*cumsum/total,2))
	# Keep only the grams that make up the cumulative 90% - Grams are in column 1
	keepGram <- filter(keepDF, pct<=pctThreshold)[,1]  


	size <- length(keepGram)
	inter <- length(intersect(keepSet, keepGram))
	consoleOut("Source:", source, "Number of top 90% tokens in KEEPER set:", inter, "-", round(100.0 * inter/size, 2), "%")
}



consoleOut("Completed at: ", Sys.time())


