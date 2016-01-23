# Merge the token sets from the 3 sources

library(hash)
library(dplyr)
# ---
source("BF_util.R")  # my personal utilities 

sourceList = c("Blog", "News", "Twitter")
dataDir = '../Data/'
fileName = 'tokenSet.txt'
distriName = 'gramCount.csv'
outFile = './aggregateTokenSet.csv'
finalFile = './aggregateTokenSet.txt'
keepFile = './keepTokenSet.txt'
pngFile = './aggregateTokenSet.png'

pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
nb2Keep = 10 # Minimum number of occurrences for us to keep


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
	if (firstTime) {  # create initial version of finalSet
		firstTime <- FALSE
		finalSet <- tokenSet
	} else { # merge new tokens into finalSet
		finalSet <- sort(union(finalSet,tokenSet))
	}

}
consoleOut("Aggregate number of  tokens: ", length(finalSet))
write(finalSet, file=finalFile, sep='\n')

# Compute what % of final token set, each source represents
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
write.csv(df, file=outFile, row.names = FALSE)
print(percentiles(df))

# Plot the cumul percentages
png(filename=pngFile)
plot(df$pct)
dev.off()



# Keep only the grams that make up the cumulative 90% - Grams are in column 1
# keepSet <- df[df$pct <=pctThreshold,'value'] # Vector
# consoleOut("Keeping:", length(keepSet), "for", pctThreshold,"% threshold")
# Keep only the grams that have at least nb2Keep occurrences
keepSet <- df[df$count >=nb2Keep,'value'] # Vector
consoleOut("Keeping:", length(keepSet), "for", nb2Keep,"min # occurrences")
consoleOut("Last Token has", df[length(keepSet),"count"], "occurrences")
consoleOut("Last Token represents", df[length(keepSet),"pct"], "% cumulative")
write(keepSet, file=keepFile, sep='\n')


# for each source 
for (source in sourceList) {
	tokenFile <- paste0(dataDir,source,'/',fileName)

	# Read the tokens identified in previous path
	tokenSet <- readLines(tokenFile)
	size <- length(keepSet)
	inter <- length(intersect(keepSet, tokenSet))
	consoleOut("Source:", source, "Number of tokens in KEEPER set:", inter, "-", round(100.0 * inter/size, 2), "%")
}


consoleOut("Completed at: ", Sys.time())

# ------------
# [1] "Source: Blog Total number of  tokens:  60675"
# [1] "Source: News Total number of  tokens:  56710"
# [1] "Source: Twitter Total number of  tokens:  52291"
# [1] "Aggregate number of  tokens:  63974"
# [1] "Source: Blog Number of tokens in aggregate set: 60675 - 94.84 %"
# [1] "Source: News Number of tokens in aggregate set: 56710 - 88.65 %"
# [1] "Source: Twitter Number of tokens in aggregate set: 52291 - 81.74 %"
#             Entity    Value
# 1      Token Count    63974
# 2  Instances Count 46860066
# 3              50%      901
# 4              55%     1178
# 5              60%     1536
# 6              65%     2001
# 7              70%     2625
# 8              75%     3486
# 9              80%     4705
# 10             85%     6541
# 11             90%     9614
# 12             95%    15905
# 13            100%    63974
# [1] "Keeping: 9621 for 90 % threshold"
# [1] "Source: Blog Number of tokens in KEEPER set: 9621 - 100 %"
# [1] "Source: News Number of tokens in KEEPER set: 9618 - 99.97 %"
# [1] "Source: Twitter Number of tokens in KEEPER set: 9620 - 99.99 %"


