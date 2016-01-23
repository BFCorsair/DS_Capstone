# Merge the token sets from the 3 sources

# ---
source("BF_util.R")  # my personal utilities 

progName =  "Merge_trigram"
src = "All"
sourceList = c("Blog", "News", "Twitter")
dataDir = '../Data/'
distriName = 'triGramCount.csv'
outFile = './aggregateTrigrams.csv'
finalFile = './aggregateTrigrams.txt'
keepFile = './keepTriGrams.txt'
pngFile = './aggregateTrigrams.png'

pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
nb2Keep = 1 # Minimum number of occurrences for us to keep
keepPctFlag = FALSE  # TRUE to use pctThreshold to truncate, FALSE to use nb2Keep

# ---- Main ----
# Redirect to log file
logFileName <- create_logFileName(src,prog=progName) 
sink(logFileName)

consoleOut("Starting at: ", Sys.time())
sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output



# --- Aggregate the count over the 3 corpora
# gramCnt is a vector indexed by the hash of each trigram
# it holds the number of occurences of each token 



# for each source, for each word, add its count to the aggregate
firstTime <- TRUE
for (source in sourceList) {
	distriFile <- paste0(dataDir,source,'/', distriName)
	gramDF <- read.csv(distriFile, stringsAsFactors=FALSE)
	consoleOut("Source:", source ,"- Number of Bigrams: ", prettyNum(nrow(gramDF),big.mark = ","))
	gramDF <- gramDF[,c("value", "count")] # just keep the words and the counts
	# trim to those which have at least nb2Keep occurrences
	gramDF <- gramDF[gramDF$count >=nb2Keep,]
	consoleOut("Source:", source ,"- Keeping: ", prettyNum(nrow(gramDF),big.mark = ","), "Bigrams")
	if (firstTime) {
		# Create set of unique values, Not sorted
		finalSet <- gramDF$value
		# Populate the count vector (this is why we can't sort the set) 
		gramCnt <- gramDF$count
		# Hash of each token
		hashTbl <- hash(finalSet, 1:length(finalSet))
	} else {
		grVct <- gramDF$value
		newToken <- setdiff(grVct,finalSet)  # elements of grVct that are not in finalSet
		nbNew <- length(newToken)
		consoleOut("Source:", source ,"Adding: ", prettyNum(nrow(gramDF),big.mark = ","), "Bigrams to finalSet")
		# Add the new tokens to the hash table and the vector of counts
		if (nbNew >0 ) { # we have new tokens
			# add the new tokens to the hash table
			nbHash <- length(hashTbl)
			hashTbl[newToken] <- (nbHash+1):(nbHash+nbNew)
			# and to the list of grams
			finalSet <- c(finalSet, newToken)
			# Initialize their count to 0
			gramCnt <- c(gramCnt, seq(0,0, length.out=nbNew))
		}
		# Update the count values		
		foreach (i = 1:nrow(gramDF)) %dopar% {
			idx <- hashTbl[[gramDF[i,"value"]]]
			gramCnt[idx] <- gramCnt[idx] + gramDF[i,"count"]
		}
	}
	print_runtime(sysStart, procStart)

}
# Create a data frame with the counts
triGramCount <-data.frame(cbind(finalSet,gramCnt), stringsAsFactors = FALSE)
colnames(triGramCount) <- c("trigram", "count")
df <- computeDistri(triGramCount)
# Save results and print statistics
write.csv(df, file=outFile, row.names = FALSE)
print(percentiles(df))

# Plot the cumul percentages
png(filename=pngFile)
plot(df$pct)
dev.off()
print_runtime(sysStart, procStart)


# Truncate to only keep meaningful ones
if (keepPctFlag) { # we trancate based on # of occurences
	# Keep only the grams that make up the cumulative 90% - Grams are in column 1
	keepSet <- df[df$pct <=pctThreshold,'value'] # Vector
	consoleOut("Keeping:", length(keepSet), "for", pctThreshold,"% threshold")
} else { # truncate on # occurrences
	keepSet <- df[df$count >=nb2Keep,'value'] # Vector
	consoleOut("Keeping:", length(keepSet), "for", nb2Keep,"Minimum occurrences")
}
consoleOut("Last Trigram has", df[length(keepSet),"count"], "occurrences")
consoleOut("Last Trigram represents", df[length(keepSet),"pct"], "% cumulative")
write(keepSet, file=keepFile, sep='\n')
consoleOut("Completed at: ", Sys.time())
sink()  # Stop the output redirect to log file
