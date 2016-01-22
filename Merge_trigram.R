# Merge the token sets from the 3 sources

# ---
source("BF_util.R")  # my personal utilities 

progName =  "Merge_trigram"
source = "All"
sourceList = c("Blog", "News", "Twitter")
dataDir = '../Data/'
distriName = 'triGramCount.csv'
outFile = './aggregateTrigrams.csv'
finalFile = './aggregateTrigrams.txt'
keepFile = './keepTriGrams.txt'
pngFile = './aggregateTrigrams.png'

pctThreshold = 90 # We only keep the tokens whose cumulative frequency is under this threshold
nb2Keep = 1 # Minimum number of occurrences for us to keep

# ---- Main ----
# Redirect to log file
logFileName <- create_logFileName(source,prog=progName) 
sink(logFileName)

consoleOut("Starting at: ", Sys.time())
sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output



# --- Aggregate the count over the 3 corpora
# gramCnt is a vector indexed by the hash of each bigram
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
df <-data.frame(cbind(finalSet,gramCnt), stringsAsFactors = FALSE)
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
print_runtime(sysStart, procStart)



# Keep only the grams that make up the cumulative 90% - Grams are in column 1
keepSet <- filter(df, pct<=pctThreshold)[,1] 
consoleOut("Keeping:", length(keepSet), "for", pctThreshold,"% threshold")
consoleOut("Last Bigram has", df[length(keepSet),"count"], "occurrences")
write(keepSet, file=keepFile, sep='\n')
consoleOut("Completed at: ", Sys.time())
sink()  # Stop the output redirect to log file
#
# --------
# [1] "Source: News - Number of Bigrams:  5,538,154"
# [1] "Source: News - Keeping:  970,258 Bigrams"
# [1] "Current Time: 2016-01-15 16:06:58 - Running time:  0:03:07"
# [1] "Proc time: "
#    user  system elapsed 
# 174.629   6.419 187.825 
# [1] "Source: Twitter - Number of Bigrams:  523,317"
# [1] "Source: Twitter - Keeping:  523,317 Bigrams"
# [1] "Current Time: 2016-01-15 16:07:30 - Running time:  0:03:40"
# [1] "Proc time: "
#    user  system elapsed 
# 206.299   6.823 220.393 
#             entity   value
# 1      Token Count  523317
# 2  Instances Count 4230338
# 3              50%   39340
# 4              55%   64915
# 5              60%   94366
# 6              65%  128287
# 7              70%  166836
# 8              75%  209139
# 9              80%  261010
# 10             85%  313889
# 11             90%  382236
# 12             95%  452741
# 13            100%  523317
# [1] "Current Time: 2016-01-15 16:07:41 - Running time:  0:03:51"
# [1] "Proc time: "
#    user  system elapsed 
# 216.514   6.973 231.260 
# [1] "Keeping: 382376 for 90 % threshold"
# [1] "Last Bigram has 3 occurrences"
# [1] "Completed at:  2016-01-15 16:07:43"