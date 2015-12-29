
library(dplyr)

gramFlag = 3  # 1 for sinle grams, 2 for bigrams and 3 for trigrams
# Source can be Blog, News or Twitter
source = "News"

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

inFile1 = paste0(dataDir, 'gramCount.csv')
outFile1 = paste0(dataDir, 'gramCountDistri.csv')
pngFile1 = paste0(dataDir, 'gramCountPct.png')
inFile2 = paste0(dataDir, 'biGramCount.csv')
outFile2 = paste0(dataDir, 'biGramCountDistri.csv')
pngFile2 = paste0(dataDir, 'biGramCountPct.png')
inFile3 = paste0(dataDir, 'triGramCount.csv')
outFile3 = paste0(dataDir, 'triGramCountDistri.csv')
pngFile3 = paste0(dataDir, 'triGramCountPct.png')


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

# ----
sortByCount <- function(df) {
# Sort by count & re-number the rows
	df <- df[order(-df$count),]
	rownames(df) <- seq(length=nrow(df)) 
	df
}


# --- Main
consoleOut("Starting at: ", Sys.time())
consoleOut("Source:", source, " - Flag:", gramFlag)

# Assign the right file names
if (gramFlag == 1) {
	inFile = inFile1
	outFile = outFile1
	pngFile = pngFile1
} else if (gramFlag == 2) {
	inFile = inFile2
	outFile = outFile2
	pngFile = pngFile2
} else if (gramFlag == 3) {
	inFile = inFile3
	outFile = outFile3
	pngFile = pngFile3
} else { stop(paste("Improper gramFlag - needs to be {1,2,3}", gramFlag))}


df <- read.csv(inFile, stringsAsFactors=FALSE)
df <- df[,-1]  # get rid of the first column (index)
# Assume that the first column is the value and second is count
colnames(df) <- c("value", "count")
# Order by Count - decreasing
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

consoleOut("Completed at: ", Sys.time())
