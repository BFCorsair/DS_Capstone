
library(dplyr)

gramFlag = 2  # 1 for sinle grams, 2 for bigrams and 3 for trigrams

inFile1 = './gramCount.csv'
outFile1 = './gramCountDistri.csv'
pngFile1 = './gramCountPct.png'
inFile2 = './biGramCount.csv'
outFile2 = './biGramCountDistri.csv'
pngFile2 = './biGramCountPct.png'
inFile3 = './triGramCount.csv'
outFile3 = './triGramCountDistri.csv'
pngFile3 = './triGramCountPct.png'


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