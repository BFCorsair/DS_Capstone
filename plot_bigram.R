


# ---
source("BF_util.R")  # my personal utilities 
source <- getSource()
consoleOut("Count_bigrams - source is: ", source)

# ---- Constants ----

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
countFile = paste0(dataDir,'biGramCount.csv')
nb2Keep = 2
pngFile = paste0(dataDir, 'biGramDistri_atleast_', nb2Keep, '.png')
consoleOut("Count_bigrams - source is: ", source, "Processing file:", countFile)

# ---- Main ----
consoleOut("Starting at: ", Sys.time())

sysStart <- Sys.time()  # start of execution
procStart <- proc.time()  # start of execution
lastStatus <- Sys.time() # Time of last status output

# Read the tokens identified in previous path
df1 <- read.csv(countFile, stringsAsFactors = FALSE)
consoleOut("# Bigrams read:", prettyNum(nrow(df1),big.mark = ","))
# Keep only about 5,000 grams - they represent roughly 80% of the word occurences
df <- df1[df1$count >= nb2Keep,]
consoleOut("Keeping: ", prettyNum(nrow(df),big.mark = ","), "Tokens - cumulative % is: ", df[nrow(df),"pct"])
consoleOut("Minimum occurences:", nb2Keep)

print(percentiles(df))

title <- paste0("Bi-gram cumulative distribution\nSource: ", source, " - With least ", nb2Keep, " occurences")
plot(df$pct,type="l",main=title, ylab="Cumulative Distribution",yaxp  = c(0,100,20))
grid(lwd=2)
# Plot the cumul percentages
png(filename=pngFile)
plot(df$pct,type="l",main=title, ylab="Cumulative Distribution",yaxp  = c(0,100,20))
grid(lwd=2)
dev.off()

consoleOut("Completed at: ", Sys.time())

# ---
