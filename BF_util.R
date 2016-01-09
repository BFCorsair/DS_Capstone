# Personal utilities
# library(RWeka)
library(stringi)  # faster string substitution
library(hash)
library(dplyr)
library(doParallel)
registerDoParallel()

# ---
# Prints a collection of variables on a single line
consoleOut <- function(...) { print(paste(..., sep=" "))}

# ---
# Returns a string in the format %H:%M:%S hours, minutes, seconds 
# representing the input tSec in seconds
# eg 3661 -> 01:01:01
sec2HMS <- function(tSec) {
	ts <- as.integer(tSec)
	hour <- floor(ts / 3600)
	min <- floor((ts - 3600*hour)/60)
	sec <- ts - 3600*hour - 60*min
	paste0(hour,':',formatC(min,digits=1,flag="0", mode="integer"),':',formatC(sec,digits=1,flag="0", mode="integer"))

}

# ---
# Prints the run times (Sys and Proc) from the times given as inputs
print_runtime <- function(sysStart, procStart) {
	run_time <- difftime(Sys.time(), sysStart, units="secs")
	proc_time <- proc.time() - procStart
	consoleOut("Current Time:", Sys.time(), "- Running time: ", sec2HMS(run_time))
	print("Proc time: ")
	print(proc_time)	
}

# --- 
# Get the number of lines in a file
getLineCount <- function(fileName) {
	wcOut  <- system(paste0("wc -l ",fileName),intern=TRUE)
	# wcOut is a string w/ leading blanks (lc[1]), the line count and file name
	lc <- unlist(strsplit(wcOut," +")) # Multiple spaces
	as.integer(lc[2]) # return the line count
}

# ---
# Predict completion time based on start time, lines processed, and lines to process
predictEndTime <- function(startTime, toProcess, processed) {
	runTime <- difftime(Sys.time(), startTime, units="secs")
	endTime <- runTime * toProcess / processed # linear interpolation
	# Convert startTime to epoch(seconds), add the projected endTime,
	# and convert back to datetime
	as.POSIXct(as.POSIXlt(startTime) + endTime)
}

# ---
rewriteLine <- function(line, keepers) {
	# Take a string as input, tokenizes it into words
	#  eliminates words that are not part of reference
	# words <- WordTokenizer(line)  # split into a vector of words
	words <- unlist(strsplit(line, " "))
	# words <- wordStem(words)  # get the stem
	# only keep the words that are in the reference
	words <- words[unlist(lapply(words, function(w){w %in% keepers}))]
	# re-assemble in a sentence and return
	paste(words, sep = " ", collapse = " ")
}

# ---
rewriteLineHash<- function(line, keepersHashTbl) {
	# Take a string as input, tokenizes it into words
	#  eliminates words that are not part of reference
	# words <- WordTokenizer(line)  # split into a vector of words
	words <- unlist(strsplit(line, " "))
	# Code below would be nice, but does not handle space at start or end
	# words <- unlist(strsplit(line, " +"))
	# Eliminate empty strings
	words <- words[unlist(lapply(words, function(w) {nchar(w) >0}))]
	# words <- wordStem(words)  # get the stem
	# only keep the words that are in the reference - i.e. whose hash is not null
	words <- words[unlist(lapply(words, function(w){! is.null(keepersHashTbl[[w]])}))]
	# re-assemble in a sentence and return
	paste(words, sep = " ", collapse = " ")
}
# ----
sortByCount <- function(df) {
# Sort by count  DECREASING
# Assume that the column name is "count"
	# df <- df[order(-df$count),]  # For some reason, this does not work
	# Make sure the count column is integer
	df <- mutate(df, count=as.integer(count))
	df <- df[with(df, order(count, decreasing=TRUE)), ]	
	# renumber the rows 
	rownames(df) <- seq(length=nrow(df)) 
	df
}

# ---
find_50_90 <- function(df) {
	# Assume that df has a column "pct" that keeps the cumulative percentage
	# Also assume that cumulative percentage is in INCREASING order
	# Asumme column "count" holds the number of occurences for each element
	instanceCount <- sum(df$count)
	# Simplify df - only keep the "pct" column
	# df is now a VECTOR
	df <- df[,"pct"]
	# pct are 0-100
	fiftyIndx <- 0
	ninetyIndx <- 0
	# Only keep the pct column
	for (i in 1:length(df)) {
		if(fiftyIndx == 0 & df[i] >= 50) fiftyIndx <- i
		else if (ninetyIndx == 0 & df[i] >= 90) {
			ninetyIndx <- i
			break # we're done
		}
	}
	# Return results
	data.frame("Token Count" = length(df), "Instances Count"=instanceCount, "Index50"=fiftyIndx, "Index90"=ninetyIndx)
}

percentiles <- function(df) {
	# Assume that df has a column "pct" that keeps the cumulative percentage
	# Also assume that cumulative percentage is in INCREASING order
	# Asumme column "count" holds the number of occurences for each element
	instanceCount <- sum(df$count)
	# Simplify df - only keep the "pct" column
	# df is now a VECTOR
	df <- df[,"pct"]
	# Check every 5% between 50% and 100% - pct are 0-100
	pctValues <- seq (50,100, 5)
	pctIndex <- c()
	# Only keep the pct column
	valIndx <- 1
	for (i in 1:length(df)) {
		if(df[i] >= pctValues[valIndx]) {
			pctIndex[valIndx] <- i
			valIndx <- valIndx + 1
			if (valIndx == length(pctValues)) { # we know the value for 100%
				pctIndex[valIndx] <- length(df)
				break  # done
			}
		}
	}
	# Return results
	results <- data.frame("Entity" = character(), "Value" = integer(), stringsAsFactors = FALSE)
	results[1,] <- c("Token Count",length(df))
	results[2,] <- c("Instances Count",instanceCount)
	pctDF <- data.frame( cbind(paste0(pctValues,"%"), pctIndex))
	colnames(pctDF) <- colnames(results)
	results <- rbind(results, pctDF)
	results
}

# ---
# Assume tokenSet is large, and thus contains repeats
# Use Run Length Encoding to count the repeats inside tokenSet
countGramHash <- function(lines, countHash,hashTbl) {
	# Remove leading and trailing spaces, otherwise strsplit returns empty strings
	lines <- gsub("^ +| +$", "", lines)
	# Break lines into single words
	words <- unlist(strsplit(lines, " "))
	# Sort orders all the tokens, and thus the repeats are one after the other
	# RLE then counts them
	tokenRLE <- rle(sort(words))
	# tokenRLE has 2 columns: values (i.e. the words) and lengths (i.e. counts)

	for (i in 1:length(tokenRLE$values)) {  # weird way to get the size of RLE
		token <- tokenRLE$values[i]
		count <- tokenRLE$lengths[i]
		# For each token, increment the gramCount for its hash value
		if (token != "") { # For safety
			countHash[hashTbl[[token]]] <- countHash[hashTbl[[token]]] + count
		}
	}
	countHash  # return the table with the count, indexed by hash table
}


# ---
computeDistri <- function (df) {
	# The first column is the value and second is count
	colnames(df) <- c("value", "count")
	# Order by Count - decreasing
	df <- sortByCount(df)
	# Get the grand-total
	total <- sum(df$count)
	# Accumulate the counts & Compute the pct coverage and return
	mutate(df, cumsum = cumsum(count), pct = round(100*cumsum/total,2))
	
}

# ---
getSource <- function() {
	repeat{
		source <- readline("Enter source - one of: Blog, News, Twitter: ") # prompt
		# ToDo: Clean the sentence
		if(nchar(source) == 0) stop("Aborted")
		if (source %in% c("Blog", "News", "Twitter")) {
			break 
		} else {
			print("Source must be one of: Blog, News, Twitter")
			print("Enter <CR> to abort")
		}
	}
	source # Success - return the value
}

