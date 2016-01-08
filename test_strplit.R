library(stringr)

inLines <- readLines("../Data/News/news_10000.txt")
reps = 50

# --------
# split1 is 3x slower than 2 or 3
# split2 is slightly faster than split3
# split2
#  user  system elapsed 
# 21.116   0.074  21.261 
# split3
#   user  system elapsed 
# 23.018   0.126  24.146 
# --------

split1 <- function (inL) {
	for (i in 1:reps) {
		for (line in inL) {
			words <- unlist(strsplit(line, " +")) # Account for multiple blanks
			# Eliminate empty strings
			words <- words[unlist(lapply(words, function(w) {nchar(w) >0}))]
		}
	}	
}

split2 <- function (inL) {
	for (i in 1:reps) {
		inL <- gsub("^ +| +$", "", inL)
		for (line in inL) {
			words <- unlist(strsplit(line, " +")) # Account for multiple blanks
		}
	}
}

split2 <- function (inL) {
	for (i in 1:reps) {
		inL <- gsub("^ +| +$", "", inL)
		for (line in inL) {
			words <- unlist(str_split(line, " +")) # Account for multiple blanks
		}
	}
}

# res1 <- system.time(split1(inLines))
# print(res1)
res2 <- system.time(split2(inLines))
print(res2)
 

res3 <- system.time(split2(inLines))
print(res3)

