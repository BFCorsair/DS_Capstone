library(stringi)


inFile = './bf_dict.tmp'
outFile = '../bf_dict_clean.txt' # Write in main directory

words <- readLines(inFile, encoding="UTF-8", skipNul=TRUE)
# Have to remove non-ASCII characters like accents
words <- stri_replace_all_regex(words,"[^[:alpha:] ]","")
words <- tolower(words)
words <- sort(unique(words))
write(words,outFile, sep='\n')

