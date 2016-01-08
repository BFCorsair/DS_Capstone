print("hello")
res  <- system("wc -l test_wc.R",intern=TRUE) 
lc <- unlist(strsplit(res," +"))
print(lc)
print(as.integer(lc[2]))

res  <- system("wc -l ../Data/Twitter/tokenizedText.txt",intern=TRUE) 
lc <- unlist(strsplit(res," +"))
print(paste0("../Data/Twitter/tokenizedText.txt: ", as.integer(lc[2])))
