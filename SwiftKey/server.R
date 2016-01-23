library(shiny)
library(utils)
library(dplyr)
library(hash)

dataURI = 'https://github.com/BFCorsair/DS_Capstone/raw/master/SwiftKey/'
biGramModelFile = 'biGramModel.csv'
triGramModelFile = 'triGramModel.csv'
gramFile = 'keepTokenSet.txt'

DEBUG = FALSE

# biGramFile = 'keepBiGrams.txt'
# ---- Helper Functions

# ---
# Prints a collection of variables on a single line
consoleOut <- function(...) { print(paste(..., sep=" "))}

# ---
# Note that at this point, the model may have multiple rows in case some {word1, word2} pairs are equally 
# Likely for the same word1
predictGram <- function(model, gram) {
  result <- filter(model, word1 == gram)
  if (nrow(result) >= 1) { 
    predict <- result[1,2] # i.e the 2nd word
    consoleOut("predictGram:", gram, predict)
  } else if (nrow(result) == 0) {
    predict <- ""
  } else {
    consoleOut("PredictGram has more than 1 result for:", gram)
    print(result)
    stop(1)
  }
  predict # return the prediction
}

cleanLine <- function(line, tokenHash) {
# Clean input line by only keeping the tokens that we use in our predictor

  # Eliminate blanks at start and end of sentences to avoid empty strings after strsplit
  # More efficient than post-processing code commented out below
  line <- gsub("^ +| +$", "", line)
  words <- unlist(strsplit(line, " +")) # Account for multiple blanks
  # Return a vector of words, where all the tokens are keepers
  words[unlist(lapply(words, function(w){has.key(w,tokenHash)}))]
}

# Place holder
sillyPredictor <- function(arg) { "Fantastic!"}

# ---
predictor <- function (sentence, tokenHash, biModelDF, triModelDF) {
  # ToDo: Clean the sentence
  words <- cleanLine(sentence,tokenHash)
  consoleOut("Input:", sentence)
  consoleOut("Cleaned Input:", paste(words, sep= " ", collapse = " "))


  prediction <- ""
  nbWords <- length(words)
  if (nbWords == 0) break

  if (nbWords >= 2) {
    # Use the last 2 words to predict
    biGram <- paste(words[nbWords-1],words[nbWords])
    consoleOut("Using triGrams with: ", biGram)
    prediction <- predictGram(triModelDF, biGram)
    if (DEBUG & prediction != "") consoleOut("Predicted with 3-gram")
  }

  # If we don't have enough words or triGram prediction failed
  # try biGram prediction
  if (prediction == "" | nbWords == 1) {
    consoleOut("Using biGrams with: ", words[nbWords])
    prediction <- predictGram(biModelDF, words[nbWords])
    if (DEBUG & prediction != "") consoleOut("Predicted with 2-gram")
  } 
  # If previous predictions failed
  if (prediction == "") {
    prediction <- "the"
    if (DEBUG & prediction != "") consoleOut("Predicted with 1-gram")
  }
  prediction  # return
}

# ----

# Retrieve the files from Github
# for (file in c(biGramModelFile,triGramModelFile,gramFile , biGramFile)) {
for (file in c(biGramModelFile,triGramModelFile,gramFile )) {
  if (DEBUG) { # skip file download if possible
        if (! file.exists(file)) {
          URL <- paste0(dataURI, file)
          download.file(URL, file, method='wget',extra='--no-check-certificate')
        } else {
            message(paste(file, " already exists - skipping the download"))
        }
  } else { # always download the file
    URL <- paste0(dataURI, file)
    download.file(URL, file, method='wget',extra='--no-check-certificate')
  }
}
tokenSet <- readLines(gramFile)
consoleOut("Total number of tokens in Model: ", length(tokenSet))
# Read the biGram and triGram models
biModelDF <- read.csv(biGramModelFile, stringsAsFactors=FALSE)
consoleOut("Total number of biGrams in Model: ", nrow(biModelDF))
triModelDF <- read.csv(triGramModelFile, stringsAsFactors=FALSE)
consoleOut("Total number of triGrams in Model: ", nrow(triModelDF))

bigramSet <- paste(biModelDF$word1, biModelDF$word2)
# Create hash tables for each
tokenHash <- hash(tokenSet, 1:length(tokenSet))
bigramHash <- hash(bigramSet,1:length(bigramSet))
rm(tokenSet,bigramSet)
gc()

shinyServer(function(input, output) {
  # if (input$goButton >= 1) {
  #   output$sentence <- renderText({input$sentence})
  #   output$predicted <- renderText({ paste(output$sentence, predictor(output$sentence), sep=" ")})
  # }

  output$sentence <- renderText({
    if (input$goButton == 0) " ..."
    else if (input$goButton >= 1) {input$sentence}
  })
  output$predicted <- renderText({
    if (input$goButton == 0) " ..."
    else if (input$goButton >= 1) {
      prediction <- predictor(input$sentence, tokenHash, biModelDF, triModelDF) 
      paste(input$sentence, prediction, sep=" ... ")
    }
  })

} )

