Presentation for Capstone Project
========================================================
author: Bernard Fraenkel
date: 1/22/2016
css: custom.css
Swiftkey Application

<!---
Questions to consider

What are the most interesting ways you could show off your algorithm?
Are there any data visualizations you think might be helpful? (Look at the SwiftKey data dashboard if you have it loaded on your phone.)
How should you document the use of your data product (separately from how you created it) so that others can rapidly deploy your algorithm?

(1) how your model predicts, (2) what results it produces, and (3) how your app works
-->
<!---
Formatting tips
http://rstudio-pubs-static.s3.amazonaws.com/27777_55697c3a476640caa0ad2099fe914ae5.html#/9
-->

Model and Results
========================================================
class: small_type

- Most of the complexity of the model is in analyzing the corpus of text to generate the statiscal model - more specifically in data shaping, i.e. defining what a word is
    - Handling punctuation and abbreviations: e.g. "I'm", "my friend's"
    - Filtering out words not in dictionary and foul language
    - Determining which "stop words" to eliminate
- The preprocessing has the following steps:
    - Clean out each corpus based on the rules defined above
    - Using the cleaned-out corpus, compile probably distribution of single words, word pairs and word triplets
        - At each stage, the output is culled to keep only tokens that appear at least N times in the whole corpus (N = 10, 2, 1 respectively)
    - Based on the probability distribution of word pairs or triplets, determine the most likely next word for each word, or word-pair
    - Save this model for the predictor app
- The Predictor is relatively simple in comparison to the model generation
    - Pre-process the sentence
        - Alphabetic only & lowercase
        - Remove bad words, stop words and words not in dictionary
    - Prediction using Markov model with backoff


Using the app
========================================================

- The app can be found here: https://bfcorsair.shinyapps.io/Shiny/
- User enters a sentence - any number of words - in the left hand pane
- When user clicks the "Predict next word" button, the  sentence - completed with the "next" word  appears in the right hand pane, as predicted by the model
- Once the first prediction has been made, the app continues to predict as the user continues typing in the input box in left-hand pane


Interesting Code
========================================================


```r
summary(cars)
```

```
     speed           dist       
 Min.   : 4.0   Min.   :  2.00  
 1st Qu.:12.0   1st Qu.: 26.00  
 Median :15.0   Median : 36.00  
 Mean   :15.4   Mean   : 42.98  
 3rd Qu.:19.0   3rd Qu.: 56.00  
 Max.   :25.0   Max.   :120.00  
```

Slide With Plot
========================================================

![plot of chunk unnamed-chunk-2](Capstone_Presentation-figure/unnamed-chunk-2-1.png) 
