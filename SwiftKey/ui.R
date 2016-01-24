library(shiny)

# sidebarLayout(sidebarPanel, mainPanel, position = c("left", "right"), fluid = TRUE)

shinyUI(pageWithSidebar(
  headerPanel("Coursera Capstone Project: SwiftKey"),
  sidebarPanel(
    # textInput(inputId="sentence", label = "Enter the sentence to complete:"),
    # tags$head(tags$style(type="text/css", "#sentence {width: 490px}")),
    h4("Enter the sentence to complete:", style="color:#0072B2"),
    tags$textarea(id="sentence", rows=5, cols=60, label = "Enter the sentence to complete:"),
    el <- div(HTML('<em>Make sure to click the "Predict Next Word" button the first time</em>')),
    cat(as.character(el)),
    actionButton("goButton", "Predict Next Word"),
    width = 6
), 
mainPanel(
    tabsetPanel(
      tabPanel("Results", 
        em("Please be patient, the application takes a few seconds to load"),
        # h5(textOutput('firstload')),
        h4('Sentence to Complete:', style="color:#0072B2"),
        textOutput('sentence'),
        h4('Predicted Sentence', style="color:#0072B2"), 
        tags$b(textOutput('predicted'))       
      ), 
      tabPanel("Description", 
        h4("Summary", style="color:#0072B2"),
        p("This application predicts the next word to appear at the end of the sentence entered in the input pane"),
        h4("Usage", style="color:#0072B2"),
        tags$li("Enter a sentence - any number of words - in the left hand pane"),
        tags$li("Click the 'Predict Next Word' button"),
        tags$li("the input sentence - completed with the 'next' word  appears  in the right hand pane 'Results' tab, as predicted by the model"),
        tags$li("Once the first prediction has been made, the app continues to predict as the user continues typing in the input box in left-hand pane"),
        h4("Usage", style="color:#0072B2"),
        p("A brief technical presentation of the implementation is available "),
        tags$a(href="http://rpubs.com/BFCorsair/146169", "Here")
        )
    ),
  width = 6
  )
))

