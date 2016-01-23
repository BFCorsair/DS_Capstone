library(shiny)

# sidebarLayout(sidebarPanel, mainPanel, position = c("left", "right"), fluid = TRUE)

shinyUI(pageWithSidebar(
  headerPanel("Coursera Capstone Project: SwiftKey"),
  sidebarPanel(
    # textInput(inputId="sentence", label = "Enter the sentence to complete:"),
    # tags$head(tags$style(type="text/css", "#sentence {width: 490px}")),
    h4("Enter the sentence to complete:"),
    tags$textarea(id="sentence", rows=5, cols=60, label = "Enter the sentence to complete:"),
    p(),
    actionButton("goButton", "Predict next word"),
    width = 6
), 
mainPanel(
  # h4("---"),
  br(),
  h4('Sentence to Complete:', style="color:#0072B2"),
  textOutput('sentence'),
  h4('Predicted Sentence', style="color:#0072B2"), 
  textOutput('predicted'),
  width = 6
  )
))

