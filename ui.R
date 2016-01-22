library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Coursera Capstone Project: SwiftKey"),
  sidebarPanel(
    textInput(inputId="sentence", label = "Enter the sentence to complete:"),
    actionButton("goButton", "Predict next word")
), 
mainPanel(
  h4('Sentence to Complete:', style="color:#0072B2"),
  textOutput('sentence'),
  h4('Predicted Sentence', style="color:#0072B2"), 
  textOutput('predicted')
  )
))

