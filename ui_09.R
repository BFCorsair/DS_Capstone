library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("Portfolio Returns 1928 - 2014"),
  sidebarPanel(
	helpText("Select Investment Allocations"),
  sliderInput('stocks', 'Stocks',value = 50, min = 0, max = 100, step = 0.5, post = '%'),
	sliderInput('bonds', 'Bonds',value = 50, min = 0, max = 100, step = 0.5, post = '%'),
  hr(style="colour:blue;size=4"),
  helpText("Select the start and end years"),
  sliderInput('dates',"Start and End Dates", min=1928, max = 2014, step = 1, sep = '', value = c(1928, 2014)),
  hr(),
  h1('DOCUMENTATION', style="color:#0072B2"),
  h3('Overview'),
  p('This application computes the historical returns of a portfolio built from a blend of 3 classes of investments'),
  p('The user can select the proportion of Stocks, Bonds and Bills with which to build the portfolio, as well as the start and end dates for which to compute
    the statistics. Once these elements are selected, the application displays the yearly returns of the blended portfolio as well as aggregate statistics
    for the selected time period - as well as those for the 3 investment classes'),
  p('In addition, a plot of the yearly returns of the 3 investment classes is displayed for the selected time period'),
  h3('Usage'),
  h4('User Input'),
  p('The user may select'),
  el <- div(HTML('<li>Stocks: Percentage of stocks to include in the portfolio</li>')),
  cat(as.character(el)),
  el <- div(HTML('<li>Bonds: Percentage of bonds to include in the portfolio</li>')),
  cat(as.character(el)),
  el <- div(HTML('<p><i>The percentage of Bills is computed to be 100% minus the percentage allocated to stocks and bonds.
    If the percentage for stocks and bonds add up to more than 100%, the application resets them to 50% each (and 0% for Bills)</i></p>')),
  cat(as.character(el)),
  el <- div(HTML('<li>Start Year: First year of the period over which to compute the portfolio returns</li>')),
  cat(as.character(el)),
  el <- div(HTML('<li>End Year: Last year of the period over which to compute the portfolio returns</li>')),
  cat(as.character(el)),
  p('Dates can range from 1928 to 2014'),
  h4('Application Output'),
  p('The application generates'),
  el <- div(HTML('<li>A chart of the yearly returns of the blended portfolio for the select years</li>')),
  cat(as.character(el)),
  el <- div(HTML('<li>Statistics for the 3 investment classes and the blended portfolio for the selected period: 
    mean yearly return, standard deviation of yearly returns, return for the best year, return for the worst year, and cumulative aggregate return (GAGR)</li>')),
  cat(as.character(el)),
  el <- div(HTML('<li>A chart of the yearly returns of the 3 investment classes</li>')),
  cat(as.character(el)),
  h3('References'),
  p('The stock market data are retrieved from ', a('Damadoran Online', href='http://www.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls')),
  width = 6
),

  mainPanel(
  	h3(textOutput('msg'), style="color:#0072B2"),
    h4(textOutput('dates')),
    tableOutput("values"),
  	h4("Returns of Blended Portfolio"),
    plotOutput('Portfolio', height="300px"),
    h4("Statistics"),
    tableOutput("metrics"),
    h4("Returns of 3 Main Investment Classes", style="color:#0072B2"),
    plotOutput('Indices', height="300px"),
    width=6
  )
))
