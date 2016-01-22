library(shiny)
library(gdata)
library(utils)
library(dplyr)
library(reshape2)
library(grid)
library(gridExtra)
library(ggplot2)

# ---- Helper Functions

# Function to convert an annual return expressed in % to a multiplicative number
# Eg 20% -> 1.2
Ret2Nb <- function (x) {1 + 0.01 * x}

# Compute CAGR on a vector of returns
# The returns are % numbers
# the CAGR is also computed as a % number
# E.g. c(10,20) -> 14.89  i.e 10% & 20% -> CAGR: 14.89%
cagr <- function(vct) {
  x <- Ret2Nb(vct)
  y <- apply(data.frame(x), 2, FUN=prod)
  z <- 100.0 * (y ** (1/length(vct)) - 1.0)
  round(z, 2)
}


# compute basic stats for an index or portfolio
compute_stats <- function(vct) {
  c( round(mean(vct),2), 
     round(sd(vct),2),
     round(min(vct),2),
     round(max(vct),2),
     cagr(vct) )
}

# Function to remove the % character from the 3 columns
rmpct <- function(x) { gsub('%','',x)}

# ----

# Retrieve the Excel file and load it into a data frame
if (! file.exists('histretSP.xls')) {
  download.file('http://www.stern.nyu.edu/~adamodar/pc/datasets/histretSP.xls', 'histretSP.xls', method='wget')
} else {
  message("File already exists - skipping the download")
}
df <- read.xls('histretSP.xls', sheet=1, stringsAsFactors = FALSE)
# Skip the header rows, and only keep the first 4 columns: Year and 3 main indices
df <- df[16:nrow(df), 1:4]
# Remove the unused bottom rows
df <- df[1:87,]
colnames(df) <- c('Year','S.P.500', 'X3.month.T.Bills', 'X10.year.Bonds')

xx <- lapply(df[,2:4],rmpct)
yy <- lapply(xx, as.numeric)
zz <- data.frame(df$Year)
colnames(zz) <-'Year'
zz <- mutate(zz, Year=as.integer(as.character(zz$Year)))
dfx <- cbind(zz,yy)
# Rename columns
colnames(dfx) <- c('Year','S.P.500', 'X3.month.T.Bills', 'X10.year.Bonds')
# max_scale ensures that all plots are on the same scale
max_scale <- 10* (1+round(0.1*max(abs(dfx$S.P.500),0)))

# Default: 50% stocks & 50% bonds
default_stocks <- 50 # percent
default_bonds <- 50 # percent


shinyServer(function(input, output) {
  # Reactive expression to compose a data frame containing all of the values
  ### IMPORTANT
  # Any computational block that includes input values must call SliderValues
  # so that shiny knows to re-run the compuations
  ###
  sliderValues <- reactive({


    stocks <- input$stocks
    bonds <- input$bonds
    # Note the {} to execute R code
    if (stocks+bonds > 100.0) {  # Must stay under 100%
      output$msg <- renderText("Combined allocation of stocks and bonds must be less than 100 percent")
      stocks <- default_stocks
      bonds <- default_bonds
      # Reference: http://shiny.rstudio.com/reference/shiny/latest/validate.html
      # validate(
      #   need(stocks+bonds <= 100.0, "Combined allocation of stocks and bonds must be less than 100%")
      # )
    } else { # All good
      output$msg <- renderText('New Portfolio')
    }
    bills <- 100.0 - (stocks + bonds)

    # Compose data frame
    dd1 <- data.frame(
      'Investment Type' = c("Stocks %", 
               "Bonds %",
               "Bills %"),
      Percentage = as.character(c(stocks, 
                             bonds,
                             bills)), 
      stringsAsFactors=FALSE)

    start_date <- input$dates[1]
    end_date <- input$dates[2]
    output$dates <- renderText({paste0("Investment Period - Start Year: ",start_date, " - End Year: ", end_date)})
    dates <- c(start_date, end_date)
    n <- 1 + start_date - dfx[1,'Year']
    m <-  nrow(dfx) - (dfx[nrow(dfx), 'Year'] - end_date)

    dd2 <- dfx[n:m,]
    dd2 <- mutate(dd2, Portfolio = 0.01 * (stocks*dd2$S.P.500+bills*dd2$X3.month.T.Bills+bonds*dd2$X10.year.Bonds))
    message(paste0(stocks, '   ', bonds, '    ', bills))
    message(paste0("dd2: ", dd2[1,'Portfolio']))

    list(dd1, dd2, dates)
  })

  # Show the values using an HTML table
  output$values <- renderTable({
    data.frame(sliderValues()[1])
  })


  
  # Plot Portfolio
  output$Portfolio <- renderPlot({
    message('renderPlot')
    df2 <- data.frame(sliderValues()[2])
    message(df2[1,'Portfolio'])

    # Compute & plot portfolio
    # df2 <- mutate(dfx, Portfolio = stocks*dfx$S.P.500+bills*dfx$X3.month.T.Bills+bonds*dfx$X10.year.Bonds)
    colnames(df2) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds', 'Portfolio')
    g <- ggplot(df2, aes(x=Year, group=1))
    g <- g + geom_line(aes(y=Portfolio),size=1.5,colour="#0072B2", alpha = 1.0)
    g <- g + geom_point(aes(y=Portfolio))
    # g <- g + scale_color_continuous()
    g <- g + ylim(-max_scale, max_scale) + ggtitle("Returns for Blended Portfolio")
    g
  })    

    # Compute Portofolio Metrics
  output$metrics <- renderTable({
    df2 <- data.frame(sliderValues()[2])

    # # Compute CAGR
    # # Convert the % return values to multiplicative numbers
    # df3 <- apply(df2[,2:5],2, Ret2Nb)
    # # Compute the cumulative returns 
    # d4 <- data.frame( apply(df3, 2, prod) )
    # # compute GAGR from (a) cumulative return and (b) number of years
    # nb_yr <- nrow(df2)
    # cagr <- function(x) {100.0 * (x ** (1/nb_year)-1)}
    # d5 <- data.frame ( apply(d4, 2, cagr))

    data.frame(
      'Portofolio Metrics' = c("Average Return (%)", 
               "Standard Deviation (%)",
               "Lowest Yearly Return (%)",
               "Highest Yeaarly Return (%)",
               "CAGR (%)"),
      'S & P 500' = as.character(compute_stats(df2[,2])), 
      '3-month T-Bills' = as.character(compute_stats(df2[,3])), 
      '10-year Bonds' = as.character(compute_stats(df2[,4])), 
      'New Portfolio' = as.character(compute_stats(df2[,5])), 
      stringsAsFactors=FALSE)  
      # data.frame(
      # 'Portofolio Metrics' = c("Average Return", 
      #          "Standard Deviation",
      #          "Lowest Yearly Return",
      #          "Highest Yeaarly Return"),
      # Percentage = as.character(compute_stats(df2$Portfolio)),
      # stringsAsFactors=FALSE)  
    })

  output$Indices <- renderPlot({
    ddd <- unlist(sliderValues()[3])  # somehow need to call unlist()
    start_date <- ddd[1]
    end_date <- ddd[2]
    n <- 1 + start_date - dfx[1,'Year']
    m <-  nrow(dfx) - (dfx[nrow(dfx), 'Year'] - end_date)
    message(paste0('Indices dates:', start_date, '  ', end_date))
    # Plot historical data
    df1 <- dfx[n:m,]
    colnames(df1) <- c('Year','S & P 500', '3-month T-Bills', '10-year Bonds')
    meltdf <- melt(df1, id='Year')
    colnames(meltdf) <- c('Year', "Investment_Class", "Returns")
    q <- ggplot(data=meltdf, aes(x=Year, y=Returns, group=Investment_Class, colour=Investment_Class, height=0.1))
    q <- q + geom_line(aes())+geom_point()
    q <- q + ylim(-max_scale, max_scale)
    q <- q + theme(legend.position = "bottom")
    q <- q  + ggtitle("Returns for 3 Main Investment Classes")
    q
  })

})

