#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library("dplyr")
library("data.table")
library("ggplot2")
library("TTR")
library("quantmod")
library("Quandl")
library("Rblpapi") 


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  google_stocks <- function(sym, start_date, end_date) {
    require(devtools)
    require(Quandl)
    # create a vector with all lines
    google_out = tryCatch(Quandl(c(
      paste0("WIKI/", sym, ".8"),  #  Adj. Open
      paste0("WIKI/", sym, ".9"),  # Adj. High
      paste0("WIKI/", sym, ".10"), # Adj. Low
      paste0("WIKI/", sym, ".11"), # Adj. Close
      paste0("WIKI/", sym, ".12")), # Adj. Volume
      start_date = start_date,
      type = "zoo"
    ))
    start_date <- as.Date(start_date)
    google_out <- as.data.frame(google_out) %>% 
      tibble::rownames_to_column()
    names(google_out) <- c("Date", "Open", "High", "Low", "Close", "Volume")
    google_out <- filter(google_out, start_date <= as.Date(Date, format = "%Y-%m-%d") & end_date >= as.Date(Date, format = "%Y-%m-%d"))
    return(google_out)
  }
  
  listings <- stockSymbols()
  get.stock.ticker <- function(stock.name) {
    stock.ticker <- listings %>% filter(grepl(stock.name, listings$Name)) %>% select(Symbol)
  }
  
  output$distPlot <- renderPlot({
    ticker <- get.stock.ticker(input$Name)
    date_vector <- input$Date 
    start_date <- date_vector[0]
    end_date <- date_vector[1]
    chosen_stock_info <- google_stocks(ticker, start_date, end_date)
    chosen_stock_info$Date <- as.Date(chosen_stock_info$Date, format = "%Y-%m-%d")
    
    ggplot(chosen_stock_info, aes(Date, Close, group = 1)) +
      geom_point(aes(color = Volume)) +
      geom_line() 
    

    

    
  })
  
})
