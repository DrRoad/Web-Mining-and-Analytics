# Example: Shiny app that search Wikipedia web pages
# File: server.R 
library(shiny)
library(tm)
library(stringi)
library(proxy)
library(wordcloud)
source("WikiSearch.R")

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({ 
    result <- SearchWiki(input$select)
    plot(result, labels = input$select, sub = "",main="Wikipedia Search")
  })
  
  output$wordCloud <- renderPlot({
    cloud <- SearchCloud(input$select)
    cloud
  })
  
  output$topFive <- renderTable({
    freq <- FrequentFive(input$select)
    as.matrix(head(freq,n=5))
  })
})

