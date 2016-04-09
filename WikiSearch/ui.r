### --- Example 8: Search Wikipedia web pagess. --------
# Save these 3 files separately in the same folder (Related to HW#4)
# Example: Shiny app that search Wikipedia web pages
# File: ui.R 
library(shiny)
titles <- c("Web_analytics","Text_mining","Integral", "Calculus", 
            "Lists_of_integrals", "Derivative","Alternating_series",
            "Pablo_Picasso","Vincent_van_Gogh","Lev_Tolstoj","Web_crawler")
# Define UI for application 
shinyUI(fluidPage(
  # Application title (Panel 1)
  titlePanel("Wiki Pages"), 
  # Widget (Panel 2) 
  sidebarLayout(
    sidebarPanel(h3("Search panel"),
                 # Where to search 
                 selectInput("select",
                             label = h5("Choose from the following Wiki Pages on"),
                             choices = titles,
                             selected = titles, multiple = TRUE),
                 # Start Search
                 submitButton("Results")
    ),
    # Display Panel (Panel 3)
    mainPanel(                   
      h1("Results",align = "center"),
      plotOutput("distPlot"),
      h3("Top Five Words"),
      tableOutput("topFive"),
      plotOutput("wordCloud")
    )
  )
))
