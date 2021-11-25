library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(nycflights13)

# Loading all data
airlines <- nycflights13::airlines
airports <- nycflights13::airports
flights <- nycflights13::flights
planes <- nycflights13::planes
weather <- nycflights13::weather

variables <- colnames(flights)

ui <- fluidPage(
    titlePanel("Visualizing Flights Data: package nycflights13"),  
     sidebarLayout(
         sidebarPanel(
            selectInput("selectedVariable", "For which variable would you like to display summary statistics?" , choices = variables),
            selectInput("Filter1", "Filtered on?" , choices = variables),
            #sliderInput("Slider1", "Which values" , min = as.integer(output$sometest), max = 1000),
         ), 
         mainPanel (
            tableOutput("FirstTable"), 
            plotOutput("Histogram") 
         )
     )
)

server <- function(input, output) {
    
    data <- reactive(
        flights
    )
    
    filteredData <- reactive(
        filter(data(), carrier == "UA")
    )
    
    output$sometest <- renderText({
        min(data()[[input$Filter1]] , na.rm = TRUE)
    })
    
    output$FirstTable <- renderTable(
       head(data())
    )
    
    output$Histogram <- renderPlot({
        
        hist(filteredData()[[input$selectedVariable]] , 
             main = paste("Histogram of", toString(input$selectedVariable)),
             xlab = toString(input$selectedVariable),
             col="blue",
             )
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
