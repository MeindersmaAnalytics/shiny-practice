library(shiny)
library(tidyverse)
library(ggplot2)
library(nycflights13)

# Loading all data
airlines <- nycflights13::airlines
airports <- nycflights13::airports
flights <- nycflights13::flights
planes <- nycflights13::planes
weather <- nycflights13::weather


ui <- fluidPage(

    # Application title
    titlePanel("Visualizing Flights Data: package nycflights13"),

    ### UI LOGIC
)


server <- function(input, output) {
    
    ### SERVER LOGIC
    
}

# Run the application 
shinyApp(ui = ui, server = server)
