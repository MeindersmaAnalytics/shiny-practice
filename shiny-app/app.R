library(shiny)
library(tidyverse)
library(nycflights13)

OriginAirportChoices <- unique(setNames(flights$origin , flights$title))
DestinationAirportChoices <- unique(setNames(flights$dest , flights$title))

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
            #selectInput("selectedVariable", "For which variable would you like to display summary statistics?" , choices = variables),
            #selectInput("Filter1", "Filtered on?" , choices = variables),
            #sliderInput("Slider1", "Which values" , min = as.integer(output$sometest), max = 1000),
            selectInput("DepartureAirport" , "Airport of departure", choices = OriginAirportChoices),
            selectInput("ArrivalAirport" , "Airport of arrival", choices = DestinationAirportChoices),
            dateRangeInput("Dates", "From when to when?",                  
                           start  = "2013-01-01",
                           end    = "2013-12-31",
                           min    = "2013-01-01",
                           max    = "2013-12-31")
         ), 
         mainPanel (
            plotOutput("Histogram") 
         )
     )
)

server <- function(input, output) {
    
    data <- reactive(merge(x = flights, y = planes, by = "tailnum", all.x = TRUE))
    
    filteredData <- reactive({
        filter(data(), origin == input$DepartureAirport, 
                    dest == input$ArrivalAirport, 
                    time_hour >= input$Dates[1],
                    time_hour <= input$Dates[2])
    })
    
    output$Histogram <- renderPlot({
        hist(filteredData()[["seats"]] , 
             main = paste("Histogram of capacity from ", toString(input$DepartureAirport) , " to ", toString(input$ArrivalAirport)),
             xlab = "Capacity per plane",
             ylab = "Number of planes",
             col="blue",
             )
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
