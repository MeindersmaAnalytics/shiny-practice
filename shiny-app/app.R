### QUESTIONS:
# Departure delay: select origin and destination
# Density plot of departure delay
# Density plot of arrival delay
# Legend: departure, arrival

library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(nycflights13)

# Loading all data
airlines <- nycflights13::airlines
airports <- nycflights13::airports
flights <- nycflights13::flights
planes <- nycflights13::planes
weather <- nycflights13::weather

# transform character to factor variables
# pivot longer to visualize departure and arrival delay
flights <- flights %>% 
    mutate(origin = as.factor(origin),
           dest = as.factor(dest)) %>% 
    pivot_longer(cols = c("dep_delay", "arr_delay"),
                 names_to = "delay_type", 
                 values_to = "delay_time")

ui <- fluidPage(

    # Application title
    titlePanel("New York Flights"),
    
    # select origin airport
    selectInput("origin", "Origin airport:", choices = levels(flights$origin)),

    # select destination airport
    selectInput("dest", "Destination airport:", choices = levels(flights$dest)),
    
    # select date of the flight
    dateRangeInput("date", "Flight date:", 
                   min = min(flights$time_hour), 
                   max = max(flights$time_hour), 
                   start = min(flights$time_hour),
                   end = max(flights$time_hour)),
    
    # visualizing departure and arrival delay
    plotOutput("delay_plot")
)


server <- function(input, output) {
    
    # reactive expression for filtering the data
    selected <- reactive(flights %>% filter(origin == input$origin,
                                            dest == input$dest,
                                            time_hour %within% interval(input$date[1], input$date[2])))
    
    output$delay_plot <- renderPlot(
        selected() %>% 
            ggplot(aes(x = delay_time, group = delay_type, fill = delay_type)) +
            geom_density(adjust=2, alpha=0.4) +
            ylab("Density") +
            labs(title = "Origin to Destination: Departure and Arrival Delay in minutes") +
            scale_x_continuous(name = "Delay (in minutes)", 
                               breaks = seq(-120, 240, 30),
                               limits = c(-120, 240),
                               labels = seq(-120, 240, 30),
                               expand = expansion(mult = c(0, 0)))
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
