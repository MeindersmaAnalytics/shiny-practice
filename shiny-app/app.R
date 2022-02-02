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
                 values_to = "delay_time") %>% 
    mutate(delay_type = ifelse(delay_type %in% "dep_delay",
                               "Departure", "Arrival"))

# Order level of factors alphabetically
levels(flights$origin) <- rev(unique(rev(flights$origin)))
levels(flights$dest) <- rev(unique(rev(flights$dest)))

ui <- fluidPage(

    # Application title
    titlePanel("New York Flights"),
    
    sidebarLayout(
        sidebarPanel(
            # select origin airport
            selectInput("origin", "Origin airport:", choices = levels(flights$origin)),
            
            # select destination airport
            selectInput("dest", "Destination airport:", choices = NULL, multiple = TRUE),
            
            # select delay type: departure or arrival
            selectInput("delay_type", "Delay type:", choices = c("Departure", "Arrival")),
            
            # select date of the flight to filter on
            dateRangeInput("date", "Flight date:", 
                           min = min(flights$time_hour), 
                           max = max(flights$time_hour), 
                           start = min(flights$time_hour),
                           end = max(flights$time_hour)),
            
            # select carriers to filter on
            # selectInput("carrier", "Carrier:", choices = NULL, multiple = TRUE),
            
            # select planes to filter on
            # selectInput("plane", "Plane:", choices = NULL, multiple = TRUE),
            
            actionButton("calculate_button", "Calculate"),
            
            width = 3
        ),
        mainPanel(
            # visualizing departure and arrival delay
            plotOutput("delay_plot"), 
            width = 9
        )
    )
)


server <- function(input, output) {
    
    # reactive expression for filtering based on origin
    select_origin <- reactive({
        flights %>% filter(origin == input$origin) %>% 
            group_by(dest) %>% 
            filter(n() >= 50)
    })
    
    # update the destination choices based on the origin choice
    observeEvent(select_origin(), {
        choices <- sort(levels(droplevels(select_origin()$dest)))
        updateSelectInput(inputId = "dest", choices = choices) 
    })
    
    selected_plot <- eventReactive(input$calculate_button , ({
      # If no selections are made for destination, show this message:
      validate(need(input$dest, 'Please select at least one destination airport!'))
      flights %>% filter(delay_type == input$delay_type,
                         !is.na(delay_type), 
                         dest %in% input$dest,
                         time_hour %within% interval(input$date[1], input$date[2])) %>% 
        ggplot(aes(x = delay_time, group = dest, fill = dest)) +
        geom_density(adjust=1, alpha=0.2) +
        ylab("Density") +
        labs(title = paste0(input$delay_type, " Delay Distribution: Origin to Destination")) +
        scale_x_continuous(name = "Delay (in minutes)", 
                           breaks = seq(-60, 120, 30),
                           limits = c(-60, 120),
                           labels = seq(-60, 120, 30),
                           expand = expansion(mult = c(0, 0)))
    }))
    
    
    # reactive expression for filtering based on origin
      output$delay_plot <- renderPlot({
        selected_plot() 
      }
     )
}

# Run the application 
shinyApp(ui = ui, server = server)
