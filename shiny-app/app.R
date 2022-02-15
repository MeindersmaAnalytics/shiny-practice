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
                               "Departure", "Arrival"),
           has_delay_time = !is.na(delay_time)) %>% 
    left_join(y = weather %>% select(origin, time_hour, humid, temp, wind_speed), 
              by.x = c('origin', 'time_hour'), by.y = c('origin', 'time_hour'))

# Order level of factors alphabetically
levels(flights$origin) <- rev(unique(rev(flights$origin)))
levels(flights$dest) <- rev(unique(rev(flights$dest)))

# Choices for x & y variable for weather panel
optionVector <- list("Temperature", "Wind Speed", "Humidity")

ui <- fluidPage(

    # Application title
    titlePanel("Flights Analytics"),
    tabsetPanel(
      tabPanel("Delay",
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
      ),
      tabPanel("Weather",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("origin2", "Origin airport:", choices = levels(flights$origin)),
                   selectInput("weatherfeature_x", "Weather feature for x-axis:", choices = optionVector),
                   selectInput("weatherfeature_y", "Weather feature for y-axis:", choices = NULL),
                   actionButton("calculate_button2", "Calculate"),
                 ),
                 mainPanel(
                  plotOutput("scatterplot", brush = "plot_brush"),
                  textOutput("someTest"),
                  tableOutput("selected_points")
                 )
               )       
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
      })
      
      weather_filter <- eventReactive(input$calculate_button2 , ({
        flights %>% filter(origin == input$origin2 , delay_type == 'Departure') %>%
          sample_n(size = 0.01 * nrow(flights)) %>% 
          ggplot(aes(x = .data[[xVariable()]], y = .data[[yVariable()]], group = has_delay_time, color = has_delay_time)) +
          geom_point(size = 0.5) 
      }))
      
      xVariable <- reactive({
        ifelse(input$weatherfeature_x == "Temperature", "temp", 
               ifelse(input$weatherfeature_x == "Wind Speed", "wind_speed" , "humid"))
      })
      
      output$someTest <- renderPrint(
        choices <- ifelse(xVariable() %in% "temp" , list("Wind Speed" , "Humidity") , 
                          ifelse(xVariable() %in% "wind_speed", list("Temperature" , "Humidity") , list("Temperature","Wind Speed")))
      )
    
      observeEvent(xVariable() , {
        if(xVariable() %in% "temp"){choices <- c("Wind Speed" , "Humidity")}
        else if (xVariable() %in% "wind_speed") {choices <- c("Temperature" , "Humidity")}
        else {choices <- c("Temperature","Wind Speed")}
        updateSelectInput(inputId = "weatherfeature_y", choices = choices)
      })
      
      yVariable <- reactive({
        ifelse(input$weatherfeature_y == "Temperature", "temp", 
               ifelse(input$weatherfeature_y == "Wind Speed", "wind_speed" , "humid"))
      })
      
      output$scatterplot <- renderPlot({
        weather_filter() 
      })
      
      brushedpoints <- reactive({
        brushedPoints(flights , xvar = xVariable(), yvar = yVariable() , brush = input$plot_brush)
      })
      
      output$selected_points <- renderTable({
        req(input$plot_brush)
        brushedpoints()
      })
}


# Run the application 
shinyApp(ui = ui, server = server)



#Todo:
#  - Layout style?
#  - FreezeReactive()
#- Dynamic/Conditional UI
#- Nearpoints, double click
#- Only show table after brushing --> req()/validate()
#- H13
