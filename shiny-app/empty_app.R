# Load necessary libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(maps)
library(fmsb)
library(viridis)
library(DT)
library(scales)
library(plotly)
library(htmlwidgets)

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .box-header {
        background-color: #4682B4;
        color: white;
      }
      .box {
        border-radius: 0;
        box-shadow: none;
        margin-bottom: 15px;
      }
      .sidebar {
        background-color: #2A3F54;
        color: white;
        padding: 20px;
        height: 100vh;
      }
      .main-header {
        background-color: #4682B4;
        color: white;
      }
    "))
  ),
  
  fluidRow(
    column(12,
           div(style = "background-color: #4682B4; color: white; padding: 10px; margin-bottom: 15px;",
               h2("Coffee Data Dashboard", style = "margin: 0;")
           )
    )
  ),
  
  fluidRow(
    column(3,
           div(class = "sidebar",
               h3("Species:"),
               selectInput("species", NULL, choices = c("All"), selected = "All"),
               
               h3("Quality Category:"),
               checkboxInput("excellent", "Excellent", value = TRUE),
               checkboxInput("veryGood", "Very Good", value = TRUE),
               checkboxInput("good", "Good", value = TRUE),
               checkboxInput("fair", "Fair", value = TRUE),
               
               h3("Country:"),
               selectizeInput("country", NULL, choices = c("All"), selected = "All", multiple = TRUE)
           )
    ),
    
    column(9,
           fluidRow(
             column(6,
                    div(class = "box",
                        div(h3("Number of Bean Variety")),
                        plotOutput("beanVarietyPlot", height = "350px")
                    )
             ),
             column(6,
                    div(class = "box",
                        div(h3("Average Flavour Profile")),
                        plotOutput("flavourProfile", height = "350px")
                    )
             )
           ),
           fluidRow(
             column(6,
                    div(class = "box",
                        div(h3("Coffee Ratings by Processing Method")),
                        plotOutput("flavourComparisonPlot", height = "350px")
                    )
             ),
             column(6,
                    div(class = "box",
                        div(h3("Average Coffee Ratings by Country")),
                        plotOutput("coffeeDetails", height = "350px")
                    )
             )
           )
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Load and clean coffee dataset
  coffee_ratings <- reactiveVal()
  
  observe({
    # Load data from external source
    data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
    
    # Preprocess data
    clean_data <- data %>%
      filter(!is.na(total_cup_points), !is.na(country_of_origin)) %>%
      mutate(
        quality_category = case_when(
          total_cup_points >= 90 ~ "Outstanding",
          total_cup_points >= 85 ~ "Excellent",
          total_cup_points >= 80 ~ "Very Good",
          total_cup_points >= 75 ~ "Good",
          TRUE ~ "Fair"
        ),
        processing_method = ifelse(is.na(processing_method), "Unknown", processing_method),
        processing_method = fct_lump(processing_method, n = 5)
      )
    
    # Store cleaned data in reactive value
    coffee_ratings(clean_data)
    
    # Update UI elements dynamically
    updateSelectInput(session, "species",
                      choices = c("All", sort(unique(clean_data$species))))
    
    updateSelectizeInput(session, "country",
                         choices = c("All", sort(unique(clean_data$country_of_origin))))
  })
  
  # Create a reactive dataset that filters based on user inputs
  filtered_data <- reactive({
    data <- coffee_ratings()
    
    if (is.null(data)) return(NULL)
    
    # Apply species filter
    if (input$species != "All") {
      data <- data %>% filter(species == input$species)
    }
    
    # Apply country filter
    if (!is.null(input$country) && !"All" %in% input$country) {
      data <- data %>% filter(country_of_origin %in% input$country)
    }
    
    # Apply quality category filter
    quality_filters <- c()
    if (input$excellent) quality_filters <- c(quality_filters, "Excellent")
    if (input$veryGood) quality_filters <- c(quality_filters, "Very Good")
    if (input$good) quality_filters <- c(quality_filters, "Good")
    if (input$fair) quality_filters <- c(quality_filters, "Fair")
    
    if (length(quality_filters) > 0) {
      data <- data %>% filter(quality_category %in% quality_filters)
    }
    
    return(data)
  })
  
  # Placeholder: Bean variety plot
  output$beanVarietyPlot <- renderPlot({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Students should add ggplot code here
    
    
  })
  
  # Placeholder: Radar chart (flavour profile)
  output$flavourProfile <- renderPlot({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Students should add ggplot code here
    
  })
  
  # Placeholder: Processing method comparison plot
  output$flavourComparisonPlot <- renderPlot({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Students should add ggplot code here
    
  })
  
  # Placeholder: Coffee ratings by country (map visualization)
  output$coffeeDetails <- renderPlot({
    data <- filtered_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Students should add ggplot code here
    
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)
