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
               h2("Coffee Data Dashboard", style = "margin: 0;"),
               actionButton("toggle", "", icon = icon("bars"),
                            style = "background: none; border: none; color: white; float: left; margin-right: 10px;")
           )
    )
  ),
  
  fluidRow(
    
    column(3,
           div(class = "sidebar",
               h3("Species:"),
               selectInput("species", NULL, choices = c("All"), selected = "All"),
               
               h3("Bean Variety:"),
               selectInput("beanVariety", NULL, choices = c("All"), selected = "All"),
               
               h3("Quality Category:"),
               checkboxInput("excellent", "Excellent", value = TRUE),
               checkboxInput("veryGood", "Very Good", value = TRUE),
               checkboxInput("good", "Good", value = TRUE),
               checkboxInput("fair", "Fair", value = TRUE),
               
               h3("Processing Method:"),
               selectInput("processingMethod", NULL, choices = c("All"), selected = "All"),
               
               h3("Country:"),
               selectInput("country", NULL, choices = c("All"), selected = "All")
           )
    ),
    
    # Main content area
    column(9,
           fluidRow(
             column(6,
                    div(class = "box",
                        div(class = "box-header", h3("Bean Variety")),
                        plotlyOutput("beanVarietyPlot", height = "350px")
                    )
             ),
             column(6,
                    div(class = "box",
                        div(class = "box-header", h3("Avg Flavour Profile")),
                        plotlyOutput("flavourProfile", height = "350px")
                    )
             )
           ),
           fluidRow(
             column(6,
                    div(class = "box",
                        div(class = "box-header", h3("Coffee Ratings by Processing Method")),
                        plotlyOutput("flavourComparisonPlot", height = "350px")
                    )
             ),
             column(6,
                    div(class = "box",
                        div(class = "box-header", h3("Average Coffee Ratings by Country")),
                        plotlyOutput("coffeeDetails", height = "350px")
                    )
             )
           )
    )
  )
)

server <- function(input, output, session) {
  coffee_ratings <- reactiveVal()
  
  observe({
    data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')
    
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
    
    coffee_ratings(clean_data)
    
    updateSelectInput(session, "species",
                      choices = c("All", sort(unique(clean_data$species))))
    
    updateSelectInput(session, "beanVariety",
                      choices = c("All", sort(unique(clean_data$variety))))
    
    updateSelectInput(session, "processingMethod",
                      choices = c("All", sort(unique(clean_data$processing_method))))
    
    updateSelectInput(session, "country",
                      choices = c("All", sort(unique(clean_data$country_of_origin))))
  })
  
  filtered_data <- reactive({
    data <- coffee_ratings()
    
    if (is.null(data)) {
      return(NULL)
    }
    
    if (input$species != "All") {
      data <- data %>% filter(species == input$species)
    }
    
    if (input$country != "All") {
      data <- data %>% filter(country_of_origin == input$country)
    }
    
    quality_filters <- c()
    if (input$excellent) quality_filters <- c(quality_filters, "Excellent")
    if (input$veryGood) quality_filters <- c(quality_filters, "Very Good")
    if (input$good) quality_filters <- c(quality_filters, "Good")
    if (input$fair) quality_filters <- c(quality_filters, "Fair")
    
    if (length(quality_filters) > 0) {
      data <- data %>% filter(quality_category %in% quality_filters)
    }
    
    if (input$beanVariety != "All") {
      data <- data %>% filter(variety == input$beanVariety)
    }
    
    if (input$processingMethod != "All") {
      data <- data %>% filter(processing_method == input$processingMethod)
    }
    
    return(data)
  })
  
  output$flavourProfile <- renderPlotly({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    top_countries <- data %>%
      count(country_of_origin) %>%
      arrange(desc(n)) %>%
      slice_head(n = 5) %>%
      pull(country_of_origin)
    
    all_data_summary <- data %>%
      summarise(across(c(aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness),
                       mean, na.rm = TRUE)) %>%
      mutate(country = "All Selected Data")
    
    top_countries_summary <- data %>%
      filter(country_of_origin %in% top_countries) %>%
      group_by(country_of_origin) %>%
      summarise(across(c(aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness),
                       mean, na.rm = TRUE)) %>%
      rename(country = country_of_origin)
    
    combined_summary <- bind_rows(all_data_summary, top_countries_summary)
    
    categories <- c("aroma", "flavor", "aftertaste", "acidity", "body", "balance", "uniformity", "clean_cup", "sweetness")
    
    plot <- plot_ly()
    

    for (i in 1:nrow(combined_summary)) {
      country_name <- combined_summary$country[i]
      values <- as.numeric(combined_summary[i, categories])
      if (country_name == "All Selected Data") {
        line_width <- 3
        color <- "rgba(70, 130, 180, 0.8)"  # Bold blue
      } else {
        line_width <- 1.5
        color <- viridisLite::viridis(nrow(combined_summary) - 1, alpha = 0.7)[i - 1]
      }
      
      plot <- plot %>% add_trace(
        type = 'scatterpolar',
        r = values,
        theta = toupper(categories),
        name = country_name,
        fill = ifelse(country_name == "All Selected Data", 'toself', 'none'),
        fillcolor = ifelse(country_name == "All Selected Data", "rgba(70, 130, 180, 0.3)", NA),
        line = list(color = color, width = line_width)
      )
    }
    plot %>% layout(
      polar = list(
        radialaxis = list(
          visible = TRUE,
          range = c(0, 10)
        )
      ),
      showlegend = TRUE,
      legend = list(x = 0.8, y = 0.1),
      title = "Average Flavour Profiles by Top Countries"
    )
  })

  output$beanVarietyPlot <- renderPlotly({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    variety_data <- data %>%
      filter(!is.na(variety)) %>%
      count(variety) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10)
    
    p <- ggplot(variety_data, aes(x = n, y = reorder(variety, n))) +
      geom_col(fill = 'skyblue') +
      theme_minimal() +
      labs(x = "Count", y = "") +
      theme(
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank()
      )
    ggplotly(p, tooltip = c("y", "x")) %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 100)  # Increase left margin for longer variety names
      )
  })
  output$flavourComparisonPlot <- renderPlotly({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    valid_methods <- data %>%
      count(processing_method) %>%
      filter(n >= 10) %>%
      pull(processing_method)
    
    plot_data <- data %>%
      filter(processing_method %in% valid_methods) %>%
      group_by(processing_method) %>%
      mutate(avg_score = mean(total_cup_points)) %>%
      ungroup()
    p <- ggplot(plot_data, aes(x = total_cup_points, y = reorder(processing_method, avg_score))) +
      geom_boxplot(width = 0.5, fill = "lightblue", alpha = 0.5, outlier.shape = NA) +
      geom_jitter(aes(text = paste("Country:", country_of_origin, 
                                   "<br>Score:", round(total_cup_points, 2),
                                   "<br>Variety:", variety)), 
                  alpha = 0.6, height = 0.2, width = 0, color = "darkblue", size = 1.5) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 4, color = "red") +
      labs(
        title = "Coffee Ratings by Processing Method",
        subtitle = "Methods with 10+ samples, ordered by average score (red diamond)",
        x = "Total Cup Points",
        y = ""
      ) +
      theme_minimal()
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white"))
  })
  output$coffeeDetails <- renderPlotly({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    country_summary <- data %>%
      group_by(country_of_origin) %>%
      summarise(
        mean_score = mean(total_cup_points, na.rm = TRUE),
        count = n(),
        median_score = median(total_cup_points, na.rm = TRUE)
      ) %>%
      filter(count >= 5) %>%
      arrange(desc(mean_score))
    plot_ly(
      data = country_summary,
      type = 'choropleth',
      locations = ~country_of_origin,
      locationmode = 'country names',
      z = ~mean_score,
      text = ~paste(
        "Country: ", country_of_origin,
        "<br>Average Score: ", round(mean_score, 2),
        "<br>Number of Samples: ", count
      ),
      colorscale = 'Viridis',
      reversescale = TRUE,
      marker = list(line = list(color = 'rgb(255,255,255)', width = 0.5))
    ) %>%
      colorbar(title = "Avg Score") %>%
      layout(
        title = "Average Coffee Ratings by Country of Origin",
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'natural earth')
        )
      )
  })
  observeEvent(input$toggle, {
  })
}
shinyApp(ui = ui, server = server)
