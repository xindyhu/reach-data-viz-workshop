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
    
    # Main content area
    column(9,
           fluidRow(
             column(6,
                    div(class = "box",
                        div(h3("Number of Bean Variety")),
                        plotlyOutput("beanVarietyPlot", height = "350px")
                    )
             ),
             column(6,
                    div(class = "box",
                        div(h3("Average Flavour Profile")),
                        plotlyOutput("flavourProfile", height = "350px")
                    )
             )
           ),
           fluidRow(
             column(6,
                    div(class = "box",
                        div(h3("Coffee Ratings by Processing Method")),
                        plotlyOutput("flavourComparisonPlot", height = "350px")
                    )
             ),
             column(6,
                    div(class = "box",
                        div(h3("Average Coffee Ratings by Country")),
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
    
    updateSelectizeInput(session, "country",
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
    
    if (is.null(input$country) || "All" %in% input$country) {
      data <- data # Return all data if "All" is selected
    } else {
      data <- data %>% filter(country_of_origin %in% input$country) # Filter otherwise
    }
    
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
  
  # Radar chart
  output$flavourProfile <- renderPlotly({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    
    all_data_summary <- data %>%
      summarise(across(c(aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness),
                       mean, na.rm = TRUE)) %>%
      mutate(country = "All Selected Data")
    
    top_countries_summary <- data %>%
      group_by(country_of_origin) %>%
      summarise(across(c(aroma, flavor, aftertaste, acidity, body, balance, uniformity, clean_cup, sweetness),
                       mean, na.rm = TRUE),
                count = n()) %>%
      filter(count >= 5) %>%
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
        r = round(values, 2),
        theta = stringr::str_to_title(categories),
        name = country_name,
        fill = ifelse(country_name == "All Selected Data", 'toself', 'none'),
        fillcolor = ifelse(country_name == "All Selected Data", "rgba(70, 130, 180, 0.3)", NA),
        line = list(color = color, width = line_width),
        hovertemplate = paste(
          "<b>Country:</b> ", country_name, "<br>",
          "<b>Category:</b> %{theta}<br>",
          "<b>Value:</b> %{r}<extra></extra>"  # <extra></extra> removes trace name in tooltip
        )
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
      title = ""
    )
  })
  
  # Bean variety barchart
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
    
    p <- ggplot(variety_data, aes(x = n, y = reorder(variety, n), text = variety)) +
      geom_col(fill = 'skyblue') +
      theme_minimal() +
      labs(x = "Count", y = "") +
      theme(
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank()
      )
    ggplotly(p, tooltip = c("text", "x")) %>%
      layout(
        hoverlabel = list(bgcolor = "white"),
        margin = list(l = 100)  # Increase left margin for longer variety names
      )
  })
  
  # Coffee ratings by processing method
  output$flavourComparisonPlot <- renderPlotly({
    data <- filtered_data()
    
    if (is.null(data) || nrow(data) == 0) {
      return(NULL)
    }
    valid_methods <- data %>%
      count(processing_method) %>%
      filter(n >= 5) %>%
      pull(processing_method)
    
    plot_data <- data %>%
      filter(processing_method %in% valid_methods) %>%
      group_by(processing_method) %>%
      mutate(avg_score = mean(total_cup_points)) %>%
      ungroup()
    p <- ggplot(plot_data, aes(x = total_cup_points, y = reorder(as.factor(processing_method), avg_score))) +
      geom_jitter(aes(text = paste("Country:", country_of_origin,
                                   "<br>Score:", round(total_cup_points, 2),
                                   "<br>Variety:", variety)),
                  alpha = 0.6, height = 0.2, width = 0, color = "darkblue", size = 1.5) +
      stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "red") +
      labs(
        title = "",
        x = "Total Cup Points",
        y = ""
      ) +
      theme_minimal()
    ggplotly(p, tooltip = "text") %>%
      layout(
        annotations = list(
          # Subtitle
          list(
            x = 0.5, y = 1.05, text = "Methods with 5+ samples, ordered by average score (red diamond)",
            showarrow = FALSE, xref = "paper", yref = "paper",
            font = list(size = 12)
          )))
  })
  
  # Map
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
      colorbar(title = "Average Score") %>%
      layout(
        title = "",
        geo = list(
          showframe = FALSE,
          showcoastlines = TRUE,
          projection = list(type = 'natural earth')
        )
      )
  })
}

shinyApp(ui = ui, server = server)
