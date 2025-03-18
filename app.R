library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(DT)
library(plotly)
library(viridis)
coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

coffee_clean <- coffee_ratings %>%
  filter(!is.na(total_cup_points), !is.na(country_of_origin)) %>%
  mutate(
    quality_category = case_when(
      total_cup_points >= 85 ~ "Excellent",
      total_cup_points >= 80 ~ "Very Good",
      total_cup_points >= 75 ~ "Good",
      TRUE ~ "Fair"
    ),
    processing_method = ifelse(is.na(processing_method), "Unknown", processing_method)
  )

ui <- dashboardPage(
  
  dashboardHeader(title = "Coffee Dashboard", titleWidth = 230),
  
  dashboardSidebar(
    width = 230,
    h3("Country", style = "padding-left: 15px; color: white;"),
    selectInput(
      "country_filter", 
      NULL,
      choices = c("All", sort(unique(coffee_clean$country_of_origin))),
      selected = "All",
      width = "90%"
    ),
    
    tags$div(
      style = "padding: 15px;",
      checkboxGroupInput(
        "quality_filter", 
        "Quality Category:",
        choices = c("Excellent", "Very Good", "Good", "Fair"),
        selected = c("Excellent", "Very Good", "Good", "Fair")
      ),
      
      selectInput(
        "variety_filter", 
        "Bean Variety:",
        choices = c("All", sort(unique(na.omit(coffee_clean$variety)))),
        selected = "All"
      ),
      
      selectInput(
        "processing_filter", 
        "Processing Method:",
        choices = c("All", sort(unique(na.omit(coffee_clean$processing_method)))),
        selected = "All"
      )
    )
  ),
  
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f5f5f5; }
        .box { border-radius: 3px; box-shadow: 0 1px 3px rgba(0,0,0,.2); }
        .box-header { border-bottom: 1px solid #f4f4f4; }
      "))
    ),
    
    fluidRow(
      
      box(
        title = "Avg Flavour Profile",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput("flavor_profile", height = 400)
      ),
      
      
      box(
        title = "Bean Variety",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput("bean_variety", height = 400)
      )
    ),
    
    fluidRow(
      
      box(
        title = "How different are the flavour profiles?",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        plotlyOutput("flavor_comparison", height = 400)
      ),
      
      box(
        title = "Coffee Details",
        status = "primary",
        solidHeader = TRUE,
        width = 6,
        DTOutput("coffee_table")
      )
    )
  )
)

server <- function(input, output) {
  
  
  filtered_data <- reactive({
    data <- coffee_clean
    
    
    if (input$country_filter != "All") {
      data <- data %>% filter(country_of_origin == input$country_filter)
    }
    
    if (input$variety_filter != "All") {
      data <- data %>% filter(variety == input$variety_filter)
    }
    
    if (input$processing_filter != "All") {
      data <- data %>% filter(processing_method == input$processing_filter)
    }
    
    data <- data %>% filter(quality_category %in% input$quality_filter)
    
    return(data)
  })
  
  output$flavor_profile <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    flavor_data <- filtered_data() %>%
      summarise(
        Aftertaste = mean(aftertaste, na.rm = TRUE),
        Body = mean(body, na.rm = TRUE),
        Acidity = mean(acidity, na.rm = TRUE),
        Aroma = mean(aroma, na.rm = TRUE),
        Balance = mean(balance, na.rm = TRUE),
        Flavor = mean(flavor, na.rm = TRUE),
        `Cupper Points` = mean(cupper_points, na.rm = TRUE),
        `Clean Cup` = mean(clean_cup, na.rm = TRUE),
        Sweetness = mean(sweetness, na.rm = TRUE),
        Uniformity = mean(uniformity, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = everything(), names_to = "Attribute", values_to = "Score")
    
    
    p <- ggplot(flavor_data, aes(x = Score, y = reorder(Attribute, Score), color = Attribute)) +
      geom_segment(aes(x = 0, xend = Score, y = Attribute, yend = Attribute), 
                   color = "gray80") +
      geom_point(size = 12, alpha = 0.7) +
      scale_color_viridis_d() +
      scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    ggplotly(p)
  })
  
  output$bean_variety <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    variety_data <- filtered_data() %>%
      filter(!is.na(variety)) %>%
      count(variety) %>%
      arrange(desc(n)) %>%
      slice_head(n = 10)
    
    p <- ggplot(variety_data, aes(x = n, y = reorder(variety, n), fill = n)) +
      geom_col() +
      scale_fill_gradient(low = "#ff9a8c", high = "#ff6347") +
      labs(x = "n", y = "") +
      theme_minimal() +
      theme(
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    ggplotly(p)
  })
  
  
  output$flavor_comparison <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    flavor_stats <- filtered_data() %>%
      summarise(across(
        c(body, aroma, aftertaste, acidity, balance, flavor, cupper_points, sweetness),
        list(
          min = ~ min(.x, na.rm = TRUE),
          q1 = ~ quantile(.x, 0.25, na.rm = TRUE),
          median = ~ median(.x, na.rm = TRUE),
          q3 = ~ quantile(.x, 0.75, na.rm = TRUE),
          max = ~ max(.x, na.rm = TRUE)
        )
      )) %>%
      pivot_longer(
        cols = everything(),
        names_to = c("attribute", "stat"),
        names_pattern = "(.*)_(.*)"
      ) %>%
      pivot_wider(names_from = stat, values_from = value) %>%
      mutate(attribute = factor(attribute, levels = c(
        "body", "aroma", "aftertaste", "acidity", "balance", 
        "flavor", "cupper_points", "sweetness"
      )))
    
    p <- ggplot(flavor_stats, aes(y = attribute)) +
      geom_segment(aes(x = min, xend = max, yend = attribute), color = "lightblue") +
      geom_segment(aes(x = q1, xend = q3, yend = attribute), color = "skyblue", size = 2) +
      geom_point(aes(x = median), color = "blue", size = 3) +
      geom_vline(xintercept = 6, linetype = "dashed", color = "black") +
      scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2.5)) +
      labs(x = "", y = "") +
      theme_minimal() +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA)
      )
    
    ggplotly(p)
  })
  
  output$coffee_table <- renderDT({
    req(nrow(filtered_data()) > 0)
    
    table_data <- filtered_data() %>%
      select(
        points = total_cup_points,
        species,
        country = country_of_origin,
        region
      ) %>%
      arrange(desc(points)) %>%
      head(10)
    
    datatable(
      table_data,
      options = list(
        pageLength = 5,
        dom = 't',
        ordering = TRUE,
        scrollX = FALSE
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui = ui, server = server)