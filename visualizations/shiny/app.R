library(dplyr)
library(ggplot2)
library(scales)
library(shiny)
library(sf)
library(tigris)

data <- read.csv("./r.csv")

location_df <- st_as_sf(states(cb = TRUE)) %>%
  st_transform(crs = 4326)

ui <- fluidPage(
  titlePanel("US Chronic Disease Indicators, 2016"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "topic_filter",
        "Indicator",
        choices = c("All", sort(unique(data$Topic))),
        multiple = TRUE,
        selected = "All"
      ),
      selectInput(
        "location_filter",
        "Location",
        choices = c("All", sort(unique(data$LocationAbbr))),
        multiple = TRUE,
        selected = "All"
      )
    ),
    
    mainPanel(
      plotOutput("indicatorPercent"),
      plotOutput("indicatorTotal"),
      plotOutput("mapPercent"),
      plotOutput("mapTotal")
    )
  )
)

server <- function(input, output, session) {
  get_indicator_data <- reactive({
    df <- data
    if (!"All" %in% input$location_filter) {
      df <- df %>% filter(LocationAbbr %in% input$location_filter)
    }
    df
  })
  
  get_map_data <- reactive({
    df <- data
    if (!"All" %in% input$topic_filter) {
      df <- df %>% filter(Topic %in% input$topic_filter)
    }
    df
  })
  
  output$indicatorPercent <- renderPlot({
    df <- get_indicator_data()
    
    topic_data <- df %>%
      group_by(Topic) %>%
      summarise(TotalDataValue = sum(DataValue)) %>%
      ungroup()
    
    ggplot(topic_data, aes(x = "", y = TotalDataValue, fill = Topic)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      theme_void() +
      labs(title = "Indicator Percent") +
      theme(legend.title = element_blank(),
            plot.title = element_text(hjust = 0.5))
  })
  
  output$indicatorTotal <- renderPlot({
    df <- get_indicator_data()
    
    topic_data <- df %>%
      group_by(Topic) %>%
      summarise(TotalDataValue = sum(DataValue)) %>%
      ungroup()
    
    ggplot(topic_data, aes(x = reorder(Topic, -TotalDataValue), y = TotalDataValue)) +
      geom_bar(stat = "identity", fill = "blue") +
      labs(title = "Indicator Total", x = NULL, y = NULL) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none",
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$mapPercent <- renderPlot({
    df <- get_map_data() %>%
      group_by(LocationAbbr) %>%
      summarise(TotalPercent = sum(Percent)) %>%
      ungroup()
    
    map_data <- location_df %>%
      left_join(df, by = c("STUSPS" = "LocationAbbr"))
    
    ggplot(map_data) +
      geom_sf(aes(fill = TotalPercent), color = "white") +
      scale_fill_gradientn(colors = c("lightblue", "blue"),
                           labels = scales::comma) +
      labs(title = "Map Percent") +
      coord_sf(
        expand = FALSE,
        xlim = c(-125, -66.93457),
        ylim = c(24.396308, 49.384358)
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position.inside = c(1, 0),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  })
  
  output$mapTotal <- renderPlot({
    df <- get_map_data() %>%
      group_by(LocationAbbr) %>%
      summarise(TotalDataValue = sum(DataValue, na.rm = TRUE)) %>%
      ungroup()
    
    map_data <- location_df %>%
      left_join(df, by = c("STUSPS" = "LocationAbbr"))
    
    ggplot(map_data) +
      geom_sf(aes(fill = TotalDataValue), color = "white") +
      scale_fill_gradientn(colors = c("lightblue", "blue"),
                           labels = scales::comma) +
      labs(title = "Map Total") +
      coord_sf(
        expand = FALSE,
        xlim = c(-125, -66.93457),
        ylim = c(24.396308, 49.384358)
      ) +
      theme_minimal() +
      theme(
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position.inside = c(1, 0),
        legend.justification = c(1, 0),
        legend.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5)
      )
  })
}

shinyApp(ui, server)
