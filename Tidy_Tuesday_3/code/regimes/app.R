#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(tidytuesdayR)
library(rnaturalearth)
library(dplyr)

tuesdata <- tidytuesdayR::tt_load('2024-11-05')

world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  select(admin, iso_a3, geometry) %>%
  rename(country_name = admin, country_code = iso_a3)

data <- tuesdata$democracy_data %>% 
  left_join(world, by = "country_code") 

data <- data %>%
  mutate(geometry = st_centroid(geometry)) %>%
  mutate(
    long = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  )

# UI
ui <- fluidPage(
  titlePanel("Global Political Landscapes Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:", choices = unique(data$year), selected = max(data$year)),
      selectInput("country", "Select a Country:", choices = unique(data$country_name.x), selected = "United States"),
      actionButton("reset", "Reset Map View"),
      checkboxInput("show_legend", "Show Legend", value = TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Map Overview",
          leafletOutput("map", height = "600px"),
          fluidRow(
            column(4, textOutput("summary_democracies")),
            column(4, textOutput("summary_monarchies")),
            column(4, textOutput("summary_communist"))
          )
        ),
        tabPanel(
          "Country Profile",
          h3(textOutput("country_title")),
          tableOutput("country_details"),
          plotOutput("country_timeline")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    data %>% filter(year == input$year)
  })
  
  # Map visualization for the first tab
  output$map <- renderLeaflet({
    pal <- colorFactor(topo.colors(3), filtered_data()$regime_category)
    
    # Create the leaflet map
    map <- leaflet(filtered_data()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~long, ~lat, 
        color = ~pal(regime_category), 
        popup = ~paste("<b>", country_name.x, "</b><br>Regime Category:", regime_category, "<br>Democracy Score:", spatial_democracy)
      )
    
    if (input$show_legend) {
      map <- map %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = filtered_data()$regime_category,
          title = "Regime Category",
          opacity = 1
        )
    }
    
    map
  })
  
  # Dynamic summaries for the selected year
  output$summary_democracies <- renderText({
    n <- filtered_data() %>% filter(is_democracy) %>% nrow()
    paste("Democracies:", n)
  })
  
  output$summary_monarchies <- renderText({
    n <- filtered_data() %>% filter(is_monarchy) %>% nrow()
    paste("Monarchies:", n)
  })
  
  output$summary_communist <- renderText({
    n <- filtered_data() %>% filter(is_communist) %>% nrow()
    paste("Communist Regimes:", n)
  })
  
  # Country profile for the selected country
  output$country_title <- renderText({
    input$country
  })
  
  output$country_details <- renderText({
    country_info <- filtered_data() %>%
      filter(country_name.x == input$country) %>%
      select(regime_category, is_monarchy, monarch_name, president_name) %>%
      distinct()
    
    regime_category <- country_info$regime_category
    is_monarchy <- country_info$is_monarchy
    monarch_name <- country_info$monarch_name
    president_name <- country_info$president_name
    
    if (is_monarchy) {
      paste("The regime category is", regime_category, 
            "and it is a monarchy. The monarch's name is", monarch_name, ".")
    } else {
      paste("The regime category is", regime_category, 
            "and it is not a monarchy. The president's name is", president_name, ".")
    }
  })
  
  output$country_timeline <- renderPlot({
    country_data <- data %>% filter(country_name.x == input$country)
    ggplot(country_data, aes(x = year, y = spatial_democracy)) +
      geom_line() +
      geom_point() +
      labs(
        title = paste("Democracy Index in", input$country),
        x = "Year",
        y = "Spatial Democracy"
      )
  })
  
  observeEvent(input$reset, {
    updateSelectInput(session, "year", selected = max(data$year))
    updateSelectInput(session, "country", selected = "United States")
  })
}

shinyApp(ui, server)