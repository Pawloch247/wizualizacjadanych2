library(shiny)
library(bdl)
library(leaflet)
library(rgdal)
library(ggplot2)

x <- generate_map(20320)
source("../load_and_filter.R")

data <- loadAndFilterCeidg("../data/")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  mainPanel(
    setBackgroundColor(color = "blue"),
    selectInput("region_type", "Wybierz jednostkę administracyjną:",
                c("Województwa", "Powiaty"), selected = "Województwa"),
    leafletOutput("map"),
    wellPanel(plotOutput("plot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  regions <- reactive ({
    if (input$region_type == "Województwa")
      readOGR("../maps/wojewodztwa.shp")
    else
      readOGR("../maps/powiaty.shp")
  })
  
  wojewodztwa <- reactive ({
    if (input$region_type == "Województwa")
      TRUE
    else
      FALSE
  })
  
  output$map <- renderLeaflet({
    leaflet(regions()) %>%
      addTiles() %>%
      addPolygons(layerId = regions()$JPT_KOD_JE,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  })
  
  
  observeEvent(input$map_shape_click,{
    event <- input$map_shape_click
    print(event$id)
    if (wojewodztwa()){
      df <- data %>%
        filter(substr(AdressTERC, 1, 2) == event$id) %>%
        rename(Region = AdressVoivodeship) %>% 
        select(DurationOfExistenceInMonths, Region)
      
    } else {
      df <- data %>%
        filter(substr(AdressTERC, 1, 4) == event$id) %>%
        rename(Region = AdressCounty) %>% 
        select(DurationOfExistenceInMonths, Region)
      
    }
    p <- ggplot(df, aes(x=DurationOfExistenceInMonths)) +
      geom_histogram() + 
      ggtitle(df$Region)
    output$plot <- renderPlot(p)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

