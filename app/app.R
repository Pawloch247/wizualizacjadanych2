library(shiny)
library(bdl)
library(leaflet)
library(rgdal)
library(ggplot2)
library(htmltools)


source("utils/load_and_filter.R")

data <- loadAndFilterCeidg("data/")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  mainPanel(
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
      readOGR("maps/wojewodztwa.shp")
    else
      readOGR("maps/Powiaty.shp")
  })
  
  myPalette <- reactive({
    colorNumeric("YlOrRd", domain = regions()$No_Buss)
  })
  
  labels <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%g  registered enterprises",
      regions()$Name, regions()$No_Buss) %>%
      lapply(htmltools::HTML)
  })
  
  
  
  wojewodztwa <- reactive ({
    if (input$region_type == "Województwa")
      TRUE
    else
      FALSE
  })
  
  print(labels)
  
  output$map <- renderLeaflet({
    leaflet(regions()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(layerId = regions()$JPT_KOD_JE,
                  fillColor = ~myPalette()(regions()$No_Buss),
                  fillOpacity = 0.3,
                  weight = 1,
                  color = "grey",
                  dashArray = "3",
                  highlightOptions = highlightOptions(color = "#666", 
                                                      weight = 2,
                                                      bringToFront = TRUE,
                                                      fillOpacity = "0.3"),
      label = labels()) %>%
      addLegend(pal = myPalette(), values = ~regions()$No_Buss, opacity = 0.7, title = "No. of enterprises",
                position = "bottomright")
  })
 
  observeEvent(input$map_shape_click,{
    event <- input$map_shape_click

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

