#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bdl)
library(leaflet)
library(rgdal)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  
  # Sidebar with a slider input for number of bins 
  
  # Show a plot of the generated distribution
  mainPanel(
    leafletOutput("map"),
    wellPanel(textOutput("cnty"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  output$map <- renderLeaflet({
    # bdl_map <- generate_map(varId = "60559", year = "2018", unitLevel = 5)
    # bdl_map
    tmp2 <- readOGR("./maps/powiaty.shp")
    leaflet(tmp2) %>%
      addTiles() %>%
      addPolygons(layerId = tmp2$JPT_NAZWA_, highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE))
  })
  
  observe({
    event <- input$map_shape_click
    # # bdl_map[["x"]][["calls"]][[5]][["args"]][[5]]
    # number = sub("X", "", event$id)
    # tmp = bdl_map[["x"]][["calls"]][[5]][["args"]][[5]][1]
    # start = stringi::stri_locate(tmp, regex = "<nobr>")
    # end = stringi::stri_locate(tmp, regex = "</nobr>")
    # # output$cnty <- renderText(substr(tmp, start[1,2] + 1, end[1,1] - 1))
    output$cnty <- renderText(c(event$id))

  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)

