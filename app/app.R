library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(ggplot2)
library(sortable)

source("utils/load_and_filter.R")


if (!"data" %in% ls())
  data <- loadAndFilterCeidg("data/")


ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Działalności gospodarcze w Polsce"),
  dashboardSidebar(
    sidebarSearchForm(textId = "findRegion", buttonId = "searchButton",
                      label = "Znajdź wybrany powiat/województwo"),

    selectInput("region_type", "Wybierz jednostkę administracyjną:",
                c("Województwa", "Powiaty"), selected = "Województwa")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML('* { font-family: Arial; }'))
    ),

    fluidRow(
      column(width = 6,
        box(
          width = NULL,
          # solidHeader = T,
          # status = "info",
          background = "light-blue",
          leafletOutput("map")
        ),
        box(
          width = NULL,
          background = "light-blue",
          "Tu będą ogólne statystyki"
        )
      ),
      column(width = 6,
        box(
          width = NULL,
          background = "light-blue",
          # bucket_list(
          #   header = NULL,
          #   group_name = "bucket_list_group",
          #   orientation = "vertical",
          textOutput("regionName"),
          rank_list(
            labels = list(
              plotOutput("plot"),
              plotOutput("plot1")
            ),
            input_id = "rank_list_1",
            class = ()
          )
        )
      )
    )
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

    if (wojewodztwa()){
      df <- data %>%
        filter(substr(AdressTERC, 1, 2) == event$id) %>%
        rename(Region = AdressVoivodeship) %>%
        select(DurationOfExistenceInMonths, Region, PKDMainSection)
      
      region_name = df$Region[1]

    } else {
      df <- data %>%
        filter(substr(AdressTERC, 1, 4) == event$id) %>%
        rename(Region = AdressCounty) %>%
        select(DurationOfExistenceInMonths, Region, PKDMainSection)
      
      region_name = df$Region[1]

    }
    p <- ggplot(df, aes(x=DurationOfExistenceInMonths)) +
      geom_histogram()
    output$plot <- renderPlot(p)
    
    p1 <- df %>%
      group_by(PKDMainSection) %>% tally() %>%
      
      ggplot(aes(x="", y=n, fill=PKDMainSection)) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_polar("y", start=0)
      # theme_ft_rc() +
      # theme(
      #   axis.title.x = element_blank(),
      #   axis.title.y = element_blank(),
      #   axis.text.x = element_blank(),
      #   panel.grid = element_blank() # to z jakiegoś powodu nie działa
      # )
    output$plot1 <- renderPlot(p1)
    
    output$regionName <- renderText(region_name)
    
    # output$statistics <- renderText("Overall statistics")
  })
  
  
}

# Run the application
shinyApp(ui = ui, server = server)

