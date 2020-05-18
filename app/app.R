library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(ggplot2)
library(htmltools)
library(sortable)
library(dplyr)

source("utils/load_and_filter.R")

if (!"data" %in% ls())
  data <- loadAndFilterCeidg("data/")

if (!"pkd_main" %in% ls())
  pkd_main <- loadPKDMain("data/")

getGroupText <- function(group) {
  match <- pkd_main[pkd_main$PKDMainSection %in% group,] %>%
    mutate(
      PKDMainSectionTitle = paste(PKDMainSection, PKDMainSectionTitle, sep=' - ')
    )
  
  return(match$PKDMainSectionTitle)
}

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Działalności gospodarcze w Polsce"),
  dashboardSidebar(
    sidebarSearchForm(textId = "findRegion", buttonId = "searchButton",
                      label = "Znajdź wybrany powiat/województwo"),

    selectInput("region_type", "Wybierz jednostkę administracyjną:",
                c("Kraj", "Województwa", "Powiaty"), selected = "Kraj")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML('* { font-family: Arial; }
                       .skin-blue .main-sidebar {
                              background-color: #00C0EF;
                      }
                      .skin-blue .main-header .logo {
                              background-color: #3C8DBC;
                      }
                      .nav-tabs-custom>.nav-tabs {
                              background-color: #39CCCC;
                      }'))
    ),
    tags$head(
      tags$style(HTML(".leaflet-container { background-color:rgba(255,0,0,0.0) }"))
    ),
    tags$head(tags$style(HTML('.info-box {min-height: 110px;}
                              .info-box-icon {height: 110px; line-height: 110px;}
                              .info-box-content {padding-top: 0px;
                                                 padding-bottom: 0px;}'))),

    fluidRow(
      column(width = 5,
        box(
          width = NULL,
          # solidHeader = T,
          # status = "info",
          background = "teal",
          leafletOutput("map")
        ),
        fluidRow(
          box(
            width = 6,
            title = textOutput("regionName"),
            solidHeader = T,
            background = "teal",
            status = "primary",
            height = 385,
            box(
              width = NULL,
              height = NULL,
              leafletOutput("statistics", height = 300)
            )
            # "Tu będą ogólne statystyki",
            
            # , width = "40%", height = 200
          ),
          box(
            width = 6,
            background = "teal",
            height = 385,
            column(
              width = 12,
              # box(
              #   width = NULL,
              #   height = 75,
              #   span(htmlOutput("statistics_people"),
              #        style="color:teal; text-align:center; font-size: 20px")
              # ),

              
              fluidRow(
                infoBoxOutput("progressBox1")
              ),
              fluidRow(
                infoBoxOutput("progressBox2")
              ),
              
              fluidRow(
                infoBoxOutput("progressBox3")
              )
              
              
              # box(
              #   width = NULL,
              #   height = 75,
              #   span(htmlOutput("statistics_area"),
              #        style="color:teal; text-align:center; font-size: 20px")
              # ),
              # box(
              #   width = NULL,
              #   height = 75,
              #   span(htmlOutput("statistics_companies"),
              #        style="color:teal;text-align:center; font-size: 20px")
              # )
            )
          )
          # box(
          #   width = 6,
          #   height = 285,
          #   background = "teal",
          #   "Tu będą ogólne statystyki"
          # )
            
              # column(width = 12,
                # box(
                #   title ="Liczba ludności",
                #   status = "primary",
                #   width = NULL,
                #   height = 75,
                #   textOutput("statistics_people")
                # ),
                # box(
                #   width = NULL,
                #   height = 75,
                #   textOutput("statistics_area")
                # ),
              # column(
              #   width = 6,
              #   valueBoxOutput("progressBox1"),
              #   # valueBoxOutput("progressBox2")
              # )
              # valueBoxOutput("progressBox2"),
              # valueBoxOutput("progressBox3")
                # box(
                #   width = NULL,
                #   height = 75,
                #   background = "teal",
                #   
                # )
          
        )
      ),
      column(
        width = 7,
        box(
          width = NULL,
          background = "teal",
          tabBox(
            width = NULL,
            side ="right",
            id = "tabset1",
            height = "785px",
            tabPanel("Tab1", plotOutput("plot", height = 740)),
            tabPanel("Tab2", plotOutput("plot1", height = 740))
          )
        )
             
        # box(
        #   width = NULL,
        #   background = "teal",
        #   # bucket_list(
        #   #   header = NULL,
        #   #   group_name = "bucket_list_group",
        #   #   orientation = "vertical",
        #   # textOutput("regionName"),
        #   rank_list(
        #     labels = list(
        #       plotOutput("plot"),
        #       plotOutput("plot1")
        #     ),
        #     input_id = "rank_list_1"
      #   )
      
        
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  # regions <- reactive ({
  #   if (input$region_type == "Województwa")
  #     
  #   else
  #     
  # })


  
  
  
  
  
  wojewodztwa <- reactive ({
    if (input$region_type == "Województwa")
      "woj"
    else if (input$region_type == "Kraj")
      "kraj"
    else
      "pow"
  })
  
  regions <- reactive({
    if (wojewodztwa() == "woj")
      readOGR("maps/wojewodztwa.shp")
    else if (wojewodztwa() =="kraj")
      readOGR("maps/kraj.shp")
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

    
    if (wojewodztwa() == "woj"){
      df <- data %>%
        filter(substr(AdressTERC, 1, 2) == event$id) %>%
        rename(Region = AdressVoivodeship) %>%
        select(DurationOfExistenceInMonths, Region, PKDMainSection)
      
      region_name = df$Region[1]
      
    } else if (wojewodztwa() =="pow") {
      df <- data %>%
        filter(substr(AdressTERC, 1, 4) == event$id) %>%
        rename(Region = AdressCounty) %>%
        select(DurationOfExistenceInMonths, Region, PKDMainSection)
      
      region_name = df$Region[1]
      
    } else {
      df = data
      
      region_name = "POLSKA"
    }

    
    p <- ggplot(df, aes(x=DurationOfExistenceInMonths)) +
      geom_histogram()
    output$plot <- renderPlot(p)
    
 
    p1 <- df %>%
      group_by(PKDMainSection) %>%
      tally() %>% 
      mutate(n = 100 * (n / nrow(df))) %>%
      
      ggplot(aes(x=PKDMainSection, y=n, fill=getGroupText(PKDMainSection))) +
      geom_bar(stat="identity", width=1, color="white") +
      coord_flip() +
      labs(
        title = "Udział poszczególnych rodzajów działalności gospodarczej",
        x = "Kod PKD",
        y = "% udziału danej kategorii"
      ) + 
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", size = (15)),
        axis.title = element_text(size = (12)),
        axis.text = element_text(size = (12)),
        legend.position="bottom",
        legend.title=element_blank(),
        legend.direction = "horizontal",
        legend.spacing.x = unit(0.05, 'cm'),
        legend.spacing.y = unit(0.05, 'cm'),
        legend.text = element_text(size = 8.5)
      ) + 
      guides(fill=guide_legend(
        nrow=11,
        byrow=TRUE))

    output$plot1 <- renderPlot(p1)
    
    output$regionName <- renderText(region_name)
    
    index_region = which(regions()@data[["JPT_KOD_JE"]] == event$id)
    
    output$statistics <- renderLeaflet({
      leaflet(regions()@polygons[[index_region]]@Polygons[[1]]@coords,
              options = leafletOptions(
                zoomControl = FALSE,
                attributionControl=FALSE)) %>% 
        addPolygons()
    })
    
    # output$statistics_people <- renderUI({
    #   str1 = "Liczba ludności:"
    #   str2 = "tu liczba"
    #   HTML(paste(str1, str2, sep = '<br/>'))
    #   })
    # output$statistics_area <- renderUI({
    #   str1 = "Powierzchnia:"
    #   str2 = "tu liczba"
    #   HTML(paste(str1, str2, sep = '<br/>'))
    # })
    # output$statistics_companies <- renderUI({
    #   str1 = "Liczba działalności:"
    #   str2 = nrow(df)
    #   HTML(paste(str1, str2, sep = '<br/>'))
    # })
    
  })
  

  
  output$progressBox1 <- renderInfoBox({
    infoBox(
      subtitle = "Liczba ludności", "tu jakaś",
      width = NULL,
      icon = icon("male"),
      fill = FALSE#male, globe, mail-bulk, balance-scale, dollar-sign, lightbulb
    )
  })

  output$progressBox2 <- renderInfoBox({
    infoBox(
      subtitle = "Powierzchnia", "tu jakaś",
      width = NULL,
      icon = icon("globe") #male, globe, mail-bulk, balance-scale, dollar-sign, lightbulb
    )
  })

  output$progressBox3 <- renderInfoBox({
    infoBox(
      subtitle = "Liczba działalności", "co",
      width = NULL,
      icon = icon("user-tie") #male, globe, mail-bulk, balance-scale, dollar-sign, lightbulb
    )
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
