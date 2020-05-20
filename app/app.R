library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(ggplot2)
library(htmltools)
library(sortable)
library(dplyr)
library(formattable)
library(leaflet.extras)


source("utils/load_and_filter.R")

if (!"data" %in% ls()) 
  data <- loadAndFilterCeidg("data/")

if (!"pkd_main" %in% ls())
  pkd_main <- loadPKDMain("data/")

if (!"woj_percent" %in% ls())
  woj_percent <- loadWojPercent("data/")

if (!"woj_count" %in% ls())
  woj_count <- loadWojCount("data/")

if (!"pow_percent" %in% ls())
  pow_percent <- loadPowPercent("data/")

if (!"pow_count" %in% ls())
  pow_count <- loadPowCount("data/")

getGroupText <- function(group) {
  match <- pkd_main[pkd_main$PKDMainSection %in% group,] %>%
    mutate(
      PKDMainSectionTitle = paste(PKDMainSection, PKDMainSectionTitle, sep=' - ')
    )
  
  return(match$PKDMainSectionTitle)
}

ui <- dashboardPage(
  # skin = "blue",
  
  dashboardHeader(title = "Działalności gospodarcze w Polsce", titleWidth = "380px"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML('* { font-family: Arial; }
                      .h3, h3 { font-family: Arial; }
                      .skin-blue .main-sidebar {
                      background-color: #84B5E5;
                      }
                      div.MathJax_Display{
                      text-align: left !important;
                      }
                      .skin-blue .main-header .logo {
                      background-color: #006695;
                      }
                      .nav-tabs-custom>.nav-tabs {
                      background-color: #84B5E5;
                      }
                      .content-wrapper, content {
                      background-color: #FDFCFE;
                      }
                      .info-box{
                      background: #FDFCFE;
                      }
                      .box.box-solid.box-primary>.box-header {
                      color:#FDFCFE;
                      background:#84B5E5;
                      }
                      .box.box-solid.box-primary{
                      
                      background:#84B5E5;
                      }
                      .skin-blue .main-header .navbar {
                      background-color: #006695;
                      }
                      .label-info, .modal-info .modal-body {
                      background-color: #84B5E5;
                      }
                      .box {
                      background: #84B5E5;
                      border-top: 0px;
                      }
                      .nav-tabs-custom>.tab-content {
                      background: #FDFCFE;
                      }
                      .nav-tabs-custom>.nav-tabs>li.active:hover>a, .nav-tabs-custom>.nav-tabs>li.active>a {
                      background-color: #FDFCFE;
                      
                      }
                      .nav-tabs-custom>.nav-tabs>li {
                      border-top: 0px
                      }
                      .alert-info, .bg-aqua, .callout.callout-info, .label-info, .modal-info .modal-body {
                      background-color: #006695!important;
                      }
                      '))
      ),
    tags$head(
      tags$style(HTML(".leaflet-container { background-color:rgba(255,0,0,0.0) }"))
    ),
    tags$head(tags$style(HTML('.info-box {min-height: 110px;}
                              .info-box-icon {height: 110px; line-height: 110px;
                              color:#84B5E5}
                              .info-box-content {padding-top: 0px;
                              padding-bottom: 0px;
                              }'))),
    
    fluidRow(
      column(width = 5,
             box(
               width = NULL,
               height = "470px",
               leafletOutput("map", height = 450),
               absolutePanel(top = 0, right = 15,
                             selectInput("region_type",
                                         "",
                                         c("Województwa", "Powiaty"), selected = "Województwa",
                                         width = "150px"))
             ),
             fluidRow(
               column(width = 7,
                      box(
                        width = NULL,
                        height = "335px",
                        title = textOutput("regionName"),
                        solidHeader = T,
                        # 
                        status = "primary",
                        
                        leafletOutput("statistics", height = 270)
                        
                        # "Tu będą ogólne statystyki",
                        
                        # , width = "40%", height = 200
                      )
               ),
               
               column(
                 width = 5,
                 
                 fluidRow(
                   width = 5,
                   # infoBoxOutput("progressBox1"),
                   h3("Liczba ludności:"),
                   h3(uiOutput("liczba_ludnosci")),
                   h3("Powierzchnia:"),
                   h3(uiOutput("powierzchnia")),
                   h3("Liczba działalności:"),
                   h3(uiOutput("liczba_dzialalnosci"))
                   # box(
                   #   width = NULL,
                   #   h1("siema"))
                 )
                 # fluidRow(
                 #   # infoBoxOutput("progressBox2")
                 #   box("siema")
                 # ),
                 # 
                 # fluidRow(
                 #   # infoBoxOutput("progressBox3")
                 #   box('siema')
                 # )
               )
               
             )
      ),
      column(
        width = 7,
        box(
          width = NULL,
          
          tabBox(
            width = NULL,
            side ="right",
            id = "tabset1",
            height = "785px",
            tabPanel("Czas istnienia", plotOutput("plot", height = 740)),
            tabPanel("Start", plotOutput("plot3", height = 740)),
            tabPanel("PKD %", plotOutput("plot1", height = 740)),
            tabPanel("PKD ranking %", plotOutput("plot4", height = 740)),
            tabPanel("PKD ranking suma", plotOutput("plot5", height = 740)),
            tabPanel("Statystyki", plotOutput("plot2", height = 740))
            
          )
        )
        
        # box(
        #   width = NULL,
        #   
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
      TRUE
    else
      FALSE
  })
  
  regions <- reactive({
    if (wojewodztwa())
      readOGR("maps/wojewodztwa.shp", use_iconv = TRUE, encoding = "UTF-8")
    else 
      readOGR("maps/Powiaty.shp", use_iconv = TRUE, encoding = "UTF-8")
  })
  
  myPalette <- reactive({
    colorNumeric("YlOrRd", domain = regions()$No_Buss)
  })
  
  popups <- reactive({
    sprintf(
      "<strong>%s</strong><br/>%g  zarejestrowanych działalności",
      regions()$Name, regions()$No_Buss) %>%
      lapply(htmltools::HTML)
  })
  
  
  
  
  output$map <- renderLeaflet({
    leaflet(regions()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
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
                  label = regions()$Name,
                  popup = popups(),
                  group = "polygons") %>%
      addLegend(pal = myPalette(), values = ~regions()$No_Buss, opacity = 0.7, title = "Liczba działalności",
                position = "bottomright") %>% 
      addSearchFeatures(
        targetGroups  = 'polygons',
        options = searchFeaturesOptions(zoom=7, openPopup=TRUE, autoType=FALSE))
  })
  
  
  regionTMP = eventReactive(input$map_shape_click,{
    event <- input$map_shape_click
    
    return(event$id)},ignoreNULL = F)
  
  regionID = reactive({
    
    if (wojewodztwa() & (nchar(regionTMP()) == 4 || is.null(regionTMP())))
      return(c("14"))
    
    if (!wojewodztwa() & (nchar(regionTMP()) == 2 || is.null(regionTMP())))
      return(c("1465"))
    
    return(regionTMP())
  })
  
  df = reactive({
    
    if (wojewodztwa()){
      df <- data %>%
        filter(substr(AdressTERC, 1, 2) == regionID()) %>%
        rename(Region = AdressVoivodeship) %>%
        select(DurationOfExistenceInMonths, Region, PKDMainSection, Sex, IsWWW, IsPhoneNo, IsEmail, MonthOfStartingOfTheBusiness)
      print(regionID())
      df_pkd_percent <- woj_percent %>%
        filter(AdressTERC == regionID()) %>%
        select(PKDMainSection, size)
      
      df_pkd_count <- woj_count %>%
        filter(AdressTERC == regionID()) %>%
        select(PKDMainSection, size)
      
      region_name = df$Region[1]
      count = 16
      
    } else {
      df <- data %>%
        filter(substr(AdressTERC, 1, 4) == regionID()) %>%
        rename(Region = AdressCounty) %>%
        select(DurationOfExistenceInMonths, Region, PKDMainSection, Sex, IsWWW, IsPhoneNo, IsEmail, MonthOfStartingOfTheBusiness)
      
      df_pkd_percent <- pow_percent %>%
        filter(AdressTERC == regionID()) %>%
        select(PKDMainSection, size)
      
      df_pkd_count <- pow_count %>%
        filter(AdressTERC == regionID()) %>%
        select(PKDMainSection, size)
      
      region_name = df$Region[1]
      count = 380
      
    }
    
    return(list(df = df, df_pkd_percent = df_pkd_percent,
                df_pkd_count = df_pkd_count, region_name = region_name,
                count = count))
    
  })
  
  # observeEvent(input$map_shape_click,{
  #   event <- input$map_shape_click
  #   
  #   if (wojewodztwa() == "woj"){
  #     df <- data %>%
  #       filter(substr(AdressTERC, 1, 2) == event$id) %>%
  #       rename(Region = AdressVoivodeship) %>%
  #       select(DurationOfExistenceInMonths, Region, PKDMainSection, Sex, IsWWW, IsPhoneNo, IsEmail, MonthOfStartingOfTheBusiness)
  #     
  #     df_pkd_percent <- woj_percent %>%
  #       filter(AdressTERC == event$id) %>%
  #       select(PKDMainSection, size)
  #     
  #     df_pkd_count <- woj_count %>%
  #       filter(AdressTERC == event$id) %>%
  #       select(PKDMainSection, size)
  #     
  #     region_name = df$Region[1]
  #     
  #   } else {
  #     df <- data %>%
  #       filter(substr(AdressTERC, 1, 4) == event$id) %>%
  #       rename(Region = AdressCounty) %>%
  #       select(DurationOfExistenceInMonths, Region, PKDMainSection, Sex, IsWWW, IsPhoneNo, IsEmail, MonthOfStartingOfTheBusiness)
  #     
  #     df_pkd_percent <- pow_percent %>%
  #       filter(AdressTERC == event$id) %>%
  #       select(PKDMainSection, size)
  #     
  #     df_pkd_count <- pow_count %>%
  #       filter(AdressTERC == event$id) %>%
  #       select(PKDMainSection, size)
  #     
  #     region_name = df$Region[1]
  #     
  #   }
  # 
  ### START PLOTS
  
  
  theme_title = reactive({
    theme_title <- theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = (15))
    )
    
    return(theme_title)
  })
  
  theme_basic = reactive({
    
    theme_basic <- theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = (15)),
      axis.title = element_text(size = (12)),
      axis.text = element_text(size = (12)),
      legend.position="bottom",
      legend.title=element_blank(),
      legend.direction = "horizontal",
      legend.spacing.x = unit(0.05, 'cm'),
      legend.spacing.y = unit(0.05, 'cm'),
      legend.text = element_text(size = 8.5)
    )
    
    return(theme_basic)
  })
  
  p = reactive({
    
    
    p <- ggplot(df()$df, aes(x=DurationOfExistenceInMonths)) +
      geom_histogram(color="#39cccc", fill="#39cccc") +
      xlim(0, NA) +
      labs(
        title = "Histogram czasu istnienia działalności",
        x = "Czas istnienia (w miesiącach)",
        y = "Liczba"
      ) + theme_basic()
    return(p)
  })
  
  output$plot <- renderPlot(p())
  
  p1 = reactive({
    p1 <- df()$df %>%
      group_by(PKDMainSection) %>%
      tally() %>%
      mutate(n = 100 * (n / nrow(df()$df))) %>%
      
      ggplot(aes(x=PKDMainSection, y=n, fill=getGroupText(PKDMainSection))) +
      geom_bar(stat="identity", width=1, color="white") +
      geom_text(aes(label=paste(round(n, digits=1), ' %', sep='')), position=position_dodge(width=0.9), hjust=-0.1) +
      coord_flip() +
      labs(
        title = "Udział poszczególnych rodzajów działalności gospodarczej",
        x = "Kod PKD",
        y = "% udziału danej kategorii"
      ) +
      theme_basic() +
      guides(fill=guide_legend(
        nrow=11,
        byrow=TRUE))
    return(p1)
  })
  
  output$plot1 <- renderPlot(p1())
  
  p2 = reactive({
    p2_1 <- df()$df %>%
      group_by(Sex) %>% tally() %>% mutate(n =  (n / nrow(df()$df))) %>%
      mutate(Sex = recode(Sex, 'F' = "Kobieta", 'M' = "Mężczyzna")) %>%
      ggplot(aes(x="", y=n, fill=Sex)) +
      geom_col() +
      geom_text(aes(label = percent(n)), position = position_stack(vjust = 0.5), color = "white", size=6, fontface='bold') +
      coord_polar("y", start=0) +
      theme_void() +
      theme_title() +
      theme(legend.title=element_blank()) +
      labs(
        title = "Płeć"
      ) +
      scale_fill_manual(values = c("Mężczyzna" = "#39cccc",
                                   "Kobieta" = "#cc3939"))
    
    p2_2 <- df()$df %>%
      group_by(IsWWW) %>% tally() %>% mutate(n =  (n / nrow(df()$df))) %>%
      mutate(IsWWW = c('Posiada', 'Nie posiada')[IsWWW + 1]) %>%
      ggplot(aes(x="", y=n, fill=IsWWW)) +
      geom_col() +
      geom_text(aes(label = percent(n)), position = position_stack(vjust = 0.5), color = "white", size=6, fontface='bold') +
      coord_polar("y", start=0) +
      theme_void() +
      theme_title() +
      theme(legend.title=element_blank()) +
      labs(
        title = "Strona WWW (podana w rejestracji)"
      ) +
      scale_fill_manual(values = c("Posiada" = "#39cccc",
                                   "Nie posiada" = "#cc3939"))
    
    p2_3 <- df()$df %>%
      group_by(IsEmail) %>% tally() %>% mutate(n =  (n / nrow(df()$df))) %>%
      mutate(IsEmail = c('Posiada', 'Nie posiada')[IsEmail + 1]) %>%
      ggplot(aes(x="", y=n, fill=IsEmail)) +
      geom_col() +
      geom_text(aes(label = percent(n)), position = position_stack(vjust = 0.5), color = "white", size=6, fontface='bold') +
      coord_polar("y", start=0) +
      scale_fill_manual(values = c("Posiada" = "#39cccc",
                                   "Nie posiada" = "#cc3939")) +
      theme_void() +
      theme_title() +
      theme(legend.title=element_blank()) +
      labs(
        title = "Adres email (podany w rejestracji)"
      )
    
    p2_4 <- df()$df %>%
      group_by(IsPhoneNo) %>% tally() %>% mutate(n =  (n / nrow(df()$df))) %>%
      mutate(IsPhoneNo = c('Posiada', 'Nie posiada')[IsPhoneNo + 1]) %>%
      ggplot(aes(x="", y=n, fill=IsPhoneNo)) +
      geom_col() +
      geom_text(aes(label = percent(n)), position = position_stack(vjust = 0.5), color = "white", size=6, fontface='bold') +
      coord_polar("y", start=0) +
      theme_void() +
      theme_title() +
      theme(legend.title=element_blank()) +
      labs(
        title = "Numer telefonu (podany w rejestracji)"
      ) +
      scale_fill_manual(values = c("Posiada" = "#39cccc",
                                   "Nie posiada" = "#cc3939"))
    
    p2 <- cowplot::plot_grid(p2_1, p2_2, p2_3, p2_4, labels = "")
    
    return(p2)
  })  
  
  output$plot2 <- renderPlot(p2())
  
  p3 = reactive({
    p3 <- df()$df %>%
      group_by(MonthOfStartingOfTheBusiness, PKDMainSection) %>% tally() %>%
      rowwise() %>%
      mutate(PKDMainSection = getGroupText(PKDMainSection)) %>%
      ggplot(aes(x=MonthOfStartingOfTheBusiness, y=n)) +
      geom_bar(stat="identity", position = 'fill', aes(fill = PKDMainSection)) +
      labs(
        title = "Udział poszczególnych rodzajów działalności gospodarczej w odniesieniu do miesiąca rozpoczęcia",
        x = "Miesiąc rozpoczęcia działalności",
        y = "Udziału danej kategorii"
      ) +
      theme_basic() +
      guides(fill=guide_legend(
        nrow=11,
        byrow=TRUE))
    return(p3)
  })
  
  output$plot3 <- renderPlot(p3())
  
  p4 = reactive({
    p4 <- df()$df_pkd_percent %>%
      group_by(PKDMainSection) %>%
      ggplot(aes(x=PKDMainSection, y=(df()$count + 1 - size), fill=getGroupText(PKDMainSection))) +
      geom_bar(stat="identity", width=1, color="white") +
      scale_y_continuous(limits = c(0, df()$count)) +
      geom_text(aes(label=size), position=position_dodge(width=0.9), vjust=-0.25) +
      labs(
        title = "Ranking według procentowego udziału poszczególnych rodzajów działalności gospodarczej",
        x = "Kod PKD",
        y = "Miejsce w porównaniu do innych jednostek"
      ) +
      theme_basic() +
      guides(fill=guide_legend(
        nrow=11,
        byrow=TRUE))
    return(p4)
  })
  
  output$plot4 <- renderPlot(p4())
  
  p5 = reactive({
    p5 <- df()$df_pkd_count %>%
      group_by(PKDMainSection) %>%
      ggplot(aes(x=PKDMainSection, y=(df()$count + 1 - size), fill=getGroupText(PKDMainSection))) +
      geom_bar(stat="identity", width=1, color="white") +
      scale_y_continuous(limits = c(0, df()$count)) +
      geom_text(aes(label=size), position=position_dodge(width=0.9), vjust=-0.25) +
      labs(
        title = "Ranking według sumarycznego udziału poszczególnych rodzajów działalności gospodarczej",
        x = "Kod PKD",
        y = "Miejsce w porównaniu do innych jednostek"
      ) +
      theme_basic() +
      guides(fill=guide_legend(
        nrow=11,
        byrow=TRUE))
    return(p5)
  })
  
  
  output$plot5 <- renderPlot(p5())
  
  
  
  ### END PLOTS
  
  
  output$regionName <- renderText(df()$region_name)
  
  
  index_region = reactive({
    return(which(regions()@data[["JPT_KOD_JE"]] == regionID()))
  })
  
  
  output$statistics <- renderLeaflet({
    leaflet(regions()@polygons[[index_region()]]@Polygons[[1]]@coords,
            options = leafletOptions(
              zoomControl = FALSE,
              attributionControl=FALSE)) %>%
      addTiles() %>% 
      addPolygons()
  })
  # #   
  #   # output$statistics_people <- renderUI({
  #   #   str1 = "Liczba ludności:"
  #   #   str2 = "tu liczba"
  #   #   HTML(paste(str1, str2, sep = '<br/>'))
  #   #   })
  #   # output$statistics_area <- renderUI({
  #   #   str1 = "Powierzchnia:"
  #   #   str2 = "tu liczba"
  #   #   HTML(paste(str1, str2, sep = '<br/>'))
  #   # })
  #   # output$statistics_companies <- renderUI({
  #   #   str1 = "Liczba działalności:"
  #   #   str2 = nrow(df)
  #   #   HTML(paste(str1, str2, sep = '<br/>'))
  #   # })
  #   
  # })
  
  data_people = data.table::fread("data/dane_ludnosc.csv",
                                  encoding = "UTF-8",
                                  data.table = F,
                                  colClasses = c("character"))
  
  
  output$progressBox1 <- renderInfoBox({
    infoBox(
      "Liczba ludności",
      value = "Liczba osób",
      subtitle = data_people[data_people$AdressTERC == regionID(),]$people,
      width = NULL,
      icon = icon("male"),
      fill = FALSE#male, globe, mail-bulk, balance-scale, dollar-sign, lightbulb
    )
  })
  
  output$progressBox2 <- renderInfoBox({
    
    infoBox(
      "",
      value = "Powierzchnia (km2)", 
      subtitle = paste(data_people[data_people$AdressTERC == regionID(),]$area,
                       "km2",sep=" "),
      
      width = NULL,
      icon = icon("globe") #male, globe, mail-bulk, balance-scale, dollar-sign, lightbulb
    )
  })
  
  # tmp = reactive({
  #   # df1 = ~df()
  #   # print(df1$df_)
  #   # print(nrow(df1))
  #   # return(nrow(df1$df_))
  # })
  
  output$progressBox3 <- renderInfoBox({
    
    infoBox(
      title = "Liczba działalności", value = "Liczba działalności", subtitle = nrow(df()$df),
      width = NULL,
      icon = icon("user-tie") #male, globe, mail-bulk, balance-scale, dollar-sign, lightbulb
    )
  })
  
  
  output$liczba_ludnosci = renderUI({
    withMathJax(sprintf("$$\\ %s$$
                        ",
                        data_people[data_people$AdressTERC == regionID(),]$people))
    
  })
  
  output$powierzchnia = renderUI({
    # withMathJax(helpText('$$\\mathbf{km}^{2}$$'))
    # # paste(data_people[data_people$AdressTERC == regionID(), ]$area,
    # #       "km2", sep=" ")
    withMathJax(sprintf("$$ %s \\ \\textrm{km}^{2}$$
                        ",
                        data_people[data_people$AdressTERC == regionID(), ]$area))
  })
  
  output$liczba_dzialalnosci = renderUI({
    withMathJax(sprintf("$$ %i $$
                        ",
                        (nrow(df()$df))[1]))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
