# Adding columns for coloring choropleth
# such as number of registered enterprises for voivodeship/county
library(dplyr)
library(rgdal)
library(rmapshaper)

source("utils/load_and_filter.R")
data <- loadAndFilterCeidg("data/")

data %>% filter(!is.na(AdressTERC)) %>% 
  mutate(VoivodeshipCode = substr(AdressTERC, 1, 2),
         CountyCode = substr(AdressTERC, 1, 4)) -> data2

voivodeship <- data2 %>% 
  group_by(VoivodeshipCode) %>% 
  mutate(CountVoivodeship = n()) %>% 
  select(AdressVoivodeship, CountVoivodeship, VoivodeshipCode) %>% 
  unique()

county <- data2 %>% 
  group_by(CountyCode) %>% 
  mutate(CountCounty = n()) %>% 
  select(AdressVoivodeship, CountCounty, CountyCode) %>% 
  unique()
  
 
map <- readOGR("maps/wojewodztwa.shp")
map <- sp::merge(map, voivodeship, by.x = "JPT_KOD_JE", by.y = "VoivodeshipCode")
writeOGR(map, "maps/wojewodztwa2.shp", driver = "CSV", layer = "wojewodztwa2", encoding = "UTF-8")


z <- readOGR("maps/wojewodztwa.shp")

?ms_s

county <- data2 %>% select(CountyCode, CountCounty, AdressCounty) %>% unique()

data2  %>% filter(is.na(CountyCode)) -> temp


ogrDrivers()

myPalette <- colorNumeric("YlOrRd", domain = map2$CountVoivodeship)

labels <- sprintf(
  "<strong>%s</strong><br/>%g  registered enterprises",
  map2$AdressVoivodeship, map2$CountVoivodeship
) %>% lapply(htmltools::HTML)
  
  

leaflet(map2)  %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(layerId = map2$JPT_KOD_JE,
                fillColor = ~myPalette(map2$CountVoivodeship),
                fillOpacity = 0.3,
                weight = 1,
                color = "grey",
                dashArray = "3",
                highlightOptions = highlightOptions(color = "#666", 
                                                    weight = 2,
                                                    bringToFront = TRUE,
                                                    fillOpacity = "0.3"),
                label = labels) %>% 
  addLegend(pal = myPalette, values = ~CountVoivodeship, opacity = 0.7, title = "No. of enterprises",
                                              position = "bottomright")


