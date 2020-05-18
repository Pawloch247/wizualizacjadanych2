# Adding columns for coloring choropleth
# such as number of registered enterprises for voivodeship/county
library(dplyr)
library(rgdal)
library(rmapshaper)

source("utils/load_and_filter.R")
data <- loadAndFilterCeidg("data/")

# columns with TERC code - prepare for join
data %>% filter(!is.na(AdressTERC)) %>% 
  mutate(VoivodeshipCode = substr(AdressTERC, 1, 2),
         CountyCode = substr(AdressTERC, 1, 4)) -> data2

voivodeship <- data2 %>% 
  group_by(VoivodeshipCode) %>% 
  mutate(CountVoivodeship = n()) %>% 
  select(Name = AdressVoivodeship, No_Buss = CountVoivodeship, VoivodeshipCode) %>% 
  unique() 

county <- data2 %>% 
  group_by(CountyCode) %>% 
  mutate(CountCounty = n()) %>% 
  select(Name = AdressCounty, No_Buss = CountCounty, CountyCode) %>% 
  unique() %>% 
  filter(!(Name == "MRĄGOWSKI" & CountyCode == "2808"))


map <- readOGR("maps/wojewodztwa.shp")
map <- sp::merge(map, voivodeship, by.x = "JPT_KOD_JE", by.y = "VoivodeshipCode")
writeOGR(map, ".", driver = "ESRI Shapefile", 
         layer = "maps/wojewodztwa", overwrite_layer = TRUE,
         layer_options = "ENCODING=UTF-8")

map <- readOGR("maps/Powiaty.shp")
map <- sp::merge(map, county, by.x = "JPT_KOD_JE", by.y = "CountyCode")
writeOGR(map, ".", driver = "ESRI Shapefile", 
         layer = "maps/Powiaty", overwrite_layer = TRUE,
         layer_options = "ENCODING=UTF-8")





