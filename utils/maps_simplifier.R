library(rmapshaper)
library(rgdal)
library(dplyr)
library(leaflet)

wojewodztwa = readOGR("../maps/wojewodztwa.shp",
                      encoding = "UTF-8")
powiaty = readOGR("../maps/powiaty.shp",
                  encoding = "UTF-8")

woj_simp = ms_simplify(wojewodztwa, keep = 0.01)
writeOGR(woj_simp, ".", "../maps/wojewodztwa", driver = "ESRI Shapefile")

pow_simp = ms_simplify(powiaty, keep = 0.005)
writeOGR(pow_simp, ".", "../maps/powiaty", driver = "ESRI Shapefile")
