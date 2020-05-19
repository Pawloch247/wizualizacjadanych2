# skrypt instalujący wszystkie potrzebne liby

# żeby ta ostatnia liba przeszła będą potrzebne pakiety dodatkowe (przynajmniej na Debianie/Ubuntu):
# sudo apt update && sudo apt install libgdal-dev libxml2-dev libudunits2-dev libv8-dev libfontconfig1-dev libcairo2-dev

install.packages("readxl") # czytanie XLSX
install.packages("remotes")
install.packages("sf", "rgeos", "PROJ") # ubuntu 18.04 was unhappy w/o this and the following line
devtools::install_github("r-spatial/lwgeom", force = T)

remotes::install_github("statisticspoland/R_Package_to_API_BDL") # API do GUS
# to przy okazji instaluje dplyr, ggplot, shiny i wszystkich świętych

install.packages("hrbrthemes") # ładne motywy do wykresów
install.packages("rmapshaper") # utils/maps_simplifier, hard to install, read carefully errors
install.packages("leaflet.extras") # search bar
install.packages("formattable")
