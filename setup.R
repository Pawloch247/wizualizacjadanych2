# skrypt instalujący wszystkie potrzebne liby

# żeby ta ostatnia liba przeszła będą potrzebne pakiety dodatkowe (przynajmniej na Debianie/Ubuntu):
# sudo apt update && sudo apt install libgdal-dev libxml2-dev libudunits2-dev libv8-dev

install.packages("readxl") # czytanie XLSX
install.packages("remotes")
remotes::install_github("statisticspoland/R_Package_to_API_BDL") # API do GUS
# to przy okazji instaluje dplyr, ggplot, shiny i wszystkich świętych