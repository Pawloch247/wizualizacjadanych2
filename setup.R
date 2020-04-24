# skrypt instalujący wszystkie potrzebne liby

install.packages("readxl") # czytanie XLSX
install.packages("remotes")
remotes::install_github("statisticspoland/R_Package_to_API_BDL") # API do GUS
# to przy okazji instaluje dplyr, ggplot, shiny i wszystkich świętych