loadAndFilterCeidg <- function(path) {
  library(dplyr)
  print(paste(path, "bdl_dictionary.xlsx", sep = ""))
  bdl_dict <- readxl::read_excel(paste(path, "bdl_dictionary.xlsx", sep = ""))
  ceidg <- read.csv(paste(path,"ceidg_data_classif.csv", sep = ""), stringsAsFactors=FALSE, encoding = "UTF-8")
  
  sprintf('przed filtrowaniem: %d', nrow(ceidg))
  
  # wiersze bez kodów PKD raczej nam się nie przydadzą
  ceidg <- ceidg %>% filter(PKDMainSection != '')
  
  # wybierz adres główny, a jeśli go nie ma to korespondecyjny
  # usuń zbędne kolumny, pomiń firmy bez adresu
  # znormalizuj nazwy województw i powiatów do uppercase
  ceidg <- ceidg %>%
    mutate(AdressVoivodeship = toupper(if_else(MainAddressVoivodeship == '', CorrespondenceAddressVoivodeship, MainAddressVoivodeship))) %>%
    mutate(AdressCounty = toupper(if_else(MainAddressVoivodeship == '', CorrespondenceAddressCounty, MainAddressCounty))) %>%
    mutate(AdressTERC = if_else(MainAddressVoivodeship == '', CorrespondenceAddressTERC, MainAddressTERC)) %>%
    mutate(AdressTERC = if_else(floor(log10(AdressTERC)) + 1 == 7, as.character(AdressTERC), paste("0", AdressTERC, sep = ""))) %>% 
    select(-c(MainAddressVoivodeship, MainAddressCounty, MainAddressTERC, CorrespondenceAddressVoivodeship, CorrespondenceAddressCounty, CorrespondenceAddressTERC)) %>%
    filter(AdressVoivodeship != '')
  
  sprintf('po filtrowaniu: %d', nrow(ceidg))
  
  return(ceidg)
}
getwd()
