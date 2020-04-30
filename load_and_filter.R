loadAndFilterCeidg <- function(path) {
  library(dplyr)
  bdl_dict <- readxl::read_excel("data/bdl_dictionary.xlsx")
  ceidg <- read.csv("data/ceidg_data_classif.csv", stringsAsFactors=FALSE)
  
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
    select(-c(MainAddressVoivodeship, MainAddressCounty, MainAddressTERC, CorrespondenceAddressVoivodeship, CorrespondenceAddressCounty, CorrespondenceAddressTERC)) %>%
    filter(AdressVoivodeship != '')
  
  sprintf('po filtrowaniu: %d', nrow(ceidg))
  
  return(ceidg)
}
