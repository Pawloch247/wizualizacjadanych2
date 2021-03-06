loadAndFilterCeidg <- function(path) {
  library(dplyr)
  library(R.utils)

  # bdl_dict <- readxl::read_excel(paste(path, "bdl_dictionary.xlsx", sep = ""))
  # ceidg <- read.csv(paste(path,"ceidg_data_classif.csv", sep = ""), stringsAsFactors=FALSE, encoding = "UTF-8")
  
  ceidg <- data.table::fread(paste(path,"ceidg_data_classif.csv.gz", sep = ""),
                             stringsAsFactors = F,
                             encoding = "UTF-8",
                             data.table = F)
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


loadPKDMain <- function(path) {
  library(dplyr)

  pkd_main <- data.table::fread(paste(path,"pkd_main.csv", sep = ""),
                             stringsAsFactors = F,
                             encoding = "UTF-8",
                             data.table = F)
  return(pkd_main)
}


loadWojPercent <- function(path) {
  library(dplyr)
  woj_percent <- data.table::fread(paste(path,"woj_percent.csv", sep = ""),
                                colClasses=c("AdressTERC"="character"),
                                stringsAsFactors = F,
                                encoding = "UTF-8",
                                data.table = F)
  return(woj_percent)
}


loadWojCount <- function(path) {
  library(dplyr)
  woj_count <- data.table::fread(paste(path,"woj_count.csv", sep = ""),
                                   colClasses=c("AdressTERC"="character"),
                                   stringsAsFactors = F,
                                   encoding = "UTF-8",
                                   data.table = F)
  return(woj_count)
}

loadPowPercent <- function(path) {
  library(dplyr)
  pow_percent <- data.table::fread(paste(path,"pow_percent.csv", sep = ""),
                                   colClasses=c("AdressTERC"="character"),
                                   stringsAsFactors = F,
                                   encoding = "UTF-8",
                                   data.table = F)
  return(pow_percent)
}


loadPowCount <- function(path) {
  library(dplyr)
  pow_count <- data.table::fread(paste(path,"pow_count.csv", sep = ""),
                                 colClasses=c("AdressTERC"="character"),
                                 stringsAsFactors = F,
                                 encoding = "UTF-8",
                                 data.table = F)
  return(pow_count)
}



