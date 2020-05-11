library(rgdal)

wojewodztwa = readOGR("./maps/wojewodztwa.shp",
                      encoding = "UTF-8")
powiaty = readOGR("./maps/powiaty.shp",
                  encoding = "UTF-8")

kody_woj = data.frame(id = wojewodztwa$JPT_KOD_JE,
                      name = toupper(wojewodztwa$JPT_NAZWA_))
kody_pow = data.frame(id = powiaty$JPT_KOD_JE,
                      name = sub("powiat ",
                                 "",
                                 toupper(powiaty$JPT_NAZWA_),
                                 ignore.case = TRUE))

kody = rbind(kody_woj, kody_pow)
write.csv(kody, "../data/codes.csv", row.names = FALSE, fileEncoding = "UTF-8")

df = loadAndFilterCeidg("/data/ceidg_data_classif.csv")

