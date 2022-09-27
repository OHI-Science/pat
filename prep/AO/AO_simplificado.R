
###librerias necesarias para correr esta meta:
library(tidyverse)
library(reldist)
library(zoo)

### esta meta se compone basicamente de 3 factores: 1)El coeficiente de Gini de los desembarques,
###2)coeficiente de sostenibilidad de las artes usadas, y 3)el n?mero de altas o nuevas entradas al RPA

### STEP 1. CARGAMOS LA DF DE LA INVERSA DE GINI DE DESEMBARQUES


gini_All_inv <- read.csv2("prep/AO/GiniInv_Desembarques1997_2021.csv",header=T,dec=",",sep=";")
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "R\xcdO VERDE"] <- "RIO VERDE"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "CURACO DE V\xc9LEZ"] <- "CURACO DE VELEZ"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "R\xcdO NEGRO"] <- "RIO NEGRO"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "QUELL\xd3N"] <- "QUELLON"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "COCHAM\xd3"] <- "COCHAMO"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "HUALAIHU\xc9"] <- "HUALAIHUE"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "PUQUELD\xd3N"] <- "PUQUELDON"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "CABO DE HORNOS(NAVARINO)"] <- "CABO DE HORNOS"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "AYS\xc9N"] <- "AYSEN"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "MAULL\xcdN"] <- "MAULLIN"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "QUEIL\xc9N"] <- "QUEILEN"
gini_All_inv["Comuna"][gini_All_inv["Comuna"] == "CHAIT\xc9N"] <-  "CHAITEN"
region_list<- cbind(region_list, "Comuna" = toupper(region_list$rgn_name))
region_list<- select(region_list, c("rgn_id", "Comuna"))

gini<- merge(gini_All_inv, region_list)
### STEP 2. COEFICIENTE DE SOSTENIBILIDAD DE LAS ARTES DE PESCA

Sust_coef_Artes<- read.csv2("prep/AO/Artes_sust_coef.csv",header=T,dec=",",sep=";")
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "R\xcdO VERDE"] <- "RIO VERDE"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "CURACO DE V\xc9LEZ"] <- "CURACO DE VELEZ"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "R\xcdO NEGRO"] <- "RIO NEGRO"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "QUELL\xd3N"] <- "QUELLON"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "COCHAM\xd3"] <- "COCHAMO"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "HUALAIHU\xc9"] <- "HUALAIHUE"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "PUQUELD\xd3N"] <- "PUQUELDON"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "CABO DE HORNOS(NAVARINO)"] <- "CABO DE HORNOS"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "AYS\xc9N"] <- "AYSEN"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "MAULL\xcdN"] <- "MAULLIN"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "QUEIL\xc9N"] <- "QUEILEN"
Sust_coef_Artes["Comuna"][Sust_coef_Artes["Comuna"] == "CHAIT\xc9N"] <-  "CHAITEN"
Sust_artes<- merge(Sust_coef_Artes, region_list)

### STEP 3. CARGAMOS DF DE RPA

RPA_births<- read.csv2("prep/AO/RPA_births.csv",header=T,dec=".",sep=",")
RPA_births["Comuna"][RPA_births["Comuna"] == "R\xcdO VERDE"] <- "RIO VERDE"
RPA_births["Comuna"][RPA_births["Comuna"] == "CURACO DE V\xc9LEZ"] <- "CURACO DE VELEZ"
RPA_births["Comuna"][RPA_births["Comuna"] == "R\xcdO NEGRO"] <- "RIO NEGRO"
RPA_births["Comuna"][RPA_births["Comuna"] == "QUELL\xd3N"] <- "QUELLON"
RPA_births["Comuna"][RPA_births["Comuna"] == "COCHAM\xd3"] <- "COCHAMO"
RPA_births["Comuna"][RPA_births["Comuna"] == "HUALAIHU\xc9"] <- "HUALAIHUE"
RPA_births["Comuna"][RPA_births["Comuna"] == "PUQUELD\xd3N"] <- "PUQUELDON"
RPA_births["Comuna"][RPA_births["Comuna"] == "CABO DE HORNOS(NAVARINO)"] <- "CABO DE HORNOS"
RPA_births["Comuna"][RPA_births["Comuna"] == "AYS\xc9N"] <- "AYSEN"
RPA_births["Comuna"][RPA_births["Comuna"] == "MAULL\xcdN"] <- "MAULLIN"
RPA_births["Comuna"][RPA_births["Comuna"] == "QUEIL\xc9N"] <- "QUEILEN"
RPA_births["Comuna"][RPA_births["Comuna"] == "CHAIT\xc9N"] <-  "CHAITEN"
RPA<- merge(RPA_births, region_list)


### step 4 juntamos df's para aplicar equaciónn de la meta

OA_data_final <- merge(RPA, gini, by.x=c("rgn_id", "year"), by.y=c("rgn_id","year"), all=T)

OA_data_final_sust <- merge(OA_data_final, Sust_artes, by.x=c("rgn_id","year"), by.y=c("rgn_id","year"), all=T)

OA_data_final2 <- OA_data_final_sust  %>%
  mutate(rollmeanBirths=(zoo::rollapply(normperct,5, mean, na.rm=T, partial=T)),
         rollmeanGini=(zoo::rollapply(Gini_inv,5, mean, na.rm=T, partial=T)),
         Xao=((rollmeanGini+rollmeanBirths+Sustcoef_norm)/3))

### Calculo de la tendencia de forma manual, POR FAVOR INTENTA CORRER LA FORMULA DEL PAQUETE OHICORE: Calculatetrend, pq a mi me est? dando problemas el c?lculo manual

### codigo para calcular tendencia de web:https://ohi-science.org/manual/#file-system-organization
AO_2017_2021 <- OA_data_final2 %>%
  filter(year %in% (2017:2021)) %>%
  dplyr::rename(region_id = "rgn_id", status = "Xao") %>%
  dplyr::select(region_id,year,status)

str(AO_2017_2021)


## trend
scen_year<- 2021
trend_years <- (scen_year - 4):(scen_year)

trend <-
  CalculateTrend(status_data = AO_2017_2021, trend_years = trend_years)

#status
status <-  AO_2017_2021 %>%
  dplyr::filter(year == scen_year) %>%
  dplyr::mutate(score     = round(status * 100, 1),
                dimension = 'status') %>%
  dplyr::select(region_id, score, dimension)


trend<- trend  %>%  rename(region_id = "rgn_id")

# assemble dimensions
scores <- rbind(status, trend) %>%
  dplyr::mutate(goal = 'AO')

