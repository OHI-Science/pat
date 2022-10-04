library(readxl)
library(scales)

####P anomalia de t°
presión_anomalía <- read_excel("prep/_pressures/presión_anomalía.xlsx")

cc_anomalia<- cbind(presión_anomalía, "score" = c(presión_anomalía$`Tº grados` / max(presión_anomalía$`Tº grados`) )) 
## Punto de referencia : 0.8389650 en Quemchi
cc_anomalia<- select(cc_anomalia, c("rgn_id", "score"))

write.csv(cc_anomalia, "prep/_pressures/cc_anomalia.csv", row.names = F)


####P erosión del suelo 
presión_erosión <- read_excel("prep/_pressures/presión_erosión.xlsx")

cc_erosion<- cbind(presión_erosión, "score" = c(presión_erosión$`área (km2)` / max(presión_erosión$`área (km2)`) )) 
## Punto de referencia : 2052.1857648 en Natales
cc_erosion<- select(cc_erosion, c("rgn_id", "score"))

write.csv(cc_erosion, "prep/_pressures/cc_erosion.csv", row.names = F)

####P temperatura 
presión_temperatura <- read_excel("prep/_pressures/presión_temperatura.xlsx")

cc_temperatura<- cbind(presión_temperatura, "score" = c(presión_temperatura$` Índice de aumento de temperatura media  `/ max(presión_temperatura$` Índice de aumento de temperatura media  `) )) 
## Punto de referencia : 0.1883 en Torres del Paine
cc_temperatura<- select(cc_temperatura, c("rgn_id", "score"))

write.csv(cc_temperatura, "prep/_pressures/cc_temperatura.csv", row.names = F)

####P acidificación 
presión_acid <- read_excel("prep/_pressures/presión_acid_ocean_halpern.xlsx")

cc_acid<- cbind(presión_acid, "score" = c(na.omit(presión_acid$`MEAN impact halpern`/ max(presión_acid$`MEAN impact halpern`) )) )
## Punto de referencia : 0.44012991 en los Muermos 

cc_acid<- select(cc_acid, c("rgn_id", "score"))

write.csv(cc_acid, "prep/_pressures/cc_acid.csv", row.names = F)













