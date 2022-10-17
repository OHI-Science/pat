library(readxl)
library(scales)

####P anomalia de t°
presion_anomalia <- read_excel("prep/_pressures/sea_temp_anom_pat2021.xlsx")

cc_anomalia<- cbind(presion_anomalia, "score" = c(presion_anomalia$t_grados/ 0.8389650 ))
## Punto de referencia : 0.8389650 en Quemchi
cc_anomalia<- select(cc_anomalia, c("rgn_id", pressure_score="score"))

write.csv(cc_anomalia, "comunas/layers/ss_sst.csv", row.names = F)


####P acidificación
presion_acid <- read_excel("prep/_pressures/acid_sea_pat2021.xlsx")

cc_acid<- cbind(presion_acid ,"pressure_score" = c(presion_acid$mean / 0.4401299 ))
## Punto de referencia : 0.4401299 en los Muermos

cc_acid<- select(cc_acid, c(rgn_id = "OBJECTID", "pressure_score"))

write.csv(cc_acid, "comunas/layers/cc_acid_pat2021.csv", row.names = F)


####P aumento nivel del mar
presion_anomalia <- read_excel("prep/_pressures/sea_temp_anom_pat2021.xlsx")

cc_anomalia<- cbind(presion_anomalia, "score" = c(presion_anomalia$t_grados/ 0.8389650 ))
## Punto de referencia : 0.8389650 en Quemchi
cc_anomalia<- select(cc_anomalia, c("rgn_id", pressure_score="score"))

write.csv(cc_anomalia, "comunas/layers/ss_sst.csv", row.names = F)











