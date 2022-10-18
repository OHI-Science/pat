

####P anomalia de t°
resiliencia <- read_excel("prep/_resilience/ERNC_pat2021.xlsx")

resiliencia<- cbind(resiliencia, "resilience.score" = c(resiliencia$consumo/ 49.73351 ))
## Punto de referencia : 49.73351 en Ancud
cc_anomalia<- select(cc_anomalia, c("rgn_id", pressure_score="score"))

write.csv(cc_anomalia, "comunas/layers/ss_sst.csv", row.names = F)








