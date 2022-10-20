library(dplyr)

####consumo de ER
resiliencia <- read_excel("prep/_resilience/ERNC_pat2021.xlsx")

resiliencia<- resiliencia %>% mutate("resilience_score" = c(consumo/max(consumo) ))
## Punto de referencia : 49.73351 en Ancud
resiliencia<- select(resiliencia, c("rgn_id", "resilience_score"))

write.csv(resiliencia, "comunas/layers/cc_consumption_ernc_pat2021.csv", row.names = F)


####P anomalia de t°
resiliencia <- read_excel("prep/_resilience/research_invest_pat2021.xlsx")

resiliencia<- resiliencia %>% mutate("resilience_score" = c(MONTO/max(MONTO) ))
## Punto de referencia : 11460357 en Ancud
resiliencia<- select(resiliencia, c("rgn_id", "resilience_score"))

write.csv(resiliencia, "comunas/layers/cc_consumption_ernc_pat2021.csv", row.names = F)





