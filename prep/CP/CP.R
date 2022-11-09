library(dplyr)
library(readxl)
library(reshape2)

name<-read_excel("prep/CP/Proteccion_costera.xlsx", sheet = "PS")
name<-select(name, rgn_id, Nombre)
meta<-read_excel("prep/CP/Proteccion_costera_v2.xlsx", sheet = "BTS")
meta<- merge(meta, name)

mt<- meta %>%  select(-c(Nombre))

mt<- melt(mt, id.vars = c("rgn_id") )
mt<- rename(mt, year = "variable", health = "value")
mt$year<- as.character(mt$year)
mt$year<- as.numeric(mt$year)

mt<- mt %>% mutate(habitat = 'Tepu') %>%


cp_habitat_trend_pat2021<-cp_habitat_trend_pat2021 %>%
  mutate(year= 2021) %>%
  select(rgn_id, year, habitat, trend)


mt1<- rbind(mt1, mt)

write.csv(cp_habitat_trend_pat2021, "comunas/layers/cp_habitat_trend_pat2021.csv", row.names = F)


pres<- cp %>%  filter(habitat == "Macrocystis")
pres <- na.omit(pres)
list<- data.frame(table(pres$rgn_id))

trend_hab <- data.frame()
for (i in c(list$Var1)) {
  t<- pres %>% filter(rgn_id == i)
  mdl = lm(km2 ~ year, data = t)
  trend =  coef(mdl)[['year']] * sd(pres$year) / sd(pres$km2)
  sector_trend = pmax(-1, pmin(1, trend * 5))
  d<- data.frame(halpern.trend = sector_trend, coef.Beta0 = coef(mdl)[['(Intercept)']],  coef.year = coef(mdl)[['year']])
  trend = data.frame(rgn_id = i, trend = d$halpern.trend)
  trend_hab = rbind(trend_hab, trend)
}

trend_hab<- trend_hab %>% mutate(habitat = 'Macrocystis')%>%
  select(rgn_id, habitat, trend)

hab_trend<- rbind(hab_trend, trend_hab)


hab_trend$rgn_id<- as.numeric(hab_trend$rgn_id)















