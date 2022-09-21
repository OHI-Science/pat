

library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)


trash_pressure <- read_excel("prep/CW/trash_pressure.xlsx")

#Ordenar la tabla en formato largo
pres <-melt(cw_fertilizer_trend, id.vars = c("rgn_id"))


fert- rename(fert, year = "variable")

#Tendencia
pres_trend <- data.frame()
for (i in c(1:36)) {
  t<- pres %>% filter(rgn_id == i)
  mdl = lm(value ~ year, data = t)
  trend =  coef(mdl)[['year']] * sd(trash$year) / sd(trash$value)
  sector_trend = pmax(-1, pmin(1, trend * 5))
  d<- data.frame(halpern.trend = sector_trend, coef.Beta0 = coef(mdl)[['(Intercept)']],  coef.year = coef(mdl)[['year']])
  trend = data.frame(rgn_id = i, trend = d$halpern.trend)
  pres_trend = rbind(trash_trend, trend)
}







