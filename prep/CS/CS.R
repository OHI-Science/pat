
library(dplyr)
library(reshape2)

cs<- select(cs, -c(rgn_name))
cs<- melt(cs, id.vars = c("rgn_id"))

##Punto de ref

p_ref<- cs %>%
  group_by(year) %>%
  summarise(max = max(value)) %>%
  mutate(p_ref = c(max+ (max*0.1))) %>%
  select(year, p_ref)


##Calculo del score
cs<- merge(cs, p_ref)

cs_scores<- cs %>%
  mutate(status = value / p_ref) %>%
  select(region_id = "rgn_id", year, status)

## Estado actual
cs_status<- cs_scores %>%
  filter(year == scen_year) %>%
  select(region_id , score ="status") %>%
  dplyr::mutate(dimension = 'status')

##Trend
trend_years <- (scen_year - 4):(scen_year)
cs_trend <-
  CalculateTrend(status_data =cs_scores, trend_years = trend_years)



cs_score <- dplyr::bind_rows(cs_status, cs_trend) %>%
  dplyr::mutate(goal = 'CS')























