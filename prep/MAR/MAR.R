
library(readxl)
library(dplyr)
library(tidyr)
library(reshape2)
library(readr)

scen_year <- layers$data$scenario_year

mar_sust <-
  AlignDataYears(layer_nm = "mar_sustainability", layers_obj = layers) %>%
  dplyr::select(rgn_id,species, sust_coeff)

mar_harvest <-
  AlignDataYears(layer_nm = "mar_harvest_tonnes", layers_obj = layers) %>%
  dplyr::select(rgn_id,species, year = mar_harvest_tonnes_year, tonnes)



c1<- merge(mar_harvest, mar_sust)

# 4-year rolling mean of data
c2 <- c1 %>%
  dplyr::group_by(rgn_id, species) %>%
  dplyr::arrange(rgn_id, species, year) %>%
  dplyr::mutate(sm_tonnes = zoo::rollapply(tonnes, 4, mean, na.rm = TRUE, partial =
                                             TRUE, align = "right")) %>%
  dplyr::ungroup()

##Punto de referencia
pto_ref<- c2 %>% group_by(rgn_id, species) %>%
  summarise(pto_max = max(sm_tonnes))%>%
  group_by(rgn_id) %>%
  summarise(pto_ref = sum(pto_max)) %>%
  mutate(Punto_ref = pto_ref*0.01) %>%
  select(rgn_id, Punto_ref)

##Para calcular el estado
c3<- c2 %>%
  dplyr::filter(year %in% c(2017:2021)) %>%
  mutate(mult = sm_tonnes* sust_coeff) %>%
  group_by(rgn_id, year) %>%
  mutate(YC = sum(mult)) %>%
  select(rgn_id, year, YC)

c3<-c3[!duplicated(c3), ]


c4<- merge(c3, pto_ref)

status<-c4 %>%
  mutate(status = YC/Punto_ref) %>%
  select(rgn_id, year, status)

# status
status_a <- status %>%
  dplyr::filter(year == scen_year) %>%
  dplyr::mutate(dimension = "status") %>%
  dplyr::select(region_id = rgn_id, score = status, dimension)


trend_years <- (scen_year - 4):(scen_year)

trend <- CalculateTrend(status_data = status, trend_years = trend_years)


# return scores
scores = rbind(status_a, trend) %>%
  dplyr::mutate(goal = 'MAR')


