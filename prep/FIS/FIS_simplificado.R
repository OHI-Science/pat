### ESTADO DE BASE  2017- 2021 ###

# Xfis= estado de las pesquerias pescadas en la naturaleza
# SS= puntuaciones de estado de las pesquerias
# B/Bmsy=indicador para informar SS
# C= contribucion de la poblaci?n a la captura total


### from script FUNCTIONS global de OHI core
####
# STEP 1. Merge the b/bmsy data with catch data
####

c <- read.csv2("prep/FIS/C_Landings.csv",header=T,dec=",",sep=";")

b <- read.csv2("prep/FIS/fis_b_bmsy_pat2022.csv",header=T,dec=",",sep=";")

data_fis <- c %>%
  dplyr::left_join(b, by = c('rgn_id', 'Spp', 'Year')) %>%
  dplyr::select(rgn_id, Spp, Year, SumLandings, bbmsy)


###
# STEP 2. Estimate scores for taxa without b/bmsy values
# Median score of other fish in the region is the starting point
# Then a penalty is applied based on the level the taxa are reported at
###

## this takes the mean score within each region and year
##
data_fis_gf <- data_fis %>%
  dplyr::group_by(rgn_id, Year) %>%
  dplyr::mutate(mean_score = mean(bbmsy, na.rm = TRUE)) %>%
  dplyr::ungroup()


## this takes the mean score across all regions within a year
# (when no stocks have scores within a region)

data_fis_gf2 <- data_fis_gf %>%
  dplyr::group_by(Year) %>%
  dplyr::mutate(mean_score_global = mean(bbmsy, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(mean_score = ifelse(!is.na(bbmsy), bbmsy, mean_score_global))

data_fis_gf3 <- data_fis_gf2 %>%
  mutate(mean_score = ifelse(!is.na(mean_score), mean_score, mean_score_global))

### step 3. seleccionamos aquellas columnas que nos interesan
#adaptacion PAT
status_data <- data_fis_gf3 %>%
  dplyr::select(rgn_id, Spp, Year, SumLandings, mean_score)
###
# STEP 4. Calculate status for each region
###

# 4a. To calculate the weight (i.e, the relative catch of each stock per region),
# the mean catch of taxon i is divided by the
# sum of mean catch of all species in region/year
## adapt to pat
status_data <- status_data %>%
  dplyr::group_by(Year, rgn_id) %>%
  dplyr::mutate(SumCatch = sum(SumLandings)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(wprop = SumLandings / SumCatch)

status_data$mean_score<- as.numeric(status_data$mean_score)

status_data_final <- status_data %>%
  dplyr::group_by(rgn_id, Year) %>%
  dplyr::summarize(status = prod(na.omit(mean_score ^ wprop))) %>%
  dplyr::ungroup()



### codigo para calcular tendencia de web:https://ohi-science.org/manual/#file-system-organization
r.trend <- status_final_names %>%
  group_by(rgn_id) %>%
  do(mdl = lm(status ~ Year, data=.)) %>%
  summarize( rgn_id = rgn_id,
             trend = coef(mdl)['Year']*5)

### seleccionamos estado actual, a?o 2021
selectStatus2021 <- status_data_final %>%
  filter(Year==2021)%>%
  mutate(EstadoActual=status*100)

###juntamos estado actual y tendencia
status2021_trend <- left_join(selectStatus2021,r.trend, by="rgn_id")

###grabamos resultados
write.csv2(status2021_trend, "FIS_status2021_trend.csv")
