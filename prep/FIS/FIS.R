#### OHI PROVISION DE ALIMENTOS- PESCA ARTESANAL
devtools::install_github('ohi-science/ohicore@dev')
install.packages("Rtools")

library(tidyverse)
library(ohicore)
library(dplyr)
### ESTADO DE BASE  2017- 2021 ###

# Xfis= estado de las pesquerias pescadas en la naturaleza
# SS= puntuaciones de estado de las pesquerias
# B/Bmsy=indicador para informar SS
# C= contribucion de la poblaci?n a la captura total

# 1st datos provenientes de los desembarques SERNAPESCA; BBDD dFull1997_2021, dataframe ya creado dentro de script RPALandings_Full
#en caso de no tenerlo cargado, crear ruta:

setwd("D:/Dropbox/IDEAL/R projects/Chile_PE")

##### LOS DATASETS FUERON ENTREGADOS POR SERNAPESCA. SON DE DOS ORIGENES DIFERENTES, UNO XSL DESDE 2010 A 2021 Y OTRO DE ACCESS DESDE 1997 A 2009, 
#####LOS HOMOGENIZE PARA QUE TUVIESEN MISMO NUMERO DE COLUMNAS. LA UNIDAD DE OBSERVACION ES LA SALIDA A PESCAR POR BARCO
dataFull_2010_2021 <- read.csv2("LandingsFull_2010_2021.csv",header=T,dec=",",sep=";")
dataFull_1997_2009 <- read.table("prep/FIS/DESEMBARQUES 1997-2009.txt", header=F,dec=",",sep=";")

colnames(dataFull_1997_2009) <- colnames(dataFull_2010_2021)# cambia el nombre de las columnas 

dFull1997_2021 <- rbind(dataFull_2010_2021, dataFull_1997_2009)#junta ambos data sets

dFull1997_2021 <- dFull1997_2021 %>% 
  mutate(Land=str_replace_all(Landings_Ton, ",","."))


###SEPARA UNA DE LAS COUMNAS DE FECHA PARA DIVIDIRLA EN DIA, MES Y AnO PARA PODER AGRUPAR DATOS
dFull1997_2021b <- dFull1997_2021 %>% 
  separate(FechaLlegada, into = c("Date", "Time"), sep = " ", remove = FALSE)


dFull1997_2021c <- dFull1997_2021b %>% 
  separate("Date", into=c("Year","Month","Day"), sep="-") %>% 
  mutate(Month=str_replace(Month, "^0",""))

str(dFull1997_2021c)
unique(dFull1997_2021c$Year)
dFull1997_2021c$Landings_Ton <- as.numeric(dFull1997_2021c$Landings_Ton)
dFull1997_2021c$Land <- as.numeric(dFull1997_2021c$Land)

filt_dFull2017_2021 <- filter(dFull1997_2021c, Year>=2017)#filtramos s?lo los a?os del periodo de estudio
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "PALENA"] <- "CHAITEN"#asignamos palena con chait?n
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "AYSÉN"] <- "AYSEN"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "CABO DE HORNOS(NAVARINO)"] <- "CABO DE HORNOS"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "CHAITÉN"] <- "CHAITEN"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "COCHAMÓ"] <- "COCHAMO"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "CURACO DE VELÉZ"] <- "CURACO DE VELEZ"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "HUAILAIHUÉ"] <- "HUALAIHUE"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "MAULLÍN"] <- "MAULLIN"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "PUQUELDÓN"] <- "PUQUELDON"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "QUEILÉN"] <- "QUEILEN"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "QUELLÓN"] <- "QUELLON"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "RÍO NEGRO"] <- "RIO NEGRO"
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "RÍO VERDE"] <- "RIO VERDE"

setwd("D:/Dropbox/IDEAL/R projects/OHI")

region_list <- read_csv("prep/FIS/region_list.csv")
comunas_id<- cbind( region_list, "rgn" = toupper(region_list$rgn_name))
comunas_id<- comunas_id %>% select(rgn, rgn_id)

dataFIS <- merge(filt_dFull2017_2021,comunas_id, by.x = "Comuna", by.y = "rgn", all=T)




c <- dataFIS  %>% 
  group_by(rgn_id,Year,Spp) %>% 
  summarise(Meanlandings=mean(Landings_Ton), SumLandings=sum(Landings_Ton))

write.csv(c, "prep/FIS/fis_meanlandings_pat2021.csv", row.names = F)

b <- read.csv2("fis_b_bmsy_pat2022.csv",header=T,dec=",",sep=";")


### from script FUNCTIONS global de OHI core
####
# STEP 1. Merge the b/bmsy data with catch data
####

c<- read_csv("prep/FIS/fis_meanlandings_pat2021.csv")
b<- read.csv2("prep/FIS/fis_b_bmsy_pat2022.csv",header=T,dec=",",sep=";")

c$Year <- as.numeric(c$Year)# necesito transformar el a?o como numerico

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

status_final_names <- merge(status_data_final,comunas_id, by.x="rgn_id", by.y="rgn_id")

### codigo para calcular tendencia de web:https://ohi-science.org/manual/#file-system-organization
r.trend <- status_final_names %>%
  group_by(rgn_id) %>%
  do(mdl = lm(status ~ Year, data=.)) %>%
  summarize( rgn_id = rgn_id,
             trend = coef(mdl)['Year']*5)

selectStatus2021 <- status_data_final %>%
  filter(Year==2021)%>%
  mutate(EstadoActual=status*100)

status2021_trend <- left_join(selectStatus2021,r.trend, by="rgn_id")

write.csv2(status2021_trend, "FIS_status2021_trend.csv")

comunas_sf_FIS <- left_join(comunas_sf,selectStatus2021, by="rgn_id")


ggplot() +
  geom_sf(data=comunas_sf_FIS, aes(fill = EstadoActual, geometry = geometry))+
  scale_fill_viridis_c("Estado actual")+
  labs(title = "Pesquerias")

### promedio del status total de todas las comunas por a?o
mean_status_year <- status_data_final %>%
  group_by(Year) %>%
  summarise(total_mean_score=mean(status))

ggplot(data=mean_status_year, aes(x=Year, y=total_mean_score))+
  geom_line(color="blue") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6) +
  theme_ipsum() +
  ggtitle("Status trend Fisheries sub-goal OHI")

### promedio del status total de todas las comunas EN EL TOTAL DE LOS A?OS
mean_status_ALL <- status_final_names %>%
  group_by(rgn_name, rgn_id) %>%
  summarise(total_mean_score=mean(status))

#para compartir los datos con meta NP_Vanessa 
write.csv(mean_status_ALL, "mean_status_FIS.csv")

#mean_status_ALL <- rename(mean_status_ALL, id_ohi=rgn_id)


ggplot()+
  geom_line(data=status_final_names, aes(x=Year, y=status))+
  facet_wrap(~rgn_name)

### plotting spatially status
setwd("D:/GISLAYERS/Chile/OHI")
comunas_sp <- readOGR("comunas_austral_pop.shp")
comunas_mar<- readOGR("rgn_id.shp")

comunas_sp_merged<-merge(comunas_mar@data, mean_status_ALL,by.x="rgn_id",by.y="id_ohi")

comunas_sp_fort <- fortify(comunas_sp)

library(sf)
comunas_sf <- st_read("rgn_id.shp")


comunas_sf_merged <- left_join(comunas_sf,mean_status_ALL, by="rgn_id")

ggplot() +
  geom_sf(data=comunas_sf_merged, aes(fill = total_mean_score, geometry = geometry))+
  scale_fill_viridis_c("Estado actual")



###
# STEP 5. Get yearly status and trend
###
scen_year<-2021

status <-  status_data_final %>%
  dplyr::filter(Year == scen_year) %>%
  dplyr::mutate(score     = round(status * 100, 1),
                dimension = 'status') %>%
  dplyr::select(rgn_id, score, dimension)

# calculate trend


trend_years <- (scen_year - 4):(scen_year)

trend <-
  CalculateTrend(status_data = status_data_final, trend_years = trend_years)


###

unique(cont_com_17_21_tot$Comuna)
unique(dFull1997_2021c$Spp)

filt_dFull2017_2021_spp <- filter(dFull1997_2021c, Year>=2017 & Spp  %in% c("MERLUZA DEL SUR O AUSTRAL", "JULIANA O TAWERA",
                                  "CENTOLLA", "ERIZO", "ALMEJA", "ANCHOVETA","REINETA", "SARDINA AUSTRAL"))


cont_com_17_21_spp <- filt_dFull2017_2021_spp  %>% 
  group_by(Comuna,Spp) %>% 
  summarise(sumLandings=sum(Landings_Ton))

## SUMA DE SPP CON VALOR DE B/bmsy
sum(cont_com_17_21_spp$sumLandings)
## suma de todas spp con y sin bbmsy 2017-2021
filt_dFull2017_2021 <- filter(dFull1997_2021c, Year>=2017)
sum(filt_dFull2017_2021$Landings_Ton)

# % del volumen que representan las spp con valor bbmsy en el total del periodo 2017-2021
(300147*100)/557236

            
full_foin_cont_com <- full_join(cont_com_17_21_spp,cont_com_17_21_tot, by="Comuna")

full_foin_cont_com <- full_foin_cont_com  %>% 
  mutate(percentage=(sumLandings.x*100)/sumLandings.y)


