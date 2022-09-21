library(tidyverse)
library(dplyr)

### 1. acceder a bbdd de sernapesca y limpiar y preparar la dataframe
setwd("D:/Dropbox/Ohi/Archivos del toolbox/prep/AO")

######### LOS DATASETS FUERON ENTREGADOS POR SERNAPESCA. SON DE DOS ORIGENES DIFERENTES, UNO XSL DESDE 2010 A 2021 Y OTRO DE ACCESS DESDE 1997 A 2009, LOS HOMOGENIZE PARA QUE TUVIESEN MISMO NUMERO DE COLUMNAS. LA UNIDAD DE OBSERVACION ES LA SALIDA A PESCAR POR BARCO
dataFull_2010_2021 <- read.csv2("LandingsFull_2010_2021.csv",header=T,dec=",",sep=";")
dataFull_1997_2009 <- read.table("DESEMBARQUES 1997-2009.txt", header=F,dec=".",sep=";")


colnames(dataFull_1997_2009) <- colnames(dataFull_2010_2021)# cambia el nombre de las columnas 

dFull1997_2021 <- rbind(dataFull_2010_2021, dataFull_1997_2009)#junta ambos data sets
str(dFull1997_2021)

### LIMPIAR CALETAS CON NOMBRES DUPLICADOS
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "BAHIA ILQUE"] <- "ILQUE"
#el que tiene acento es de la comuna de P. Arenas y el otro de San Juan, dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "BAHÍA MANSA"] <- "BAHIA MANSA"
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "EL MANZANO X Reg"] <- "MANZANO (H)"
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "MANZANO (Hualaihué)"] <- "MANZANO (H)"
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "MAILLEN"] <- "ISLA MAILLEN"
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "TALCAN"] <- "ISLA TALCAN"
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "NALLAHUE"] <- "SAN JOSE DE BUTACHAUQUE"
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "San Pedro de Quellón"] <- "QUELLON"
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "SAN ANTONIO X Reg"] <- "SAN ANTONIO DE CHADMO"
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "TAC"] <- "TAC (ISLA TAC)"
dFull1997_2021["CaletaDesembarque"][dFull1997_2021["CaletaDesembarque"] == "PUNTA QUILLAHUA"] <- "QUILLAHUA"

dFull1997_2021_caletas <- print(unique(dFull1997_2021$CaletaDesembarque))##chequamos que estén las caletas unicas



### eliminamos de spp los valores "sin movimiento"
dFull1997_2021 <- dFull1997_2021[!(dFull1997_2021$Spp=="SIN MOVIMIENTO"),]

dFull1997_2021 <- dFull1997_2021 %>%
  mutate(Comuna = toupper(Comuna))

# Asignamos los desembarques de Palena a la comuna de Chaitén para tener las 30 comunas marinas de Pesca
dFull1997_2021["Comuna"][dFull1997_2021["Comuna"] == "PALENA"] <- "CHAITÉN"

# comprobar si los nombres de las comunas están duplicados
unique(dFull1997_2021$Comuna)

###SEPARA UNA DE LAS COUMNAS DE FECHA PARA DIVIDIRLA EN DIA, MES Y Ano PARA PODER AGRUPAR DATOS

dFull1997_2021b <- dFull1997_2021 %>% 
  select(RPAEmbarcacion,FechaLlegada,Comuna,CaletaDesembarque,Gear,Spp,Landings_Ton) %>% 
  separate(FechaLlegada, into = c("Date", "Time"), sep = " ", remove = FALSE) %>% 
  mutate(Date2=str_replace_all(Date, "-", "/"))


dFull1997_2021c <- dFull1997_2021b %>% 
  separate("Date2", into=c("Day","Month","Year"), sep="/") %>% 
  mutate(Month=str_replace(Month, "^0",""))


dFull1997_2021c$Landings_Ton <- as.numeric(dFull1997_2021c$Landings_Ton)

str(dFull1997_2021c)

#### STEP 1: cALCULO DEL coeficiente de sustentabilidad para cada arte

rpa_gear <- print(unique(dFull1997_2021c$Gear))

sust_gear <- data.frame(Gear=c("ESPINEL","BUCEO","TRAMPAS","CERCO","ENMALLE","LINEA DE MANO","TRAMPA TUBO", "ARRASTRE FONDO",
                               "RECOLECT.ORILLA","ARAÑA","ARPON","POTERAS","CANASTILLO","ARRASTRE M.AGUA","PALANGRE","CHINCHORRO"),
                        Sust_coef=as.numeric(c(".5",".9",".7",".3",".4",".8",".7",".2",".9",".8",".9",".8",".7",".3",".5",".4")))

sust_df <- left_join(dFull1997_2021c,sust_gear, by=c("Gear" = "Gear"), all.x=T)


sust_df1 <- sust_df %>% 
  group_by(Comuna, Year, Gear) %>% 
  filter(Landings_Ton>0.001) %>% 
  summarise(SumLandings=sum(Landings_Ton))


sust_df2<- left_join(sust_df1,sust_gear, by=c("Gear" = "Gear"), all.x=T)


sust_df3<- sust_df2 %>% 
  mutate(Sust_coef_final=SumLandings*Sust_coef)%>%
  mutate(norm_mean=mean(Sust_coef_final)-sd(Sust_coef_final)/max(Sust_coef_final)-min(Sust_coef_final))

### calculo de coeficiente de sostenibilidad y que funciona como punto de referencia temporal por Comuna (Sustcoef_norm), 
##cada comuna en la serie de años 1997-2021 tiene un punto de ref diferente
sust_df4<-sust_df3  %>% 
  group_by(Comuna,Year) %>% 
  summarise(MeanSust_coef_final=mean(Sust_coef_final, na.rm=T))%>%
  mutate(Sustcoef_norm=(MeanSust_coef_final - min(MeanSust_coef_final))/(max(MeanSust_coef_final)-min(MeanSust_coef_final)))


  


ggplot()+
  geom_line(data=sust_df4, aes(x=Year, y=Sustcoef_norm, group=1))+
  scale_x_discrete(breaks=c(1997,2001,2005,2010,2015,2020))+
  ylab("Sust_coefficient")+
  facet_wrap(~Comuna)


### STEP 2. gini coeff.  POR PESCADOR DE LOS DESEMBARQUES 

library(reldist)


landingsRPA <- dFull1997_2021c  %>% 
  group_by(Comuna,Year, RPAEmbarcacion)  %>% 
  summarise(TotalLandbyRPA=sum(Landings_Ton, na.rm=T))

giniAll <- aggregate(TotalLandbyRPA ~ Comuna+Year,
                    data = landingsRPA,
                    FUN = "gini")

gini_All_inv <- giniAll  %>% 
  mutate(Gini_inv=1-TotalLandbyRPA)  %>% 
  dplyr::select(Comuna, Year, Gini_inv)


ggplot()+
  geom_line(data=gini_All_inv, aes(x=Year, y=Gini_inv, group=1))+
  scale_x_discrete(breaks=c(1997,2001,2005,2010,2015,2020))+
  ylab("Gini coeff")+
  facet_wrap(~Comuna)

### habria que hacer la inversa de Gini para meter dentro de la equación de AO


### step 3. BBDD RPA, SACAMOS EL NUMERO DE "NACIMIENTOS" DE PESCADORES, COMO FACTOR DE DEMANDA

setwd("D:/Dropbox/IDEAL/R projects/Chile_PE")

dataAll <- read.table("RPA_X_XI_XII.txt", header=F,dec=",",sep=";")

rpadata <- dataAll %>% 
  rename(Region=V1, Caleta=V2, RPA=V3, YearRPA=V4, Spp=V5, YearSpp=V6, Gear=V7)

### LIMPIAR CALETAS CON NOMBRES DUPLICADOS
rpadata["Caleta"][rpadata["Caleta"] == "BAHIA ILQUE"] <- "ILQUE"
rpadata["Caleta"][rpadata["Caleta"] == "EL MANZANO X Reg"] <- "MANZANO (H)"
rpadata["Caleta"][rpadata["Caleta"] == "MANZANO (Hualaihué)"] <- "MANZANO (H)"
rpadata["Caleta"][rpadata["Caleta"] == "MAILLEN"] <- "ISLA MAILLEN"
rpadata["Caleta"][rpadata["Caleta"] == "TALCAN"] <- "ISLA TALCAN"
rpadata["Caleta"][rpadata["Caleta"] == "NALLAHUE"] <- "SAN JOSE DE BUTACHAUQUE"
rpadata["Caleta"][rpadata["Caleta"] == "PUNTA QUILLAHUA"] <- "QUILLAHUA"
rpadata["Caleta"][rpadata["Caleta"] == "SAN ANTONIO X Reg"] <- "SAN ANTONIO DE CHADMO"
rpadata["Caleta"][rpadata["Caleta"] == "San Pedro de Quellón"] <- "QUELLON"
rpadata["Caleta"][rpadata["Caleta"] == "TAC"] <- "TAC (ISLA TAC)"
rpadata["Caleta"][rpadata["Caleta"] == "TALCAN"] <- "ISLA TALCAN"
# CON ACENTO ES LA DE LA BAHIA MANSA DE LA XII Y SIN ACENTO ES LA DE LA X,rpadata["Caleta"][rpadata["Caleta"] == "BAHÍA MANSA"] <- "BAHIA MANSA"
rpadata["Caleta"][rpadata["Caleta"] == "ANIHUE"] <- "AÑIHUE"
rpadata["Caleta"][rpadata["Caleta"] == "BAHIA CHILOTA"] <- "BAHÍA CHILOTA"
rpadata["Caleta"][rpadata["Caleta"] == "CALETA BARRANCO AMARILLO"] <- "BARRANCO AMARILLO"


rpa_caletas <- print(unique(rpadata$YearRPA))
str(rpadata)

write.csv2(rpa_caletas, "rpa_caletas.csv")

### tenemos que crear una columna con la info de comuna, para eso primero generamos un csv con todas las caletas y sus correspondientes comunas de pertenencia

rpadata_comunas <- rpadata %>% 
  group_by(Caleta) %>% 
  summarise(nSpp=n_distinct(Spp))#COMPROBAR NUMERO DE CALETAS EN BBDD DE RPA ORIGINAL

data_comunas <-dFull1997_2021c %>% 
  group_by(CaletaDesembarque, Comuna) %>% 
  summarise(n=n_distinct(Spp)) %>% 
  dplyr::select(-n)#TOMAMOS LA BBDD DE DESEMBARQUES Y COMO YA TIENE LA CALETA ASOCIADA A COMUNA 

write.csv2(data_comunas, "comunas_caletas_dFull1997_2021c.csv")


rpadata_def <-merge(rpadata,data_comunas, by.x = "Caleta", by.y = "CaletaDesembarque", all=T) # HACEMOS MERGE DE LAS DOS ANTERIORES PARA ASOCIAR CALETAS A COMUNAS EN LA RPA DATA


birthRPA_all <- rpadata_def  %>% 
  group_by(Comuna,YearRPA)  %>%
  summarise(N_births= n_distinct(RPA))  %>% 
  mutate(CumSum=cumsum(N_births), ind_rate=N_births/CumSum, perct=((N_births/(max(CumSum))*100)), 
         normperct=(perct - min(perct)) / (max(perct)-min(perct))) %>%
  na.omit()


#norm_data_RPA <- birthRPA_all  %>% 
  group_by(Comuna)%>%
  filter(YearRPA>=1993)%>%
  mutate(normrank  = (N_births - min(N_births)) / (max(N_births)-min(N_births)))  %>% 
  dplyr::select(Comuna, YearRPA,normrank)#se normaliza los datos para cada comuna dentro del propio rango de valores de la serie de años

#norm_data_RPA2 <- birthRPA_all  %>% 
  group_by(Comuna)%>%
  filter(YearRPA>=1993)%>%
  dplyr::mutate(normrank  = (ind_rate - min(ind_rate)) / (max(ind_rate)-min(ind_rate)))  %>% 
  dplyr::select(Comuna, YearRPA,normrank)#se normaliza los datos para cada comuna dentro del propio rango de valores de la serie de años


### step 4. Calculo del indicador final para Oportunidades de pesca
install.packages("zoo")
library(zoo)

OA_data_final <- merge(birthRPA_all, gini_All_inv, by.x=c("Comuna", "YearRPA"), by.y=c("Comuna","Year"), all=T)

OA_data_final_sust <- merge(OA_data_final, sust_df4, by.x=c("Comuna","YearRPA"), by.y=c("Comuna","Year"), all=T)

OA_data_final2 <- OA_data_final_sust  %>% 
  mutate(rollmeanBirths=(zoo::rollapply(normperct,5, mean, na.rm=T, partial=T)), 
                         rollmeanGini=(zoo::rollapply(Gini_inv,5, mean, na.rm=T, partial=T)), 
          Xao=((rollmeanGini+rollmeanBirths+Sustcoef_norm)/3))

### plotting Status in time
ggplot()+
  geom_line(data=subset(OA_data_final2, !is.na(Xao)), aes(x=YearRPA, y=Xao, group=1))+
  scale_x_discrete(breaks=c(1999,2005,2013,2021))+
  ylab("Status AO")+
  xlab("Year")+
  facet_wrap(~Comuna)

### Calculo de la tendencia de forma manual

### codigo para calcular tendencia de web:https://ohi-science.org/manual/#file-system-organization
AO_2017_2021 <- OA_data_final2 %>%
  filter(YearRPA %in% (2017:2021)) %>%
  dplyr::select(Comuna,YearRPA,Xao)

str(AO_2017_2021)


r.trend_AO <- AO_2017_2021 %>%
  group_by(Comuna) %>%
  do(mdl = lm(Xao ~ YearRPA, data=.), na.action=na.omit) %>%
  summarize( Comuna = Comuna,
             trend = coef(mdl)['Year']*5) 



### Promedio de valores de estado de todas las comunas en toda la serie temporal
OA_mean_year <- OA_data_final2  %>% 
  group_by(YearRPA)  %>% 
  summarise(mean_OA=mean(Xao,na.rm=TRUE))

ggplot(data=OA_mean_year, aes(x=YearRPA, y=mean_OA))+
  geom_line(color="blue") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=6)+
  theme_ipsum() +
  ggtitle("Status trend Artisanal Fishing Oportunities - OHI")

### preparar data para plotear espacial

AO_Score2021 <- OA_data_final2  %>% 
  filter(YearRPA==2021)  %>% 
  group_by(Comuna)%>% 
  mutate(Score=Xao*100) %>% 
  select(Score)

write.csv2(AO_Score2021, "AO_Status")

### codigo para calcular tendencia de web:https://ohi-science.org/manual/#file-system-organization

r.trend <- status_final_names %>%
  group_by(rgn_id) %>%
  do(mdl = lm(status ~ Year, data=.)) %>%
  summarize( rgn_id = rgn_id,
             trend = coef(mdl)['Year']*5)



### plotting spatially status
comunas_id <- read.csv2("comunas_id.csv",header=T,dec=",",sep=";")

setwd("D:/GISLAYERS/Chile/OHI")
library(sf)

comunas_sf <- st_read("rgn_id.shp")

AO_2021 <- merge(AO_Score2021, comunas_id, by.x="Comuna", by.y="rgn_name", all=T, na.rm=F) 


comunas_sf_AO2021 <- left_join(comunas_sf,AO_2021, by="rgn_id")


ggplot() +
  geom_sf(data=comunas_sf_AO2021, aes(fill = Score, geometry = geometry))+
  scale_fill_viridis_c("Estado actual")+
  ggtitle("Oportunidades Pesca Artesanal")




### plotting spatially status
setwd("D:/GISLAYERS/Chile/OHI")
comunas_sp <- readOGR("comunas_austral_pop.shp")
comunas_mar<- readOGR("rgn_id.shp")

comunas_sp_merged<-merge(comunas_mar@data, mean_status_ALL,by.x="rgn_id",by.y="id_ohi")

comunas_sp_fort <- fortify(comunas_sp)