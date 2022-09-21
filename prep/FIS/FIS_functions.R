### ESTADO DE BASE  2017- 2021 ###

# Xfis= estado de las pesquerias pescadas en la naturaleza
# SS= puntuaciones de estado de las pesquerias
# B/Bmsy=indicador para informar SS
# C= contribucion de la poblaci?n a la captura total

# 1st datos provenientes de los desembarques SERNAPESCA; BBDD dFull1997_2021, dataframe ya creado dentro de script RPALandings_Full
#en caso de no tenerlo cargado, crear ruta:

setwd("C:/Users/luiso/Dropbox/IDEAL/R projects/OHI")

######### LOS DATASETS FUERON ENTREGADOS POR SERNAPESCA. SON DE DOS ORIGENES DIFERENTES, UNO XSL DESDE 2010 A 2021 Y OTRO DE ACCESS DESDE 1997 A 2009, LOS HOMOGENIZE PARA QUE TUVIESEN MISMO NUMERO DE COLUMNAS. LA UNIDAD DE OBSERVACION ES LA SALIDA A PESCAR POR BARCO
dataFull_2010_2021 <- read.csv2("LandingsFull_2010_2021.csv",header=T,dec=",",sep=";")
dataFull_1997_2009 <- read.table("DESEMBARQUES 1997-2009.txt", header=F,dec=",",sep=";")

colnames(dataFull_1997_2009) <- colnames(dataFull_2010_2021)# cambia el nombre de las columnas 

dFull1997_2021 <- rbind(dataFull_2010_2021, dataFull_1997_2009)#junta ambos data sets
###SEPARA UNA DE LAS COUMNAS DE FECHA PARA DIVIDIRLA EN DIA, MES Y AnO PARA PODER AGRUPAR DATOS
dFull1997_2021b <- dFull1997_2021 %>% 
  separate(FechaLlegada, into = c("Date", "Time"), sep = " ", remove = FALSE)


dFull1997_2021c <- dFull1997_2021b %>% 
  separate("Date", into=c("Day","Month","Year"), sep="/") %>% 
  mutate(Month=str_replace(Month, "^0",""))



filt_dFull2017_2021 <- filter(dFull1997_2021c, Year>=2017)#filtramos s?lo los a?os del periodo de estudio
filt_dFull2017_2021["Comuna"][filt_dFull2017_2021["Comuna"] == "PALENA"] <- "CHAIT?N"#asignamos palena con chait?n


setwd("D:/Dropbox/IDEAL/R projects/OHI")

comunas_id <- read.csv2("comunas_id.csv",header=T,dec=",",sep=";")

dataFIS <- merge(filt_dFull2017_2021,comunas_id, by.x = "Comuna", by.y = "rgn_name", all=T)


c <- dataFIS  %>% 
  group_by(rgn_id,Year,Spp) %>% 
  summarise(Meanlandings=mean(Landings_Ton))

b <- read.csv2("fis_b_bmsy_pat2022.csv",header=T,dec=",",sep=";")

write.csv(dataFIS2, "fis_meancatch_pat2022.csv")