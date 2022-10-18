library(readxl)
library(dplyr)
library(reshape2)


Hab <- read_excel("prep/HAB/Hab_OHI.xlsx")

sup<- select(Hab, c("rgn_id", "total_km2"))

hab<- select(Hab, -c("total_km2", "macro"))
hab<- melt(hab, id.vars = c("rgn_id"))
hab$variable<- as.character(hab$variable)
hab<- merge(hab, sup)


##Punto de ref
p_ref<- hab %>%
  dplyr::mutate(value = c(value*1.1),
                p_ref = value/ total_km2)  %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(ref = max(p_ref,  na.rm = TRUE)) %>%
  dplyr::select(variable, ref)

## Numero de habitats
com_hab <- hab[!is.na(hab$value),]
com_h1<-data.frame( rgn_id= 1,
                    n_h = nrow(table(com$variable)))
for (i in c(2:36)) {
  com<- filter(com_hab, rgn_id == i)
  com_h<-data.frame(rgn_id= i,
                    n_h = nrow(table(com$variable)))
  com_h1<- rbind(com_h1, com_h)
}
com_h1 <- com_h1[!is.na(com_h1$n_h),]


##Scores
hab<- merge(com_hab, p_ref)
hab<-merge(hab, sup)

scores_hab<- hab %>%
  dplyr::mutate(Cc= value/total_km2)  %>%
  dplyr::mutate(C= Cc/ref) %>%
  dplyr::group_by(rgn_id) %>%
  dplyr::summarise(c_sum = sum(C,  na.rm = TRUE)) %>%
  dplyr::full_join(com_h1, by= c("rgn_id"))%>%
  dplyr::mutate(status= (c_sum/n_h) *100)


##Status
scores_hab <- scores_hab %>%
  mutate(dimension = 'status',
         score     = round(status, 4)) %>%
  mutate(goal = 'HAB')%>%
  select(region_id = "rgn_id", goal, dimension, score)


##Tendencia
#Debido a que el cambio de cobertura de los habitats no estan disponibles al momento de la realización de este indice
#Utilizaremos la tedencia 0, suponiendo que ese es el presente estudio es el habitat inicial

trend_data<- data.frame(region_id = c(1:36),
                        goal = c(rep("HAB", 36)),
                        dimension = c(rep("trend", 36)),
                        score = c(rep(0, 36)))



scores<- rbind(scores_hab, trend_data)





















