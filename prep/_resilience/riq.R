

sp <- estatus %>%
  separate("scientific", into=c("s1","s2","s3", "s4"), sep=" ")

sp <- sp %>%
  unite(col='specie', c('s1', 's2'), sep=' ')  %>%
  filter(year >= 2000) %>%
  select(rgn_id, specie)

ico<- ico_spp_extinction_status_pat2021 %>%
  select(rgn_id = "reg_id", specie)


t_sp<- rbind(sp, ico)
t_sp<- t_sp[!duplicated(t_sp), ]

riq<-data.frame(table(t_sp$rgn_id))
riq<- riq %>% rename(rgn_id = rgnid)

riq_sp<- merge(riq, region_list)

riq_sp<- riq_sp %>% mutate(pond = Freq/area_km2,
                           score = pond/ 0.1613294189)  %>%
  select(rgn_id = rgnid, 	resilience_score = score)

#Punto de referencia la segunda comuna con mayor proporción: 0.1613294189 en Curaco de Velez










