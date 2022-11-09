library(dplyr)
library(readxl)

meta<-read_excel("prep/_pressures/presiones.xlsx", sheet = "CP")
m2<- meta %>% filter (element == "BM")

mode <- function(x) {
  if(x = NA) return(NA)
  return(as.numeric(names(which.max(table(x)))))
}


m1<- m2 %>%
  dplyr::summarise( po_chemicals = mode(na.omit(po_chemicals)),
                    po_pathogens = mode(na.omit(po_pathogens)),
                    po_pathogens_fan = mode(na.omit(po_pathogens_fan)),
                    po_nutrients = mode(na.omit(po_nutrients)),
                    po_trash = mode(na.omit(po_trash)),
                    sp_alien = mode(na.omit(sp_alien)),
                    hd_infa = mode(na.omit(hd_infa)),
                    hd_coastal = mode(na.omit(hd_coastal)),
                    hd_traffic = mode(na.omit(hd_traffic)),
                    hd_density = mode(na.omit(hd_density)),
                    fp_ilegal = mode(na.omit(fp_ilegal)),
                    fp_unstt = mode(na.omit(fp_unstt)),
                    cs_sst = mode(na.omit(cs_sst)),
                    cc_acid = mode(na.omit(cc_acid)),
                    cc_slr = mode(na.omit(cc_slr)),
                    ss_wgi = mode(na.omit(ss_wgi))) %>%
  mutate(goal = "AO", element) %>%
  #mutate(element = NA) %>%
  select(goal, element, po_chemicals , po_pathogens, po_pathogens_fan, po_nutrients,
         po_trash, sp_alien , hd_infa , hd_coastal , hd_traffic, hd_density,
         fp_ilegal,  fp_unstt, cs_sst,  cc_acid,  cc_slr , ss_wgi)


matriz<- rbind(matriz, m1)




















