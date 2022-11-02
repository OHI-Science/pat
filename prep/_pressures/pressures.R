library(dplyr)
library(readxl)

meta<-read_excel("prep/_pressures/presiones.xlsx", sheet = "AO")
                 ,
                 col_types = c("text", "text",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric",
                               "numeric", "numeric", "numeric"))
meta$po_chemicals<- as.character(meta$po_chemicals, na.rm = F)
meta$po_chemicals<- as.numeric(meta$po_chemicals, na.rm = F)


m1<- meta %>%
  dplyr::summarise( po_chemicals = round(mean(po_chemicals,  na.rm = TRUE)),
                    po_pathogens = round(mean(po_pathogens,  na.rm = TRUE)),
                    po_pathogens_fan = round(mean(po_pathogens_fan,  na.rm = TRUE)),
                    po_nutrients = round(mean(po_nutrients,  na.rm = TRUE)),
                    po_trash = round(mean(po_trash,  na.rm = TRUE)),
                    sp_alien = round(mean(sp_alien,  na.rm = TRUE)),
                    hd_infa = round(mean(hd_infa,  na.rm = TRUE)),
                    hd_coastal = round(mean(hd_coastal,  na.rm = TRUE)),
                    hd_traffic = round(mean(hd_traffic,  na.rm = TRUE)),
                    fp_ilegal = round(mean(fp_ilegal,  na.rm = TRUE)),
                    fp_unstt = round(mean(fp_unstt,  na.rm = TRUE)),
                    cs_sst = round(mean(cs_sst,  na.rm = TRUE)),
                    cc_acid = round(mean(cc_acid,  na.rm = TRUE)),
                    cc_slr = round(mean(cc_slr,  na.rm = TRUE)),
                    ss_wgi = round(mean(ss_wgi,  na.rm = TRUE)))



















