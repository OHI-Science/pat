  pct_ref <- 90

scen_year <- layers$data$scenario_year

#Ordenar la tabla en formato largo
tr_sustainability_pat2021<-melt(tr_sustainability_pat2021, id.vars = c("rgn_id"))


tr_sustainability_pat2021<- rename(tr_sustainability_pat2021,s_score = "value")
tr_sustainability_pat2021<- rename(tr_sustainability_pat2021,year = "variable")
tr_sustainability_pat2021$year<- as.numeric(tr_sustainability_pat2021$year)

write.csv(tr_sustainability_pat2021, "comunas/layers/tr_sustainability_pat2021.csv")

## read in layers
tourism <-
  AlignDataYears(layer_nm = "tr_jobs_pct_tourism", layers_obj = layers) %>%
  dplyr::select(-layer_name)
sustain <-
  AlignDataYears(layer_nm = "tr_sustainability", layers_obj = layers) %>%
  dplyr::select(-layer_name)

tr_data  <-
  dplyr::full_join(tourism, sustain, by = c('rgn_id', 'scenario_year'))

tr_model <- tr_data %>%
  dplyr::mutate(E   = ep,
                S   = s_score,
                Xtr = E * S)


# assign NA for uninhabitated islands (i.e., islands with <100 people)
if (conf$config$layer_region_labels == 'rgn_id') {
  unpopulated = layers$data$uninhabited %>%
    dplyr::filter(est_population < 100 | is.na(est_population)) %>%
    dplyr::select(rgn_id)
  tr_model$Xtr = ifelse(tr_model$rgn_id %in% unpopulated$rgn_id,
                        NA,
                        tr_model$Xtr)
}



### Calculate status based on quantile reference (see function call for pct_ref)
tr_model <- tr_model %>%
  dplyr::filter(scenario_year >=2017) %>%
  dplyr::mutate(Xtr_q = quantile(Xtr, probs = pct_ref / 100, na.rm = TRUE)) %>%
  dplyr::mutate(status  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) %>% # rescale to qth percentile, cap at 1
  dplyr::ungroup()

## Reference Point Accounting
ref_point <- tr_model %>%
  dplyr::filter(scenario_year == scen_year) %>%
  dplyr::select(Xtr_q) %>%
  unique() %>%
  data.frame() %>%
  .$Xtr_q

#WriteRefPoint(
#  goal = "TR",
#  method = paste0('spatial: ', as.character(pct_ref), "th quantile"),
#  ref_pt = as.character(ref_point))
## Reference Point End

# get status
tr_status <- tr_model %>%
  dplyr::filter(scenario_year == scen_year) %>%
  dplyr::select(region_id = rgn_id, score = status) %>%
  dplyr::mutate(score = score * 100) %>%
  dplyr::mutate(dimension = 'status')


# calculate trend

trend_data <- tr_model %>%
  dplyr::filter(!is.na(status)) %>%
  dplyr::select(rgn_id, scenario_year, status)

trend_years <- (scen_year - 4):(scen_year)


tr_trend <-
  CalculateTrend(status_data = trend_data, trend_years = trend_years)


# bind status and trend by rows
tr_score <- dplyr::bind_rows(tr_status, tr_trend) %>%
  dplyr::mutate(goal = 'TR')


# return final scores
scores <- tr_score %>%
  dplyr::select(region_id, goal, dimension, score)

scores<-scores[!duplicated(scores), ]

return(scores)
