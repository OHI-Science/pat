## functions.R.
## Each OHI goal model is a separate R function.
## The function name is the 2- or 3- letter code for each goal or subgoal;
## for example, FIS is the Fishing subgoal of Food Provision (FP).


FIS <- function(layers) {
    scen_year <- layers$data$scenario_year
    ### ESTADO DE BASE  2017- 2021 ###

    # Xfis= estado de las pesquerias pescadas en la naturaleza
    # SS= puntuaciones de estado de las pesquerias
    # B/Bmsy=indicador para informar SS
    # C= contribucion de la poblaci?n a la captura total


    ### from script FUNCTIONS global de OHI core

  #catch data
  c <-
    AlignDataYears(layer_nm = "fis_landings", layers_obj = layers) %>%
    dplyr::select(rgn_id, Year = scenario_year, Spp, SumLandings)


  #  b_bmsy data

    b <-
      AlignDataYears(layer_nm = "fis_b_bmsy", layers_obj = layers) %>%
      dplyr::select(rgn_id, Year = scenario_year,  Spp, bbmsy)

  # The following stocks are fished in multiple regions and often have high b/bmsy values
  # Due to the underfishing penalty, this actually penalizes the regions that have the highest
  # proportion of catch of these stocks.  The following corrects this problem:
   # tmp <- dplyr::filter(b, stock_id %in% c('Katsuwonus_pelamis-71', 'Sardinella_aurita-34')) %>%
   #   dplyr::arrange(stock_id, year) %>%
   #   data.frame()

  ### from script FUNCTIONS global de OHI core
  ####
  # STEP 1. Merge the b/bmsy data with catch data
  ####

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
    dplyr::rename(year = "Year") %>%
    dplyr::ungroup()


  ###
  # STEP 5. Get yearly status and trend
  ###

  status <-  status_data_final %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(score     = round(status * 100, 1),
           dimension = 'status') %>%
    dplyr::select(region_id = "rgn_id", score, dimension)


  # calculate trend

  trend_years <- (scen_year - 4):(scen_year)

  trend <-
    CalculateTrend(status_data = status_data_final, trend_years = trend_years)


  # assemble dimensions
  scores <- data.frame()
  scores <- rbind(status, trend) %>%
    dplyr::mutate(goal = 'FIS')


  return(scores)
}


MAR <- function(layers) {

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
    dplyr::summarise(pto_max = max(sm_tonnes))%>%
    dplyr::group_by(rgn_id) %>%
    dplyr::summarise(pto_ref = sum(pto_max)) %>%
    dplyr::mutate(Punto_ref = pto_ref*0.01) %>%
    dplyr::select(rgn_id, Punto_ref)

  ##Para calcular el estado
  c3<- c2 %>%
    dplyr::filter(year %in% c(2017:2021)) %>%
    dplyr::mutate(mult = sm_tonnes* sust_coeff) %>%
    dplyr::group_by(rgn_id, year) %>%
    dplyr::mutate(YC = sum(mult)) %>%
    dplyr::select(rgn_id, year, YC)

  c3<-c3[!duplicated(c3), ]


  c4<- merge(c3, pto_ref)

  status<-c4 %>%
    dplyr::mutate(status = YC/Punto_ref) %>%
    dplyr::select(rgn_id, year, status)

  # status
  status_a <- status %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::select(region_id = rgn_id, score = status, dimension)


  trend_years <- (scen_year - 4):(scen_year)

  trend <- CalculateTrend(status_data = status, trend_years = trend_years)


  # return scores
  score = rbind(status_a, trend) %>%
    dplyr::mutate(goal = 'MAR')
  scores = rbind(score, scores)

  return(scores)
}


FP <- function(layers, scores) {

  scen_year <- layers$data$scenario_year

  w <-
    AlignDataYears(layer_nm = "fp_wildcaught_weight", layers_obj = layers) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, w_fis)

  # scores
  s <- scores %>%
    dplyr::filter(goal %in% c('FIS', 'MAR')) %>%
    dplyr::filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    dplyr::left_join(w, by = "region_id")  %>%
    dplyr::mutate(w_mar = 1 - w_fis) %>%
    dplyr::mutate(weight = ifelse(goal == "FIS", w_fis, w_mar))


  ## Some warning messages due to potential mismatches in data:
  ## In the future consider filtering by scenario year so it's easy to see what warnings are attributed to which data
  # NA score but there is a weight
  tmp <-
    dplyr::filter(s,
           goal == 'FIS' &
             is.na(score) & (!is.na(w_fis) & w_fis != 0) & dimension == "score")
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a FIS weight but no score: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <-
    dplyr::filter(s,
           goal == 'MAR' &
             is.na(score) & (!is.na(w_mar) & w_fis != 0) & dimension == "score")
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a MAR weight but no score: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  # score, but the weight is NA or 0
  tmp <-
    dplyr::filter(
      s,
      goal == 'FIS' &
        (!is.na(score) &
           score > 0) &
        (is.na(w_fis) | w_fis == 0) & dimension == "score" & region_id != 0
    )
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a FIS score but weight is NA or 0: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  tmp <-
    dplyr::filter(
      s,
      goal == 'MAR' &
        (!is.na(score) &
           score > 0.05) &
        (is.na(w_mar) | w_mar == 0) & dimension == "score" & region_id != 0
    )
  if (dim(tmp)[1] > 0) {
    warning(paste0(
      "Check: these regions have a MAR score but weight is NA or 0: ",
      paste(as.character(tmp$region_id), collapse = ", ")
    ))
  }

  s <- s  %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = weighted.mean(score, weight, na.rm = TRUE)) %>%
    dplyr::mutate(goal = "FP") %>%
    dplyr::ungroup() %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


AO <- function(layers) {
  Sustainability <- 1.0

  scen_year <- layers$data$scenario_year

  gini<-
    AlignDataYears(layer_nm = "ao_coef_gini", layers_obj = layers) %>%
    dplyr::select(rgn_id,  year = ao_coef_gini_year, Gini_inv)

  Sust_artes<-
    AlignDataYears(layer_nm = "ao_coef_sust", layers_obj = layers) %>%
    dplyr::select(rgn_id,  year = ao_coef_sust_year, Sustcoef_norm)

  RPA<-
    AlignDataYears(layer_nm = "ao_rpa", layers_obj = layers) %>%
    dplyr::select(rgn_id,  year = ao_rpa_year, normperct)

  #Calculo de status

  OA_data_final <- merge(RPA, gini, by.x=c("rgn_id", "year"), by.y=c("rgn_id","year"), all=T)

  OA_data_final_sust <- merge(OA_data_final, Sust_artes, by.x=c("rgn_id","year"), by.y=c("rgn_id","year"), all=T)

  OA_data_final2 <- OA_data_final_sust  %>%
    mutate(rollmeanBirths=(zoo::rollapply(normperct,5, mean, na.rm=T, partial=T)),
           rollmeanGini=(zoo::rollapply(Gini_inv,5, mean, na.rm=T, partial=T)),
           Xao=((rollmeanGini+rollmeanBirths+Sustcoef_norm)/3))


  #Para calcular la tendencia

  AO_2017_2021 <- OA_data_final2 %>%
    filter(year %in% (2017:2021)) %>%
    dplyr::rename(region_id = "rgn_id", status = "Xao") %>%
    dplyr::select(region_id,year,status)

  r.status <-  AO_2017_2021 %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(score     = round(status * 100, 1),
                  dimension = 'status') %>%
    dplyr::select(region_id, score, dimension)

  # trend


  trend_years <- (scen_year - 4):(scen_year)

  r.trend <-
    CalculateTrend(status_data = AO_2017_2021, trend_years = trend_years)


  # return scores
  scores <- rbind(r.status, r.trend) %>%
    dplyr::mutate(goal = 'AO')

  return(scores)
}

NP <- function(layers) {

  scen_year <- layers$data$scenario_year


  ### Reassembles NP harvest information from separate data layers:
  ### [rgn_name  rgn_id  product  year  tonnes  tonnes_rel  prod_weight]
  #########################################.

  ## load data from layers dataframe
  h_tonnes <-
    AlignDataYears(layer_nm = "np_harvest_tonnes", layers_obj = layers) %>%
    dplyr::select(year = scenario_year, region_id = rgn_id, Producto, tonnes)

  h_tonnes_rel <-
    AlignDataYears(layer_nm = 'np_harvest_tonnes_relative', layers_obj = layers) %>%
    dplyr::select(year = scenario_year,
                  region_id = rgn_id,
                  Producto,
                  proportion)

  h_w <-
    AlignDataYears(layer_nm = "np_harvest_product_weight", layers_obj = layers) %>%
    dplyr::select(
      year = scenario_year,
      region_id = rgn_id,
      Producto,
      prod_weight = Weigth
    )


  # merge harvest in tonnes and usd
  np_harvest <- h_tonnes %>%
    dplyr::full_join(h_tonnes_rel, by = c('region_id', 'Producto', 'year')) %>%
    dplyr::left_join(h_w, by = c('region_id', 'Producto', 'year'))


  #Sustentabilidad
  lyr1 = c('np_sust' = 'sustentabilidad')
  np_sust = SelectLayersData(layers, layers=names(lyr1))
  np_sust <- dplyr::select(np_sust, c("id_num", "Producto", "Sostenibilidad"))
  np_sust<- dplyr:: rename(np_sust, c('region_id'='id_num'))


  ##Merge
  np_harvest<- merge(np_harvest, np_sust)

  ## Calcular el estado de cada producto

  np_status_all = np_harvest %>%
    mutate(product_status = proportion * Sostenibilidad)

  ##
  np_status_all = np_status_all %>%
    filter(!is.na(Sostenibilidad) & !is.na(prod_weight)& !is.na(product_status)) %>%
    select(region_id, year, Producto, product_status, prod_weight) %>%
    group_by(region_id, year) %>%
    dplyr::summarize(status = weighted.mean(product_status, prod_weight)) %>%
    filter(!is.na(status)) %>%
    ungroup()



  ### Calculates NP status for all production years for each region, based
  ### upon weighted mean of all products produced.
  ### Reports scenario year as the NP status.
  ### Calculates NP trend for each region, based upon slope of a linear
  ### model over the past five years
  ### Returns data frame with status and trend by region:
  ### [goal   dimension   region_id   score]
  #########################################.


  ### get current status
  np_status_current <- np_status_all %>%
    filter(year == scen_year & !is.na(status)) %>%
    mutate(dimension = 'status',
           score     = round(status, 4)) %>%
    select(region_id, dimension, score)


  ### trend

  trend_years <- (scen_year - 4):(scen_year)

  np_trend <-
    CalculateTrend(status_data = np_status_all, trend_years = trend_years)

  ### return scores
  np_scores <- np_status_current %>%
    full_join(np_trend, by = c('region_id', 'dimension', 'score')) %>%
    mutate(goal = 'NP') %>%
    select(goal, dimension, region_id, score) %>%
    arrange(goal, dimension, region_id)


  return(np_scores)
}


CS <- function(layers) {
  scen_year <- layers$data$scenario_year

  # layers for carbon storage
  extent_lyrs <-
    c('hab_mangrove_extent',
      'hab_seagrass_extent',
      'hab_saltmarsh_extent')
  health_lyrs <-
    c('hab_mangrove_health',
      'hab_seagrass_health',
      'hab_saltmarsh_health')
  trend_lyrs <-
    c('hab_mangrove_trend',
      'hab_seagrass_trend',
      'hab_saltmarsh_trend')

  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    dplyr::filter(!(habitat %in% c(
      "mangrove_inland1km", "mangrove_offshore"
    ))) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, extent = km2) %>%
    dplyr::mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    dplyr::filter(!(habitat %in% c(
      "mangrove_inland1km", "mangrove_offshore"
    ))) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, health) %>%
    dplyr::mutate(habitat = as.character(habitat))

  trend <- AlignManyDataYears(trend_lyrs) %>%
    dplyr::filter(!(habitat %in% c(
      "mangrove_inland1km", "mangrove_offshore"
    ))) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, trend) %>%
    dplyr::mutate(habitat = as.character(habitat))

  ## join layer data
  d <-  extent %>%
    dplyr::full_join(health, by = c("region_id", "habitat")) %>%
    dplyr::full_join(trend, by = c("region_id", "habitat"))

  ## set ranks for each habitat
  habitat.rank <- c('mangrove'         = 139,
                    'saltmarsh'        = 210,
                    'seagrass'         = 83)

  ## limit to CS habitats and add rank
  d <- d %>%
    dplyr::mutate(rank = habitat.rank[habitat],
           extent = ifelse(extent == 0, NA, extent))

  # status
  status <- d %>%
    dplyr::filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) / (sum(
      extent * rank, na.rm = TRUE
    ))) * 100,
    dimension = 'status') %>%
    ungroup()

  # trend

  trend <- d %>%
    filter(!is.na(rank) & !is.na(trend) & !is.na(extent)) %>%
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = sum(rank * trend * extent, na.rm = TRUE) / (sum(extent *
                                                                        rank, na.rm = TRUE)),
              dimension = 'trend') %>%
    dplyr::ungroup()


  scores_CS <- rbind(status, trend)  %>%
    dplyr::mutate(goal = 'CS') %>%
    dplyr::select(goal, dimension, region_id, score)


  ## create weights file for pressures/resilience calculations
  weights <- extent %>%
    dplyr::filter(extent > 0) %>%
    dplyr::mutate(rank = habitat.rank[habitat]) %>%
    dplyr::mutate(extent_rank = extent * rank) %>%
    dplyr::mutate(layer = "element_wts_cs_km2_x_storage") %>%
    dplyr::select(rgn_id = region_id, habitat, extent_rank, layer)

  write.csv(
    weights,
    sprintf(here("region/temp/element_wts_cs_km2_x_storage_%s.csv"), scen_year),
    row.names = FALSE
  )

  layers$data$element_wts_cs_km2_x_storage <- weights


  # return scores
  return(scores_CS)
}



CP <- function(layers) {

  ## read in layers
  scen_year <- layers$data$scenario_year

  # layers for coastal protection
  extent_lyrs <-
    c(
      'hab_mangrove_extent',
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_coral_extent',
      'hab_seaice_extent'
    )
  health_lyrs <-
    c(
      'hab_mangrove_health',
      'hab_seagrass_health',
      'hab_saltmarsh_health',
      'hab_coral_health',
      'hab_seaice_health'
    )
  trend_lyrs <-
    c(
      'hab_mangrove_trend',
      'hab_seagrass_trend',
      'hab_saltmarsh_trend',
      'hab_coral_trend',
      'hab_seaice_trend'
    )


  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    dplyr::filter(!(habitat %in% "seaice_edge")) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, extent = km2) %>%
    dplyr::mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    dplyr::filter(!(habitat %in% "seaice_edge")) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, health) %>%
    dplyr::mutate(habitat = as.character(habitat))

  trend <- AlignManyDataYears(trend_lyrs) %>%
    dplyr::filter(!(habitat %in% "seaice_edge")) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, trend) %>%
    dplyr::mutate(habitat = as.character(habitat))

  ## sum mangrove_offshore + mangrove_inland1km = mangrove to match with extent and trend
  mangrove_extent <- extent %>%
    dplyr::filter(habitat %in% c('mangrove_inland1km', 'mangrove_offshore'))

  if (nrow(mangrove_extent) > 0) {
    mangrove_extent <- mangrove_extent %>%
      dplyr::group_by(region_id) %>%
      dplyr::summarize(extent = sum(extent, na.rm = TRUE)) %>%
      dplyr::mutate(habitat = 'mangrove') %>%
      dplyr::ungroup()
  }

  extent <- extent %>%
    dplyr::filter(!habitat %in% c('mangrove', 'mangrove_inland1km', 'mangrove_offshore')) %>%  #do not use all mangrove
    rbind(mangrove_extent)  #just the inland 1km and offshore

  ## join layer data
  d <-  extent %>%
    dplyr::full_join(health, by = c("region_id", "habitat")) %>%
    dplyr::full_join(trend, by = c("region_id", "habitat"))

  # Removing countries within the Baltic, Iceland, and North Sea regions (UK, Germany, Denmark)
  # because seaice edge is due to ice floating into the environment and does not provide coastal protection
  # for these regions

  floaters <- c(174, 178, 222, 70, 69, 189, 143, 180, 176, 175)


   d <- d %>%
    dplyr::filter(!(region_id %in% floaters & habitat == "seaice_shoreline"))

  ## set ranks for each habitat
  habitat.rank <- c(
    'coral'            = 4,
    'mangrove'         = 4,
    'saltmarsh'        = 3,
    'seagrass'         = 1,
    'seaice_shoreline' = 4
  )

  ## limit to CP habitats and add rank
  d <- d %>%
    dplyr::filter(habitat %in% names(habitat.rank)) %>%
    dplyr::mutate(rank = habitat.rank[habitat],
           extent = ifelse(extent == 0, NA, extent))


  # status
  scores_CP <- d %>%
    dplyr::filter(!is.na(rank) & !is.na(health) & !is.na(extent)) %>%
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = pmin(1, sum(rank * health * extent, na.rm = TRUE) /
                             (sum(
                               extent * rank, na.rm = TRUE
                             ))) * 100) %>%
    dplyr::mutate(dimension = 'status') %>%
    ungroup()

  # trend
  d_trend <- d %>%
    dplyr::filter(!is.na(rank) & !is.na(trend) & !is.na(extent))

  if (nrow(d_trend) > 0) {
    scores_CP <- dplyr::bind_rows(
      scores_CP,
      d_trend %>%
        dplyr::group_by(region_id) %>%
        dplyr::summarize(
          score = sum(rank * trend * extent, na.rm = TRUE) / (sum(extent * rank, na.rm =
                                                                    TRUE)),
          dimension = 'trend'
        )
    )
  } else {
    # if no trend score, assign NA
    scores_CP <- dplyr::bind_rows(scores_CP,
                                  d %>%
                                    dplyr::group_by(rgn_id) %>%
                                    dplyr::summarize(score = NA,
                                              dimension = 'trend'))
  }

  ## finalize scores_CP
  scores_CP <- scores_CP %>%
    dplyr::mutate(goal = 'CP') %>%
    dplyr::select(region_id, goal, dimension, score)


  ## create weights file for pressures/resilience calculations

  weights <- extent %>%
    dplyr::filter(extent > 0) %>%
    dplyr::mutate(rank = habitat.rank[habitat]) %>%
    dplyr::mutate(extent_rank = extent * rank) %>%
    dplyr::mutate(layer = "element_wts_cp_km2_x_protection") %>%
    dplyr::select(rgn_id = region_id, habitat, extent_rank, layer)

  write.csv(
    weights,
    sprintf(here("region/temp/element_wts_cp_km2_x_protection_%s.csv"), scen_year),
    row.names = FALSE
  )

  layers$data$element_wts_cp_km2_x_protection <- weights

  # return scores
  return(scores_CP)

}

TR <- function(layers) {
  ## formula:
  ##  E   = Ep                         # Ep: % of direct tourism jobs. tr_jobs_pct_tourism.csv
  ##  S   = (S_score - 1) / (7 - 1)    # S_score: raw TTCI score, not normalized (1-7). tr_sustainability.csv
  ##  Xtr = E * S
  pct_ref <- 90

  scen_year <- layers$data$scenario_year


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
           S   = (s_score - 1) / (7 - 1),
           # scale score from 1 to 7.
           Xtr = E * S)


  # regions with Travel Warnings
  rgn_travel_warnings <-
    AlignDataYears(layer_nm = "tr_travelwarnings", layers_obj = layers) %>%
    dplyr::select(-layer_name)

  ## incorporate Travel Warnings
  tr_model <- tr_model %>%
     dplyr::left_join(rgn_travel_warnings, by = c('rgn_id', 'scenario_year')) %>%
     dplyr::mutate(Xtr = ifelse(!is.na(multiplier), multiplier * Xtr, Xtr)) %>%
     dplyr::select(-multiplier)

  # assign NA for uninhabitated islands (i.e., islands with <100 people)
  if (conf$config$layer_region_labels == 'rgn_global') {
    unpopulated = layers$data$uninhabited %>%
      dplyr::filter(est_population < 100 | is.na(est_population)) %>%
      dplyr::select(rgn_id)
    tr_model$Xtr = ifelse(tr_model$rgn_id %in% unpopulated$rgn_id,
                            NA,
                          tr_model$Xtr)
  }



  ### Calculate status based on quantile reference (see function call for pct_ref)
  tr_model <- tr_model %>%
    dplyr::filter(scenario_year >=2008) %>%
    dplyr::mutate(Xtr_q = quantile(Xtr, probs = pct_ref / 100, na.rm = TRUE)) %>%
    dplyr::mutate(status  = ifelse(Xtr / Xtr_q > 1, 1, Xtr / Xtr_q)) %>% # rescale to qth percentile, cap at 1
    dplyr::ungroup()


  # get status
  tr_status <- tr_model %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, score = status) %>%
    dplyr::mutate(score = score * 100) %>%
    dplyr::mutate(dimension = 'status')


  # calculate trend

  trend_data <- tr_model %>%
    dplyr::filter(!is.na(status))

  trend_years <- (scen_year - 4):(scen_year)

  tr_trend <-
    CalculateTrend(status_data = trend_data, trend_years = trend_years)


  # bind status and trend by rows
  tr_score <- dplyr::bind_rows(tr_status, tr_trend) %>%
    dplyr::mutate(goal = 'TR')


  # return final scores
  scores <- tr_score %>%
    dplyr::select(region_id, goal, dimension, score)

  return(scores)
}


LIV <- function(layers) {

  le_wages = SelectLayersData(layers, layers='le_wage_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, wage_usd = val_num)

  le_jobs  = SelectLayersData(layers, layers='le_jobs_sector_year') %>%
    dplyr::select(rgn_id = id_num, year, sector = category, jobs = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    dplyr::select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    dplyr::select(rgn_id = id_num, year, pct_unemployed = val_num)


  ## multipliers from Table S10 (Halpern et al 2012 SOM)
  multipliers_jobs = data.frame('sector' = c('Turismo','Pesca','Acuicultura', 'Alojamiento','Transporte'),
                                'multiplier' = c(1, 1.582, 2.7, 1,1))
  ## multipler not listed for tour (=1)

  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) /100 ,
           employed            = jobs_all * proportion_employed)

  liv =
    # adjust jobs
    le_jobs %>%
    left_join(multipliers_jobs, by = 'sector')          %>%
    mutate(jobs_mult = jobs * multiplier)               %>%  # adjust jobs by multipliers
    left_join(le_employed, by= c('rgn_id', 'year'))     %>%
    mutate(jobs_adj = jobs_mult * proportion_employed)  %>% # adjust jobs by proportion employed
    left_join(le_wages, by=c('rgn_id','year','sector')) %>%
    arrange(year, sector, rgn_id)

  # LIV calculations ----

  # LIV status
  liv_status1 = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd))
  liv_status = liv_status1 %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    arrange(rgn_id, year, sector) %>%
    # summarize across sectors
    group_by(rgn_id, year) %>%
    dplyr :: summarise(
      # across sectors, jobs are summed
      jobs_sum  = sum(jobs_adj, na.rm=T),
      # across sectors, wages are averaged
      wages_avg = mean(wage_usd, na.rm=T)) %>%
    group_by(rgn_id) %>%
    arrange(rgn_id, year) %>%
    mutate(
      # reference for jobs [j]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
      jobs_sum_first  = first(jobs_sum),                     # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
      # original reference for wages [w]: target value for average annual wages is the highest value observed across all reporting units
      # new reference for wages [w]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
      wages_avg_first = first(wages_avg)) %>% # note:  `first(jobs_sum, order_by=year)` caused segfault crash on Linux with dplyr 0.3.0.2, so using arrange above instead
    # calculate final scores
    ungroup() %>%
    mutate(
      x_jobs  = pmax(-1, pmin(1,  jobs_sum / jobs_sum_first)),
      x_wages = pmax(-1, pmin(1, wages_avg / wages_avg_first)),
      score   = ((x_jobs + x_wages) / 2)*100  ,na.rm=T ) %>%
    # filter for most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    dplyr::select(
      region_id = rgn_id,
      score) %>%
    mutate(
      goal      = 'LIV',
      dimension = 'status')

  ## LIV trend ----

  # get trend across years as slope of individual sectors for jobs and wages
  liv_trend = liv %>%
    filter(!is.na(jobs_adj) & !is.na(wage_usd)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # get sector weight as total jobs across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(jobs_adj, na.rm=T)) %>%
    # reshape into jobs and wages columns into single metric to get slope of both
    reshape2::melt(id=c('rgn_id','year','sector','weight'), variable='metric', value.name='value') %>%
    mutate(
      sector = as.character(sector),
      metric = as.character(metric)) %>%
    # get linear model coefficient per metric
    group_by(metric, rgn_id, sector, weight) %>%
    do(mdl = lm(value ~ year, data=.)) %>%
    dplyr::summarize(
      metric = metric,
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    arrange(rgn_id, metric, sector) %>%
    # get weighted mean across sectors per region-metric
    group_by(metric, rgn_id) %>%
    dplyr::summarize(
      metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # get mean trend across metrics (jobs, wages) per region
    group_by(rgn_id) %>%
    dplyr::summarize(
      score = mean(metric_trend, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'LIV',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)


  ## create scores and rbind to other goal scores
  scores = rbind(liv_status, liv_trend) %>%
    dplyr::select(region_id,
                  score,
                  dimension,
                  goal)

  return(scores)
}


ECO <- function(layers) {
  ## read in data layers
  le_gdp   = SelectLayersData(layers, layers='le_gdp')  %>%
    dplyr::select(rgn_id = id_num, year, gdp_usd = val_num)

  le_workforce_size = SelectLayersData(layers, layers='le_workforcesize_adj') %>%
    dplyr::select(rgn_id = id_num, year, jobs_all = val_num)

  le_unemployment = SelectLayersData(layers, layers='le_unemployment') %>%
    dplyr::select(rgn_id = id_num, year, pct_unemployed = val_num)


  # calculate employment counts
  le_employed = le_workforce_size %>%
    left_join(le_unemployment, by = c('rgn_id', 'year')) %>%
    mutate(proportion_employed = (100 - pct_unemployed) / 100,
           employed            = jobs_all * proportion_employed)

  # ECO calculations ----
  eco = le_gdp %>%
    mutate(
      rev_adj = gdp_usd,
      sector = 'gdp') %>%
    # adjust rev with national GDP rates if available. Example: (rev_adj = gdp_usd / ntl_gdp)
    dplyr::select(rgn_id, year, sector, rev_adj)

  # ECO status
  eco_status = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    # across sectors, revenue is summed
    group_by(rgn_id, year) %>%
    dplyr::summarize(
      rev_sum  = sum(rev_adj, na.rm=T)) %>%
    # reference for revenue [e]: value in the current year (or most recent year) [c], relative to the value in a recent moving reference period [r] defined as 5 years prior to [c]
    arrange(rgn_id, year) %>%
    group_by(rgn_id) %>%
    mutate(
      rev_sum_first  = first(rev_sum)) %>%
    # calculate final scores
    ungroup() %>%
    mutate(
      score  = pmin(rev_sum / rev_sum_first, 1) * 100) %>%
    # get most recent year
    filter(year == max(year, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'status') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  # ECO trend
  eco_trend = eco %>%
    filter(!is.na(rev_adj)) %>%
    filter(year >= max(year, na.rm=T) - 4 ) %>% # 5 year trend
    # get sector weight as total revenue across years for given region
    arrange(rgn_id, year, sector) %>%
    group_by(rgn_id, sector) %>%
    mutate(
      weight = sum(rev_adj, na.rm=T)) %>%
    # get linear model coefficient per region-sector
    group_by(rgn_id, sector, weight) %>%
    do(mdl = lm(rev_adj ~ year, data=.)) %>%
    dplyr::summarize(
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    # get weighted mean across sectors per region
    group_by(rgn_id) %>%
    dplyr::summarize(
      score = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'ECO',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)

  ## create scores and rbind to other goal scores
  score = rbind(eco_status, eco_trend) %>%
    dplyr::select(region_id,
                  score,
                  dimension,
                  goal)

  return(scores)
}

LE <- function(scores, layers) {

  s <- scores %>%
    dplyr::filter(goal %in% c('LIV', 'ECO'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "LE") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))

}

ICO <- function(layers) {
  scen_year <- layers$data$scenario_year
  lyr1 = c('ico_spp_extinction_status' = 'risk_category')
  lyr2 = c( 'ico_spp_popn_trend'        = 'popn_trend')

  # cast data ----
  l_data1 = SelectLayersData(layers, layers=names(lyr1))
  l_data2 = SelectLayersData(layers, layers=names(lyr2))
  l_data1 <- select(l_data1, c("id_num", "category", "val_chr"))
  l_data1<- dplyr:: rename(l_data1, c("risk_category" = "val_chr",'region_id'='id_num', 'sciname' ='category'))
  l_data2<- select(l_data2, c("id_num", "category", "val_chr"))
  l_data2<- dplyr:: rename(l_data2, c("popn_trend" = "val_chr",'region_id'='id_num', 'sciname' ='category'))
  rk<- merge(l_data1, l_data2)


  # lookup for weights status
  w.risk_category = c('LC' = 0,
                      'NT' = 0.2,
                      'VU' = 0.4,
                      'EN' = 0.6,
                      'CR' = 0.8,
                      'EX' = 1)

  # lookup for population trend
  w.popn_trend = c('Decreciendo' = -0.5,
                   'Estable'     =  0,
                   'Creciendo' =  0.5)

  # status
  r.status = rename(ddply(rk, .(region_id), function(x){
    mean(1 - w.risk_category[x$risk_category], na.rm=T) * 100 }),
    c('V1'='score'))

  # trend
  r.trend = rename(ddply(rk, .(region_id), function(x){
    mean(w.popn_trend[x$popn_trend], na.rm=T) }),
    c('V1'='score'))

  # return scores
  s.status = cbind(r.status, data.frame('dimension'='status'))
  s.trend  = cbind(r.trend , data.frame('dimension'='trend' ))
  scores = cbind(rbind(s.status, s.trend), data.frame('goal'='ICO'))

  return(scores)

}

LSP <- function(layers) {
  scen_year <- layers$data$scenario_year

  ref_pct_cmpa <- 30
  ref_pct_cp <- 30

  # select data
  total_area <-
    rbind(layers$data$rgn_area_inland1km,
          layers$data$rgn_area_offshore3nm) %>% #total offshore/inland areas
    dplyr::select(region_id = rgn_id, area_km2, layer) %>%
    tidyr::spread(layer, area_km2) %>%
    dplyr::select(region_id,
                  area_inland1km = rgn_area_inland1km,
                  area_offshore3nm = rgn_area_offshore3nm)


  offshore <-
    AlignDataYears(layer_nm = "lsp_prot_area_offshore3nm", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id,
                  year = scenario_year,
                  cmpa = area_km2)
  inland <-
    AlignDataYears(layer_nm = "lsp_prot_area_inland1km", layers_obj = layers) %>%
    dplyr::select(region_id = rgn_id,
                  year = scenario_year,
                  cp = area_km2)


  ry_offshore <-  layers$data$lsp_prot_area_offshore3nm %>%
    select(region_id = rgn_id, year, cmpa = area_km2)
  ry_inland <- layers$data$lsp_prot_area_inland1km %>%
    select(region_id = rgn_id, year, cp = area_km2)

  lsp_data <- full_join(ry_offshore, ry_inland, by = c("region_id", "year"))

  # fill in time series for all regions
  lsp_data_expand <-
    expand.grid(region_id = unique(lsp_data$region_id),
                year = unique(lsp_data$year)) %>%
    dplyr::left_join(lsp_data, by = c('region_id', 'year')) %>%
    dplyr::arrange(region_id, year) %>%
    dplyr::mutate(cp = ifelse(is.na(cp), 0, cp),
                  cmpa = ifelse(is.na(cmpa), 0, cmpa)) %>%
    dplyr::mutate(pa     = cp + cmpa)


  # get percent of total area that is protected for inland1km (cp) and offshore3nm (cmpa) per year
  # and calculate status score
  status_data <- lsp_data_expand %>%
    dplyr::full_join(total_area, by = "region_id") %>%
    dplyr::mutate(
      pct_cp    = pmin(cp   / area_inland1km   * 100, 100),
      pct_cmpa  = pmin(cmpa / area_offshore3nm * 100, 100),
      status    = (pmin(pct_cp / ref_pct_cp, 1) + pmin(pct_cmpa / ref_pct_cmpa, 1)) / 2
    ) %>%
    dplyr::filter(!is.na(status))

  # extract status based on specified year

  r.status <- status_data %>%
    dplyr::filter(year == scen_year) %>%
    dplyr::mutate(score = status * 100) %>%
    dplyr::select(region_id, score) %>%
    dplyr::mutate(dimension = "status")

  # calculate trend

  trend_years <- (scen_year - 4):(scen_year)

  r.trend <-
    CalculateTrend(status_data = status_data, trend_years = trend_years)



  # return scores
  scores <- dplyr::bind_rows(r.status, r.trend) %>%
    mutate(goal = "LSP")
  return(scores[, c('region_id', 'goal', 'dimension', 'score')])
}

SP <- function(scores) {
  ## to calculate the four SP dimesions, average those dimensions for ICO and LSP
  s <- scores %>%
    dplyr::filter(goal %in% c('ICO', 'LSP'),
           dimension %in% c('status', 'trend', 'future', 'score')) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(region_id) %>%
    dplyr::mutate(goal = "SP") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()

  # return all scores
  return(rbind(scores, s))
}


CW <- function(layers) {

  scen_year <- layers$data$scenario_year

  ### function to calculate geometric mean:
  geometric.mean2 <- function (x, na.rm = TRUE) {
    if (is.null(nrow(x))) {
      exp(mean(log(x), na.rm = TRUE))
    }
    else {
      exp(apply(log(x), 2, mean, na.rm = na.rm))
    }
  }


  # layers
  trend_lyrs <-
    c('cw_chemical_trend',
      'cw_nutrient_trend',
      'cw_coastalpopn_trend',
      'cw_pathogen_trend')
  prs_lyrs <-
    c('po_pathogen',
      'po_nutrient_3nm',
      'po_chemical',
      'po_trash')

  # get data together:
#  prs_data <- AlignManyDataYears(prs_lyrs) %>%
#    dplyr::filter(scenario_year == scen_year) %>%
#    dplyr::select(region_id = rgn_id, value = pressure_score)

  d_pressures <- pres_data %>%
    dplyr::mutate(pressure = 1 - value) %>%  # invert pressure
    dplyr::mutate(pressure = ifelse(pressure == 0 , pressure + 0.01, pressure)) %>% # add small modifier to zeros to
    dplyr::group_by(region_id) %>%                                                  # prevent zeros with geometric mean
    dplyr::summarize(score = geometric.mean2(pressure, na.rm = TRUE)) %>% # take geometric mean
    dplyr::mutate(score = score * 100) %>%
    dplyr::mutate(dimension = "status") %>%
    dplyr::ungroup()


  # get trend data together:
#  trend_data <- AlignManyDataYears(trend_lyrs) %>%
#    dplyr::filter(scenario_year == scen_year) %>%
#    dplyr::select(region_id = rgn_id, value = trend)

  d_trends <- trend_data %>%
    dplyr::mutate(trend = -1 * value)  %>%  # invert trends
    dplyr::group_by(region_id) %>%
    dplyr::summarize(score = mean(trend, na.rm = TRUE)) %>%
    dplyr::mutate(dimension = "trend") %>%
    dplyr::ungroup()


  # return scores
  scores <- rbind(d_pressures, d_trends) %>%
    dplyr::mutate(goal = "CW") %>%
    dplyr::select(region_id, goal, dimension, score) %>%
    data.frame()


  return(scores)
}


HAB <- function(layers) {
  scen_year <- layers$data$scenario_year


  extent_lyrs <-
    c(
      'hab_mangrove_extent',
      'hab_seagrass_extent',
      'hab_saltmarsh_extent',
      'hab_coral_extent',
      'hab_seaice_extent',
      'hab_softbottom_extent'
    )
  health_lyrs <-
    c(
      'hab_mangrove_health',
      'hab_seagrass_health',
      'hab_saltmarsh_health',
      'hab_coral_health',
      'hab_seaice_health',
      'hab_softbottom_health'
    )
  trend_lyrs <-
    c(
      'hab_mangrove_trend',
      'hab_seagrass_trend',
      'hab_saltmarsh_trend',
      'hab_coral_trend',
      'hab_seaice_trend',
      'hab_softbottom_trend'
    )

  # get data together:
  extent <- AlignManyDataYears(extent_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, extent = km2) %>%
    dplyr::mutate(habitat = as.character(habitat))

  health <- AlignManyDataYears(health_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, health) %>%
    dplyr::mutate(habitat = as.character(habitat))

  trend <- AlignManyDataYears(trend_lyrs) %>%
    dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id, habitat, trend) %>%
    dplyr::mutate(habitat = as.character(habitat))


  # join and limit to HAB habitats
  d <- health %>%
    dplyr::full_join(trend, by = c('region_id', 'habitat')) %>%
    dplyr::full_join(extent, by = c('region_id', 'habitat')) %>%
    dplyr::filter(
      habitat %in% c(
        'coral',
        'mangrove',
        'saltmarsh',
        'seaice_edge',
        'seagrass',
        'soft_bottom'
      )
    ) %>%
    dplyr::mutate(w  = ifelse(!is.na(extent) & extent > 0, 1, NA)) %>%
    dplyr::filter(!is.na(w))

  if (sum(d$w %in% 1 & is.na(d$trend)) > 0) {
    warning(
      "Some regions/habitats have extent data, but no trend data.  Consider estimating these values."
    )
  }

  if (sum(d$w %in% 1 & is.na(d$health)) > 0) {
    warning(
      "Some regions/habitats have extent data, but no health data.  Consider estimating these values."
    )
  }


  ## calculate scores
  status <- d %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(!is.na(health)) %>%
    dplyr::summarize(score = pmin(1, sum(health) / sum(w)) * 100,
              dimension = 'status') %>%
    ungroup()

  trend <- d %>%
    dplyr::group_by(region_id) %>%
    dplyr::filter(!is.na(trend)) %>%
    dplyr::summarize(score =  sum(trend) / sum(w),
              dimension = 'trend')  %>%
    dplyr::ungroup()

  scores_HAB <- rbind(status, trend) %>%
    dplyr::mutate(goal = "HAB") %>%
    dplyr::select(region_id, goal, dimension, score)


  ## create weights file for pressures/resilience calculations

  weights <- extent %>%
    filter(
      habitat %in% c(
        'seagrass',
        'saltmarsh',
        'mangrove',
        'coral',
        'seaice_edge',
        'soft_bottom'
      )
    ) %>%
    dplyr::filter(extent > 0) %>%
    dplyr::mutate(boolean = 1) %>%
    dplyr::mutate(layer = "element_wts_hab_pres_abs") %>%
    dplyr::select(rgn_id = region_id, habitat, boolean, layer)

  write.csv(weights,
            sprintf(here("region/temp/element_wts_hab_pres_abs_%s.csv"), scen_year),
            row.names = FALSE)

  layers$data$element_wts_hab_pres_abs <- weights


  # return scores
  return(scores_HAB)
}


SPP <- function(layers) {

  scen_year <- layers$data$scenario_year

status <- AlignDataYears(layer_nm = "spp_status", layers_obj = layers) %>%
   dplyr::filter(scenario_year == scen_year) %>%
    dplyr::select(region_id = rgn_id,
                  score) %>%
  dplyr::mutate(dimension = "status") %>%
  dplyr::mutate(score = score * 100)

trend <- layers$data$spp_trend %>%
  dplyr::select(region_id = rgn_id,
                score) %>%
  dplyr::mutate(dimension = "trend")

scores <- rbind(status, trend) %>%
    dplyr::mutate(goal = 'SPP') %>%
    dplyr::select(region_id, goal, dimension, score)


  return(scores)
}

BD <- function(scores) {
  d <- scores %>%
    dplyr::filter(goal %in% c('HAB', 'SPP')) %>%
    dplyr::filter(!(dimension %in% c('pressures', 'resilience'))) %>%
    dplyr::group_by(region_id, dimension) %>%
    dplyr::summarize(score = mean(score, na.rm = TRUE)) %>%
    dplyr::mutate(goal = 'BD') %>%
    data.frame()

  # return all scores
  return(rbind(scores, d[, c('region_id', 'goal', 'dimension', 'score')]))
}

PreGlobalScores <- function(layers, conf, scores) {
  # get regions
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    dplyr::select(id_num = rgn_id, val_chr = label)

  # limit to just desired regions and global (region_id==0)
  scores <- subset(scores, region_id %in% c(rgns[, 'id_num'], 0))

  # apply NA to Antarctica
  id_ant <- subset(rgns, val_chr == 'Antarctica', id_num, drop = TRUE)
  scores[scores$region_id == id_ant, 'score'] = NA

  return(scores)
}

FinalizeScores <- function(layers, conf, scores) {
  # get regions
  name_region_labels <- conf$config$layer_region_labels
  rgns <- layers$data[[name_region_labels]] %>%
    dplyr::select(id_num = rgn_id, val_chr = label)


  # add NAs to missing combos (region_id, goal, dimension)
  d <- expand.grid(list(
    score_NA  = NA,
    region_id = c(rgns[, 'id_num'], 0),
    dimension = c(
      'pressures',
      'resilience',
      'status',
      'trend',
      'future',
      'score'
    ),
    goal      = c(conf$goals$goal, 'Index')
  ),
  stringsAsFactors = FALSE)
  head(d)
  d <- subset(d,!(
    dimension %in% c('pressures', 'resilience', 'trend') &
      region_id == 0
  ) &
    !(
      dimension %in% c('pressures', 'resilience', 'trend', 'status') &
        goal == 'Index'
    ))
  scores <-
    merge(scores, d, all = TRUE)[, c('goal', 'dimension', 'region_id', 'score')]

  # order
  scores <- dplyr::arrange(scores, goal, dimension, region_id)

  # round scores
  scores$score <- round(scores$score, 2)

  return(scores)
}
