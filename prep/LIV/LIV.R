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
if (nrow(liv_status)==0){
  liv_status = liv %>%
    dplyr::select(region_id=rgn_id) %>%
    group_by(region_id) %>%
    summarize(
      goal      = 'LIV',
      dimension = 'status',
      score     = NA)
  liv_trend = liv %>%
    dplyr::select(region_id=rgn_id) %>%
    group_by(region_id) %>%
    summarize(
      goal      = 'LIV',
      dimension = 'trend',
      score     = NA)
} else {
  liv_status = liv_status1 %>%
    filter(year >= max(year, na.rm=T) - 4) %>% # reference point is 5 years ago
    arrange(rgn_id, year, sector) %>%
    # summarize across sectors
    group_by(rgn_id, year) %>%
    summarize(
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
    summarize(
      metric = metric,
      weight = weight,
      rgn_id = rgn_id,
      sector = sector,
      sector_trend = pmax(-1, pmin(1, coef(mdl)[['year']] * 5))) %>%
    arrange(rgn_id, metric, sector) %>%
    # get weighted mean across sectors per region-metric
    group_by(metric, rgn_id) %>%
    summarize(
      metric_trend = weighted.mean(sector_trend, weight, na.rm=T)) %>%
    # get mean trend across metrics (jobs, wages) per region
    group_by(rgn_id) %>%
    summarize(
      score = mean(metric_trend, na.rm=T)) %>%
    # format
    mutate(
      goal      = 'LIV',
      dimension = 'trend') %>%
    dplyr::select(
      goal, dimension,
      region_id = rgn_id,
      score)
}

## create scores and rbind to other goal scores
scores = rbind(liv_status, liv_trend) %>%
  dplyr::select(region_id,
                score,
                dimension,
                goal)

return(scores)