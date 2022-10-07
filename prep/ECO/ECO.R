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








status <- le_gdp_30_sin_cero %>%
  dplyr::group_by(rgn_id, year) %>%
  dplyr::summarize(value = sum(na.omit(usd)))







scores.LE = scores %>%
  filter(goal %in% c('LIV','ECO') & dimension %in% c('status','trend','score','future')) %>%
  spread(key = goal, value = score) %>%
  mutate(score = rowMeans(cbind(ECO, LIV), na.rm=TRUE)) %>%
  select(region_id, dimension, score) %>%
  mutate(goal  = 'LE')

# rbind to all scores
scores = scores %>%
  rbind(scores.LE)

# return scores
return(scores)












