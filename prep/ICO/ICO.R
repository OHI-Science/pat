library(plyr)
library(maditr)

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
score = cbind(rbind(s.status, s.trend), data.frame('goal'='ICO'))
return(score)
