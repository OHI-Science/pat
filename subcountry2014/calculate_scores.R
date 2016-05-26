library(devtools)
devtools::install_github("ohi-science/ohicore@dev_resil") # when testing changes to ohicore

# load required libraries
suppressWarnings(require(ohicore))

# set working directory to the scenario directory, ie containing conf and layers directories
setwd('subcountry2014')

# load scenario configuration
conf = Conf('conf')

# run checks on scenario layers
CheckLayers('layers.csv', 'layers', flds_id=conf$config$layers_id_fields)

# load scenario layers
layers = Layers('layers.csv', 'layers')

# calculate scenario scores
scores = CalculateAll(conf, layers)
write.csv(scores, 'scores.csv', na='', row.names=F)
