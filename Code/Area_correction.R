library(tidyr)
library(mgcv)
Ab_plants = fread(
  '/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_simulation/data/Plants_abundance.csv'
)


plant_species = colnames(Ab_plants)[-1]

comm = Ab_plants
comm[, region := substr(Plot, 1, 1)]
comm[region == 'C', region := sample(c('A', 'H', 'S'), nrow(comm[region == 'C',]), replace = T)]
comm[, area := ifelse(grepl('W', Plot), 100, 16)]


if (by_region == FALSE){
  comm$region = 'All'
}

rep = 100
sites = 50

simulation_richness = function(Size, Region) {
  comm1 = comm[region == Region, ]
  drawplots = sample(1:nrow(comm1), size = Size)
  comm0 = comm1[drawplots, ]
  ric_plants = specnumber(colSums(comm0[, .SD, .SDcols = plant_species]))
  area = comm0[, sum(area)]
  prop_forest = comm0[grepl('W', Plot), .N] / comm0[, .N]
  return(list(
    Ric = ric_plants,
    Area = area,
    prop_forest = prop_forest
  ))
}

Area_correction_data = data.table(expand_grid(
  sites = 1:sites,
  rep = 1:rep,
  region = unique(comm$region)
))

Area_correction_data[, c('Ric_plants', 'Area', 'prop_forest') := simulation_richness(sites, region), by = c('sites', 'rep', 'region')]

Area_correction_data[, prop_quali := cut(prop_forest, 4)]

ggplot(Area_correction_data, aes(Ric_plants, x = Area, color = region)) + 
  theme_bw() + 
  geom_point(alpha = 0.1) + geom_smooth(span = 0.9)
  

