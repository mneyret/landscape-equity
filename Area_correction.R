# -------------------------------------------------------------------------------------------
# This is part of the work used for the publication Neyret et al. 2022. Landscape management for multifunctionality and Equity. Nature Sustainability.
# by Margot Neyret

# In this script, we normalise plant richness by sampling area

# Input: 
# Output: 
# -------------------------------------------------------------------------------------------


library(tidyr)
library(mgcv)


Plant_abundance = fread(
  'Temporary_data/Plants_abundance.csv'
)


plant_species = colnames(Plant_abundance)[-1]

Community = Plant_abundance
Community[, region := substr(Plot, 1, 1)]
Community[region == 'C', region := sample(c('A', 'H', 'S'), nrow(Community[region == 'C',]), replace = T)]
Community[, area := ifelse(grepl('W', Plot), 100, 16)]


if (by_region == FALSE){
  Community$region = 'All'
}

rep = 300
sites = 50

simulation_richness = function(Size, Region) {
  Community1 = Community[region == Region, ]
  drawplots = sample(1:nrow(Community1), size = Size)
  Community0 = Community1[drawplots, ]
  ric_plants = specnumber(colSums(Community0[, .SD, .SDcols = plant_species]))
  area = Community0[, sum(area)]
  prop_forest = Community0[grepl('W', Plot), .N] / Community0[, .N]
  return(list(
    Ric = ric_plants,
    Area = area,
    prop_forest = prop_forest
  ))
}

Area_correction_data = data.table(expand_grid(
  sites = 1:sites,
  rep = 1:rep,
  region = unique(Community$region)
))

Area_correction_data[, c('Ric_plants', 'Area', 'prop_forest') := simulation_richness(sites, region), by = c('sites', 'rep', 'region')]

Area_correction_data[, prop_quali := cut(prop_forest, c(-0.1, 0.1, 0.2, 0.3, 1))]

gg_area = ggplot(Area_correction_data, aes(Ric_plants, x = Area)) + 
  theme_bw() + 
  geom_point(alpha = 0.3) + geom_smooth(span = 0.9) +
 # geom_point(data = Area_correction_data[sites == 20,], color = 'skyblue3') +
  ylab('Plant richness') + xlab('Area covered (m2)')
  
ggsave(gg_area, file = "Results/FigS8_plot_SAC.pdf", height = 5, width = 6)
