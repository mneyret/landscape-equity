# -------------------------------------------------------------------------------------------
# This is part of the work used for the publication Neyret et al. 2022. Landscape management for multifunctionality and Equity. Nature Sustainability.
# by Margot Neyret

# This script identifies what are the current proportions of different land uses in the regions to use as a baseline in the models

# Input: Exploratories land use and land use intensity data, Grid plot management data, Corine land use data
# Output: Proportion of grasslands and forests of different types
# -------------------------------------------------------------------------------------------

library(data.table)
library(readxl)
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(sf)
library(data.table)

setwd('~/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition')

# %%%%%%%%%%%%%%%% #
#### Grasslands ####
# %%%%%%%%%%%%%%%% #

# For grassland we compare the LUI within the plots to management in 3000 grid plots across the three regions. 
# Grid plots are expected to be representative of the whole region. By comparing the distribution of LUI in the grid plots
# and exploratories, we can measure the proportion of low, high, medium intensity plots accordingly.

# Exploratories Plots grassland LUI
Grassland_LUI <- fread('Raw_data/Data_to_load/LUI_standardized_regional.txt')

# Grid plots management
grid_plot_grasslands = fread('Raw_data/Data_to_load/5120_Grassland\ land\ use\ identification\ on\ all\ Grid\ Plots\ 2006\ -\ 2008_1.98.55/5120.txt',
                             skip = 1)

#### Reformatting data ####
Grassland_LUI[, Year := gsub('separately\\(','', YEAR)][, Year := gsub('\\)','', Year)]
Grassland_LUI[, Plot := ifelse(nchar(PLOTID)== 5, PLOTID, paste(substr(PLOTID, 1, 3), '0', substr(PLOTID, 4, 4 ), sep = ''))]
Grassland_LUI = Grassland_LUI[as.character(Year) < 2016, list(LUI_2008_2015 = mean(LUI)), by = Plot]

grid_plot_grasslands[, Region := substr(Plotid, 1, 1)]
grid_plot_grasslands[Size == -1, Size := NA]

# Cleaning up the grid plot dataset
grid_plot_grasslands[Animal == '' | is.na(Animal), c('LivestockUnits', 'DaysGrazing') := 0]
grid_plot_grasslands[Animal == '' | is.na(Animal), c('LivestockUnits', 'DaysGrazing') := 0]
grid_plot_grasslands[LivestockUnits == -1, c('LivestockUnits') := NA]
grid_plot_grasslands[DaysGrazing == -1, c('LivestockUnits', 'DaysGrazing') := NA]
grid_plot_grasslands[is.na(Schnitte), Schnitte := 0]
grid_plot_grasslands[Schnitte == -1, Schnitte := NA]
grid_plot_grasslands[is.na(Liquid_manure),Liquid_manure:= 0]
grid_plot_grasslands[Liquid_manure == -1,Liquid_manure:= NA]
grid_plot_grasslands[is.na(Manure),Manure:= 0]
grid_plot_grasslands[Manure == -1,Manure:= NA]
grid_plot_grasslands[is.na(Nitrogen), Nitrogen:= 0]
grid_plot_grasslands[Nitrogen == -1, Nitrogen:= NA]
grid_plot_grasslands[, Grazing := LivestockUnits*DaysGrazing/Size]
grid_plot_grasslands[, Fertilizing := sum(c(Manure*0.6,Liquid_manure*3.2,Nitrogen), na.rm = T)/Size, by = 1:nrow(grid_plot_grasslands)]
grid_plot_grasslands[, Cuts := Schnitte]
grid_plot_grasslands[Fertilisation == 'nein' , Fertilizing := 0]


grid_plot_grasslands[, c('Grazing_norm', 'Fertilizing_norm', 'Cuts_norm') := lapply(.SD, function(x){return(x/mean(x, na.rm = T))}), 
                     .SDcols = c('Grazing', 'Fertilizing', 'Cuts'), 
                     by = 'Region']


# We calculate a pseudo-LUI for the grid plots (pseudo because not exactly the same input data)
grid_plot_grasslands[, pseudo_LUI := sqrt(Grazing_norm+Fertilizing_norm+Cuts_norm)]

grid_plot_grasslands_short = grid_plot_grasslands[, list(pseudo_LUI = mean(pseudo_LUI, na.rm = T)), by = c('Plotid', 'EP_Plotid', 'Region')]

#### Now we check in which category (low, medium, high intensity) would the GP fall ####
Grassland_LUI[, Classif := cut(LUI_2008_2015, quantile(LUI_2008_2015, c(0, 0.33, 0.66, 1)), include.lowest = T,
                               labels = c('Grassland_low', 'Grassland_medium', 'Grassland_high'))]

Compare_LUI = merge(grid_plot_grasslands_short[, Plot := EP_Plotid], Grassland_LUI, all.x = T)

# Comparing pseudo-LUI and real LUI on Exploratories plots: looks ok
ggplot(Compare_LUI, aes(LUI_2008_2015, pseudo_LUI)) + geom_point(aes(color = Classif)) + geom_smooth(method = 'lm') + facet_wrap(~Region) + theme_bw() +
  scale_color_brewer(palette = 'Set1', direction = -1) + ylab('LUI calculated from GP data') + xlab('Mean LUI 2007-2012')

ggplot(Compare_LUI, aes(pseudo_LUI)) + geom_histogram(color = 'grey') + facet_wrap(~Region) +
  geom_histogram(data = Compare_LUI, aes(LUI_2008_2015, color = Classif))

cor(Compare_LUI[complete.cases(Compare_LUI[, list(pseudo_LUI,LUI_2008_2015)]), list(pseudo_LUI,LUI_2008_2015)])


### We try to predict the LUI of GP based on the real LUI of the EPs
mod = lm(pseudo_LUI~LUI_2008_2015*Region, data = Compare_LUI)
# Mean in terms of real LUI
mod2 = lm(LUI_2008_2015~pseudo_LUI*Region, data = Compare_LUI)
test = Compare_LUI[, list(pseudo_LUI = mean(pseudo_LUI, na.rm = T)), by = Region]

# Identify the upper limits of each category (low, medium, high intensity) to reclassify
max_luis = Compare_LUI[!is.na(Classif), list(LUI_2008_2015 = max(LUI_2008_2015, na.rm = T)), by = c('Classif', 'Region')]
max_luis[, Max_GPs := predict(mod, .SD), .SDcols = c('LUI_2008_2015', 'Region')]

reclassify_LUI = function(region, x){
  if (is.na(x)){return('NA')} else {
    if (x <= max_luis[Region == region & Classif == 'Grassland_low', Max_GPs]){
      new_class = 'Grassland_low'
    } else {
      if (x <= max_luis[Region == region & Classif == 'Grassland_medium', Max_GPs]){
        new_class = 'Grassland_med'
      } else {
        new_class = 'Grassland_high'
      }
    }
    return(new_class)}
}

Compare_LUI[, 'New_classif' := reclassify_LUI(Region, pseudo_LUI), by = 1:nrow(Compare_LUI)]
Proportions = Compare_LUI[New_classif != 'NA', .N, by = c('New_classif', 'Region')][New_classif != 'NA', prop := N/sum(N), by = 'Region']


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
#### Forests type, age and overall grassland/forests proportions ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #

# For forests we use corine data for accurate classification of forest type at the regional level.
# For forest age the data is not available on Corine so we have to use forest grid plots

### Forest age from forest grid plots ###
grid_plot_forests = fread('Raw_data/Data_to_load/11101_forest\ inventory\ -\ land\ use\ gradient_4.0.0_PublicDataset/11101.txt')

grid_plot_forests[DHpLBH > 80, forest_type := 'Deciduous'] # If more than 80% deciduous trees, classified as deciduous
grid_plot_forests[DHpNDH > 80, forest_type := 'Coniferous'] # If more than 80% coniferous trees, classified as deciduous
grid_plot_forests[DHpNDH < 80 & DHpLBH < 80, forest_type := 'Mixed'] # Otherwise mixed forest
Prop_GP = grid_plot_forests[, .N, by = c('forest_type', 'EXP')][, prop := N/sum(N), by = EXP]

grid_plot_forests[BST %in% c('femelartig', 'mehrschichtig', 'plenterartig'), Age_class := 'uneven-aged']
grid_plot_forests[!(BST %in% c('femelartig', 'mehrschichtig', 'plenterartig')), Age_class := 'even-aged']

Prop_Age = grid_plot_forests[, .N, by = c('Age_class', 'EXP')][, prop := N/sum(N), by = EXP]

### Forest type from Corine ###
corine = raster('Raw_data/Data_to_load/Corine/DATA/U2018_CLC2018_V2020_20u1.tif')
Germany = readOGR("Raw_data/Data_to_load/Germany_shapefile/de_10km.shp")
Alb =  readOGR("Raw_data/Data_to_load/5460_2_Dataset/ConvexHullAlb.shp")
Hai =  readOGR("Raw_data/Data_to_load/5460_2_Dataset/convexHullHai.shp")
Sch =  readOGR("Raw_data/Data_to_load/5460_2_Dataset/convexHullSch.shp")
clcLeg <- read.csv(url("https://raw.githubusercontent.com/joaofgoncalves/R_exercises_raster_tutorial/master/data/legend_clc.csv"),
                   stringsAsFactors = FALSE)

clcLeg <- data.table(clcLeg[,1:2], 
                     CLC_abr=toupper(abbreviate(gsub("-"," ",clcLeg[,3]), 6)), 
                     Label=clcLeg[,3], row.names = 1:nrow(clcLeg))

Alb <- spTransform(Alb, crs(corine))
Hai <- spTransform(Hai, crs(corine))
Sch <- spTransform(Sch, crs(corine))

plot(corine)
plot(Germany, add = T)
plot(Alb, add = T, col = 'black')
plot(Hai, add = T, col = 'black')
plot(Sch, add = T, col = 'black')

Alb_rast = extract(corine, Alb)
Hai_rast = extract(corine, Hai)
Sch_rast = extract(corine, Sch)
#Germany_rast = mask( corine, Germany)
#Germany_ex = extract( Germany_rast, Germany)
#Germany_list = 

cropAlb <- crop(corine, extent(Alb))
plot(cropAlb)
plot(Alb, add = T, col = 'black')

cropHai <- crop(corine, extent(Hai))
plot(cropHai)
plot(Hai, add = T)

setMinMax(cropHai)

All_table = rbind(data.table(table(Alb_rast))[, c('Region', 'Value') := list('Alb', Alb_rast)][, 2:4],
                  rbind(data.table(table(Hai_rast))[, c('Region', 'Value') := list('Hai',Hai_rast)][, 2:4],
                                  data.table(table(Sch_rast))[, c('Region', 'Value') := list('Sch',Sch_rast)][, 2:4]))
Germany_table = data.table(Value = Germany_list)

clcLeg[, Value := as.numeric(Raster_value)]

All_table = merge(All_table[, Value := as.numeric(Value)], clcLeg[, c('Value', 'Label')])
#Germany_table = merge(Germany_table[, Value := as.numeric(Value)], clcLeg[, c('Value', 'Label')])
#fwrite(Germany_table, 'Temporary_data/Germany_baseline.csv')
Germany_short = Germany_table[, list(N = .N), by = 'Label']
Germany_short[, Land_use_type := dplyr::recode(Label, 'Continuous urban fabric' = 'remove',
                                               'Discontinuous urban fabric' = 'remove',
                                               'Mineral extraction sites' = 'remove',
                                               'Agriculture with natural vegetation' = 'crops',
                                               'Complex cultivation patterns' = 'crops',
                                               'Non-irrigated arable land' = 'crops',
                                               'Pastures' = 'grasslands',
                                               'Moors and heathland' = 'remove',
                                               'Natural grasslands' = 'grasslands',
                                               'Transitional woodland-shrub' = 'forests',
                                               'Coniferous forest' = 'forests',
                                               'Broad-leaved forest' = 'forests',
                                               'Mixed forest' = 'forests',
                                               'Water courses' = 'remove',
                                               'Water bodies' = 'remove',
                                               'Vineyards' = 'crops')
]

Prop_Germany = Germany_short[Land_use_type != 'remove', list(N = sum(N)), by = c('Land_use_type')]
Prop_Germany[, prop := N/sum(N)]



All_table[, prop := N/sum(N)*100, by = c('Region')]
All_table[, Land_use_type := dplyr::recode(Label, 'Continuous urban fabric' = 'remove',
                                       'Discontinuous urban fabric' = 'remove',
                                       'Mineral extraction sites' = 'remove',
                                       'Agriculture with natural vegetation' = 'crops',
                                       'Complex cultivation patterns' = 'crops',
                                       'Non-irrigated arable land' = 'crops',
                                       'Pastures' = 'grasslands',
                                       'Natural grasslands' = 'grasslands',
                                       'Transitional woodland-shrub' = 'remove',
                                       'Coniferous forest' = 'forests',
                                       'Broad-leaved forest' = 'forests',
                                       'Mixed forest' = 'forests',
                                       'Water bodies' = 'remove')
                                       ]

Prop_big = All_table[Land_use_type != 'remove', list(N = sum(N)), by = c('Land_use_type', 'Region')]
Prop_big[, prop := N/sum(N), by = 'Region'][order(Region),]

Prop_forests = All_table[Land_use_type == 'forests', list(N = sum(N)), by = c('Label', 'Region')]
Prop_forests[, prop := N/sum(N), by = 'Region'][order(Region),]

bProp_grasslands = All_table[Land_use_type == 'grasslands', list(N = sum(N)), by = c('Label', 'Region')]
Prop_grasslands[, prop := N/sum(N), by = 'Region']

Prop_corine_GPs = merge(Prop_GP[, list(Type = forest_type, Region = tolower(EXP), Proportion_GP = prop)],
                        merge(Prop_forests[, list(Type = recode(Label, 'Broad-leaved forest' = 'Deciduous', 'Coniferous forest' = 'Coniferous', 'Mixed forest' = 'Mixed'),
                                                          Region = tolower(Region), 
                                                          Proportion_Corine = prop)],
                        data.table(Type = rep(c('Coniferous', 'Mixed', 'Deciduous'), each = 3),
                                   Region = rep(c('alb', 'hai', 'sch'), 3),
                                   Proportion_EPs = c(0.24, 0.02, 0.3, 0.44, 0.34, 0.3, 0.32, 0.64, 0.4))), by = c('Type', 'Region'))

ggplot(melt(Prop_corine_GPs, id.vars = c('Type', 'Region')), aes(value, x = Type, fill =  variable)) + facet_wrap(~Region) +
  geom_col(position = position_dodge()) + ylab('Proportion') +
  scale_fill_brewer(palette = 'Set2') + theme_bw()
