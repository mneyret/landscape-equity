library(data.table)
# Grasslands

Grassland_LUI <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Grassland_data/LUI_standardized.csv", dec = ",")
Grassland_LUI[, EP_Plotid := Plot_id1]
Grassland_LUI[, c("Region") := list(substr(Plot_id1, 1, 1))]
Grassland_LUI[, Classif := cut(LUI_2007to12, breaks = quantile(LUI_2007to12, c(0, 0.33, 0.66, 1)),
                 labels = c("Grassland_low", "Grassland_medium", "Grassland_high"), include.lowest = TRUE),
                                by = Region]
grid_plot_grasslands = fread('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Grassland_data/5120_Grassland\ land\ use\ identification\ on\ all\ Grid\ Plots\ 2006\ -\ 2008_1.98.55/5120.txt',
                             skip = 1)

grid_plot_grasslands[, Region := substr(Plotid, 1, 1)]
grid_plot_grasslands[Size == -1, Size := NA]

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

grid_plot_grasslands[, LUI_all := sqrt(Grazing_norm+Fertilizing_norm+Cuts_norm)]

grid_plot_grasslands_short = grid_plot_grasslands[, list(LUI_all = mean(LUI_all, na.rm = T)), by = c('Plotid', 'EP_Plotid', 'Region')]

Compare_LUI = merge(grid_plot_grasslands_short, Grassland_LUI, all.x = T)
ggplot(Compare_LUI, aes(LUI_2007to12, LUI_all)) + geom_point(aes(color = Classif)) + geom_smooth(method = 'lm') + facet_wrap(~Region) + theme_bw() +
  scale_color_brewer(palette = 'Set1', direction = -1) + ylab('LUI calculated from GP data') + xlab('Mean LUI 2007-2012')

ggplot(Compare_LUI, aes(LUI_all)) + geom_histogram(color = 'grey') + facet_wrap(~Region) +
  geom_histogram(data = Compare_LUI, aes(LUI_2007to12, color = Classif))


mod = lm(LUI_all~LUI_2007to12*Region, data = Compare_LUI)

# Mean in terms of real LUI
mod2 = lm(LUI_2007to12~LUI_all*Region, data = Compare_LUI)
test = Compare_LUI[, list(LUI_all = mean(LUI_all, na.rm = T)), by = Region]
predict(mod2, test)

max_luis = Compare_LUI[!is.na(Classif), list(LUI_2007to12 = max(LUI_2007to12, na.rm = T)), by = c('Classif', 'Region')]
max_luis[, Max_GPs := predict(mod, .SD), .SDcols = c('LUI_2007to12', 'Region')]

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



Compare_LUI[, 'New_classif' := reclassify_LUI(Region, LUI_all), by = 1:nrow(Compare_LUI)]
Proportions = Compare_LUI[New_classif != 'NA', .N, by = c('New_classif', 'Region')][New_classif != 'NA', prop := N/sum(N), by = 'Region']

ggplot(Compare_LUI, aes(LUI_all,fill = New_classif)) + geom_histogram() + facet_wrap(~Region) +
  geom_histogram(data = Compare_LUI, aes(LUI_2007), color = 'grey')


#### Forests, from GPs ####
grid_plot_forests = fread('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/11101_forest\ inventory\ -\ land\ use\ gradient_4.0.0_PublicDataset/11101.txt')

grid_plot_forests[DHpLBH > 80, forest_type := 'Deciduous']
grid_plot_forests[DHpNDH > 80, forest_type := 'Coniferous']
grid_plot_forests[DHpNDH < 80 & DHpLBH < 80, forest_type := 'Mixed']
Prop_GP = grid_plot_forests[, .N, by = c('forest_type', 'EXP')][, prop := N/sum(N), by = EXP]

grid_plot_forests[BST %in% c('femelartig', 'mehrschichtig', 'plenterartig'), Age_class := 'uneven-aged']
grid_plot_forests[!(BST %in% c('femelartig', 'mehrschichtig', 'plenterartig')), Age_class := 'even-aged']

Prop_Age = grid_plot_forests[, .N, by = c('Age_class', 'EXP')][, prop := N/sum(N), by = EXP]

#### Forests, from Corine ####
library(sp)
library(rgdal)
library(raster)
library(rgeos)
library(sf)
library(data.table)

corine = raster('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/data/GIS/Corine/DATA/U2018_CLC2018_V2020_20u1.tif')
Alb =  readOGR("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/data/GIS/Boundaries_Exploratories/ConvexHullAlb.shp")
Hai =  readOGR("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/data/GIS/Boundaries_Exploratories/convexHullHai.shp")
Sch =  readOGR("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/data/GIS/Boundaries_Exploratories/shpconvexHullSch.shp")

clcLeg <- read.csv(url("https://raw.githubusercontent.com/joaofgoncalves/R_exercises_raster_tutorial/master/data/legend_clc.csv"),
                   stringsAsFactors = FALSE)

clcLeg <- data.table(clcLeg[,1:2], 
                     CLC_abr=toupper(abbreviate(gsub("-"," ",clcLeg[,3]), 6)), 
                     Label=clcLeg[,3], row.names = 1:nrow(clcLeg))

Alb <- spTransform(Alb, crs(corine))
Hai <- spTransform(Hai, crs(corine))
Sch <- spTransform(Sch, crs(corine))

plot(corine)
plot(Alb, add = T, col = 'black')
plot(Hai, add = T, col = 'black')
plot(Sch, add = T, col = 'black')

Alb_rast = extract(corine, Alb)
Hai_rast = extract(corine, Hai)
Sch_rast = extract(corine, Sch)

cropAlb <- crop(corine, extent(Alb))
plot(cropAlb)
plot(Alb, add = T, col = 'black')

cropHai <- crop(corine, extent(Hai))
plot(cropHai)
plot(Hai, add = T)

setMinMax(cropHai)

View(legend)

All_table = rbind(data.table(table(Alb_rast))[, c('Region', 'Value') := list('Alb', Alb_rast)][, 2:4],
                  rbind(data.table(table(Hai_rast))[, c('Region', 'Value') := list('Hai',Hai_rast)][, 2:4],
                                  data.table(table(Sch_rast))[, c('Region', 'Value') := list('Sch',Sch_rast)][, 2:4]))

clcLeg[, Value := as.numeric(Raster_value)]

All_table = merge(All_table[, Value := as.numeric(Value)], clcLeg[, c('Value', 'Label')])

All_table[, prop := N/sum(N)*100, by = c('Region')]
All_table[, Big_class := dplyr::recode(Label, 'Continuous urban fabric' = 'remove',
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

Prop_big = All_table[Big_class != 'remove', list(N = sum(N)), by = c('Big_class', 'Region')]
Prop_big[, prop := N/sum(N), by = 'Region'][order(Region),]

Prop_forests = All_table[Big_class == 'forests', list(N = sum(N)), by = c('Label', 'Region')]
Prop_forests[, prop := N/sum(N), by = 'Region'][order(Region),]

bProp_grasslands = All_table[Big_class == 'grasslands', list(N = sum(N)), by = c('Label', 'Region')]
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
