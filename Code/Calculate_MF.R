library(data.table)
library(ghibli)
library(wesanderson)
library(emojifont)
library(ggnewscale)
library(Hmisc)

#for (env_corr in c('env_corr', '')){
  if (env_corr == 'env_corr'){
    sim_data = fread(paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/raw_sim_data_' , env_corr, '.csv', sep = ''))
  } else {
    sim_data = fread(paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/raw_sim_data_' , env_corr, '.csv', sep = ''))
  }


my_palette = c(ghibli_palette('LaputaMedium', 7, direction = 1, type = c("discrete")),
               ghibli_palette('PonyoMedium', 7, direction = 1, type = c("discrete")))
my_palette_services = c("lightsteelblue1","lightsteelblue2", "lightsteelblue3","lightsteelblue4",
                        "burlywood1","sandybrown", "lightsalmon1","darksalmon","lightsalmon3","salmon4",
                        "paleturquoise4")
mf_method = 'average'
#for (mf_method in c('threshold_availability', 'average', 'threshold50')){

#### 1. Transform data based on chosen method ####
sim_data[, (all_indicators) := lapply(.SD, as.numeric), .SDcols = all_indicators]

if (mf_method %in% c('average', 'threshold_availability')){
sim_data[,  (all_indicators) := lapply(.SD, function(x){
 print("______________________") 
  x = as.numeric(x)
     max <- quantile(x, 0.975, na.rm = T)
     min <- quantile(x, 0.025, na.rm = T)
     y <- (x - min) / (max - min)
   y[y < 0] <- 0
   y[y > 1] <- 1
      return(y)
}), .SDcols = all_indicators, by = Region]
  }

#if (mf_method == 'threshold50'){
#  sim_data[,  (all_indicators) := lapply(.SD, function(x){
#    print("______________________") 
#    x = as.numeric(x)
#    max <- quantile(x, 0.975, na.rm = T)
#    threshold = max/3
#    y = x
#    y[y < threshold] <- 0
#    y[y >= threshold] <- 1
#    return(y)
#  }), .SDcols = all_indicators, by = Region]}

average_data = sim_data[, list(
                         Ric = Ric_plants*0.5 +  Ric_birds*0.5, 
                         Hunting = Hunting,
                         Harvesting = Harvesting,
                         Production_food = Production_food,
                         Production_livestock = Production_livestock,
                         Production_timber = Production_timber,
                         Production_energy  = Production_energy,
                         Aesthetic = (Aesthetic_naturalness + (Aest_diversity_landscape + Aesth_diversity_ADI)/2)/2,
                         Leisure = (Leisure_landscape + Leisure_openness)/2,
                         Reg_id = Reg_ID,
                         C_stock = C_stock),
                    by = c('Scenarios_land_use', 'Region', 'Rep')]

average_data$Scenarios_land_use = factor(average_data$Scenarios_land_use, 
                                         levels = c('Baseline', 'Equal_areas',
                                                    'More_forests', 'Only_forest',
                                                    'More_grasslands', 'Only_grasslands',
                                                    'More_crops', 'Only_crops',
                                                    "Low_grasslands" , "High_grasslands",
                                                    'Crops_invade_grasslands', 'Abandonment',
                                                    "UAForests_invade_grasslands", "EAForests_invade_grasslands",
                                                    "Mixed_forests", "Monoculture_forests",
                                                    "UnevenAged_forests","EvenAged_forests",
                                                    "Back_to_nature", "Productive_landscape"
                                                    ))


average_data = average_data[Scenarios_land_use %in% c('Baseline', 'Abandonment',
                                                      'More_forests', 'Only_forest',
                                                      'More_grasslands', 'Only_grasslands',
                                                      'More_crops', 'Crops_invade_grasslands',
                                                      "Low_grasslands" ,"EAForests_invade_grasslands",
                                                      "Mixed_forests", "UAForests_invade_grasslands",
                                                      "Back_to_nature", "Productive_landscape")]

groups = data.table(Group = unique(Scenarios_demand_use$Group))
average_data0 = copy(average_data)
whole_data = copy(data.table(groups[, average_data0[], by = Group]))
cols = colnames(whole_data)[5:15]

# If we threshold based on what stakeholders say is available
# if (mf_method == 'threshold_availability'){
#  for (group in unique(whole_data$Group)){
#   for (region in unique(whole_data$Region)){
#    group_data = whole_data[Group == group & Region == region,]
#    group_data[, paste0(cols, '_mean') := lapply(group_data[Scenarios_land_use == "Baseline", ..cols], mean)]
#    group_data_mean = group_data[, grepl('_mean', colnames(group_data)), with = FALSE]
#    colnames(group_data_mean) = gsub('_mean', '', colnames(group_data_mean))
#    group_data_diff <- group_data[, cols, with = FALSE] - group_data[, paste0(cols, "_mean"), with = FALSE] 
#    group_data = data.frame(group_data[,  .SD, .SDcols = colnames(group_data)[!grepl('_mean', colnames(group_data))]])
#    availability = Availability_to_use[Group == group & Region == region,]
#    if(nrow(availability) != 0){
#       for (ES in cols){
#        if(availability[[ES]] == 1){
#          group_data[, ES][ group_data_diff[, ..ES] > 0] = max(group_data_mean[, ..ES]) }
#        if(availability[[ES]] == 0){
#          group_data[, ES][ group_data_diff[, ..ES] < 0] = min(group_data_mean[, ..ES]) }
#       }}
#    whole_data[Group == group & Region == region,] = data.table(group_data)
#   }}
  # We also need to rescale everything between 0 and 1
#  whole_data[, (cols) := lapply(.SD, function(x){
#                                x = as.numeric(x)
#                                Max <- max(x, na.rm = T)
#                                Min <- min(x, na.rm = T)
#                                y <- (x - Min) / (Max - Min)
#                                return(y)}), .SDcols = cols, by = Region]

#}

## Then we want to look at raw service values and differences compared to baseline.
# The reason we don't do it at the same time as the previous step (when calculating group_data_diff)
# is that in the case of mf_method == 'threshold_availability' we have to consider the differences accordingly.

whole_data[, paste0(cols, '_mean') := lapply(.SD[Scenarios_land_use == "Baseline"], mean), .SDcols = cols, by = Region
           ]
whole_data_mean = whole_data[, grepl('_mean', colnames(whole_data)), with = FALSE]
whole_data_diff = cbind(whole_data[, c( 'Group', 'Scenarios_land_use', 'Region', 
                                        'Rep')],
                        whole_data[, cols, with = FALSE] - whole_data_mean[, paste0(cols, "_mean"), with = FALSE] )
# remove temporary columns
whole_data = whole_data[, .SD, .SDcols = colnames(whole_data)[!grepl('_mean', colnames(whole_data))]]
  
whole_data_melt = merge(
                  melt(whole_data, value.name = 'value', id.vars = c( 'Group', 'Scenarios_land_use',  'Region', 
                                                                      'Rep')),
                  melt(whole_data_diff, value.name = 'value_diff', id.vars = c( 'Group', 'Scenarios_land_use','Region', 
                                                                                'Rep')),
                  by = c( 'Group', 'Scenarios_land_use',
                          'Region', 'Rep', 'variable'))
         
# For ES-level plots, we keep everything if mf_method == 'threshold_availability' 
# otherwise, to avoid repeated rows (no difference among stakeholde groups), we subset by one group only
#if (mf_method == 'threshold_availability'){
#  Diff_by_service_mean = whole_data_melt[, list(mean_value = mean(value), mean_diff = mean(value_diff) , sd = sd(value_diff)), by = c('Scenarios_land_use', 'Region', 'variable')]
#}
if (mf_method != 'threshold_availability'){
  Diff_by_service_mean = whole_data_melt[Group == 'Agric', list(mean_value = mean(value), mean_diff = mean(value_diff) , sd = sd(value_diff)), by = c('Scenarios_land_use', 'Region', 
                                                                                                                                                      'variable')]
}

#Diff_by_service_mean = merge(Diff_by_service_mean, Availability_of_ES, by = c('variable','Region' 
#                                                                             ), all = TRUE)
Diff_by_service_mean = merge(Diff_by_service_mean, Availability_of_ES_overall, by = c('variable'), all = TRUE)

Diff_by_service_mean[endangered == TRUE & abs(mean_diff)>sd & mean_diff >0, Status := 'Improved']
Diff_by_service_mean[endangered == TRUE & abs(mean_diff)>sd & mean_diff <0, Status := 'Worsened']
Diff_by_service_mean = Diff_by_service_mean[!is.na(Scenarios_land_use),]
Diff_by_service_mean[is.na(Status), Status := 'NA']
Summary_availability = Diff_by_service_mean[!is.na(Status), list(Worsened = length(Status[Status == 'Worsened']),
                                                                 Improved = length(Status[Status == 'Improved'])), by = c('Region',
                                                                   'Scenarios_land_use')]

Diff_by_service_mean[, variable := factor(variable, levels = c('Ric', 'Aesthetic', 'Reg_id', 'Leisure',
                                                               'Production_food','Production_livestock', 'Production_timber',  'Production_energy', 'Harvesting', 'Hunting', 
                                                               'C_stock'))]
barplot_plot_raw = ggplot(Diff_by_service_mean, aes(y = mean_value, 
                                            x = variable,
                                            ymin = mean_value - sd, 
                                            ymax = mean_value + sd, 
                                            group = Region, 
                                            fill = variable)) + 
  theme_bw()+
  geom_col(position = position_dodge(width = 1)) +
  scale_fill_manual(values = my_palette_services, 
                      labels = c('Richness', 'Aesthetic', 'Leisure','Regional id', 
                                 'Production food', 'Production livestock', 'Production timber', 'Production energy', 'Harvesting',  'Hunting',
                                                             'C stock')) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 1))+
  theme(legend.position = "bottom") +
  facet_wrap(~Scenarios_land_use, ncol = 2) +
  geom_hline(yintercept=0) +
  new_scale_fill() +
  ylim(c(0, 1.2))+
  ylab('Service value') +
  geom_point(data = Diff_by_service_mean[!is.na(Status),], aes(y = max(mean_value)+0.1, shape = Status,  group = Region, fill = Status, color = Status), position = position_dodge(width = 1.02), alpha = 0.6) +
  scale_shape_manual(values = c(24, 25)) +  scale_fill_manual(values = c('chartreuse4', 'orangered3')) +  scale_color_manual(values = c('chartreuse4', 'orangered3'))
ggsave(plot = barplot_plot_raw, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/barplot_plot_raw_', mf_method, '_', env_corr, '.pdf', sep = ''), width= 12, height = 20)


barplot_plot_diff = ggplot(Diff_by_service_mean, aes(y = mean_diff, 
                                                    x = variable,
                                                    ymin = mean_diff - sd, 
                                                    ymax = mean_diff + sd, 
                                                   # group = Region, 
                                                    fill = variable)) + 
  geom_col(position = position_dodge(width = 1)) +   theme_bw()+
  scale_fill_manual(values = my_palette_services, 
                    labels = c('Richness', 'Aesthetic', 'Leisure','Regional id', 
                               'Production food', 'Production livestock', 'Production timber', 'Production energy', 'Harvesting',  'Hunting',
                               'C stock')) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 1))+
  theme(legend.position = "bottom") +
  facet_wrap(~Scenarios_land_use, ncol = 2) +
  geom_hline(yintercept=0) +
  new_scale_fill() +
  #ylim(c(-0.85, 0.85))+
  geom_point(data = Diff_by_service_mean[!is.na(Status),], aes(y = max(mean_diff)+0.15, x = variable, shape = Status,  group = Region, 
                                                               fill = Status, color = Status), position = position_dodge(width = 1), alpha = 0.6) +
  scale_shape_manual(values = c(24,NA, 25)) +  scale_fill_manual(values = c('chartreuse4','white', 'orangered3')) +  scale_color_manual(values = c('chartreuse4','white', 'orangered3')) 


ggsave(plot = barplot_plot_diff, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/barplot_plot_diff_', mf_method, '_', env_corr,'.pdf', sep = ''), device = 'pdf', width= 8, height = 20)


####  Calculate MF ####
# Scenarios_demand_use
# Scenarios_demand

# List_scenario_MF =  apply(Scenarios_demand_use, 1, function(x){
#   print(x)
#   x = as.list(x)
#   Group = x$Group
#   availablity = Availability2[Group == Group,]
#   Weights = lapply(x[3:13], as.numeric)
#   Average_with_MF = average_data[, list(Scenarios_land_use = Scenarios_land_use,
#                                         Region = Region,
#                                         Rep = Rep,
#                                         MF = (Weights$Ric * Ric + 
#                                                 Weights$Hunting * Hunting + 
#                                                 Weights$Harvesting * Harvesting + 
#                                                 Weights$Production_food * Production_food +
#                                                 Weights$Production_livestock * Production_livestock +
#                                                 Weights$Production_energy * Production_energy +
#                                                 Weights$Reg_id * Reg_id +
#                                                 Weights$Leisure * Leisure +            
#                                                 Weights$Production_timber * Production_timber +
#                                                 Weights$Aesthetic * Aesthetic + 
#                                                 Weights$C_stock * C_stock)/
#                                           sum(unlist(Weights)))]
#   Average_with_MF[, c('Group', 'Demand_scenario') := list(x$Group, x$Demand_scenario)]
#   return(Average_with_MF)
# })


whole_data
whole_data2 = copy(whole_data) 
List_scenario_MF =  apply(Scenarios_demand_use, 1, function(x){
  #print(x)
  x = as.list(x)
  Weights = lapply(x[3:13], as.numeric)
  group = x$Group
  Average_with_MF = whole_data2[Group == group,
                                list(Scenarios_land_use = Scenarios_land_use,
                                     Region = Region,
                                     Rep = Rep,
                                     Group = group,
                                     Ric = Weights$Ric * Ric /sum(unlist(Weights)),
                                     Hunting =  Weights$Hunting * Hunting /sum(unlist(Weights)),
                                     Harvesting = Weights$Harvesting * Harvesting /sum(unlist(Weights)),
                                     Production_food = Weights$Production_food * Production_food /sum(unlist(Weights)),
                                     Production_livestock = Weights$Production_livestock * Production_livestock /sum(unlist(Weights)),
                                     Production_energy = Weights$Production_energy * Production_energy /sum(unlist(Weights)),
                                     Reg_id = Weights$Reg_id * Reg_id /sum(unlist(Weights)),
                                     Leisure = Weights$Leisure * Leisure /sum(unlist(Weights)),
                                     Production_timber = Weights$Production_timber * Production_timber /sum(unlist(Weights)),
                                     Aesthetic = Weights$Aesthetic * Aesthetic /sum(unlist(Weights)),
                                     C_stock = Weights$C_stock * C_stock /sum(unlist(Weights)))]
    Average_with_MF[, MF := sum(Ric, Hunting, Harvesting, Production_food, Production_livestock, Production_energy, Reg_id, Leisure, Production_timber, Aesthetic, C_stock),
                    by = 1:nrow(Average_with_MF)]
    Average_with_MF[, c('Demand_scenario') := list(x$Demand_scenario)]
    return(Average_with_MF)
  })

MF_melt = rbindlist(List_scenario_MF)

MF_melt[, MF_mean := lapply(.SD[Scenarios_land_use == "Baseline"], mean), .SDcols = 'MF', by = c( 'Region', 
  'Demand_scenario', 'Group')]
MF_melt[, MF_diff := MF - MF_mean]

#MF_melt_w_power = merge(MF_melt, Power, by = c('Group', 'Region'))
MF_melt_w_power = merge(MF_melt, Power[, list(Perceived_influence = mean(Perceived_influence)), by = c('Group', 'ES_priority')]
                        , by = c('Group', 'Region'
                                 ))

# supply_PCA
#supply_PCA_A = dudi.pca(MF_melt_w_power[Region == 'A' & Scenarios_land_use == 'Baseline' & Group == 'Agric' & Demand_scenario == 'current_demand', 5:15])
#supply_PCA_H = dudi.pca(MF_melt_w_power[Region == 'H' & Scenarios_land_use == 'Baseline' & Group == 'Agric' & Demand_scenario == 'current_demand', 5:15])
#supply_PCA_S = dudi.pca(MF_melt_w_power[Region == 'S' & Scenarios_land_use == 'Baseline' & Group == 'Agric' & Demand_scenario == 'current_demand', 5:15])
#fviz_pca(supply_PCA_A, title = 'Alb', alpha.ind = 0.3, geom = 'point')
#fviz_pca(supply_PCA_H, title = 'Hainich', alpha.ind = 0.3, geom = 'point')
#fviz_pca(supply_PCA_S, title = 'Schorfheide', alpha.ind = 0.3, geom = 'point')


MF_melt_w_power_aggregate = MF_melt_w_power[, list('mean_value'= mean(MF), 
                                                   'mean_diff' = mean(MF_diff), 
                                                   'sd' = sd(MF_diff),
                                                   'Ric' = mean(Ric),
                                                   'Hunting' = mean(Hunting), 
                                                   'Harvesting' = mean(Harvesting),
                                                   'Production_food' = mean(Production_food),
                                                   'Production_livestock' = mean(Production_livestock),
                                                   'Production_energy'    = mean(Production_energy),
                                                   'Reg_id' = mean(Reg_id),   
                                                   'Leisure' = mean(Leisure),
                                                   'Production_timber' = mean(Production_timber),
                                                   'Aesthetic' = mean(Aesthetic),
                                                   'C_stock' = mean(C_stock)
                                                   ),
                                            by = c('Scenarios_land_use','ES_priority', 'Perceived_influence', # Uncomment here for region-specific values 
                                                   'Group','Demand_scenario', 'Region'
                                                   )]


#### Now I should have all the data tables I need to plot stuff ####

### Plot per general interest groups
bar_plot_MF_big_groups = ggplot(MF_melt_w_power_aggregate[Demand_scenario =='current_demand', ], aes(y = mean_value, 
                                                                                                          x = Scenarios_land_use,
                                                                                                          ymin = mean_value - sd, 
                                                                                                          ymax = mean_value + sd, 
                                                                                                          #group = Region, 
                                                                                                          fill = Scenarios_land_use)) + 
  geom_col(position = position_dodge(width = 1)) + theme_bw() +
  scale_fill_manual(values = my_palette) +
  #geom_errorbar(width = 0.5, position = position_dodge(width = 1))+
  theme(legend.position = "bottom") +
  facet_wrap(~ES_priority, ncol = 2) +
  geom_hline(yintercept=0) #+
# geom_text(data = Summary_availability, aes(label = paste('Worsened:', Worsened, ' - Improved:', Improved), x = 5, y = 0.2), inherit.aes = F, size = 2) 
ggsave(plot = bar_plot_MF_big_groups, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/bar_plot_MF_big_groups_', mf_method, '_', env_corr,'.pdf', sep = ''), width= 8, height = 8)


bar_plot_diff_MF_big_groups = ggplot(MF_melt_w_power_aggregate[Demand_scenario =='current_demand', ], aes(y = mean_diff, 
                                                                            x = ES_priority,
                                                                            ymin = mean_diff - sd, 
                                                                            ymax = mean_diff + sd, 
                                                                            group = Region, 
                                                                            fill = ES_priority)) + 
  geom_col(position = position_dodge(width = 1)) +
  scale_fill_manual(values = my_palette[c(2, 6, 9, 10)]) +
  #geom_errorbar(width = 0.5, position = position_dodge(width = 1))+
  theme(legend.position = "bottom") +
  facet_wrap(~Scenarios_land_use, ncol = 2) +
  geom_hline(yintercept=0) #+
 # geom_text(data = Summary_availability, aes(label = paste('Worsened:', Worsened, ' - Improved:', Improved), x = 5, y = 0.2), inherit.aes = F, size = 2) 
ggsave(plot = bar_plot_diff_MF_big_groups, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/bar_plot_diff_MF_big_groups_', mf_method, '_', env_corr,'.pdf', sep = ''), width= 12, height = 20)


### Plot per detailed interest groups
bar_plot_diff_MF = ggplot(MF_melt_w_power_aggregate[Demand_scenario =='current_demand', ], aes(y = mean_diff, 
                                            x = Group,
                                            ymin = mean_diff - sd, 
                                            ymax = mean_diff + sd, 
                                            group = Region, 
                                            fill = Group)) + 
  geom_col(position = position_dodge(width = 1)) +
  scale_fill_manual(values = my_palette) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 1))+
  theme(legend.position = "bottom") +  theme_bw() +
  facet_wrap(~Scenarios_land_use, ncol = 2) +
  geom_hline(yintercept=0) #+
 # geom_text(data = Summary_availability, aes(label = paste('Worsened:', Worsened, ' - Improved:', Improved), x = 5, y = 0.2), inherit.aes = F, size = 2) 
ggsave(plot = bar_plot_diff_MF, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/bar_plot_diff_MF_', mf_method, '_', env_corr,'.pdf', sep = ''), width= 8, height = 20)

bar_plot_MF = ggplot(MF_melt_w_power_aggregate[Demand_scenario =='current_demand', ], aes(y = mean_value, 
                                                                                               x = Group,
                                                                                               ymin = mean_value - sd, 
                                                                                               ymax = mean_value + sd, 
                                                                                               group = Region, 
                                                                                               fill = Group)) +   geom_col(position = position_dodge(width = 1)) +
  scale_fill_manual(values = my_palette) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 1))+
  theme(legend.position = "bottom") + theme_bw() +
  facet_wrap(~Scenarios_land_use, ncol = 2) +
  geom_hline(yintercept=0)
ggsave(plot = bar_plot_MF, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/bar_plot_MF_', mf_method,'_', env_corr, '.pdf', sep = ''), width= 12, height = 20)


### What if I change the demand
bar_plot_diff_MF_increased_sp = ggplot(MF_melt_w_power_aggregate[Demand_scenario =='increased_specialisation', ], aes(y = mean_diff, 
                                                                                               x = Group,
                                                                                               ymin = mean_diff - sd, 
                                                                                               ymax = mean_diff + sd, 
                                                                                               group = Region, 
                                                                                               fill = Group)) + 
  geom_col(position = position_dodge(width = 1)) +
  scale_fill_manual(values = my_palette) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 1))+
  theme(legend.position = "bottom") +
  facet_wrap(~Scenarios_land_use, ncol = 2) +
  geom_hline(yintercept=0)
ggsave(plot = bar_plot_diff_MF_increased_sp, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/bar_plot_diff_MF_increased_sp_', mf_method,'_', env_corr, '.pdf', sep = ''), width= 12, height = 20)

bar_plot_diff_MF_decreased_sp = ggplot(MF_melt_w_power_aggregate[Demand_scenario =='decreased_specialisation', ], aes(y = mean_diff, 
                                                                                                                      x = Group,
                                                                                                                      ymin = mean_diff - sd, 
                                                                                                                      ymax = mean_diff + sd, 
                                                                                                                      group = Region, 
                                                                                                                      fill = Group)) + 
  geom_col(position = position_dodge(width = 1)) +
  scale_fill_manual(values = my_palette) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 1))+
  theme(legend.position = "bottom") +
  facet_wrap(~Scenarios_land_use, ncol = 2) +
  geom_hline(yintercept=0)
ggsave(plot = bar_plot_diff_MF_decreased_sp, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/bar_plot_diff_MF_decreased_sp_', mf_method, '_', env_corr,'.pdf', sep = ''), width= 12, height = 20)

bar_plot_diff_MF_main3 = ggplot(MF_melt_w_power_aggregate[Demand_scenario =='main3', ], aes(y = mean_diff, 
                                                                               x = Group,
                                                                               ymin = mean_diff - sd, 
                                                                                          ymax = mean_diff + sd, 
                                                                                              group = Region, 
                                                                                                                      fill = Group)) + 
  geom_col(position = position_dodge(width = 1)) +
  scale_fill_manual(values = my_palette) +
  geom_errorbar(width = 0.5, position = position_dodge(width = 1))+
  theme(legend.position = "bottom") +
  facet_wrap(~Scenarios_land_use, ncol = 2) +
  geom_hline(yintercept=0)
ggsave(plot = bar_plot_diff_MF_main3, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/bar_plot_diff_MF_main3_', mf_method, '_', env_corr, '.pdf', sep = ''), width= 12, height = 20)



# See services within each MF
#MF_melt_w_power_aggr_melt = melt(MF_melt_w_power_aggregate[Demand_scenario == 'current_demand',], id.vars = c('Scenarios_land_use',   'ES_priority', 'Demand_scenario', 'mean_value', 'sd', 'mean_diff'))

#MF_melt_w_power_aggr_melt$variable = factor(MF_melt_w_power_aggr_melt$variable,
#                                            levels = names(sort(Scenarios_demand_use[, lapply(.SD, sum), .SDcols = c('Production_food', 'Production_livestock',  'Ric',   
#                                                                                                                     'C_stock', 'Production_timber', 'Production_energy',
#                                                                                                                     'Harvesting',  'Aesthetic',      'Reg_id',    'Leisure',     'Hunting')])))

#ggplot(MF_melt_w_power_aggr_melt, aes(y = value, x = ES_priority, fill = variable)) + 
#  geom_col() +
#  theme(legend.position = "bottom") +
#  facet_wrap(~Scenarios_land_use, ncol = 2) +
#  geom_point(data = MF_melt_w_power_aggr_melt[variable == 'Ric',],
#             aes(y = mean_value), alpha = 0.5, size = 0.5) +
#  geom_errorbar(data = MF_melt_w_power_aggr_melt[variable == 'Ric',],
#                aes(y = mean_value,
#                      ymin = mean_value - sd, 
#                     ymax = mean_value + sd), width = 0.3) +
#  scale_fill_manual(values = my_palette[-c(2, 4, 12)][11:1])

# %%%%%%%%%%%%%%%%%%%%%%%%%%% #   
#### Conflicts and harmony ####
# %%%%%%%%%%%%%%%%%%%%%%%%%%% #   

MF_harmony_per_Rep = MF_melt_w_power[, list(harmony = sqrt(1/var(MF)), 
                                            harmony_weighted = sqrt(1/wtd.var(MF, Perceived_influence)), 
                                            meanMF = mean(MF),
                                            meanMF_weighted = wtd.mean(MF, Perceived_influence)), 
                                     by = c('Scenarios_land_use', 'Region', 'Rep', 'Demand_scenario') ][order(Scenarios_land_use),]
MF_harmony_per_Rep_melt = melt.data.table(MF_harmony_per_Rep, value.var = c('harmony', 'meanMF', 'harmony_weighted'), id.var = c('Scenarios_land_use', 'Region', 'Rep', 'Demand_scenario' ))
MF_harmony_per_Rep_melt[, c('weighted.by.power', 'type') := list(as.character(grepl('_weighted', variable)), gsub('_weighted', '', variable))]
MF_harmony_per_Rep_melt[weighted.by.power == TRUE, weighted.by.power := "Weighted by power"]
MF_harmony_per_Rep_melt[weighted.by.power == FALSE, weighted.by.power := "Not weighted"]

MF_harmony_per_Rep2 = dcast.data.table(MF_harmony_per_Rep_melt, Scenarios_land_use+ Region + Rep  +    Demand_scenario + weighted.by.power ~ type, value.var = 'value')
MF_harmony_per_Rep2_agg = MF_harmony_per_Rep2[, list(harmony  = mean(harmony),
                                                     meanMF = mean(meanMF),
                                                     sd_harmony  = sd(harmony),
                                                     sd_meanMF = sd(meanMF)), by = c('Scenarios_land_use', 'Region', 'Demand_scenario', 'weighted.by.power')]


Summary_availability_by_region = Diff_by_service_mean[!is.na(Status), list(Worsened = length(Status[Status == 'Worsened']),
                                                                 Improved = length(Status[Status == 'Improved'])), by = c('Scenarios_land_use', 'Region')]
Summary_availability_by_region[, score :=  Improved - Worsened]
MF_harmony_per_Rep2_agg = merge(MF_harmony_per_Rep2_agg, Summary_availability_by_region[, c('Scenarios_land_use', 'Region', 'score')], all = TRUE)





remove_baseline3 = function(region, scenario, demand_sc, w, val, data){
  m = mean(data[Region == region & Demand_scenario == demand_sc & weighted.by.power == w & Scenarios_land_use == 'Baseline', get(val)], na.rm = T)
  value_diff = data[Region == region & Demand_scenario == demand_sc & weighted.by.power == w & Scenarios_land_use == scenario, get(val)]-m
  return(value_diff)
}


MF_harmony_per_Rep2_agg[, 'meanMF_diff' :=  list(remove_baseline3(Region, Scenarios_land_use, Demand_scenario, weighted.by.power, 'meanMF', MF_harmony_per_Rep2_agg)),  by = c('Scenarios_land_use', 'Region', 'Demand_scenario', 'weighted.by.power')]
MF_harmony_per_Rep2_agg[, 'harmony_diff' :=  list(remove_baseline3(Region, Scenarios_land_use,Demand_scenario, weighted.by.power, 'harmony', MF_harmony_per_Rep2_agg)),  by = c('Scenarios_land_use', 'Region', 'Demand_scenario', 'weighted.by.power')]


MF_harmony = MF_melt_w_power_aggregate[, list(harmony = sqrt(1/var(mean_value)), 
                                              harmony_weighted = sqrt(1/wtd.var(mean_value, Perceived_influence)), 
                                              meanMF = mean(mean_value),
                                              meanMF_weighted = wtd.mean(mean_value, Perceived_influence)), 
                                        by = c('Scenarios_land_use', 'Region', 'Demand_scenario') ][order(Scenarios_land_use),]
MF_harmony_melt = melt.data.table(MF_harmony, value.var = c('harmony', 'meanMF', 'harmony_weighted'), id.var = c('Scenarios_land_use', 'Region',  'Demand_scenario' ))
MF_harmony_melt[, c('weighted.by.power', 'type') := list(grepl('_weighted', variable), gsub('_weighted', '', variable))]
MF_harmony2 = dcast.data.table(MF_harmony_melt, Scenarios_land_use+ Region +Demand_scenario + weighted.by.power ~ type, value.var = 'value')

library(ggnewscale)

harmony_plot_per_rep = ggplot(MF_harmony_per_Rep2_agg, aes(x = meanMF, y= harmony,fill = Scenarios_land_use, shape = Region)) +
   theme_bw(base_size = 8) + facet_grid(Demand_scenario~weighted.by.power) +
  geom_errorbar(aes(ymin = harmony-sd_harmony, ymax = harmony+sd_harmony, color = Scenarios_land_use), width = 0, alpha = 0.5) + 
  geom_errorbarh(aes(xmin = meanMF-sd_meanMF, xmax = meanMF+sd_meanMF, color = Scenarios_land_use), height = 0, alpha = 0.5) +
  scale_color_manual(values= rep(ghibli_palette(7, name = "PonyoMedium", type = "continuous"), each = 2)) +
  
  new_scale_color() +
  scale_color_manual(values= c(rbind(rep('black', 7), ghibli_palette(7, name = "PonyoMedium", type = "continuous")))) +
  scale_fill_manual(values= c(rbind(ghibli_palette(7, name = "PonyoMedium", type = "continuous"), rep('White', 7) ))) +
  geom_point(size = 1.5, aes(color = Scenarios_land_use)) +
  xlab('Mean MF across stakeholder groups') +
  ylab('Harmony: 1/(sd MF)') +
  scale_shape_manual(values = c(21, 24, 22)) +
  guides(fill = guide_legend(override.aes = list(shape = 21)))
ggsave(plot = harmony_plot_per_rep, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/harmony_plot_per_rep_NOWEIGHT', mf_method,'_', env_corr, '.pdf', sep = ''), width= 7, height = 5.5)

harmony_plot_per_rep_diff = ggplot(MF_harmony_per_Rep2_agg[Demand_scenario == 'current_demand' & weighted.by.power != 'Not weighted',], aes(x = meanMF_diff, y= harmony_diff,fill = Scenarios_land_use, shape = Region)) +
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0), fill = "#fee08b", alpha = 1) + 
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -0, ymax = +Inf), fill = "#ffffbf", alpha = 1) + 
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0), fill = "#ffffbf", alpha = 1) + 
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0, ymax = Inf), fill = "#e6f598", alpha = 1) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "white", alpha = 0.02) +
  geom_hline(yintercept = c( -20, -10, 0, 10, 20), color = "gray70", lwd = 0.05) +
  geom_vline(xintercept = seq(-0.2, 0.2, by = 0.05), color = "gray70", lwd = 0.05) +
  geom_hline(yintercept = c(-20, 0,  20), color = "gray60", lwd = 0.1) +
  geom_vline(xintercept = seq(-0.2, 0.2, by = 0.1), color = "gray60", lwd = 0.1) +
  geom_hline(yintercept = 0, lwd = 0.1) + geom_vline(xintercept = 0, lwd = 0.15)+
  theme_bw(base_size = 8) +# facet_grid(Demand_scenario~weighted.by.power) +
 # geom_errorbar(aes(ymin = harmony_diff-sd_harmony, ymax = harmony_diff+sd_harmony, color = Scenarios_land_use), width = 0, alpha = 0.3) + 
#  geom_errorbarh(aes(xmin = meanMF_diff-sd_meanMF, xmax = meanMF_diff+sd_meanMF, color = Scenarios_land_use), height = 0, alpha = 0.3) +
  #scale_color_manual(values= rep(ghibli_palette(7, name = "PonyoMedium", type = "continuous"), each = 2)) +
  new_scale_color() +
  scale_color_manual(values= c(rbind(rep('black', 7), ghibli_palette(7, name = "PonyoMedium", type = "continuous")))) +
  scale_fill_manual(values= c(rbind(ghibli_palette(7, name = "PonyoMedium", type = "continuous"), rep('White', 7) ))) +
  geom_point(size = 2, aes(color = Scenarios_land_use)) +
  xlab('Mean MF across stakeholder groups') +
  ylab('Harmony: 1/(sd MF)') +
  xlim(c(-0.25, 0.1)) +
  #ylim(c(-10, 10)) +
  scale_shape_manual(values = c(21, 24, 22)) +
  guides(fill = guide_legend(override.aes = list(shape = 21)))
ggsave(plot = harmony_plot_per_rep_diff, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/harmony_plot_per_rep_diff_WEIGHT', mf_method,'_', env_corr, '.pdf', sep = ''),  width= 8, height =5)

score_plot_per_rep_diff = ggplot(MF_harmony_per_Rep2_agg[Demand_scenario == 'current_demand' & weighted.by.power == 'Not weighted',], aes(x = meanMF_diff, y= score, fill = Scenarios_land_use, shape = Region)) +
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -Inf, ymax = 0), fill = "#fee08b", alpha = 1) + 
  geom_rect(aes(xmin = -Inf, xmax = 0, ymin = -0, ymax = +Inf), fill = "#ffffbf", alpha = 1) + 
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -Inf, ymax = 0), fill = "#ffffbf", alpha = 1) + 
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0, ymax = Inf), fill = "#e6f598", alpha = 1) + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "white", alpha = 0.02) +
  geom_hline(yintercept = c(-6, -2, 2), color = "gray70", lwd = 0.05) +
  geom_vline(xintercept = seq(-0.2, 0.2, by = 0.05), color = "gray70", lwd = 0.05) +
  geom_hline(yintercept = c(-4, 0), color = "gray60", lwd = 0.1) +
  geom_vline(xintercept = seq(-0.2, 0.2, by = 0.1), color = "gray60", lwd = 0.1) +
  geom_hline(yintercept = 0, lwd = 0.1) + geom_vline(xintercept = 0, lwd = 0.15)+
  theme_bw(base_size = 8) +# facet_grid(Demand_scenario~weighted.by.power) +
  # geom_errorbar(aes(ymin = harmony_diff-sd_harmony, ymax = harmony_diff+sd_harmony, color = Scenarios_land_use), width = 0, alpha = 0.3) + 
  #  geom_errorbarh(aes(xmin = meanMF_diff-sd_meanMF, xmax = meanMF_diff+sd_meanMF, color = Scenarios_land_use), height = 0, alpha = 0.3) +
  scale_color_manual(values= rep(ghibli_palette(7, name = "PonyoMedium", type = "continuous"), each = 2)) +
  new_scale_color() +
  scale_color_manual(values= c(rbind(rep('black', 7), ghibli_palette(7, name = "PonyoMedium", type = "continuous")))) +
  scale_fill_manual(values= c(rbind(ghibli_palette(7, name = "PonyoMedium", type = "continuous"), rep('White', 7) ))) +
  geom_point(size = 2, aes(color = Scenarios_land_use)) +
  xlab('Mean MF across stakeholder groups') +
  ylab('Endangered services score') +
  xlim(c(-0.25, 0.1)) +
  #ylim(c(-10, 10)) +
  
  scale_shape_manual(values = c(21, 24, 22)) +
  guides(fill = guide_legend(override.aes = list(shape = 21)))
ggsave(plot = score_plot_per_rep_diff, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/score_plot_per_rep_diff_', mf_method,'_', env_corr, '.pdf', sep = ''), width= 8, height =5)




test_radar = MF_harmony2[Demand_scenario == 'current_demand' & weighted.by.power == FALSE, ]
test_radar_melt = melt(test_radar[, .SD, .SDcols = c("Scenarios_land_use", "harmony",     "meanMF", "Region")])
test_radar_cast = dcast(test_radar_melt, Region + variable ~ Scenarios_land_use, value.var = 'value')
#test_radar2 =test_radar[, list(MF = (MF-min(MF)/(max(MF)-min(MF))),
#                  harmony = (harmony-min(harmony)/(max(harmony)-min(harmony))))]
#test_radar2 = test_radar[, list(MF = (MF- min(MF))/(max(MF) - min(MF)),
#                  harmony = (harmony- min(harmony))/(max(harmony) - min(harmony)))]
#test_radar2$Scenarios_land_use = test_radar$Scenarios_land_use
#test_radar_cast = dcast(melt(test_radar2), variable ~ Scenarios_land_use, value.var = 'value')
require(ggiraphExtra)
require(ggplot2)
#ggRadar(test_radar_cast[, .SD, .SDcols = c('variable', 'Baseline', 'Equal_areas',
#                                                      'More_forests', 'More_grasslands', 'Only_grasslands',
#                                                       'More_crops', 'Low_grasslands', 'High_grasslands',
#                                                       'Mixed_forests', 'Monoculture_forests', 'Back_to_nature', 
#                                                       'Productive_landscape')], mapping = aes(facet=Region))

p <- ggRadar(data = test_radar_cast, mapping = aes(colour = variable, facet=Region), 
             rescale = FALSE, interactive = FALSE, use.label = TRUE, size = 2,
             legend.position = "right")
ggRadar(data=iris,aes(group=Species))
ggRadar(data=acs,aes(colour=Dx,facet=Dx))
ggRadar(iris[c(50,51,130),],aes(color = Species), rescale = F)
ggRadar(data=data.frame(test_radar_cast[variable == 'harmony']),aes(facet=Region),   rescale = F)
ggRadar(data=data.frame(test_radar_cast[variable != 'harmony']),aes(facet=Region),   rescale = F)

#  mean MF by staheholder group
overall_community = ggplot(MF_harmony_per_Rep2_agg, aes(meanMF, x = Scenarios_land_use, fill = Scenarios_land_use, group = Region, color = Scenarios_land_use)) +
  geom_col(position = position_dodge(width = 1), color = NA) + theme_bw() +
  geom_errorbar(aes(ymin = meanMF-sd_meanMF, ymax = meanMF+sd_meanMF), width = 0, alpha = 0.5, position = position_dodge(width = 1)) + 
  scale_fill_manual(values= c(ghibli_palette(7, name = "PonyoMedium", type = "continuous"), ghibli_palette(7, name = "LaputaMedium", type = "continuous"))) +
  scale_color_manual(values= c(ghibli_palette(7, name = "PonyoDark", type = "continuous"), ghibli_palette(7, name = "LaputaDark", type = "continuous"))) +
  xlab('Scenarios')  + facet_grid(Demand_scenario~weighted.by.power) 
ggsave(plot = overall_community, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/overall_community_', mf_method, '_', env_corr,'.pdf', sep = ''), width= 7, height = 5.5)




# Does it correlate with % of services actually available?
test = merge(Availability2_per_group, MF_melt_w_power_aggregate[Scenarios_land_use == 'Baseline' & Demand_scenario =='current_demand',])

plot_MF_availability = ggplot(test, aes(prop01, mean_value, linetype = Region, shape = Region, color = Group)) + geom_point() + theme_bw() +
  geom_smooth(method = 'lm', color = 'black') + ylab('Proportion of services available accourding to group') + ylab('Mean MF for group')
ggsave(plot = plot_MF_availability, filename = paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/plot_MF_availability_', mf_method, '_', env_corr,'.pdf', sep = ''), width= 7, height = 5.5)

}}



