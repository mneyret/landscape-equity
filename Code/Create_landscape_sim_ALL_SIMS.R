#### Landscape-scale results:  Big loop ####
library(tictoc)
library(ade4)
library(geometry)
library(data.table)
## to create landscapes, quantify ecosystem multifunctionality, average and variation in LUI and quantify relationships
# Create matrix for (almost) all scenarios 
no_plots = 15
all_scenarios0 = data.table(expand.grid('Grassland_medium' = c(0, seq(1, 15, 2)), #= 0:15,
                                         'Grassland_high' = c(0, seq(1, 15, 2)), #= 0:15,
                                         'Grassland_low' = c(0, seq(1, 15, 2)), #= 0:15,
                                         'Crop' = c(0, seq(1, 15, 2)), #= 0:15,
                                         "Forest_Deciduous" = c(0, seq(1, 15, 2)), #= 0:15,
                                         "Forest_Mixed"= c(0, seq(1, 15, 2)), #= 0:15,
                                         "Forest_Coniferous"= c(0, seq(1, 15, 2))))
all_scenarios0 = all_scenarios0[rowSums(all_scenarios0) == 15,]
all_scenarios0[, c('Forest_even_aged', 'Forest_uneven_aged') := 0]

all_scenarios1 = data.table(expand.grid('Grassland_medium' = c(0, seq(1, 15, 2)), #= 0:15,
                              'Grassland_high' = c(0, seq(1, 15, 2)), #= 0:15,
                              'Grassland_low' = c(0, seq(1, 15, 2)), #= 0:15,
                              'Crop' = c(0, seq(1, 15, 2)), #= 0:15,
                              'Forest_even_aged' = c(0, seq(1, 15, 2)), #= 0:15,
                              'Forest_uneven_aged' = c(0, seq(1, 15, 2))))
all_scenarios1 = all_scenarios1[rowSums(all_scenarios1) == 15,]
all_scenarios1[, c('Forest_Deciduous', 'Forest_Mixed', 'Forest_Coniferous') := 0]
all_scenarios_grid = rbind(all_scenarios0, all_scenarios1)
all_scenarios_grid = unique(all_scenarios_grid)
 

# Add 'real-life" scenarios
Scenarios_land_use <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Scenarios/Scenarios_management2.xlsx", sheet = 3))

Scenarios_use = Scenarios_land_use[, c(3, 4, 10, 13:21) ]
Scenarios_use[, colnames(Scenarios_use)[4:12] := lapply(.SD, function(x){
  x[is.na(x)] = 0
  return(round(x*15/100))}), .SDcols = c(4:12)]

Scenarios_use[ , All_grasslands := rowSums(.SD), .SDcols = c("Grassland_low",  "Grassland_medium",  "Grassland_high")]
Scenarios_use[Forest_classification == 'Type' , All_forests := rowSums(.SD, na.rm = T), .SDcols = c( "Forest_Mixed", "Forest_Coniferous", "Forest_Deciduous")]
Scenarios_use[Forest_classification != 'Type' , All_forests := rowSums(.SD, na.rm = T), .SDcols = c( "Forest_even_aged", "Forest_uneven_aged")]
Scenarios_use[, problem_overall := 15 - rowSums(.SD, na.rm = T), .SDcols = c( "All_grasslands", "All_forests", 'Crop')]


# First we can add all the possibilities to fill the main LU : crops, forests, grasslands
Scenarios_use_correctLU = rbind(
  Scenarios_use[problem_overall == 0,],
  Scenarios_use[abs(problem_overall) == 2, ][, c('All_grasslands', 'All_forests') := list(All_grasslands + problem_overall/2, All_forests+problem_overall/2)],
  Scenarios_use[abs(problem_overall) == 2, ][, c('All_grasslands', 'Crop') := list(All_grasslands + problem_overall/2, Crop+problem_overall/2)],
  Scenarios_use[abs(problem_overall) == 2, ][, c('Crop', 'All_forests') := list(Crop + problem_overall/2, All_forests+problem_overall/2)],
  Scenarios_use[abs(problem_overall) == 1, ][, All_forests := All_forests + problem_overall],
  Scenarios_use[abs(problem_overall) == 1, ][, Crop := Crop + problem_overall],
  Scenarios_use[abs(problem_overall) == 1, ][, All_grasslands := All_grasslands + problem_overall])
  

# Then we have to check that within each LU we have the right number of plots
Scenarios_use_correctLU[, problem_grasslands := All_grasslands - rowSums(.SD, na.rm = T) , .SDcols = c('Grassland_low', 'Grassland_medium','Grassland_high')]

Scenarios_use_correctLU = rbind(
  Scenarios_use_correctLU[problem_grasslands == 0, ],
  Scenarios_use_correctLU[problem_grasslands != 0, ][, Grassland_low := Grassland_low + problem_grasslands],
  Scenarios_use_correctLU[problem_grasslands != 0, ][, Grassland_medium := Grassland_medium + problem_grasslands],
  Scenarios_use_correctLU[problem_grasslands != 0, ][, Grassland_high := Grassland_high + problem_grasslands])

Scenarios_use_correctLU[, problem_forests := All_forests - rowSums(.SD, na.rm = T), .SDcols = c('Forest_Mixed','Forest_Coniferous', 'Forest_Deciduous', 'Forest_even_aged', 'Forest_uneven_aged')]

Scenarios_use_combined_all = rbind(
  Scenarios_use_correctLU[problem_forests == 0,],
  Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == 'Type', ][, Forest_Mixed := Forest_Mixed + problem_forests],
  Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == 'Type', ][, Forest_Coniferous := Forest_Coniferous + problem_forests],
  Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == 'Type', ][, Forest_Deciduous := Forest_Deciduous + problem_forests],
  Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == 'Age', ][, Forest_even_aged := Forest_even_aged + problem_forests],
  Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == 'Age', ][, Forest_uneven_aged := Forest_uneven_aged + problem_forests])


Scenarios_use_combined_all = Scenarios_use_combined_all[Forest_Mixed >= 0 & 
                           Forest_Coniferous>= 0 &
                           Forest_Deciduous>= 0 &
                           Forest_even_aged>= 0 &
                           Forest_uneven_aged>= 0 &
                           Grassland_low>= 0 &
                           Grassland_medium>= 0 &
                           Grassland_high>= 0 &
                           Crop>= 0 ,]




all_scenarios = rbind(Scenarios_use_combined_all[, .SD, .SDcols = colnames(all_scenarios_grid)], all_scenarios_grid)

all_scenarios = unique(all_scenarios)

all_scenarios$scenario_no = 1:nrow(all_scenarios)
fwrite(all_scenarios, '/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/All_landscape_compositions.csv')
#fwrite(Scenarios_use_combined_all, '/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Predefined_scenarios.csv')


# 
# # env_corr = 'env_corr'
# # env_corr = ''
# All_env_data = fread('data/environments_new.csv')[order(Plot),]
# PCA_data = fread('data/environments_pca.csv')[order(Plot),]
# 
# All_ES_data_classif = fread('data/services_new.csv')[order(Plot),]
# Abundance_plants = fread('data/Plants_abundance.csv')[order(Plot),]
# Abundance_plants[, colnames(Abundance_plants)[-1] := lapply(.SD, function(x){x[x>0] <- 1; return(x)}),
#                  .SDcols = colnames(Abundance_plants)[-1]]
# Abundance_fungi = fread('data/Fungi_abundance.csv')[order(Plot),]
# Abundance_fungi[, colnames(Abundance_fungi)[-1] := lapply(.SD, function(x){x[x>0] <- 1; return(x)}),
#                         .SDcols = colnames(Abundance_fungi)[-1]]
# Abundance_birds = fread('data/Birds_abundance.csv')[order(Plot),]
# Abundance_birds[, colnames(Abundance_birds)[-1] := lapply(.SD, function(x){x[x>0] <- 1; return(x)}),
#                  .SDcols = colnames(Abundance_birds)[-1]]
# Abundance_charism_birds = fread('data/Birds_charism_abundance.csv')[order(Plot),]
# Abundance_charism_birds[, colnames(Abundance_charism_birds)[-1] := lapply(.SD, function(x){x[x>0] <- 1; return(x)}),
#                 .SDcols = colnames(Abundance_charism_birds)[-1]]
# all_plants = colnames(Abundance_plants)[-1]
# all_fungi = colnames(Abundance_fungi)[-1]
# all_birds = colnames(Abundance_birds)[-1]
# all_charism_birds = colnames(Abundance_charism_birds)[-1]
# 
# All_Abundances = do.call("cbind", list(Abundance_plants,Abundance_birds[,-1],Abundance_fungi[,-1]))
# 
# # Use this to check  pre-picked landscapes
# already_picked = T
# Picked_landscapes = fread('Prelim_results/picked.csv')
# colnames(Picked_landscapes)[1:3]= c('Scenario', 'Replica', 'Region')
# 
# # Use this to create landscapes from scratch
# already_picked = F
# All_scenarios = fread('data/scenarios.csv')
# no_simulations <- 1000
# 
# 
# tic("total")
# 
# #PCA_data = rbind(dudi.pca(All_env_data[Region == 'A', c('Core_depth', 'pH','elevation', 'TWI','prop_clay','Mean_Temp','Mean_precip')],  scannf = FALSE, nf = 3)$li,
# #                 dudi.pca(All_env_data[Region == 'H', c('Core_depth', 'pH','elevation', 'TWI','prop_clay','Mean_Temp','Mean_precip')],  scannf = FALSE, nf = 3)$li,
# #                 dudi.pca(All_env_data[Region == 'S', c('Core_depth', 'pH','elevation', 'TWI','prop_clay','Mean_Temp','Mean_precip')],  scannf = FALSE, nf = 3)$li)
# #dat = PCA_env$li
# #dat$Plot = rownames(dat)
# #write.csv(dat, '/Users/Margot/Desktop/PCA_data.csv', row.names = F)
# 
# 
# for (region in 'A'){
#   #unique(All_ES_data_classif$Region)) {
#  # All_ES_data_region = All_ES_data_classif[Region == region,]
# #  All_env_data_region = All_env_data[Region == region,]
# #  All_Abundances_region = All_Abundances[All_ES_data_classif$Region == region,]
#   #All_env_data = data.frame(All_env_data)
#   #rownames(All_env_data) = All_env_data$Plot
#   
#   raw_sim_data <- data.table(
#     expand_grid(
#       Scenarios_land_use = All_scenarios[, scenario_no],
#       Rep = 1:no_simulations,
#       Region = region))
#   raw_plot_numbers <- data.table(
#     expand_grid(
#       Scenarios_land_use = All_scenarios[, scenario_no],
#       Rep = 1:no_simulations,
#       Region = region))
#   
#   line_number = 0
#   for (sc in unique(raw_sim_data$Scenarios_land_use)) {
#     line_number = line_number + 1
#     print(sc)
#     
#    
#     # Starts generation of each landscape
#     list_landscapes = list()
#     for (i in 1:5){#no_simulations) {
#       
#       if (already_picked == F){
#       error2 = NA
#       k = 0
#       NEW = FALSE
#       while (NEW == FALSE & k < 50) {
#         new_combination <-
#           create_new_combinations_all(All_scenarios, sc, All_ES_data_region)
#         plots_selected_collapse = paste(new_combination$plots_to_keep, collapse = '_')
#         plots_selected_all = new_combination$plots_to_keep
#         if (plots_selected_collapse %in% list_landscapes) {
#           k = k + 1
#           if (k == 50)
#             error2 = 'Warning: could not find a new combination in 50 iterations, reusing already existing one!'
#         }
#         else{
#           list_landscapes = append(list_landscapes, plots_selected_collapse)
#           NEW = TRUE
#         }
#       }}
#     
#     if (already_picked == T){
#       plots_selected_names = as.character(Picked_landscapes[Region == as.character(region) & Scenario == sc & Replica == i-1, 5:ncol(Picked_landscapes)])
#       plots_selected_all = which(All_ES_data$Plot %in% plots_selected_names)
#       print(plots_selected_all)
#     }
#       
#       plants_pres = colSums(Abundance_plants[plots_selected_all, ..all_plants])
#       birds_pres = colSums(Abundance_birds[plots_selected_all, ..all_birds])
#       fungi_pres = colSums(Abundance_fungi[plots_selected_all, ..all_fungi])
#       charism_birds_pres = colSums(Abundance_charism_birds[plots_selected_all, ..all_charism_birds])
#       
#       ric_plants <- length(plants_pres[plants_pres>0])
#       ric_birds <-length(birds_pres[birds_pres>0])
#       ric_fungi <-length(fungi_pres[fungi_pres>0])
#       ric_charism_birds <- length(charism_birds_pres[charism_birds_pres>0])
#       
#       sum_all_es <- All_ES_data_classif[plots_selected_all, colSums(.SD, na.rm = T), .SDcols = indicators_plot]
#       
#       raw_sim_data[Scenarios_land_use == sc & Rep == i & Region == region, c('Ric_plants', 'Ric_birds', 'Ric_fungi', 'Ric_charism',
#                                   names(sum_all_es), 'error') := as.list(
#         c(
#           ric_plants,
#           ric_birds,
#           ric_fungi,
#           ric_charism_birds,
#           sum_all_es,
#           unique(new_combination$error, error2)
#         )
#       )]
#       
#       heterogeneity = try(convhulln(PCA_data[plots_selected_all,-4], options = c('FS'))$vol, silent = T)
#       raw_sim_data[Scenarios_land_use == sc & Rep == i & Region == region, Heterogeneity := heterogeneity]
#       raw_sim_data[Scenarios_land_use == sc & Rep == i & Region == region, c(
#         'Core_depth',
#         'pH',
#         'elevation',
#         'TWI',
#         'prop_clay',
#         'Mean_Temp',
#         'Mean_precip'
#       ) :=
#         All_env_data[plots_selected_all, lapply(.SD, mean), .SDcols = c(
#           'Core_depth',
#           'pH',
#           'elevation',
#           'TWI',
#           'prop_clay',
#           'Mean_Temp',
#           'Mean_precip'
#         )]]
#       raw_plot_numbers[Scenarios_land_use == sc & Rep == i & Region == region, Plots := plots_selected_collapse]
#       
#     }
#     
#     fwrite(
#       raw_sim_data,
#       paste(
#         '/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Raw_data_check_',
#         region,
#         '.csv',
#         sep = ''
#       )
#     )
#     fwrite(
#       raw_plot_numbers,
#       paste(
#         '/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Raw_plotnumbers_',
#         region,
#         '.csv',
#         sep = ''
#       )
#     )
#   }
# }
# 
# 
# toc(quiet = F)
#/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Complete_simulations
#toc()
#