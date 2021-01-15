#### Landscape-scale results:  Big loop ####
library(tictoc)
## to create landscapes, quantify ecosystem multifunctionality, average and variation in LUI and quantify relationships
# Create result matrix
env_corr = 'env_corr'
#env_corr = ''
All_ES_data_classif = fread(paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/All_ES_data_classif_', env_corr, '.csv', sep = ''))

tic("total")
no_simulations <- 1000
no_plots = 15
raw_sim_data <- data.table(
  expand_grid(
    Scenarios_land_use = scenarios_land_use,
    Region = unique(All_ES_data_classif$Region),
    Rep = 1:no_simulations
  )
)
raw_sim_data[, c(indicators_plot) := list(NA)]
raw_sim_data[, c(indicators_plot) := lapply(.SD, as.numeric), .SDcols = indicators_plot]

for (sc in scenarios_land_use) {
  print(sc)
  for (region in unique(All_ES_data_classif$Region)) {
    print(region)
    # Starts generation of each landscape
    list_landscapes = list()
    for (i in 1:no_simulations) {
    #  print(i)
      k = 0
      NEW = FALSE
      while (NEW == FALSE & k < 50){
      plots_selected_all <- create_new_combinations(Scenarios_land_use, sc, no_plots, All_ES_data_classif[Region == region,], region)
      plots_selected_collapse = paste(plots_selected_all, collapse = '_')
      if (plots_selected_collapse %in% list_landscapes){
        k = k+1
        print(paste('Warning: could not find a new combination in 50 iterations, reusing already existing one!'))
      }
      else{
        list_landscapes = append(list_landscapes, plots_selected_collapse)
        NEW = TRUE
      }
      }
      
     # print(plots_selected_all)
      ### Landscape-level services, MEAN

      ric_plants <- specnumber(All_abundances_cast[Plot %in% plots_selected_all, colSums(.SD, na.rm = T), .SDcols = all_plants])
      ric_birds <- specnumber(All_abundances_cast[Plot %in% plots_selected_all, colSums(.SD, na.rm = T), .SDcols = all_birds])

      sum_all_es <- All_ES_data_classif[Plot %in% plots_selected_all, colSums(.SD, na.rm = T), .SDcols = indicators_plot]
      
      # Correction if not right number of plots
      ric_plants = ric_plants * no_plots/length(plots_selected_all)
      ric_birds = ric_birds * no_plots/length(plots_selected_all)
      sum_all_es = sum_all_es * no_plots/length(plots_selected_all)
      
      landscape_div <- diversity(table(All_ES_data_classif[Plot %in% plots_selected_all, LU]))
      
      tab_classif = All_ES_data_classif[Plot %in% plots_selected_all, table(Classif)]
      prop_grasslands_low = ifelse(!('Grassland_low' %in% names(tab_classif)), 0, 2*sum(tab_classif['Grassland_low'])/sum(tab_classif, na.rm = T))
      #prop_forests_mixed_broadleaved = 2*sum(tab_classif[c('Forest_Deciduous', 'Forest_Mixed')], na.rm = T)/sum(tab_classif, na.rm = T)
      if(prop_grasslands_low>1){prop_grasslands_low = 1}
      #if(prop_forests_mixed_broadleaved>1){prop_forests_mixed_broadleaved = 1}
      

     # reg_id = prop_grasslands_low + prop_forests_mixed_broadleaved
      leisure_landscape = prop_grasslands_low
      
      # Fill the big dataset
      raw_sim_data[Region == region & Scenarios_land_use == sc & Rep == i, (all_indicators) := as.list(c(ric_plants, ric_birds, landscape_div, leisure_landscape, sum_all_es))]
    }
  }
}
toc()
fwrite(raw_sim_data, paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/raw_sim_data_' , env_corr, '.csv', sep = ''))
