#### Landscape-scale results:  Big loop ####
library(tictoc)
library(ade4)
library(geometry)
library(data.table)
library(readxl)
## to create landscapes, quantify ecosystem multifunctionality, average and variation in LUI and quantify relationships
# Create matrix for (almost) all scenarios

for (constrain_crop in c(FALSE, TRUE)) {
  for (by_region in c(FALSE, TRUE)) {
    if (by_region == TRUE) no_plots <- 15

    if (by_region == FALSE) no_plots <- 20

    if (constrain_crop == FALSE) {
      plot_seq <- unique(c(0, seq(1, no_plots, 2), no_plots))
      all_scenarios0 <- data.table(expand.grid(
        "Grassland_medium" = plot_seq,
        "Grassland_high" = plot_seq,
        "Grassland_low" = plot_seq,
        "Crop" = plot_seq,
        "Forest_Deciduous" = plot_seq,
        "Forest_Mixed" = plot_seq,
        "Forest_Coniferous" = plot_seq
      ))
      all_scenarios0 <- all_scenarios0[rowSums(all_scenarios0) == no_plots, ]
      all_scenarios0[, c("Forest_even_aged", "Forest_uneven_aged") := 0]

      all_scenarios1 <- data.table(expand.grid(
        "Grassland_medium" = plot_seq,
        "Grassland_high" = plot_seq,
        "Grassland_low" = plot_seq,
        "Crop" = plot_seq,
        "Forest_even_aged" = plot_seq,
        "Forest_uneven_aged" = plot_seq
      ))
      all_scenarios1 <- all_scenarios1[rowSums(all_scenarios1) == no_plots, ]
      all_scenarios1[, c("Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous") := 0]
      all_scenarios_grid <- rbind(all_scenarios0, all_scenarios1)
    }

    if (constrain_crop == TRUE) {
      if (by_region == TRUE) {
        fixed_crop <- round(no_plots / 100 * c(21, 60, 35))
        plot_seq <- c(0, 1, 2, 3, 5, 7, 9, 11, 13, 15)
}
      if (by_region == FALSE) {
        fixed_crop <- round(no_plots * 39 / 100)
        plot_seq <- c(0, seq(1, (no_plots - min(fixed_crop)), 1))

      }

      all_scenarios0 <- data.table(expand.grid(
        "Grassland_medium" = plot_seq,
        "Grassland_high" = plot_seq,
        "Grassland_low" = plot_seq,
        "Crop" = c(0, fixed_crop, no_plots),
        "Forest_Deciduous" = plot_seq,
        "Forest_Mixed" = plot_seq,
        "Forest_Coniferous" = plot_seq
      ))
      all_scenarios0 <- all_scenarios0[rowSums(all_scenarios0) == no_plots, ]
      all_scenarios0[, c("Forest_even_aged", "Forest_uneven_aged") := 0]

      all_scenarios1 <- data.table(expand.grid(
        "Grassland_medium" = plot_seq,
        "Grassland_high" = plot_seq,
        "Grassland_low" = plot_seq,
        "Crop" = c(0, fixed_crop, no_plots),
        "Forest_even_aged" = plot_seq,
        "Forest_uneven_aged" = plot_seq
      ))
      all_scenarios1 <- all_scenarios1[rowSums(all_scenarios1) == no_plots, ]
      all_scenarios1[, c("Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous") := 0]
      all_scenarios_grid <- rbind(all_scenarios0, all_scenarios1)
    }

    all_scenarios_grid <- unique(all_scenarios_grid)


    # Add 'real-life" scenarios
    Scenarios_land_use <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Scenarios/Scenarios_management_new_baseline.xlsx", sheet = 3))
    Scenarios_land_use[is.na(Region), Region := "All"]

    # Add 'best' scenarios for each configuration
    # temp = list.files(path="/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/Best_landscapes/", pattern = "*.csv", full.names = TRUE)
    # all_best = rbindlist(lapply(temp, function(x){fread(x)}))
    # all_best = all_best[Metric == 'No',]
    # all_best[, Scenario_description := paste(Scenario_description, region, sep = '__')]
    # all_best[, Region := region]
    # all_best[, Scenario_description := gsub('__[A-z]*$', '', Scenario_description)]


    # Check: compare variations
    # temp_old = list.files(path="/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/Best_landscapes/Previous/", pattern = "*.csv", full.names = TRUE)
    # all_best_old = rbindlist(lapply(temp_old, function(x){fread(x)}))
    # test = merge(melt(all_best, id.vars = c('V1', 'region', 'N','Forest_classification', 'Scenario_description'), value.name = 'now'),
    #             melt(all_best_old, id.vars = c('V1', 'region',  'N','Forest_classification', 'Scenario_description'), value.name = 'old'),
    #             by = c('Scenario_description', 'variable', 'region', 'Forest_classification'))
    # test[, best_no_loser := grepl('best_no_loser', Scenario_description)]
    #
    # ggplot(test, aes(now, old, color = variable)) + geom_point() + facet_wrap(region + best_no_loser~Forest_classification)

    Scenarios_use <- Scenarios_land_use[, c(3, 4, 13, 16:24)]

    Scenarios_use[, colnames(Scenarios_use)[4:12] := lapply(.SD, function(x) {
      x[is.na(x)] <- 0
      return(round(x * no_plots / 100))
    }), .SDcols = c(4:12)]
    Scenarios_use[, c("N") := NA]

    # all_best[, colnames(all_best)[3:11] := lapply(.SD, function(x){
    #  x[is.na(x)] = 0
    #  return(round(x))}), .SDcols = c(3:11)]

    # Scenarios_use = rbind(Scenarios_use, all_best[, .SD, .SDcols = colnames(Scenarios_use)])

    Scenarios_use[, All_grasslands := rowSums(.SD), .SDcols = c("Grassland_low", "Grassland_medium", "Grassland_high")]
    Scenarios_use[, All_forests := rowSums(.SD, na.rm = T), .SDcols = c("Forest_Mixed", "Forest_Coniferous", "Forest_Deciduous", "Forest_even_aged", "Forest_uneven_aged")]
    Scenarios_use[, problem_overall := no_plots - rowSums(.SD, na.rm = T), .SDcols = c("All_grasslands", "All_forests", "Crop")]


    # First we can add all the possibilities to fill the main LU : crops, forests, grasslands
    Scenarios_use_correctLU <- rbind(
      Scenarios_use[problem_overall == 0, ],
      Scenarios_use[abs(problem_overall) == 2, ][, c("All_grasslands", "All_forests") := list(All_grasslands + problem_overall / 2, All_forests + problem_overall / 2)],
      Scenarios_use[abs(problem_overall) == 2, ][, c("All_grasslands", "Crop") := list(All_grasslands + problem_overall / 2, Crop + problem_overall / 2)],
      Scenarios_use[abs(problem_overall) == 2, ][, c("Crop", "All_forests") := list(Crop + problem_overall / 2, All_forests + problem_overall / 2)],
      Scenarios_use[abs(problem_overall) == 1, ][, All_forests := All_forests + problem_overall],
      Scenarios_use[abs(problem_overall) == 1, ][, Crop := Crop + problem_overall],
      Scenarios_use[abs(problem_overall) == 1, ][, All_grasslands := All_grasslands + problem_overall]
    )


    # Then we have to check that within each LU we have the right number of plots
    Scenarios_use_correctLU[, problem_grasslands := All_grasslands - rowSums(.SD, na.rm = T), .SDcols = c("Grassland_low", "Grassland_medium", "Grassland_high")]

    Scenarios_use_correctLU <- rbind(
      Scenarios_use_correctLU[problem_grasslands == 0, ],
      Scenarios_use_correctLU[problem_grasslands != 0, ][, Grassland_low := Grassland_low + problem_grasslands],
      Scenarios_use_correctLU[problem_grasslands != 0, ][, Grassland_medium := Grassland_medium + problem_grasslands],
      Scenarios_use_correctLU[problem_grasslands != 0, ][, Grassland_high := Grassland_high + problem_grasslands]
    )

    Scenarios_use_correctLU[, problem_forests := All_forests - rowSums(.SD, na.rm = T), .SDcols = c("Forest_Mixed", "Forest_Coniferous", "Forest_Deciduous", "Forest_even_aged", "Forest_uneven_aged")]

    Scenarios_use_combined_all <- rbind(
      Scenarios_use_correctLU[problem_forests == 0, ],
      Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == "Type", ][, Forest_Mixed := Forest_Mixed + problem_forests],
      Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == "Type", ][, Forest_Coniferous := Forest_Coniferous + problem_forests],
      Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == "Type", ][, Forest_Deciduous := Forest_Deciduous + problem_forests],
      Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == "Age", ][, Forest_even_aged := Forest_even_aged + problem_forests],
      Scenarios_use_correctLU[problem_forests != 0 & Forest_classification == "Age", ][, Forest_uneven_aged := Forest_uneven_aged + problem_forests]
    )


    Scenarios_use_combined_all <- Scenarios_use_combined_all[Forest_Mixed >= 0 &
      Forest_Coniferous >= 0 &
      Forest_Deciduous >= 0 &
      Forest_even_aged >= 0 &
      Forest_uneven_aged >= 0 &
      Grassland_low >= 0 &
      Grassland_medium >= 0 &
      Grassland_high >= 0 &
      Crop >= 0, ]

    all_scenarios <- rbind(
      Scenarios_use_combined_all[, .SD, .SDcols = c(colnames(all_scenarios_grid), "Region", "Scenario_description", "Forest_classification", "N")],
      all_scenarios_grid[, c("Region", "Scenario_description", "Forest_classification", "N") := ""]
    )

    all_scenarios <- unique(all_scenarios)

    all_scenarios <- all_scenarios[!((Forest_Coniferous + Forest_Deciduous + Forest_Mixed > 0) & (Forest_even_aged + Forest_uneven_aged > 0)), ]

    all_scenarios$scenario_no <- 1:nrow(all_scenarios)
    fwrite(all_scenarios, paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_simulation/data/scenarios_", "by_region", by_region, "_constrained", constrain_crop, ".csv", sep = ""))
  }
}



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
# /Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Complete_simulations
# toc()
#
