# -------------------------------------------------------------------------------------------
# This is part of the work used for the publication Neyret et al. Landscape management for multifunctionality and Equity. In revision for Nature Sustainability.
# by Margot Neyret

# In this script, we create the landscapes that will be used for the simulations

# Input: Pre-defined scenario table
# Output: Full scenarios table including most possible landscape compositions
# -------------------------------------------------------------------------------------------

library(tictoc)
library(ade4)
library(geometry)
library(data.table)
library(readxl)

## Parameters
# There is a number of parameters we use for all the sensitivity analyses. 
# Default parameters are used for the main results shown in the manuscript
#         - by_region: should the optimisation should be done within (TRUE) or across regions (FALSE, default)
#         - constrain_crop: is the proportion of cropland fixed to its baseline value? (efault: TRUE)
setwd('~/Landscape_composition')
for (constrain_crop in c(FALSE, TRUE)) {
  for (by_region in c(FALSE, TRUE)) {
    # Set number of plots per landscape
    if (by_region == TRUE) no_plots <- 15
    if (by_region == FALSE) no_plots <- 20

    # Create scenario grids
    
    if (constrain_crop == FALSE) {
      # We skip a few possible number of plots because it's too much to simulate otherwise
      plot_seq <- unique(c(0, seq(1, no_plots, 2), no_plots)) 
      # We create all possible combinations with forest type
      all_scenarios0 <- data.table(expand.grid(
        "Grassland_medium" = plot_seq,
        "Grassland_high" = plot_seq,
        "Grassland_low" = plot_seq,
        "Crop" = plot_seq,
        "Forest_Deciduous" = plot_seq,
        "Forest_Mixed" = plot_seq,
        "Forest_Coniferous" = plot_seq
      ))
      # We keep only those that sum up to the right number of plots
      all_scenarios0 <- all_scenarios0[rowSums(all_scenarios0) == no_plots, ]
      all_scenarios0[, c("Forest_even_aged", "Forest_uneven_aged") := 0]

      # Same for scenarios with forest age
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

    # Same for scenarios with constrained crops
    if (constrain_crop == TRUE) {
      if (by_region == TRUE) {
        fixed_crop <- round(no_plots / 100 * c(21, 60, 35))
        plot_seq <- c(0, 1, 2, 3, 5, 7, 9, 11, 13, 14, 15)
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


    # Add pre-defined scenarios
    Scenarios_land_use <- fread("Raw_data/Other/Scenarios_management_new_baseline.csv")
    Scenarios_use <- Scenarios_land_use[, c(3, 4, 13, 16:24)] # Use only a selection of scenarios

    # We calculate the number of plots falling into each category by rounding the proportions
    Scenarios_use[, colnames(Scenarios_use)[4:12] := lapply(.SD, function(x) {
      x[is.na(x)] <- 0
      return(round(x * no_plots / 100))
    }), .SDcols = c(4:12)]
    Scenarios_use[, c("N") := NA]
    
    Scenarios_use[, All_grasslands := rowSums(.SD), .SDcols = c("Grassland_low", "Grassland_medium", "Grassland_high")]
    Scenarios_use[, All_forests := rowSums(.SD, na.rm = T), .SDcols = c("Forest_Mixed", "Forest_Coniferous", "Forest_Deciduous", "Forest_even_aged", "Forest_uneven_aged")]
    
    # However, this might lead to variable number of plots which we don't want. These issues are cleared below
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
    fwrite(all_scenarios, paste("Temporary_data/scenarios_", "by_region", by_region, "_constrained", constrain_crop, ".csv", sep = ""))
  }
}