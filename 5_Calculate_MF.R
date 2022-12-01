# -------------------------------------------------------------------------------------------
# This is part of the work used for the publication Neyret et al. Landscape management for multifunctionality and Equity. In revision for Nature Sustainability.
# by Margot Neyret

# In this script, we calculate multifunctionality from all the generated scenarios

# Input: Availability and power data, landscape simulation outputs
# Output: Overall service values and multifunctionality for all stakeholders
# -------------------------------------------------------------------------------------------

library(data.table)
library(vegan)
library(ggplot2)
library(ggnewscale)
#library(ghibli)
library(Hmisc)
#library(mgcv)
library(readxl)
library(boot)
library(R.utils)
library(Rmisc)
library(cowplot)
library(DescTools)
library(tidyr)

setwd('~/Landscape_composition')

scale01 <- function(x) {
  x <- as.numeric(x)
  max <- quantile(x, 0.975, na.rm = T)
  min <- min(x, na.rm = T)
  y <- (x - min) / (max - min)
  y[y < 0] <- 0
  y[y > 1] <- 1
  return(y)
}

### Parameters loop
for (by_region in c(FALSE#, TRUE
)){ 
  print(paste("by_region: ", by_region))
  
  # Load model for area correction
  source(
    "Area_correction.R"
  )
  
  if (by_region == FALSE){ crop_choice = c(TRUE#, FALSE
                                           )}
  if (by_region == TRUE){ crop_choice = c(TRUE)}

  for (crop_constrained in crop_choice) {

  #### Data ####
  # Priority and service availability
  Availability <-
    fread(
      "Temporary_data/Availability.csv"
    )
  Priority <-
    fread(
      "Temporary_data/Priority_Power.csv"
    )


    # Scenario composition
    landscape_scenarios <-
      fread(
        paste(
          "Temporary_data/scenarios_",
          "by_region",
          by_region,
          "_constrained",
          crop_constrained,
          ".csv",
          sep = ""
        )
      )
    
   no_plots = landscape_scenarios[,unique(rowSums(.SD)), .SDcols = c(
      "Grassland_medium",
      "Grassland_high",
      "Grassland_low",
      "Crop",
      "Forest_Deciduous",
      "Forest_Mixed",
      "Forest_Coniferous",
      "Forest_even_aged",
      "Forest_uneven_aged"
    )]
   
   if (length(no_plots) > 1){
     stop("Variable number of plots in landscape files")
   }
    
    landscape_scenarios[, Aesth_diversity_LU := diversity(.SD), .SDcols = c(
      "Grassland_medium",
      "Grassland_high",
      "Grassland_low",
      "Crop",
      "Forest_Deciduous",
      "Forest_Mixed",
      "Forest_Coniferous",
      "Forest_even_aged",
      "Forest_uneven_aged"
    )]
    landscape_scenarios[, Leisure_grasslands := ifelse(Grassland_low + Grassland_medium < (no_plots/2),
      (Grassland_low + Grassland_medium) / no_plots,
      0.5
    )]
    landscape_scenarios[, Leisure_forests := ifelse(rowSums(.SD) < (no_plots/2), rowSums(.SD) / no_plots, 0.5), .SDcols = c(
      "Forest_Deciduous",
      "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged","Forest_uneven_aged"
    )]
    landscape_scenarios[, scenario := scenario_no]
    landscape_scenarios[, Area := (
      Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged) * 100 +
      (Grassland_medium + Grassland_high + Grassland_low + Crop) * 16]
    landscape_scenarios[, proportion_grasslands := (Grassland_medium + Grassland_high + Grassland_low) / no_plots]
    landscape_scenarios[, proportion_forests := (Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged) / no_plots]
    
    landscape_scenarios[, concatenate := paste(.SD, collapse = "_"), by = 1:nrow(landscape_scenarios), .SDcols = c(
      "Crop",
      "Grassland_low",
      "Grassland_medium",
      "Grassland_high",
      "Forest_Deciduous",
      "Forest_Mixed",
      "Forest_Coniferous",
      "Forest_even_aged",
      "Forest_uneven_aged"
    )]
    landscape_scenarios[Scenario_description == "Current proportion of LU types", Scenario_description := "Baseline"]
    landscape_scenarios[, region := Region]
    landscape_scenarios[Forest_even_aged + Forest_uneven_aged > 0, Forest_age_scenario := "Age"]
    landscape_scenarios[Forest_Deciduous + Forest_Mixed + Forest_Coniferous > 0, Forest_age_scenario := "Type"]
    landscape_scenarios[Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged == 0, Forest_age_scenario := "Both"]

 
    # * --- Loop on environment correction ####
    if (by_region == FALSE & crop_constrained == TRUE) {
      env_choices <- c("env_corr"#, ""
                       )
    }   else {
      env_choices <- "env_corr"
    }
    for (env_corr in env_choices
         ) {
      print(paste("env_corr: ", env_corr))

      file <-
        paste(
          "Temporary_data/landscapes_",
          env_corr,
          "_region",
          by_region,
          ifelse(crop_constrained == TRUE, "_cropconstrained",''),
          ".csv.gz",
          sep = ""
        )
      picked_file <- paste(
        "Temporary_data/picked_",
        env_corr,
        "_region",
        by_region,
        ifelse(crop_constrained == TRUE, "_cropconstrained",''),
        ".csv.gz",
        sep = ""
      )

      if (file.exists(file)) {
        print("********** File exists **************")
        sim_data_complete <- fread(file)
        picked = fread(picked_file)
      } else {
        print(file)
        print("********** File does not exist, SKIPPED **************")
        next
      }

      # Identify faulty scenarios (i.e. asks for more plot than what exists)
       sim_data_complete = merge.data.table(sim_data_complete, picked[, c('scenario', 'replica', 'region', 'not_enough_plots')], 
                                 by = c('scenario', 'replica', 'region'))
       sim_data_complete = sim_data_complete[not_enough_plots == FALSE,]
       rm(picked)
       print('a')
       
      # Add data calculated from landscape_scenario proportions
      sim_data_complete <-
        merge.data.table(
          sim_data_complete,
          landscape_scenarios[, .SD,
            .SDcols = c(
              "Aesth_diversity_LU",
              "Leisure_grasslands",
              "Forest_age_scenario",
              "Leisure_forests",
              "Crop",
              "Area",
              "scenario",
              "concatenate",
              "proportion_grasslands",
              'proportion_forests'
            )
          ],
          by = c("scenario"),
          all.x = T
        )
      
      # Also adds corresponding description, if exists
      only_predef <- landscape_scenarios[Scenario_description != "", ]
      sim_data_complete <- merge.data.table(
        sim_data_complete,
        only_predef[, c("scenario", "region", "N", "Scenario_description")],
        by = c("scenario", "region"),
        all.x = T
      )
      sim_data_complete[, LUI_corr := LUI_2008_2015 / proportion_grasslands]
      sim_data_complete <- sim_data_complete[!is.na(heterogeneity), ]

       print('d')

      my_palette_services <- c(
        "lightsteelblue1",
        "lightsteelblue2",
        "lightsteelblue3",
        "lightsteelblue4",
        "burlywood1",
        "sandybrown",
        "lightsalmon1",
        "darksalmon",
        "lightsalmon3",
        "salmon4",
        "paleturquoise4"
      )
      
      ### Start working on simulation results
       
     ### Correcting diversities
      sim_data_complete[, c(
        "birds_diversity",
        "plants_diversity",
        "birds_charism_diversity",
        "fungi_diversity"
      ) :=
        lapply(.SD, as.numeric), .SDcols = c(
        "birds_diversity",
        "plants_diversity",
        "birds_charism_diversity",
        "fungi_diversity"
      )]

       print('f')
       
      # Area correction
      if (by_region == FALSE) {
        area_model <- gam(Ric_plants ~ Area, data = Area_correction_data)
      }
      if (by_region == TRUE) {
        area_model <-
          gam(Ric_plants ~ Area * region, data = Area_correction_data)
      }

       print('g')
      sim_data_complete[, "predicted_plant_richness" :=
        list(predict(area_model, .SD)),
      .SDcols = c("Area", "region")
      ]

      # print('h')
      sim_data_complete[, c("plants_diversity_corr") := plants_diversity - predicted_plant_richness]

       print('i')

      if (env_corr == "env_corr") {
        sim_data_complete[, c(
          "birds_diversity",
          "birds_charism_diversity",
          "fungi_diversity"
        ) :=
          lapply(.SD, function(x) {
            mod <-
              glm(
                x ~  Core_depth + Mean_Temp + Mean_precip + TWI + elevation + pH + prop_clay + heterogeneity,
                family = "poisson"
              )
            return(residuals(mod))
          }),
        .SDcols = c(
          "birds_diversity",
          "birds_charism_diversity",
          "fungi_diversity"
        )
        ]

      }
       
      ### Dealing with the hunting services
       sim_data_complete[, c('Hunting_forest_deer', 
                            'Hunting_forest_boar', 
                            'Hunting_other_habitat_roe_deer',
                            'Hunting_other_habitat_boar') := list(
                              Hunting_habitat_deer,
                              proportion_forests,
                              ifelse(proportion_grasslands + Crop/no_plots <= 0.15, proportion_grasslands + Crop/no_plots, 0.15), # Change to check other habitat suitability thresholds
                              ifelse(proportion_grasslands + Crop/no_plots <= 0.10, proportion_grasslands + Crop/no_plots, 0.10)) # Change to check other habitat suitability thresholds
                            ]
       

      ### Normalisation of all services and service indicators by maximum at landscape level
      sim_data_complete[, c(
        "Aesth_diversity_ADI",
        "Aesth_uniqueness_charismatic_plants",
        "Aesthetic_naturalness",
        "C_stock",
        "Harvesting_plants",       
        'Hunting_forest_deer', 
        'Hunting_forest_boar', 
        'Hunting_other_habitat_roe_deer',
        "Production_energy",
        "Production_food",
        "Production_livestock",
        "Production_timber",
        "Reg_ID_habitat",
        "birds_diversity",
        "plants_diversity_corr",
        "birds_charism_diversity",
        "fungi_diversity",
        "Aesth_diversity_LU",
        "Leisure_grasslands",
        "Leisure_forests"
      ) :=
        lapply(.SD, function(x) {
          Max <- quantile(x, 0.975, na.rm = T)
          Min <- quantile(x, 0.025, na.rm = T)
          y <- (x - Min) / (Max - Min)
          y[y > 1] <- 1
          y[y < 0] <- 0
          return(y)
        }),
      .SDcols = c(
        "Aesth_diversity_ADI",
        "Aesth_uniqueness_charismatic_plants",
        "Aesthetic_naturalness",
        "C_stock",
        "Harvesting_plants",
        'Hunting_forest_deer', 
        'Hunting_forest_boar', 
        'Hunting_other_habitat_roe_deer',
        "Production_energy",
        "Production_food",
        "Production_livestock",
        "Production_timber",
        "Reg_ID_habitat",
        "birds_diversity",
        "plants_diversity_corr",
        "birds_charism_diversity",
        "fungi_diversity",
        "Aesth_diversity_LU",
        "Leisure_grasslands",
        "Leisure_forests"
      ),
      by = region
      ]
      
      # There are some Nan for landscapes with  grasslands or crops above the limit for hunting, we replace them with 0
      if (is.nan(unique(sim_data_complete$Hunting_other_habitat_roe_deer))  == TRUE){
        sim_data_complete$Hunting_other_habitat_roe_deer = 0
      }
      if (is.nan(unique(sim_data_complete$Hunting_other_habitat_boar))  == TRUE){
        sim_data_complete$Hunting_other_habitat_boar = 0
      }
      
      ## Check: indicators min/max
     # sim_data_complete[, lapply(.SD, function(x) {
     #   list(min(x), mean(x), max(x))
     # }), .SDcols = colnames(sim_data_complete)[-c(1:3)]]

      ### Aggregating all indicators into services
      services <- c(
        "Ric",
        "Hunting",
        "Harvesting",
        "Production_food",
        "Production_livestock",
        "Production_timber",
        "Production_energy",
        "Aesthetic",
        "Leisure",
        "Reg_id",
        "C_stock"
      )

      sim_data_complete <- sim_data_complete[, c(
        "Ric",
        "Hunting",
        "Harvesting",
        "Production_food",
        "Production_livestock",
        "Production_timber",
        "Production_energy",
        "Aesthetic",
        "Leisure",
        "Reg_id",
        "C_stock"
      ) := list(
        (plants_diversity_corr + birds_diversity),
       ((Hunting_forest_deer + Hunting_other_habitat_roe_deer)/ 2 + (
           Hunting_forest_boar + Hunting_other_habitat_boar
         )/ 2)/2,
        (Harvesting_plants + fungi_diversity) / 2,
        Production_food,
        Production_livestock,
        Production_timber,
        Production_energy,
        Aesthetic_naturalness + (Aesth_diversity_LU + Aesth_diversity_ADI) / 2,
        Leisure_grasslands + Leisure_forests,
        (Reg_ID_habitat + 1) / 2 + Aesth_uniqueness_charismatic_plants + birds_charism_diversity,
        C_stock
      ),
      by = c("scenario", "replica", "region")
      ]

      sim_data_complete[, c('Ric', "Aesthetic", "Leisure", "Reg_id", 'Hunting') := lapply(.SD, scale01), .SDcols = c('Ric', "Aesthetic", "Leisure", "Reg_id", 'Hunting'), by = region]
      
      ############ Now that all the services are properly normalised, we can remove all the unwanted landscapes
      # where the proportion of crops differs from the baseline

      if (crop_constrained == TRUE) {
          sim_data_complete <- sim_data_complete[(region == "A" &
                                                    Crop %in% landscape_scenarios[Scenario_description == 'Baseline' & Region == 'A',as.numeric(names(sort(table(Crop), decreasing = T)[1]))]) |
                                                   (region == "H" &
                                                      Crop %in% landscape_scenarios[Scenario_description == 'Baseline' & Region == 'H',as.numeric(names(sort(table(Crop), decreasing = T)[1]))]) |
                                                   (region == "S" &
                                                      Crop %in% landscape_scenarios[Scenario_description == 'Baseline' & Region == 'S', as.numeric(names(sort(table(Crop), decreasing = T)[1]))]) |
                                                   (region == "All" &
                                                      Crop %in% landscape_scenarios[Scenario_description == 'Baseline' & Region == 'All',as.numeric(names(sort(table(Crop), decreasing = T)[1]))]) ]
        }

      
      ## Check: see how services vary with % of forests = Area
      sim_average <- sim_data_complete[, lapply(.SD, mean),
                                       .SDcols = c(
                                         "Ric",
                                         "Hunting",
                                         "Harvesting",
                                         "Production_food",
                                         "Production_livestock",
                                         "Production_timber",
                                         "Production_energy",
                                         "Aesthetic",
                                         "Leisure",
                                         "Reg_id",
                                         "C_stock",
                                         "Area"
                                       ),
                                       by = c("scenario", "region")
      ]
      sim_average_melt <-
        melt.data.table(sim_average, id.vars = c("scenario", "region", "Area"))
      sim_average_melt[, variable_pretty := dplyr::recode(variable,
                                                          'Ric' = 'Biodiversity conservation',
                                                          'Hunting' = 'Hunting',
                                                          'Harvesting' = 'Foraging',
                                                          'Production_food' = 'Food crop production',
                                                          'Production_livestock' = 'Livestock production',
                                                          'Production_timber' = 'Timber production',
                                                          'Production_energy' = 'Energy production',
                                                          'Aesthetic' = 'Aesthetic value',
                                                          'Leisure' = 'Leisure',
                                                          'Reg_id' = 'Regional identity',
                                                          'C_stock' = 'Carbon stocks'
      )]
      
      
      sim_average_melt[, Forest_prop := (Area - 320)/84 * 100/20]
      
      gg_forest_prop =  ggplot(sim_average_melt, aes(value, x = Forest_prop)) +
        theme_bw() +
        geom_point(alpha = 0.01, size = 0.1) +
        geom_smooth(color = 'black') +
        facet_wrap(~variable_pretty) + 
        xlab('Proportion of forest plots (%)') +
        ylab('Service value (scaled)')
      
      ggsave(
        plot = gg_forest_prop,
        file = paste("Results/forest_prop_env_corr", env_corr, "by_region", by_region,
                     "_crop_constrained", crop_constrained, ".png", sep = ""
        ),
        height = 6,
        width = 8
      )

      # * --- Loop on SB ####
      if (by_region == FALSE &
        crop_constrained == TRUE & env_corr == "env_corr") {
        SB_choices <- c(FALSE#, TRUE
                        )
      } else {
        SB_choices <- FALSE
      }

      for (use_SB in SB_choices
           ) {
        print(paste("SB: ", use_SB))
        ### If we're using supply-benefit relationship, we transform the services accordingly
        if (use_SB == TRUE) {
          SB_relationship <- function(X, form, threshold = 0.5) {
            if (min(X) < 0 | max(X) > 1) {
              print("Warning: X should range from 0 to 1")
            }
            if (threshold <= 0 |
              threshold >= 1 & threshold != "median") {
              print(
                "Warning: the threshold should be higher than 0 and lower than 1 or aqual to 'median'"
              )
            }

            if (threshold == "median") {
              threshold <- median(X)
            }

            Y <- numeric(length = length(X))

            if (form == "linear") {
              # Linear, eg C stock
              Y <- X
            }
            if (form == "cubic") {
              # Conservation
              Y <- X^3 * 2
            }
            if (form == "threshold_linear") {
              # Production
              Y[X < threshold] <- 0
              Y[X >= threshold] <- (X[X >= threshold] - threshold) / (1 - threshold)
            }
            return(Y)
          }


          sim_data_complete[,
            c(
              "Ric",
              "Hunting",
              "Harvesting",
              "Production_food",
              "Production_livestock",
              "Production_timber",
              "Production_energy",
              "Aesthetic",
              "Leisure",
              "Reg_id",
              "C_stock"
            ) := list(
              SB_relationship(Ric, "cubic"),
              SB_relationship(Hunting, "cubic"),
              SB_relationship(Harvesting, "cubic"),
              SB_relationship(Production_food, "cubic"),
              SB_relationship(Production_livestock, "cubic"),
              SB_relationship(Production_timber, "cubic"),
              SB_relationship(Production_energy, "cubic"),
              SB_relationship(Aesthetic, "cubic"),
              SB_relationship(Leisure, "cubic"),
              SB_relationship(Reg_id, "cubic"),
              SB_relationship(C_stock, "linear")
            ),
            by = region
          ]
        }


        #  *--- Loop on Forest classification ####
        if (by_region == FALSE &
          crop_constrained == TRUE &
          env_corr == "env_corr" | use_SB == FALSE) {
          forest_choices <- c("Type"#, "Age"
                              )
        } else {
          forest_choices <- "Type"
        }

        for (forest_class in forest_choices
             ) {
          print(forest_class)
          if (forest_class == "Type") {
            service_data <-
              sim_data_complete[Forest_age_scenario %in% c("Type", "Both"), .SD, .SDcols = c(
                "scenario",
                "Scenario_description",
                "concatenate",
                "N",
                "replica",
                "region",
                "LUI_corr",
                services
              )]
          }
          if (forest_class == "Age") {
            service_data <-
              sim_data_complete[Forest_age_scenario %in% c("Age", "Both"), .SD, .SDcols = c(
                "scenario",
                "Scenario_description",
                "concatenate",
                "N",
                "replica",
                "region",
                "LUI_corr",
                services
              )]
          }
          
          for (gr in unique(Priority$Group)) {
            Weights <- lapply(unique(Priority[Group == gr, .SD, .SDcols = colnames(Priority)[!(colnames(Priority)%in% c('Group','Perceived_influence'))]]), as.numeric)
            service_data[, paste(gr, "MF", sep = "__") := (
              Weights$Ric * Ric
                + Weights$Hunting * Hunting
                + Weights$Harvesting * Harvesting
                + Weights$Production_food * Production_food
                + Weights$Production_livestock * Production_livestock
                + Weights$Production_energy * Production_energy
                + Weights$Reg_id * Reg_id
                + Weights$Leisure * Leisure
                + Weights$Production_timber * Production_timber
                + Weights$Aesthetic * Aesthetic
                + Weights$C_stock * C_stock
            )]
          }

          service_data2 <-
            melt.data.table(
              service_data,
              id.vars = c(
                "scenario",
                "Scenario_description",
                "concatenate",
                "replica",
                "region",
                "LUI_corr"
              )
            )
    
          Baseline_values <- service_data2[Scenario_description == "Baseline", list(Value = mean(value)), by = list("Region" = region, "Variable" = variable)]

          service_data2[, value_baseline := Baseline_values[Region == region &
            Variable == variable, Value], by = c("region", "variable")]
          service_data2[, value_diff := value - value_baseline]

          fwrite(service_data2[, .SD, .SDcols = c('value_diff', 'value', 'LUI_corr', 'variable', 'scenario', 'region', 'replica', 'value_baseline', 'Scenario_description', 'concatenate')],
                 paste("Temporary_data/Community_services_full",
                 env_corr, "-by_region", by_region, "-SB", use_SB,
                 "_forest", forest_class, "_constrained", crop_constrained, ".csv", sep = "")
          )
          
          # Add Power value in
          service_data2[, is.MF := grepl("__MF", variable, perl = TRUE), by = variable]
          service_data2[is.MF == TRUE, Group := gsub("([A-z]*)__MF", "\\1", variable, perl = TRUE), by = variable]
          service_data2 <-
            merge.data.table(service_data2, Priority[, list(
              "Group" = Group,
              "Power" = Perceived_influence
            )], all.x = T)


          #### Plots for interest groups in individual scenarios ####
          if (forest_class == "Type") {
            Data_detailed <-
              sim_data_complete[Scenario_description %in% c("Baseline") &
                Forest_age_scenario %in% c("Type", "Both"),
              .SD,
              .SDcols = c("scenario", "replica", "region", services)
              ]
          }
          if (forest_class == "Age") {
            Data_detailed <-
              sim_data_complete[Scenario_description %in% c("Baseline") &
                Forest_age_scenario %in% c("Age", "Both"),
              .SD,
              .SDcols = c("scenario", "replica", "region", services)
              ]
          }

          Data_detailed$Scenario_description <- "Baseline"
          groups <- data.table(unique(data.frame(Group = Priority$Group)))
          Data_detailed <- data.table(groups[, Data_detailed[], by = Group])

          for (gr in unique(Priority$Group)) {
            Weights <- unique(data.frame(Priority[Group == gr, .SD, .SDcols = colnames(Priority)[!colnames(Priority) %in% c('Group','Perceived_influence')] ]))
            Data_detailed[Group == gr, c(
              "Ric_w",
              "Hunting_w",
              "Harvesting_w",
              "Production_food_w",
              "Production_livestock_w",
              "Production_energy_w",
              "Reg_id_w",
              "Leisure_w",
              "Production_timber_w",
              "Aesthetic_w",
              "C_stock_w"
            ) := list(
                Weights$Ric * Ric,
                Weights$Hunting * Hunting,
                Weights$Harvesting * Harvesting,
                Weights$Production_food * Production_food,
                Weights$Production_livestock * Production_livestock,
                Weights$Production_energy * Production_energy,
                Weights$Reg_id * Reg_id,
                Weights$Leisure * Leisure,
                Weights$Production_timber * Production_timber,
                Weights$Aesthetic * Aesthetic,
                Weights$C_stock * C_stock
              )]
          }

          Data_detailed_average <-
            Data_detailed[, lapply(.SD, mean), by = c("Group", "region", "Scenario_description"), .SDcols = c(
              "Ric_w",
              "Hunting_w",
              "Harvesting_w",
              "Production_food_w",
              "Production_livestock_w",
              "Production_timber_w",
              "Production_energy_w",
              "Aesthetic_w",
              "Leisure_w",
              "Reg_id_w",
              "C_stock_w"
            )]
          Data_detailed_melt <-
            melt.data.table(
              Data_detailed_average,
              id.vars = c("region", "Group", "Scenario_description")
            )
          Data_detailed_melt[, variable := factor(
            variable,
            levels = c(
              "Ric_w",
              "Aesthetic_w",
              "Reg_id_w",
              "Leisure_w",
              "Production_food_w",
              "Production_livestock_w",
              "Production_timber_w",
              "Production_energy_w",
              "Harvesting_w",
              "Hunting_w",
              "C_stock_w"
            )
          )]

          Data_detailed_melt <- Data_detailed_melt[order(variable), ]

          Data_detailed_melt$Group <- factor(
            Data_detailed_melt$Group,
            levels = c(
              "Quarrying",
              "Agric",
              "Hunting",
              "Forestry",
              "Landowner",
              "Tourism",
              "Loc_her_asso",
              "Policy_admin",
              "Press",
              "Locals",
              "Econ",
              "Nat_cons_asso",
              "Research",
              "Reg_dev_prog"
            )
          )
          
          #  *--- Loop on power weighting ####
          if (by_region == FALSE &
            crop_constrained == TRUE &
            env_corr == "env_corr" |
            use_SB == FALSE | forest_class == "Type") {
            weighting_choices <- c(#TRUE, 
              FALSE)
          }  else {
            weighting_choices <- FALSE
          }

          for (weighted in weighting_choices) {

            ### Now we can calculate the community-level MF
            if (weighted == TRUE) {
              Community_MF <- service_data2[is.MF == TRUE, list(
                mean_MF = wtd.mean(value, Power),
                gini = Gini(value, Power),
                gini_unwtd = Gini(value),
                LUI = mean(LUI_corr)
                
              ), by = c(
                "scenario",
                "region",
                "replica",
                "Scenario_description",
                "concatenate"
              )]
            }
            if (weighted == FALSE) {
              Community_MF <- service_data2[is.MF == TRUE, list(
                mean_MF = mean(value),
                gini = Gini(value),
                gini_unwtd = Gini(value),
                LUI = mean(LUI_corr)
              ), by = c(
                "scenario",
                "region",
                "replica",
                "Scenario_description",
                "concatenate"
              )]
            }
            
            
            for (R in unique(Data_detailed_melt$region)){
              plot_baseline <- ggplot(
                Data_detailed_melt,
                aes(x = Group,y = value,fill = variable,group = region)
              ) + geom_col() + theme_bw() + xlab('')+
                ylab("") + theme(legend.position = "none", 
                                 text = element_text(size=20),
                                 axis.ticks.y = element_blank(),
                                 axis.text.y = element_blank(), 
                                 panel.grid.minor =   element_blank()) +
                coord_flip()+
                scale_fill_manual(
                  values = my_palette_services,
                  breaks = c("Ric_w","Aesthetic_w","Reg_id_w","Leisure_w","Production_food_w","Production_livestock_w","Production_timber_w","Production_energy_w","Harvesting_w","Hunting_w","C_stock_w"))
              
              
              dir.create(file.path(paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region, 
                                         "/SB", use_SB, "/weighted", weighted, "/forest", forest_class, 
                                         "/constrained", crop_constrained, "/", sep = "")), recursive = T) 
              
              ggsave(
                plot = plot_baseline,
                file = paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region, 
                             "/SB", use_SB, "/weighted", weighted, "/forest", forest_class, 
                             "/constrained", crop_constrained, "/plot_baseline_MF.pdf", sep = ""),
                width = 5,
                height = 6
              )
            }
            
            
            
            
            Community_services <- service_data2[, as.list(list(MEAN = mean(value, na.rm = T), SD = sd(value, na.rm = T))), by = c(
              'variable',
              "scenario",
              "region",
              "Scenario_description",
              "concatenate"
            )]

            ### Additional definitions of equity
            ## We also want to know whether each scenario increases or decreases, on average, the MF of individual groups
            MF_by_group_all <-
              service_data2[is.MF == TRUE, as.list(CI(value_diff)), by = c("scenario", "concatenate", "region", "variable")][,
                list(n_losers = -length(upper[upper <
                  0])),
                by = list(
                  "scenario" = scenario,
                  "concatenate" = concatenate,
                  "Region" = region
                )
              ]

            # Besides, we need to know how much each scenario decreases endangered services
              Availability_merged <-
              merge.data.table(
                merge.data.table(
                  service_data2[is.MF == FALSE, list(value_diff = mean(value_diff)), by = c("scenario", "concatenate", "region", "variable")],
                  service_data2[is.MF == FALSE, as.list(CI(value_diff)), by = c("scenario", "concatenate", "region", "variable")],
                                  by = c('variable', 'region', 'scenario', 'concatenate')),
                Availability[, list(variable = as.factor(variable), region, endangered)],
                by = c("variable", "region"),
                all.x = T
              )
            Availability_merged <- Availability_merged[endangered >= 0.65, ]
          
              Threat_scores <-
              Availability_merged[, list(
                Score  = sum(
                value_diff * endangered,
                na.rm = T
              ),
              Score01 = sum(upper < 0.0)),
                , by = list(
                "scenario" = scenario,
                "Region" = region,
                "concatenate" = concatenate
              )]


            # Compare equity and LUI to baseline
            Baseline_H <- Community_MF[Scenario_description == "Baseline", list(
              gini_baseline = mean(gini),
              gini_unwtd_baseline = mean(gini_unwtd)
            ), by = list("Region" = region)]
            Baseline_MF <- Community_MF[Scenario_description == "Baseline", list(MF_baseline = mean(mean_MF), LUI_baseline = mean(LUI)), by = list("Region" = region)]


            Community_MF_average <- Community_MF[, list(
              mean_MF = mean(mean_MF),
              sd_MF = sd(mean_MF),
              mean_gini = mean(gini),
              mean_unwtd_gini = mean(gini_unwtd),
              sd_gini = sd(gini),
              LUI = mean(LUI)
            ),
            by = c(
              "scenario",
              "region",
              "Scenario_description",
              "concatenate"
            )
            ]

            Community_MF_average[, gini_baseline := Baseline_H[Region == region, gini_baseline], by = region]
            Community_MF_average[, mean_gini_diff := mean_gini - gini_baseline]
            Community_MF_average[, gini_unwtd_baseline := Baseline_H[Region == region, gini_unwtd_baseline], by = region]
            Community_MF_average[, mean_gini_unwtd_diff := mean_unwtd_gini - gini_unwtd_baseline]
            Community_MF_average[, MF_baseline := Baseline_MF[Region == region, MF_baseline], by = region]
            Community_MF_average[, mean_MF_diff := mean_MF - MF_baseline]
            Community_MF_average[, LUI_baseline := Baseline_MF[Region == region, LUI_baseline], by = region]
            Community_MF_average[, LUI_diff := LUI - LUI_baseline]
            

            # Add threat scores
            Community_MF_average <-
              merge.data.table(
                Community_MF_average,
                Threat_scores[, region := Region],
                by = c("scenario", "concatenate", "region"),
                all.x = T
              )
            Community_MF_average <-
              merge.data.table(
                Community_MF_average,
                MF_by_group_all[, region := Region],
                by = c("scenario", "concatenate", "region"),
                all.x = T
              )


            write.csv(Community_MF_average,  paste(
              "Temporary_data/Community_average",
              env_corr,
              "-by_region",
              by_region,
              "-SB",
              use_SB,
              "-weighted",
              weighted,
              "_forest",
              forest_class,
              "_constrained",
              crop_constrained,
              ".csv",
              sep = ""
            ))
            write.csv(Community_services,  paste(
              "Temporary_data/Community_services",
              env_corr,
              "-by_region",
              by_region,
              "-SB",
              use_SB,
              "-weighted",
              weighted,
              "_forest",
              forest_class,
              "_constrained",
              crop_constrained,
              ".csv",
              sep = ""
            ))
          }
        }
      }
    }
  }
  }
