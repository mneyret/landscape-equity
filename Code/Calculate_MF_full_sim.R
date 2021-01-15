# This script takes as input the results of the full simulations, calculate multifunctionality, and indentifies the best landscapes
library(data.table)
library(vegan)
library(ggplot2)
library(ggnewscale)
library(ghibli)
library(Hmisc)

#### Data ####
# Demand and service availability
Availability <- fread("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Availability.csv")
Demand_raw <- fread("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Demand_Power.csv")

# Scenario composition
landscape_scenarios <- fread("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Landscape_simulation/data/scenarios_new.csv")
landscape_scenarios[, Aesth_diversity_LU := diversity(.SD), .SDcols = c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged")]
landscape_scenarios[, Leisure_grasslands := ifelse(Grassland_low < 7, Grassland_low / 15, 7 / 15)]
landscape_scenarios[, Leisure_forests := ifelse(rowSums(.SD) < 7, rowSums(.SD) / 15, 7 / 15), .SDcols = c("Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged")]
landscape_scenarios[, scenario := scenario_no]
landscape_scenarios[, Area := (Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged) * 100 +
  (Grassland_medium + Grassland_high + Grassland_low + Crop) * 16]
landscape_scenarios[, concatenate := paste(.SD, collapse = "_"), by = 1:nrow(landscape_scenarios), .SDcols = c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged")]

# Predefined scenarios
Scenarios_predefined <- fread("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Predefined_scenarios.csv")
Scenarios_predefined[Scenario_description == "Current proportion of LU types", Scenario_description := "Baseline"]
Scenarios_predefined[, concatenate := paste(.SD, collapse = "_"), by = 1:nrow(Scenarios_predefined), .SDcols = c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged")]
Scenarios_predefined <- merge(Scenarios_predefined, landscape_scenarios[, .SD, .SDcols = c("scenario", "concatenate")])

# Area correction
## We need to correct plant, bird and fungi richness by the area of the plot

### Parameters loop
for (by_region in c(#FALSE, 
  TRUE)) {
  print(paste("by_region: ", by_region))
  source("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Area_correction.R")
  
  for (scale_within_land_use in c(#FALSE, 
    TRUE)) {
    print(paste("scale_within_land_use: ", scale_within_land_use))

    for (env_corr in c(#"env_corr", 
      "")) {
      print(paste("env_corr: ", env_corr))

      #    env_corr <- "env_corr"
      #    by_region <- TRUE
      #    use_SB <- FALSE
      #    scale_within_land_use <- FALSE
      #    big_groups <- FALSE
      #    weighted = FALSE

      if (env_corr == "env_corr") {
        file <- paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Landscape_simulation/landscapes_", env_corr, "-region", by_region, "-scale_within", scale_within_land_use, ".csv", sep = "")
      }

      if (env_corr == "") {
        file <- paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Landscape_simulation/landscapes", "_region", by_region, "-scale_within", scale_within_land_use, ".csv", sep = "")
      }

     # file = '/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Landscape_simulation/landscapes_env_corr-regionTRUE-scale_withinTRUE.csv'
      if (file.exists(file)) {
        print("********** File exists **************")
        sim_data_complete <- fread(file)
      } else {
        print("********** File does not exist, SKIPPED **************")

        next
      }
      # Temporarily shorten for easier management
      sim_data_complete <- sim_data_complete[replica < 100, ]

      # landscape_log = fread(paste('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Landscape_simulation/output_', env_corr, '-region', by_region,'-scale_within', scale_within_land_use,'.log', sep = ''), header = FALSE)

      # Identify faulty scenarios (i.e. asks for more plot than what exists)
      # landscape_log = unique(landscape_log[!grepl('INFO: could not generate more than .*', V1),])
      # landscape_log[, c('scenario', 'region') := list(as.numeric(gsub('TRACE: scenario ([0-9]*): not enough plots of type.* in [A-z]*', '\\1', V1)),
      #                                                substr(gsub('TRACE: scenario [0-9]*: not enough plots of type.* in ([A-z]*)', '\\1', V1), 1, 1))]
      # landscape_log$Faulty = 1

      sim_data_complete <- merge.data.table(sim_data_complete, landscape_scenarios[, .SD, .SDcols = c("Aesth_diversity_LU", "Leisure_grasslands", "Leisure_forests", "Area", "scenario")], by = "scenario")
      # sim_data_complete = merge.data.table(sim_data_complete, landscape_log[, c('scenario', 'region', 'Faulty')], all.x = T, by = c('scenario', 'region'))
      # sim_data_complete = sim_data_complete[is.na(Faulty),]
      sim_data_complete <- sim_data_complete[!is.na(heterogeneity), ]

      my_palette_services <- c(
        "lightsteelblue1", "lightsteelblue2", "lightsteelblue3", "lightsteelblue4",
        "burlywood1", "sandybrown", "lightsalmon1", "darksalmon", "lightsalmon3", "salmon4",
        "paleturquoise4"
      )

      ### Start working on simulation results
      sim_data_complete[, c("birds_diversity", "plants_diversity", "birds_charism_diversity", "fungi_diversity") :=
        lapply(.SD, as.numeric), .SDcols = c("birds_diversity", "plants_diversity", "birds_charism_diversity", "fungi_diversity")]

      # Area correction
      if (by_region == FALSE) {
        area_model <- gam(Ric_plants ~ Area, data = Area_correction_data)
      }
      if (by_region == TRUE) {
        area_model <- gam(Ric_plants ~ Area * region, data = Area_correction_data)
      }


      sim_data_complete[, "predicted_plant_richness" :=
        list(predict(area_model, .SD)),
      .SDcols = c("Area", "region")
      ]

      sim_data_complete[, c("plants_diversity_corr") := plants_diversity - predicted_plant_richness]

      # Plot service per landscape area <=> proportion of forests
      # ggplot(
      #    sim_data_complete[, list(div = mean(plants_diversity_corr), area = mean(Area)), by = c("region", "scenario")],
      #    aes(div, x = area, color = region)
      #  ) +
      #    geom_smooth()

      if (env_corr == "env_corr") {
        sim_data_complete[, c("birds_diversity", "birds_charism_diversity", "fungi_diversity") :=
          lapply(.SD, function(x) {
            mod <- glm(x ~ Core_depth + Mean_Temp + Mean_precip + TWI + elevation + pH + prop_clay + heterogeneity, family = "poisson")
            return(residuals(mod))
          }),
        .SDcols = c("birds_diversity", "birds_charism_diversity", "fungi_diversity"),
        by = region
        ]
        sim_data_complete[, c("plants_diversity_corr") :=
          lapply(.SD, function(x) {
            mod <- lm(x ~ Core_depth + Mean_Temp + Mean_precip + TWI + elevation + pH + prop_clay + heterogeneity, family = "poisson")
            return(residuals(mod))
          }),
        .SDcols = c("plants_diversity_corr"),
        by = region
        ]
      }


      ### Normalisation of all services and service indicators by maximum at landscape level
      sim_data_complete[, c(
        "Aesth_diversity_ADI", "Aesth_uniqueness_charismatic_plants", "Aesthetic_naturalness",
        "C_stock", "Harvesting_plants", "Hunting", "Production_energy", "Production_food",
        "Production_livestock", "Production_timber", "Reg_ID_habitat",
        "birds_diversity", "plants_diversity_corr", "birds_charism_diversity", "fungi_diversity",
        "Aesth_diversity_LU", "Leisure_grasslands", "Leisure_forests"
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
        "Aesth_diversity_ADI", "Aesth_uniqueness_charismatic_plants", "Aesthetic_naturalness",
        "C_stock", "Harvesting_plants", "Hunting", "Production_energy", "Production_food",
        "Production_livestock", "Production_timber", "Reg_ID_habitat",
        "birds_diversity", "plants_diversity_corr", "birds_charism_diversity", "fungi_diversity",
        "Aesth_diversity_LU", "Leisure_grasslands", "Leisure_forests"
      ),
      by = region
      ]

      ## Check: indicators min/max
      # sim_data_complete[,lapply(.SD, function(x){list(min(x), mean(x), max(x))}), .SDcols = colnames(sim_data_complete)[-c(1:3)]]


      ### Aggregating all indicators into services
      services <- c(
        "Ric", "Hunting", "Harvesting", "Production_food", "Production_livestock",
        "Production_timber", "Production_energy", "Aesthetic", "Leisure",
        "Reg_id", "C_stock"
      )

      sim_data_complete <- sim_data_complete[, c(
        "Ric", "Hunting", "Harvesting", "Production_food", "Production_livestock",
        "Production_timber", "Production_energy", "Aesthetic", "Leisure", "Reg_id", "C_stock"
      ) := list(
        (plants_diversity_corr + birds_diversity) / 2,
        Hunting,
        (Harvesting_plants + fungi_diversity) / 2,
        Production_food,
        Production_livestock,
        Production_timber,
        Production_energy,
        scale01(Aesthetic_naturalness + (Aesth_diversity_LU + Aesth_diversity_ADI) / 2),
        scale01(Leisure_grasslands + Leisure_forests),
        scale01((Reg_ID_habitat + 1) / 2 + Aesth_uniqueness_charismatic_plants + birds_charism_diversity),
        C_stock
      ),
      by = c("scenario", "replica", "region")
      ]

      ## Check: see how services vary with % of forests = Area
      # sim_average <- sim_data_complete[, lapply(.SD, mean),
      #                                 .SDcols = c(
      #                                   "Ric", "Hunting", "Harvesting", "Production_food", "Production_livestock",
      #                                   "Production_timber", "Production_energy", "Aesthetic", "Leisure", "Reg_id", "C_stock", "Area"
      #                                 ),
      #                                 by = c("scenario", "region")
      # ]
      # sim_average_melt <- melt(sim_average, id.vars = c("scenario", "region", "Area"))
      # ggplot(sim_average_melt, aes(value, x = Area, color = region)) +
      #   theme_bw() +
      #   geom_point(alpha = 0.01, size = 0.1) +
      #   geom_smooth() +
      #   facet_wrap(~variable)

      ## Check: service distribution
      # for (i in services){
      #  plot(histogram(sim_data_complete[, get(i)], xlab = i))
      # }

      
     # sim_test = sim_data_complete[replica < 2,]
      sim_test2 = melt(sim_test, id.vars = c('scenario', 'replica', 'region'))
      sim_test2[, isSB := ifelse(grepl('SB', variable), 'yes', 'no')]
      sim_test2[, var2 := gsub('_SB','', variable)]
      
      sim_test3 = dcast(sim_test2, scenario +replica +region + var2 ~isSB, value.var = 'value')[!is.na(yes),]
      ggplot(sim_test3, aes(yes, x = no, color = region )) + geom_point() + facet_grid(region~var2)
      
      ggplot(sim_test3, aes(yes, color = region )) + geom_histogram() + facet_grid(region~var2)
      ggplot(sim_test3, aes(no, color = region )) + geom_histogram() + facet_grid(region~var2)
      
      # * --- Loop on SB ####
      for (use_SB in c(FALSE, TRUE)) {
        print(paste("SB: ", use_SB))

        ### If we're using supply-benefit relationship, we transform the services accordingly
        if (use_SB == TRUE) {
          SB_relationship <- function(X, form, threshold = 0.5) {
            if (min(X) < 0 | max(X) > 1) {
              print("Warning: X should range from 0 to 1")
            }
            if (threshold <= 0 | threshold >= 1 & threshold != 'median') {
              print("Warning: the threshold should be higher than 0 and lower than 1 or aqual to 'median'")
            }
            
            if (threshold == 'median'){
              threshold = median(X)
            }

            Y <- numeric(length = length(X))

            if (form == "linear") { # Linear, eg C stock
              Y <- X
            }
            if (form == "cubic") { # Conservation
              Y <- X^3
            }
            if (form == "threshold_linear") { # Production
              Y[X < threshold] <- 0
              Y[X >= threshold] <- (X[X >= threshold] - threshold) / (1 - threshold)
            }
            return(Y)
          }

          sim_data_complete[,
            c(
              "Ric", "Hunting", "Harvesting", "Production_food", "Production_livestock",
              "Production_timber", "Production_energy", "Aesthetic", "Leisure", "Reg_id", "C_stock"
            ) := list(
              SB_relationship(Ric, "cubic"),
              SB_relationship(Hunting, "threshold_linear"),
              SB_relationship(Harvesting, "threshold_linear"),
              SB_relationship(Production_food, "threshold_linear"),
              SB_relationship(Production_livestock, "threshold_linear"),
              SB_relationship(Production_timber, "threshold_linear"),
              SB_relationship(Production_energy, "threshold_linear"),
              SB_relationship(Aesthetic, "threshold_linear"),
              SB_relationship(Leisure, "threshold_linear"),
              SB_relationship(Reg_id, "threshold_linear"),
              SB_relationship(C_stock, "linear")
            ),
            by = region
          ]
        }

        ## Check: services min/max
        #  sim_data_complete[, lapply(.SD, function(x) {
        #    list(min(x), mean(x), max(x))
        #  }), .SDcols = services]

        ### Adding demand in for all stakeholder groups
        service_data <- sim_data_complete[, .SD, .SDcols = c("scenario", "replica", "region", services)]
        print('a')
        #  *--- Loop on cluster/stakeholders ####
        for (big_groups in c(FALSE, TRUE)) {
          print(paste("big_groups: ", big_groups))
          if (big_groups == TRUE) {
            Demand <- Demand_raw[Category == "Cluster", ]
          } else {
            Demand <- Demand_raw[Category == "Stakeholder", ]
          }
          print('b')
          for (gr in unique(Demand$Group)) {
            Weights <- lapply(unique(Demand[Group == gr, 3:13]), as.numeric)
            service_data[, paste(gr, "MF", sep = "__") := (Weights$Ric * Ric
              + Weights$Hunting * Hunting
              + Weights$Harvesting * Harvesting
              + Weights$Production_food * Production_food
              + Weights$Production_livestock * Production_livestock
              + Weights$Production_energy * Production_energy
              + Weights$Reg_id * Reg_id
              + Weights$Leisure * Leisure
              + Weights$Production_timber * Production_timber
              + Weights$Aesthetic * Aesthetic
              + Weights$C_stock * C_stock) / sum(unlist(Weights))]
          }

          service_data2 <- melt.data.table(service_data, id.vars = c("scenario", "region", "replica"))
          print('c')
          # Removing baseline values
          Baseline_values <- rbindlist(
            lapply(unique(Scenarios_predefined$Region), function(x) {
              print(x)
              scenarios <- unique(Scenarios_predefined[Region == x & Scenario_description == "Baseline", scenario])
              values <- service_data2[region == x & scenario %in% scenarios, list(value = mean(value)), by = c("region", "variable")]
              names(values) <- c("Region", "Variable", "Value")
              return(values)
            })
          )

          service_data2[, value_baseline := Baseline_values[Region == region & Variable == variable, Value], by = c("region", "variable")]
          service_data2[, value_diff := value - value_baseline]

          # Add Power value in
          service_data2[, is.MF := grepl("__MF", variable, perl = TRUE), by = variable]
          service_data2[is.MF == TRUE, Group := gsub("([A-z]*)__MF", "\\1", variable, perl = TRUE), by = variable]
          service_data2 <- merge.data.table(service_data2, Demand[, list("Group" = Group, "region" = Region, "Power" = Perceived_influence)], all.x = T)

          #### Plots for interest groups in individual scenarios ####
          # Data_detailed <- sim_data_complete[scenario %in% Scenarios_predefined[Scenario_description %in% c(
          #   "All forests are deciduous",
          #   "All grasslands are high intensity",
          #   "33% each",
          #   "Low-lui and med-lui grasslands become uneven-aged forests"
          # ), scenario],
          # .SD,
          # .SDcols = c("scenario", "replica", "region", services)
          # ]
          #
          # Data_detailed <- merge(Data_detailed, unique(Scenarios_predefined[, c("scenario", "Scenario_description")]))
          # groups <- data.table(unique(data.frame(Big_group = Demand$Big_group)))
          # Data_detailed <- data.table(groups[, Data_detailed[], by = Big_group])
          #
          # for (gr in unique(Demand_big_groups$Big_group)) {
          #   print(gr)
          #   Weights <- lapply(Demand_big_groups[Big_group == gr, 2:12], as.numeric)
          #   Data_detailed[Big_group == gr, c("Ric", "Hunting", "Harvesting", "Production_food", "Production_livestock", "Production_energy", "Reg_id", "Leisure", "Production_timber", "Aesthetic", "C_stock") :=
          #                   list(
          #                     Weights$Ric * Ric / sum(unlist(Weights)),
          #                     Weights$Hunting * Hunting / sum(unlist(Weights)),
          #                     Weights$Harvesting * Harvesting / sum(unlist(Weights)),
          #                     Weights$Production_food * Production_food / sum(unlist(Weights)),
          #                     Weights$Production_livestock * Production_livestock / sum(unlist(Weights)),
          #                     Weights$Production_energy * Production_energy / sum(unlist(Weights)),
          #                     Weights$Reg_id * Reg_id / sum(unlist(Weights)),
          #                     Weights$Leisure * Leisure / sum(unlist(Weights)),
          #                     Weights$Production_timber * Production_timber / sum(unlist(Weights)),
          #                     Weights$Aesthetic * Aesthetic / sum(unlist(Weights)),
          #                     Weights$C_stock * C_stock / sum(unlist(Weights))
          #                   )]
          # }
          #
          # Data_detailed_average <- Data_detailed[, lapply(.SD, mean), by = c("Big_group", "region", "Scenario_description"), .SDcols = c(
          #   "Ric", "Hunting", "Harvesting", "Production_food",
          #   "Production_livestock", "Production_timber", "Production_energy",
          #   "Aesthetic", "Leisure", "Reg_id", "C_stock"
          # )]
          # Data_detailed_melt <- melt.data.table(Data_detailed_average, id.vars = c("region", "Big_group", "Scenario_description"))
          # Data_detailed_melt[, variable := factor(variable, levels = c(
          #   "Ric", "Aesthetic", "Reg_id", "Leisure",
          #   "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting",
          #   "C_stock"
          # ))]
          # Data_detailed_melt[, Big_group_short := dplyr::recode(Big_group,
          #                                                       "Cultural_services" = "C",
          #                                                       "Forest_provisioning_services" = "F",
          #                                                       "Environmental_protection" = "E",
          #                                                       "Open-land_provisioning_services" = "O"
          # )]
          # Data_detailed_melt <- Data_detailed_melt[order(variable), ]
          # ggplot(Data_detailed_melt, aes(x = Big_group_short, y = value, fill = variable, group = region)) +
          #   geom_col() +
          #   theme_bw() +
          #   facet_grid(Scenario_description ~ region) +
          #   ylim(c(0, 0.5)) +
          #   scale_fill_manual(values = my_palette_services, breaks = c(
          #     "Ric", "Aesthetic", "Reg_id", "Leisure",
          #     "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting",
          #     "C_stock"
          #   ))
          # plot_equal <- ggplot(Data_detailed_melt[Scenario_description == "33% each", ], aes(x = Big_group_short, y = value, fill = variable, group = region)) +
          #   geom_col() +
          #   theme_bw() +
          #   facet_grid(~region) +
          #   ylim(c(0, 0.55)) +
          #   xlab("Interest group") +
          #   ylab("Multifunctionality") +
          #   theme(legend.position = "none") +
          #   scale_fill_manual(values = my_palette_services, breaks = c(
          #     "Ric", "Aesthetic", "Reg_id", "Leisure",
          #     "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting",
          #     "C_stock"
          #   ))
          # plot_low_to_forest <- ggplot(Data_detailed_melt[Scenario_description == "Low-lui and med-lui grasslands become uneven-aged forests", ], aes(x = Big_group_short, y = value, fill = variable, group = region)) +
          #   geom_col() +
          #   theme_bw() +
          #   facet_grid(~region) +
          #   ylim(c(0, 0.55)) +
          #   xlab("Interest group") +
          #   ylab("Multifunctionality") +
          #   theme(legend.position = "none") +
          #   scale_fill_manual(values = my_palette_services, breaks = c(
          #     "Ric", "Aesthetic", "Reg_id", "Leisure",
          #     "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting",
          #     "C_stock"
          #   ))
          # plot_high_grasslands <- ggplot(Data_detailed_melt[Scenario_description == "All grasslands are high intensity", ], aes(x = Big_group_short, y = value, fill = variable, group = region)) +
          #   geom_col() +
          #   theme_bw() +
          #   facet_grid(~region) +
          #   ylim(c(0, 0.55)) +
          #   xlab("Interest group") +
          #   ylab("Multifunctionality") +
          #   theme(legend.position = "none") +
          #   scale_fill_manual(values = my_palette_services, breaks = c(
          #     "Ric", "Aesthetic", "Reg_id", "Leisure",
          #     "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting",
          #     "C_stock"
          #   ))
          # plot_deciduous <- ggplot(Data_detailed_melt[Scenario_description == "All forests are deciduous", ], aes(x = Big_group_short, y = value, fill = variable, group = region)) +
          #   geom_col() +
          #   theme_bw() +
          #   facet_grid(~region) +
          #   ylim(c(0, 0.55)) +
          #   xlab("Interest group") +
          #   ylab("Multifunctionality") +
          #   theme(legend.position = "none") +
          #   scale_fill_manual(values = my_palette_services, breaks = c(
          #     "Ric", "Aesthetic", "Reg_id", "Leisure",
          #     "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting",
          #     "C_stock"
          #   ))

          # Look at variation in service values slack
          # service_data_melt[scenario %in% c(1, 2, 3), scenario_bis := 'BL_Alb']
          # service_data_melt[scenario %in% c(7,8), scenario_bis := 'BL_H']
          # service_data_melt[scenario %in% c(10), scenario_bis := 'BL_S']
          #
          # All_variables_mean = service_data_melt[!is.na(scenario_bis), list(Mean = mean(value),
          #                                               Sd = sd(value),
          #                                               Mean_diff = mean(value_diff)), by = c('scenario_bis', 'region','variable')]
          #
          # All_variables_mean[, is.MF := grepl('MF',variable), by = variable]
          # All_variables_mean[, variable := factor(variable, levels = c('Ric', 'Aesthetic', 'Reg_id', 'Leisure',
          #                                                                'Production_food','Production_livestock', 'Production_timber',  'Production_energy', 'Harvesting', 'Hunting',
          #                                                                'C_stock'))]
          #
          # ggplot(All_variables_mean[ is.MF == FALSE, ], aes(Mean/11, x = scenario_bis, fill = variable)) +
          #   geom_col() + facet_wrap(~region) +
          #   scale_fill_manual(values = my_palette_services)
          # ggplot(All_variables_mean[!is.na(scenario_bis) & is.MF == FALSE, ], aes(Mean_diff/11, x = scenario_bis, fill = variable)) +
          #   geom_col() + facet_wrap(~region) +
          #   scale_fill_manual(values = my_palette_services)
          #
print('d')
          #  *--- Loop on power weighting ####
          for (weighted in c(TRUE, FALSE)) {
            if (big_groups == TRUE & weighted == TRUE){next}
            ### Now we can calculate the community-level MF
            if (weighted == TRUE) {
              Community_MF <- service_data2[is.MF == TRUE, list(
                mean_MF = wtd.mean(value, Power),
                mean_MF_diff = wtd.mean(value_diff, Power),
                harmony = -sqrt(wtd.var(value_diff, Power))
              ), by = c("scenario", "region", "replica")]
            }
            if (weighted == FALSE) {
              Community_MF <- service_data2[is.MF == TRUE, list(
                mean_MF = mean(value),
                mean_MF_diff = mean(value_diff),
                harmony = -sd(value)
              ), by = c("scenario", "region", "replica")]
            }
            print('e')
            # We also want to know whether each scenario increases or decreases, on average, the MF of individual groups
            MF_by_group_all <- service_data2[is.MF == TRUE, list(value_diff = mean(value_diff)), by = c("scenario", "region", "variable")][, list(positive_all = min(value_diff) >= 0), by = c("scenario", "region")]

            # Besides, we need to know how much each scenario decreases endangered services
            Availability_merged <- merge.data.table(service_data2[is.MF == FALSE, list(value_diff = mean(value_diff)), by = c("scenario", "region", "variable")],
              Availability[, list(variable, "region" = Region, endangered)],
              by = c("variable", "region"),
              all.x = T
            )
            Threat_scores <- Availability_merged[value_diff < 0, list(Score = -sum(endangered, na.rm = T)), by = c("scenario", "region")]


            Baseline_MF <- rbindlist(
              lapply(unique(Scenarios_predefined$Region), function(x) {
                scenarios <- unique(Scenarios_predefined[Region == x & Scenario_description == "Baseline", scenario])
                values <- Community_MF[region == x & scenario %in% scenarios, list(harmony = mean(harmony)), by = c("region")]
                names(values) <- c("Region", "harmony")
                return(values)
              })
            )

            Community_MF_average <- Community_MF[, list(
              mean_MF = mean(mean_MF),
              mean_MF_diff = mean(mean_MF_diff),
              sd_MF = sd(mean_MF),
              mean_harmony = mean(harmony),
              sd_harmony = sd(harmony)
            ),
            by = c("scenario", "region")
            ]

            Community_MF_average[, harmony_baseline := Baseline_MF[Region == region, harmony], by = region]
            Community_MF_average[, mean_harmony_diff := mean_harmony - harmony_baseline]

            # Identify best scenarios
            best_scenarios <- lapply(list("A", "H", "S", "All"), function(x) {
              # high_MF <- Community_MF_average[region == x, ][order(mean_MF, decreasing = T), scenario][1:50]
              # high_H <- Community_MF_average[region == x, ][order(mean_harmony, decreasing = T), scenario][1:50]
              # high_both <- c(intersect(high_MF, high_H))
              no_loser <- MF_by_group_all[region == x & positive_all == TRUE, scenario]
              good_MF <- Community_MF_average[region == x & mean_MF_diff > 0, scenario]
              good_H <- Community_MF_average[region == x & mean_MF_diff > 0, scenario]
              good_both <- Community_MF_average[region == x & mean_harmony_diff > 0 & mean_MF_diff > 0, scenario]
              # max_both <- Community_MF_average[region == x & max_both > 0, ][order(max_both, decreasing = T), scenario][1:10]
              return(list(
                # "high_MF" = high_MF[!is.na(high_MF)],
                #  "high_H" = high_H[!is.na(high_H)],
                "good_MF" = good_MF[!is.na(good_MF)],
                "good_H" = good_MF[!is.na(good_H)],
                # "high_both" = high_both[!is.na(high_both)],
                "good_both" = good_both[!is.na(good_both)],
                #  "max_both" = max_both[!is.na(max_both)],
                "no_loser" = no_loser,
                "ok_threat" = Threat_scores[region == x, ][order(Score, decreasing = T), scenario][1:nrow(Threat_scores[region == x, ]) / 4]
              ))
            })
            names(best_scenarios) <- c("A", "H", "S", "All")

            ## Match with pre-defined scenarios
            Community_MF_average <- merge(Community_MF_average, Scenarios_predefined[, list(region = Region, scenario, Scenario_description)], all.x = T)

            Community_MF_average <- merge(Community_MF_average, Threat_scores, all.x = T)

            Community_MF_average_by_predefscenario <- Community_MF_average[!is.na(Scenario_description), lapply(.SD, mean),
              .SDcols = c("mean_MF", "mean_MF_diff", "mean_harmony", "mean_harmony_diff", "Score"),
              by = c("region", "Scenario_description")
            ]


            Community_MF_average_by_predefscenario[Scenario_description == "half crops converted to grasslands", Scenario_description := "half crops converted to grasslands more low grassland"]

            palette_test <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a")
            big_plot_MF_H <- ggplot(Community_MF_average, aes(y = mean_harmony_diff, x = mean_MF_diff)) +
              geom_hline(yintercept = 0, lwd = 0.3) +
              geom_vline(xintercept = 0, lwd = 0.3) +
              scale_color_brewer(palette = "Set2") + # ghibli_palette(10, name = "PonyoMedium", type = "continuous")))) +
              geom_point(size = 0.2, alpha = 0.4, aes(color = region)) +
              theme_bw() +
              new_scale_color() +
              scale_color_manual(values = c(rbind(rep("black", 10), palette_test, palette_test))) + # ghibli_palette(10, name = "PonyoMedium", type = "continuous")))) +
              scale_fill_manual(values = c(rbind(
                palette_test # ghibli_palette(10, name = "PonyoMedium", type = "continuous")
                , rep("White", 10), rep("Grey", 10)
              ))) +
              geom_point(
                data = Community_MF_average_by_predefscenario[Scenario_description != "Baseline", ],
                aes(fill = Scenario_description, color = Scenario_description, shape = region, stroke = Scenario_description, ), size = 3
              ) +
              scale_discrete_manual(
                aesthetics = "stroke",
                values = rep(c(0.5, 1.5, 1.5), length.out = 30)
              ) +
              scale_shape_manual(values = c(21, 24, 22)) +
              # ylim(c(-80, 70)) +
              guides(
                fill = guide_legend(override.aes = list(shape = 21)),
                shape = guide_legend(position = "none")
              ) +
              ylab("Equity") +
              xlab("Multifunctionality") +
              theme(legend.position = "bottom")
            ggsave(
              plot = big_plot_MF_H, file = paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/big_plot_MF_H", env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup", big_groups, ".pdf", sep = ""),
              width = 30, height = 15
            )

            big_plot_threat <- ggplot(Community_MF_average, aes(y = Score, x = mean_MF_diff)) +
              geom_hline(yintercept = 0, lwd = 0.3) +
              geom_vline(xintercept = 0, lwd = 0.3) +
              scale_color_brewer(palette = "Set2") + # ghibli_palette(10, name = "PonyoMedium", type = "continuous")))) +
              geom_point(size = 0.2, alpha = 0.4, aes(color = region)) +
              theme_bw() +
              new_scale_color() +
              scale_color_manual(values = c(rbind(rep("black", 10), palette_test, palette_test))) + # ghibli_palette(10, name = "PonyoMedium", type = "continuous")))) +
              scale_fill_manual(values = c(rbind(
                palette_test # ghibli_palette(10, name = "PonyoMedium", type = "continuous")
                , rep("White", 10), rep("Grey", 10)
              ))) +
              geom_point(
                data = Community_MF_average_by_predefscenario[Scenario_description != "Baseline", ],
                aes(fill = Scenario_description, color = Scenario_description, shape = region, stroke = Scenario_description, ), size = 3
              ) +
              scale_discrete_manual(
                aesthetics = "stroke",
                values = rep(c(0.5, 1.5, 1.5), length.out = 30)
              ) +
              scale_shape_manual(values = c(21, 24, 22)) +
              # ylim(c(-80, 70)) +
              guides(
                fill = guide_legend(override.aes = list(shape = 21)),
                shape = guide_legend(position = "none")
              ) +
              ylab("Threat score") +
              xlab("Multifunctionality") +
              theme(legend.position = "bottom")

            ggsave(
              plot = big_plot_threat, file = paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/big_plot_threat", env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup", big_groups, ".pdf", sep = ""),
              width = 30, height = 15
            )

            # big_plot2 = big_plot + theme_bw(base_size = 8)+theme(legend.position = 'bottom') +
            #   annotation_custom(grob = ggplotGrob(plot_equal +ylab('')+xlab('')), xmin=-0.2, xmax=-0.1, ymin=-85, ymax=-45)+
            #   annotation_custom(grob = ggplotGrob(plot_high_grasslands +ylab('')+xlab('')), xmin=-0.2, xmax=-0.1, ymin=35, ymax=75)+
            #   annotation_custom(grob = ggplotGrob(plot_low_to_forest +ylab('')+xlab('')), xmin=0.02, xmax=0.12, ymin=-85, ymax=-45)+
            #   annotation_custom(grob = ggplotGrob(plot_deciduous +ylab('')+xlab('')), xmin=0.02, xmax=0.12, ymin=35, ymax=75) +
            #   geom_segment(data = data.frame(x = c(-0.007, -0.018, -0.03), y = c(4, 15, 9), xend = rep(-0.1, 3), yend = rep(40, 3)),
            #                aes(x = x, y = y, xend = xend, yend = yend), color = 'red', inherit.aes = F) +
            #   geom_segment(data = data.frame(x = c(-0.105, -0.021, 0.008), y = c(9, -14, -14), xend = rep(-0.1, 3), yend = rep(-45, 3)),
            #               aes(x = x, y = y, xend = xend, yend = yend), color = 'steelblue', inherit.aes = F) +
            #   geom_segment(data = data.frame(x = c(-0.045, -0.002, 0.015), y = c(4, 0, 6), xend = rep(0.05, 3), yend = rep(40, 3)),
            #               aes(x = x, y = y, xend = xend, yend = yend), color = 'darkgreen', inherit.aes = F) +
            #   geom_segment(data = data.frame(x = c(-0.001, -0.005, -0.01), y = c(-12, -10, -3), xend = rep(0.05, 3), yend = rep(-45, 3)),
            #               aes(x = x, y = y, xend = xend, yend = yend), color = 'purple', inherit.aes = F)

            # ggsave(big_plot2, file = '/Users/Margot/Desktop/bigplot.pdf', width = 12, height = 9)



            #  *--- Loop on "good scenario" criteria ####
            for (criteria in c("no_loser", "both_good", "3_good")) {
              print(criteria)
              if (criteria == "no_loser") {
                Best_landscape_compo_A <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$A$good_MF, best_scenarios$A$no_loser), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                ) [, region := "A"]
                Best_landscape_compo_H <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$H$good_MF, best_scenarios$H$no_loser), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                ) [, region := "H"]
                Best_landscape_compo_S <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$S$good_MF, best_scenarios$S$no_loser), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                )[, region := "S"]
                Best_landscape_compo_All <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$All$good_MF, best_scenarios$All$no_loser), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                )[, region := "All"]
              }
              if (criteria == "both_good") {
                Best_landscape_compo_A <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$A$good_MF, best_scenarios$A$good_H), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                ) [, region := "A"]
                Best_landscape_compo_H <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$H$good_MF, best_scenarios$H$good_H), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                ) [, region := "H"]
                Best_landscape_compo_S <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$S$good_MF, best_scenarios$S$good_H), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                )[, region := "S"]
                Best_landscape_compo_All <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$All$good_MF, best_scenarios$All$good_H), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                )[, region := "All"]
              }
              if (criteria == "3_good") {
                Best_landscape_compo_A <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$A$ok_threat, intersect(best_scenarios$A$good_MF, best_scenarios$A$good_H)), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                ) [, region := "A"]
                Best_landscape_compo_H <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$H$ok_threat, intersect(best_scenarios$H$good_MF, best_scenarios$H$good_H)), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                ) [, region := "H"]
                Best_landscape_compo_S <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$S$ok_threat, intersect(best_scenarios$S$good_MF, best_scenarios$S$good_H)), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                )[, region := "S"]
                Best_landscape_compo_All <- melt.data.table(landscape_scenarios[scenario %in% intersect(best_scenarios$All$ok_threat, intersect(best_scenarios$All$good_MF, best_scenarios$All$good_H)), c("Grassland_medium", "Grassland_high", "Grassland_low", "Crop", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "scenario")],
                  id.var = "scenario"
                )[, region := "All"]
              }


              Best_landscape_all <- rbindlist(list(Best_landscape_compo_A, Best_landscape_compo_H, Best_landscape_compo_S, Best_landscape_compo_All))

              Best_landscape_all[paste(region, scenario) %in% Scenarios_predefined[Scenario_description == "Baseline", paste(Region, scenario)], scenario_num := -1]
              Best_landscape_all[is.na(scenario_num), scenario_num := as.numeric(factor(scenario)), by = region]
              Best_landscape_all[, variable := factor(variable, levels = c("Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "Crop"))]
              Best_landscape_all_average <- Best_landscape_all[, list(value = mean(value)), by = c("variable", "region", "scenario_num")]

              LUnames <- c("Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "Crop")

              # ggplot(Best_landscape_all_average, aes(y = value, x = scenario_num, fill = variable)) +
              #    theme_bw() +
              #    geom_col() +
              #    facet_wrap(~region, ncol = 1) +
              #    scale_fill_manual(
              #      values = c("palegreen1", "palegreen3", "palegreen4", "cadetblue1", "cadetblue3", "cadetblue4", "lightsteelblue3", "lightsteelblue4", "tan1"),
              #      breaks = c("Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "Crop")
              #    )

              mean_comp <- dcast.data.table(Best_landscape_all, region + scenario ~ variable, value.var = "value")
              mean_comp[Forest_even_aged + Forest_uneven_aged > 0, Forest_age_scenario := 1]
              mean_comp <- merge(mean_comp, Community_MF_average[, .SD, .SDcols = c("scenario", "region")])

              mean_comp_mean <- mean_comp[, lapply(.SD, function(x) {
                mean(x)
              }), by = c("region", "Forest_age_scenario"), .SDcols = LUnames]
              mean_comp_sd <- mean_comp[, lapply(.SD, function(x) {
                sd(x)
              }), by = c("region", "Forest_age_scenario"), .SDcols = LUnames]
              mean_comp_all <- merge(
                melt(mean_comp_mean, id.vars = c("region", "Forest_age_scenario"), value.name = "mean"),
                melt(mean_comp_sd, id.vars = c("region", "Forest_age_scenario"), value.name = "sd")
              )

              Scenarios_predefined[Forest_even_aged + Forest_uneven_aged > 0, Forest_age_scenario := 1]

              Baseline_scenarios_compo <-
                rbind(
                  rbindlist(
                    lapply(unique(Community_MF_average$region), function(x) {
                      print(x)
                      scenarios <- unique(Scenarios_predefined[Scenario_description == "Baseline" & Region == x & Forest_age_scenario == 1, scenario])
                      values <- melt.data.table(landscape_scenarios[scenario %in% scenarios, lapply(.SD, mean), .SDcols = LUnames],
                        value.name = "baseline"
                      )
                      values[, region := x]
                      values[, Forest_age_scenario := 1]
                      return(values)
                    })
                  ),
                  rbindlist(
                    lapply(unique(Scenarios_predefined$Region), function(x) {
                      print(x)
                      scenarios <- unique(Scenarios_predefined[Scenario_description == "Baseline" & Region == x & is.na(Forest_age_scenario), scenario])
                      values <- melt.data.table(landscape_scenarios[scenario %in% scenarios, lapply(.SD, mean), .SDcols = LUnames],
                        value.name = "baseline"
                      )
                      values[, region := x]
                      values[, Forest_age_scenario := NA]
                      return(values)
                    })
                  )
                )


              mean_comp_w_baseline <- merge(Baseline_scenarios_compo, mean_comp_all, by = c("variable", "region", "Forest_age_scenario"))
              mean_comp_w_baseline[, diff := mean - baseline]
              mean_comp_w_baseline[, main_LU := tstrsplit(variable, "_", keep = 1)]
              mean_comp_w_baseline[, Forest_age_scenario := ifelse(Forest_age_scenario %in% c(1, "Incl. even/uneven-aged forests"), "Incl. even/uneven-aged forests", "Incl. forest types")]

              gg_diff_LU <- ggplot(mean_comp_w_baseline, aes(diff, x = variable, fill = variable, ymin = diff - sd, ymax = diff + sd)) +
                facet_grid(Forest_age_scenario ~ region) +
                ylab("Difference in n° of plots in the best scenarios v. baseline") +
                geom_col() +
                geom_errorbar() +
                theme_bw() +
                xlab("") +
                scale_fill_manual(name = "LU type", values = c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", "#cab2d6"))

              ggsave(plot = gg_diff_LU, file = paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/gg_diff_LU", env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup", big_groups, "-", criteria, ".pdf", sep = ""))

              mean_comp_w_baseline_big <- mean_comp_w_baseline[, list(
                baseline = mean(baseline),
                mean = mean(mean),
                sd = mean(sd)
              ), by = c("main_LU", "region")]
              mean_comp_w_baseline_big[, diff := mean - baseline]

              gg_diff_big_LU <- ggplot(mean_comp_w_baseline_big, aes(diff, x = main_LU, fill = main_LU, ymin = diff - sd, ymax = diff + sd)) +
                facet_wrap(~region) +
                theme_bw() +
                ylab("Difference in number of plots in the best scenarios compared to baseline") +
                geom_col() +
                geom_errorbar()

              ggsave(plot = gg_diff_big_LU, file = paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/gg_diff_big_LU", env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup", big_groups, "-", criteria, ".pdf", sep = ""))
            }
          }
        }
      }
    }
  }
}
