# This script takes as input the results of the full simulations, calculate multifunctionality, and indentifies the best landscapes
library(data.table)
library(vegan)
library(ggplot2)
library(ggnewscale)
library(ghibli)
library(Hmisc)
library(mgcv)
library(readxl)
library(boot)
library(R.utils)
library(Rmisc)

scale01 <- function(x) {
  x <- as.numeric(x)
  max <- quantile(x, 0.975, na.rm = T)
  min <- min(x, na.rm = T)
  y <- (x - min) / (max - min)
  y[y < 0] <- 0
  y[y > 1] <- 1
  return(y)
}

for (crop_constrained in c(TRUE, FALSE)) {
  # crop_constrained = 'TRUE'

  #### Data ####
  # Demand and service availability
  Availability <-
    fread(
      "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Availability.csv"
    )
  Demand_raw <-
    fread(
      "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Demand_Power.csv"
    )


  # Baseline scenarios
  # Baseline_scenarios <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Scenarios/Scenarios_management_new_baseline.xlsx", sheet = 3))
  # Baseline = melt(Baseline_scenarios[Scenario_name == 'Baseline', .SD, .SDcols = c('Forest_classification','Region', 'Crop', 'Forest_Mixed', 'Forest_Coniferous', 'Forest_Deciduous', 'Forest_even_aged', 'Forest_uneven_aged', 'Grassland_low', 'Grassland_medium', 'Grassland_high')], value.name = "Prop",
  #               id.vars = c('Forest_classification', 'Region'))
  # Baseline[is.na(Region), Region := 'All']
  # Baseline[, No := Prop*15/100]
  # Baseline1 = read_excel('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Scenarios/BASELINE.xlsx')

  ### Parameters loop
  for (by_region in c(TRUE)){ #, TRUE)) {
    # by_region = 'FALSE'
    print(paste("by_region: ", by_region))

    # Scenario composition
    landscape_scenarios <-
      fread(
        paste(
          "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_simulation/data/scenarios_",
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
      "Forest_Mixed",
      "Forest_Coniferous",
      "Forest_even_aged",
      "Forest_uneven_aged"
    )]
    landscape_scenarios[, scenario := scenario_no]
    landscape_scenarios[, Area := (
      Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged
    ) * 100 +
      (Grassland_medium + Grassland_high + Grassland_low + Crop) * 16]
    landscape_scenarios[, proportion_grasslands := (Grassland_medium + Grassland_high + Grassland_low) / no_plots]
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

    # Load model for area correction
    source(
      "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Area_correction.R"
    )

    # * --- Loop on scale_within ####
   #  for (scale_within_land_use in c(FALSE, TRUE)) {
    scale_within_land_use <- TRUE
    print(paste("scale_within_land_use: ", scale_within_land_use))


    # * --- Loop on environment correction ####
    if (by_region == FALSE & crop_constrained == TRUE) {
      env_choices <- c("env_corr", "")
    }   else {
      env_choices <- "env_corr"
    }
    for (env_corr in env_choices) {
    #env_corr = "env_corr"
      print(paste("env_corr: ", env_corr))

      file <-
        paste(
          "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_simulation/landscapes_",
          env_corr,
          "_region",
          by_region,
          "_scale_within",
          scale_within_land_use,
          ifelse(crop_constrained == TRUE, "constrained",''),
          ".csv.gz",
          sep = ""
        )
      picked_file <- paste(
        "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_simulation/picked_",
        env_corr,
        "_region",
        by_region,
        "_scale_within",
        scale_within_land_use,
        ifelse(crop_constrained == TRUE, "_cropconstrained",''),
        ".csv.gz",
        sep = ""
      )

      # file = '/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_simulation/landscapes_env_corr_regionTRUE_scale_withinFALSE.csv'
      #  log_file =  '/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_simulation/output_env_corr_regionTRUE_scale_withinFALSE.log'

      if (file.exists(file)) {
        print("********** File exists **************")
        sim_data_complete <- fread(file)
        picked = fread(picked_file)
      } else {
        print(file)
        print("********** File does not exist, SKIPPED **************")
        next
      }
      # Temporarily shorten for easier management
       sim_data_complete <- sim_data_complete[replica < 200, ]
  
      # Identify faulty scenarios (i.e. asks for more plot than what exists)
       sim_data_complete = merge(sim_data_complete, picked[, c('scenario', 'replica', 'region', 'not_enough_plots')], 
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
              "proportion_grasslands"
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
      sim_data_complete[, LUI_corr := LUI_2007to12 / proportion_grasslands]
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

       print('e')
      ### Start working on simulation results
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
      # Plot service per landscape area <=> proportion of forests
      # ggplot(
      #    sim_data_complete[, list(div = mean(plants_diversity_corr), area = mean(Area)), by = c("region", "scenario")],
      #    aes(div, x = area, color = region)
      #  ) +
      #    geom_smooth()

      if (env_corr == "env_corr") {
        sim_data_complete[, c(
          "birds_diversity",
          "birds_charism_diversity",
          "fungi_diversity"
        ) :=
          lapply(.SD, function(x) {
            mod <-
              glm(
                x ~ region + Core_depth + Mean_Temp + Mean_precip + TWI + elevation + pH + prop_clay + heterogeneity,
                family = "poisson"
              )
            return(residuals(mod))
          }),
        .SDcols = c(
          "birds_diversity",
          "birds_charism_diversity",
          "fungi_diversity"
        )#,
      #  by = region
        ]
     #   sim_data_complete[, c("plants_diversity_corr") :=
    #      lapply(.SD, function(x) {
    #        mod <-
    #          lm(
    #            x ~ Core_depth + Mean_Temp + Mean_precip + TWI + elevation + pH + prop_clay + heterogeneity
    #          )
    #        return(residuals(mod))
    #      }),
    #    .SDcols = c("plants_diversity_corr"),
    #    by = region
    #    ]
      }

      #sim_data_complete[, Scenario_description := Scenario_description.x]
      # print('j')


      ### Normalisation of all services and service indicators by maximum at landscape level
      sim_data_complete[, c(
        "Aesth_diversity_ADI",
        "Aesth_uniqueness_charismatic_plants",
        "Aesthetic_naturalness",
        "C_stock",
        "Harvesting_plants",
        "Hunting",
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
        "Hunting",
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


      # hist(sim_data_complete[region == 'H', Aesth_diversity_ADI])
      # mean(sim_data_complete[region == 'H' & Scenario_description == 'Baseline', Aesth_diversity_ADI])

      ## Check: indicators min/max
      sim_data_complete[, lapply(.SD, function(x) {
        list(min(x), mean(x), max(x))
      }), .SDcols = colnames(sim_data_complete)[-c(1:3)]]

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
        (plants_diversity_corr + birds_diversity) / 2,
        Hunting,
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

      # hist(sim_data_complete[region == 'H', Aesthetic])
      # mean(sim_data_complete[region == 'H' & Scenario_description == 'Baseline', Aesthetic])

      sim_data_complete[, c("Aesthetic", "Leisure", "Reg_id") := lapply(.SD, scale01), .SDcols = c("Aesthetic", "Leisure", "Reg_id"), by = region]

      ## Check: see how services vary with % of forests = Area
     # sim_average <- sim_data_complete[, lapply(.SD, mean),
    #    .SDcols = c(
    #      "Ric",
    #      "Hunting",
    #      "Harvesting",
    #      "Production_food",
    #      "Production_livestock",
    #      "Production_timber",
    #      "Production_energy",
    #      "Aesthetic",
    #      "Leisure",
    #      "Reg_id",
    #      "C_stock",
    #      "Area"
    #    ),
    #    by = c("scenario", "region")
    #  ]
    #  sim_average_melt <-
    #    melt(sim_average, id.vars = c("scenario", "region", "Area"))
    #  ggplot(sim_average_melt, aes(value, x = Area, color = region)) +
    #    theme_bw() +
    #    geom_point(alpha = 0.01, size = 0.1) +
    #    geom_smooth() +
    #    facet_wrap(~variable)

      ## Check: service distribution
      # for (i in services){
      #  plot(histogram(sim_data_complete[, get(i)], xlab = i))
      # }


      ############ Now that all the services are properly normalised, we can remove all the unwanted landscapes
      # where the proportion of crops differs form the baseline

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



      # * --- Loop on SB ####
      if (by_region == FALSE &
        crop_constrained == TRUE & env_corr == "env_corr") {
        SB_choices <- c(FALSE, TRUE)
      } else {
        SB_choices <- FALSE
      }

      for (use_SB in SB_choices) {
       # use_SB <- FALSE
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
              Y[X >= threshold] <-
                (X[X >= threshold] - threshold) / (1 - threshold)
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

        ## Check: services min/max
     #   sim_data_complete[, lapply(.SD, function(x) {
    #      list(min(x), mean(x), max(x))
    #    }), .SDcols = services, by = region]

     #   sim_data_complete[, lapply(.SD, function(x) {
    #      sum(x) / 1000
    #    }), .SDcols = services, by = region]

      #  for (i in services) {
      #    plot(histogram(sim_data_complete[, get(i)], xlab = i))
      #  }

        #  *--- Loop on Forest classification ####
        if (by_region == FALSE &
          crop_constrained == TRUE &
          env_corr == "env_corr" | use_SB == FALSE) {
          forest_choices <- c("Type", "Age")
        } else {
          forest_choices <- "Type"
        }

        for (forest_class in forest_choices) {
          print(forest_class)
          #forest_class <- "Type"
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
          
          ### Adding demand in for all stakeholder groups
          #  *--- Loop on cluster/stakeholders ####
          # for (big_groups in c(FALSE, TRUE)) {

          big_groups <- FALSE
          # print(paste("big_groups: ", big_groups))
          if (big_groups == TRUE) {
            Demand <- Demand_raw[Category == "Cluster", ]
          } else {
            Demand <- Demand_raw[Category == "Stakeholder", ]
          }
          # print('b')
          for (gr in unique(Demand$Group)) {
            Weights <- lapply(unique(Demand[Group == gr, 3:13]), as.numeric)
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
                #"N.x",
                "replica",
                "region",
                "LUI_corr"
              )
            )

          # Removing baseline values
          ### Baseline check
         # test <- service_data2[Scenario_description == "Baseline", list(concatenate = unique(concatenate)), by = region]
        #  test[, c(
        #    "Crop",
        #    "Grassland_low",
        #    "Grassland_medium",
        #    "Grassland_high",
        #    "Forest_Deciduous",
        #    "Forest_Mixed",
        #    "Forest_Coniferous",
        #    "Forest_even_aged",
        #    "Forest_uneven_aged"
        #  ) := tstrsplit(concatenate, "_")]
         # test2 <- test[, lapply(.SD, function(x) {
        #    mean(as.numeric(x))
        #  }), .SDcols = c(
        #    "Crop",
        #    "Grassland_low",
        #    "Grassland_medium",
        #    "Grassland_high",
        #    "Forest_Deciduous",
        #    "Forest_Mixed",
        #    "Forest_Coniferous",
        #    "Forest_even_aged",
        #    "Forest_uneven_aged"
        #  ), by = region]
        #  test2$Crop / no_plots * 100 > 56

          Baseline_values <- service_data2[Scenario_description == "Baseline", list(Value = mean(value)), by = list("Region" = region, "Variable" = variable)]

          service_data2[, value_baseline := Baseline_values[Region == region &
            Variable == variable, Value], by = c("region", "variable")]
          service_data2[, value_diff := value - value_baseline]


          # Add Power value in
          service_data2[, is.MF := grepl("__MF", variable, perl = TRUE), by = variable]
          service_data2[is.MF == TRUE, Group := gsub("([A-z]*)__MF", "\\1", variable, perl = TRUE), by = variable]
          service_data2 <-
            merge.data.table(service_data2, Demand[, list(
              "Group" = Group,
              "region" = Region,
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


          # hist(sim_data_complete[region == 'H', Aesthetic])
          # mean(Data_detailed[region == 'H', Aesthetic])
          Data_detailed$Scenario_description <- "Baseline"
          groups <-
            data.table(unique(data.frame(Group = Demand$Group)))
          Data_detailed <-
            data.table(groups[, Data_detailed[], by = Group])

          for (gr in unique(Demand$Group)) {
            # print(gr)
            Weights <- unique(data.frame(Demand[Group == gr, 3:13]))
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
            ) :=
              list(
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
          plot_baseline <- ggplot(
            Data_detailed_melt,
            aes(
              x = Group,
              y = value,
              fill = variable,
              group = region
            )
          ) +
            geom_col() +
            theme_bw() +
            facet_grid( ~ region) +
            # ylim(c(0, 0.5)) +
            ylab("Multifunctionality") +
            theme(legend.position = "bottom") +
            scale_fill_manual(
              values = my_palette_services,
              breaks = c(
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
            )
          ggsave(
            plot = plot_baseline,
            file = paste(
              "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/plot_baseline",
              env_corr,
              "-by_region",
              by_region,
              "scale_within",
              scale_within_land_use,
              "-SB",
              use_SB,
              "-biggroup",
              big_groups,
              "_forest",
              forest_class,
              "_constrained",
              crop_constrained,
              ".pdf",
              sep = ""
            ),
            width = 10,
            height = 4
          )

          #  *--- Loop on power weighting ####
          if (by_region == FALSE &
            crop_constrained == TRUE &
            env_corr == "env_corr" |
            use_SB == FALSE | forest_class == "Type") {
            weighting_choices <- c(TRUE, FALSE)
          }  else {
            weighting_choices <- TRUE
          }


          for (weighted in weighting_choices) {
           # weighted <- TRUE
            print(paste("weighted:", weighted))
            if (big_groups == TRUE & weighted == TRUE) {
              next
            }

            ### Now we can calculate the community-level MF
            if (weighted == TRUE) {
              Community_MF <- service_data2[is.MF == TRUE, list(
                mean_MF = wtd.mean(value, Power),
                mean_MF_diff = wtd.mean(value_diff, Power),
                harmony = -sqrt(wtd.var(value, Power)),
                harmony_unwtd = -sd(value),
                LUI = mean(LUI_corr)
                
              ), by = c(
                "scenario",
                "region",
                "replica",
                "Scenario_description",
                "concatenate"#,
               # "N"
              )]
            }
            if (weighted == FALSE) {
              Community_MF <- service_data2[is.MF == TRUE, list(
                mean_MF = mean(value),
                #  mean_MF_diff = mean(value_diff),
                harmony = -sd(value),
                harmony_unwtd = -sd(value),
                LUI = mean(LUI_corr)
              ), by = c(
                "scenario",
                "region",
                "replica",
                "Scenario_description",
                "concatenate"
              )]
            }
            Community_services <- service_data2[, as.list(list(MEAN = mean(value, na.rm = T), SD = sd(value, na.rm = T))), by = c(
              'variable',
              "scenario",
              "region",
              "Scenario_description",
              "concatenate"
            )]
           
            # print('e')

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
                service_data2[is.MF == FALSE, list(value_diff = mean(value_diff)), by = c("scenario", "concatenate", "region", "variable")],
                Availability[, list(variable, "region" = Region, endangered)],
                by = c("variable", "region"),
                all.x = T
              )
            Availability_merged <- Availability_merged[endangered > 0.6, ]
            Threat_scores <-
              Availability_merged[, list(Score = (sum(
                value_diff * endangered,
                na.rm = T
              ))), by = list(
                "scenario" = scenario,
                "Region" = region,
                "concatenate" = concatenate
              )]



            # Compare equity and LUI to baseline
            Baseline_H <- Community_MF[Scenario_description == "Baseline", list(
              harmony_baseline = mean(harmony),
              harmony_unwtd_baseline = mean(harmony_unwtd)
            ), by = list("Region" = region)]
            Baseline_MF <- Community_MF[Scenario_description == "Baseline", list(MF_baseline = mean(mean_MF), LUI_baseline = mean(LUI)), by = list("Region" = region)]


            Community_MF_average <- Community_MF[, list(
              mean_MF = mean(mean_MF),
              sd_MF = sd(mean_MF),
              mean_harmony = mean(harmony),
              mean_unwtd_harmony = mean(harmony_unwtd),
              sd_harmony = sd(harmony),
              LUI = mean(LUI)
            ),
            by = c(
              "scenario",
              "region",
              "Scenario_description",
              "concatenate"#,
             # "N"
            )
            ]

            Community_MF_average[, harmony_baseline := Baseline_H[Region == region, harmony_baseline], by = region]
            Community_MF_average[, mean_harmony_diff := mean_harmony - harmony_baseline]
            Community_MF_average[, harmony_unwtd_baseline := Baseline_H[Region == region, harmony_unwtd_baseline], by = region]
            Community_MF_average[, mean_harmony_unwtd_diff := mean_unwtd_harmony - harmony_unwtd_baseline]
            Community_MF_average[, MF_baseline := Baseline_MF[Region == region, MF_baseline], by = region]
            Community_MF_average[, mean_MF_diff := mean_MF - MF_baseline]
            Community_MF_average[, LUI_baseline := Baseline_MF[Region == region, LUI_baseline], by = region]
            Community_MF_average[, LUI_diff := LUI - LUI_baseline]
            

            # print('g')

            # Add threat scores
            Community_MF_average <-
              merge(
                Community_MF_average,
                Threat_scores[, region := Region],
                by = c("scenario", "concatenate", "region"),
                all.x = T
              )
            Community_MF_average <-
              merge(
                Community_MF_average,
                MF_by_group_all[, region := Region],
                by = c("scenario", "concatenate", "region"),
                all.x = T
              )


            write.csv(Community_MF_average,  paste(
              "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Community_average",
              env_corr,
              "-by_region",
              by_region,
              "scale_within",
              scale_within_land_use,
              "-SB",
              use_SB,
              "-weighted",
              weighted,
              "-biggroup",
              big_groups,
              "_forest",
              forest_class,
              "_constrained",
              crop_constrained,
              ".csv",
              sep = ""
            ))
            write.csv(Community_services,  paste(
              "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Community_services",
              env_corr,
              "-by_region",
              by_region,
              "scale_within",
              scale_within_land_use,
              "-SB",
              use_SB,
              "-weighted",
              weighted,
              "-biggroup",
              big_groups,
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
