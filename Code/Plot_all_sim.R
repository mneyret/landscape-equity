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
my_palette_services <- c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue4","burlywood1","sandybrown","lightsalmon1","darksalmon","lightsalmon3","salmon4","paleturquoise4"
)
big_groups <- FALSE

for (crop_constrained in c(TRUE, FALSE)) {
  for (by_region in c(FALSE, TRUE)) {
    for (scale_within_land_use in c(#FALSE, 
      TRUE)) {
      # * --- Loop on environment correction ####
      for (env_corr in c("env_corr"#, ""
                         )) {
        # * --- Loop on SB ####
        for (use_SB in c(FALSE#, TRUE
                         )) {

          #  *--- Loop on Forest classification ####
          for (forest_class in c("Type"#, "Age"
                                 )) {
            #  *--- Loop on power weighting ####
            for (weighted in c(TRUE#, FALSE
                               )) {
              com_file <-
                paste( "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Community_average", 
                       env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup", 
                       big_groups, "_forest", forest_class, "_constrained", crop_constrained, ".csv", sep = ""
                )
              
              service_file <-
                paste( "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Community_services", 
                       env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup",
                       big_groups, "_forest", forest_class, "_constrained", crop_constrained, ".csv", sep = ""
                )
              
              service_file_full = paste(
                "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Community_services_full",
                env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-biggroup",
                big_groups, "_forest", forest_class, "_constrained", crop_constrained, ".csv", sep = ""
              )
         
              if (file.exists(service_file_full)) {
                print("********** File exists **************")
                Community_MF_average <- fread(com_file)
                Service_average <- fread(service_file)
                Community_services_full = fread(service_file_full)
                
              } else {
                print(com_file)
                print("********** File does not exist, SKIPPED **************")
                next
              }

              Community_services_full[, is.MF := grepl('__MF', variable, perl = T)]
              
              MF_by_group_all <-
                Community_services_full[is.MF == TRUE, 
                                        as.list(CI(value_diff)),
                                        by = c("scenario", "concatenate", "region", "variable")][,
                      list(n_losers = length(upper[upper < 0])),
                      by = list(
                        "scenario" = scenario,
                        "concatenate" = concatenate,
                        "region" = region
                      )
                ]
              
              Community_MF_average = merge.data.table(Community_MF_average,
                                                      MF_by_group_all, by = c('scenario', 'concatenate', 'region'))
              
              
              Community_MF_average[, equity_diff := -mean_gini_diff]
              # Identification of focus scenarios: deforestation, good scenarios, best for biodiversity, best for conservation asso
              Community_MF_average[, c("good_MF", "good_H", "good_both") := list(
                mean_MF_diff > 0,
                equity_diff > 0,
                mean_MF_diff > 0 & equity_diff > 0
              )]
              Community_MF_average[, no_loser := (n_losers == 0), by = c("scenario", "region")]
              Community_MF_average[Scenario_description == "Baseline", no_loser := FALSE]
              
              # Test of best MF + equity
              test = Community_MF_average[good_both == TRUE,]
              test[, MfEqScore := sqrt(mean_MF_diff) + sqrt(equity_diff)]
              test[, Rank := rank(-MfEqScore)]
              Community_MF_average[scenario %in% test[Rank <= 10, scenario], both_best := TRUE]
              
              # No losers, no loss of vulnerable services
              Community_MF_average[, no_loser_no_threat := (Score > 0 &
                no_loser == TRUE
                ), by = c("scenario", "region")]
              
              # Best among no losers, no loss of vulnerable services
              Community_MF_average[, order_no_losers := rank(-mean_MF), by = c("region", 'no_loser_no_threat')]
              Community_MF_average[no_loser_no_threat == TRUE & order_no_losers <= 10, best_no_loser_no_threat := TRUE, by = c("region")]
              
              # Deforestation scenarios
              max_defo <- Community_MF_average[Scenario_description == "Deforestation", ]
              Community_MF_average[Scenario_description == "Deforestation", deforestation := 1]

              # Highest biodiversity scenarios
              Richness_average = Service_average[variable == 'Ric', ]
              Richness_average[, order_div := (rank(-MEAN)), by = c("region")]
              Community_MF_average[scenario %in% Richness_average[order_div <= 10, scenario], 
                                   best_diversity := TRUE, by = c("region")]
              
              # Best for nat conservation asso
              NatAsso_average = Service_average[variable == 'Nat_cons_asso__MF', ]
              NatAsso_average[, order_div := (rank(-MEAN)), by = c("region")]
              Community_MF_average[scenario %in% NatAsso_average[order_div <= 10, scenario], 
                                   best_natasso := TRUE, by = c("region")]
              
              
              
              
              Community_MF_average_to_plot <- melt(
                Community_MF_average[, list(
                  Scenario_description,
                  mean_MF_diff,
                  equity_diff,
                  region,
                  n_losers,
                  Score,
                  both_best
                )],
                id.var = c("Scenario_description", "mean_MF_diff", "region", 'both_best')
              )

              ## Extract pre-defined scenarios
              Community_MF_average_by_predef <-
                melt(
                  Community_MF_average[!is.na(Scenario_description) &
                    Scenario_description != "Baseline" &
                    !grepl("Scenario_post", Scenario_description),
                  lapply(.SD, mean),
                  .SDcols = c(
                    "mean_MF_diff",
                    "equity_diff",
                    "n_losers",
                    "Score"
                  ),
                  by = c("region", "Scenario_description")
                  ],
                  id.var = c("Scenario_description", "mean_MF_diff", "region")
                )




              # Plot results
             # if (by_region == FALSE) {
                Community_MF_average_by_predef[, Scenario_description := factor(Scenario_description, levels = c(
                  "All forests are coniferous",
                  "All forests are deciduous",
                  "All forests are mixed",
                  "Same prop of each forest type",
                  "50% more high int grasslands",
                  "50% more low int grasslands",
                  "All grasslands are high int",
                  "All grasslands are low int",
                  "All mixed forests and low-intensity grasslands (same crops)",
                  "Only coniferous forests and only high LUI grasslands",

                  "Deforestation",
                  "Add 50% grasslands",
                  "Low-lui and med-lui grasslands become coniferous forests",
                  "33% each",
                  "Reforestation",
                  "Low-lui and med-lui grasslands become mixed forests"
                ))]
           #   }

              Community_MF_average_by_predef <- Community_MF_average_by_predef[order(Scenario_description), ]

              palette_test <-
                # c("#54478c","#2c699a","#048ba8","#0db39e","#16db93","#83e377","#b9e769","#efea5a","#f1c453","#f29e4c")
                c("#f94144", "#f3722c", "#f8961e", "#f9844a","#f9c74f", "#90be6d", "#43aa8b", "#4d908e", "#577590", "#277da1"
                )[10:1]
              

              
              big_plot_MF_H <-
                ggplot(
                  Community_MF_average_to_plot,
                  aes(y = value, x = mean_MF_diff, color = both_best)) +
                facet_wrap(region ~ variable, scales = "free_y") +
                geom_hline(aes(yintercept = 0), lwd = 0.3) +
                geom_vline(aes(xintercept = 0), lwd = 0.3) +
                geom_point(
                  size = 0.7,
                  alpha = 0.5,
                  #color = "lightskyblue3",
                  fill = "lightskyblue3"
                ) +
                theme_bw() + # guides(#fill = guide_legend(override.aes = list(shape = 21), col = 4),
                #  shape = guide_legend(position = "none", col = 4)) +
                # Change color scale, add predefined scenarios
                new_scale_color() +
                new_scale_fill() +
                
                scale_color_manual(values = c(palette_test, palette_test, palette_test, palette_test)) + #  +
                scale_fill_manual(values  = c(palette_test, rep("white", 10), rep("grey", 10), rep("black", 10))) +
                scale_shape_manual(values = rep(c(21, 23), 40)) +

                # scale_shape_manual(values = rep(c(21, 16), each = 7)) + #rep("White", 10)))) +
                geom_point(
                  data = Community_MF_average_by_predef,
                  aes(
                    color = Scenario_description,
                    shape = Scenario_description,
                    fill = Scenario_description
                  ),
                  size = 3,
                  lwd = 0.5,
                  stroke = 1.5
                ) +
                ylab("Equity") +
                xlab("Multifunctionality difference compared to current landscape") +
                theme(
                  legend.position = "bottom",
                  legend.title = element_blank()
                ) 
                

              ggsave(
                plot = big_plot_MF_H,
                file = paste( "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/big_plot_MF_H", env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup", big_groups, "_forest", forest_class, "_constrained", crop_constrained, ".pdf", sep = ""
                ),
                width = 15, height = ifelse(by_region == TRUE, 15, 5)
              )


              ### Find best landscapes and their composition ####
              ### Baseline check

              Community_MF_average[, c( "Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged"
              ) :=
                tstrsplit(concatenate, "_")]
              Community_MF_average[, c( "Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged"
              ) := lapply(.SD, function(x){as.numeric(x)}), .SDcols = c( "Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged"
              )]
              
              no_plots = Community_MF_average[, unique(Crop +
                                              Grassland_low +
                                              Grassland_medium +
                                              Grassland_high +
                                              Forest_Deciduous +
                                              Forest_Mixed +
                                              Forest_Coniferous +
                                              Forest_even_aged +
                                              Forest_uneven_aged) ]

              Baseline_composition <- Community_MF_average[Scenario_description == "Baseline", lapply(.SD, mean),
                by = region,
                .SDcols = c( "mean_MF", "mean_harmony", "Score", "Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged"
                )
              ]

              Baseline_composition_melt <- melt(
                Baseline_composition,
                id.vars = c("region"),
                value.name = "baseline"
              )

              if (by_region == TRUE) {
                Baseline_composition_melt[variable == "Grassland_low", baseline := c(13.12, 3.38, 6.15) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Grassland_med", baseline := c(10.88, 4.55, 5.07) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Grassland_high", baseline := c(8, 5.07, 4.65) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_Deciduous", baseline := c(39.36, 23.49, 19) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_Mixed", baseline := c(1.92, 1.08, 5) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_Coniferous", baseline := c(6.72, 2.43, 26) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_even_aged", baseline := c(26.8, 15.39, 44.5) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_uneven_aged", baseline := c(21.12, 11.6, 5.5) *
                  no_plots / 100]
              }

              if (by_region == FALSE) {
                Baseline_composition_melt[variable == "Grassland_low", baseline := c(7.55) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Grassland_med", baseline := c(6.54) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Grassland_high", baseline := c(5.9) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_Deciduous", baseline := c(27.28) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_Mixed", baseline := c(2.66) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_Coniferous", baseline := c(11.72) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_even_aged", baseline := c(28.92) *
                  no_plots / 100]
                Baseline_composition_melt[variable == "Forest_uneven_aged", baseline := c(12.7) *
                  no_plots / 100]
              }

# Plot landscape Baseline
              if (forest_class == "Type") {
                Baseline_composition_melt[variable %in% c('Forest_even_aged','Forest_uneven_aged'), baseline:=0]
                }
              if (forest_class == "Age") {
                Baseline_composition_melt[variable %in% c('Forest_Coniferous','Forest_Mixed', 'Forest_Deciduous'), baseline:=0]}
              
              plot_landscape_baseline <- dcast(Baseline_composition_melt, region~variable)[, list(
                Grassland = (Grassland_low + Grassland_medium + Grassland_high) / (Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                Crop = (Crop) / (   Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
                Forest = (   Forest_Deciduous + Forest_Mixed + Forest_Coniferous ) / (   Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
                Grassland_low = (Grassland_low) / (   Grassland_low + Grassland_medium + Grassland_high ),
                Grassland_med = (Grassland_medium) / (   Grassland_low + Grassland_medium + Grassland_high ),
                Grassland_high = (Grassland_high) / (   Grassland_low + Grassland_medium + Grassland_high ),
                Forest_even_aged = (Forest_even_aged) / (   Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
                Forest_uneven_aged = (Forest_uneven_aged) / (    Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                Forest_Deciduous = (Forest_Deciduous) / (    Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                Forest_Mixed = (Forest_Mixed) / (    Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                Forest_Coniferous = (Forest_Coniferous) / (    Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  )
              ),
              by = region
              ]
              
              
              plot_landscape_data_baseline <-plot_landscape_baseline[, list(
                region = region,
                crop_top = Crop,
                forest_top = Crop + Forest,
                forest_decid_right = Forest_Deciduous,
                forest_mixed_right = Forest_Deciduous + Forest_Mixed,
                forest_even_aged_right = Forest_even_aged,
                grass_low_right = Grassland_low,
                grass_med_right = Grassland_low + Grassland_med
              )]
              
              if (forest_class == "Type") {
                landscape_box_baseline <- ggplot(data = plot_landscape_data_baseline) +
                  facet_wrap(~region) +
                  theme_void() +
                  # Crop
                  geom_rect(
                    fill = "#f48b30",
                    aes(  xmin = 0,  xmax = 1,  ymin = 0,  ymax = crop_top)
                  ) +
                  # Forests
                  geom_rect(
                    fill = "#c3d640",
                    aes(  xmin = 0,  xmax = forest_decid_right,  ymin = crop_top,  ymax = forest_top)
                  ) +
                  geom_rect(
                    fill = "#9cc947",
                    aes(   xmin = forest_decid_right,   xmax = forest_mixed_right,   ymin = crop_top,   ymax = forest_top )
                  ) +
                  geom_rect(
                    fill = "#6c8b40",
                    aes(    xmin = forest_mixed_right,    xmax = 1,    ymin = crop_top,    ymax = forest_top  )
                  ) +
                  # Grasslands
                  geom_rect(
                    fill = "#8ccde8",
                    aes(    xmin = 0,    xmax = grass_low_right,    ymin = forest_top,    ymax = 1  )
                  ) +
                  geom_rect(
                    fill = "#70a6ca",
                    aes(   xmin = grass_low_right,   xmax = grass_med_right,   ymin = forest_top,   ymax = 1 )
                  ) +
                  geom_rect(
                    fill = "#4d708a",
                    aes( xmin = grass_med_right, xmax = 1, ymin = forest_top, ymax = 1))
             }
              if (forest_class == "Age") {
                landscape_box_baseline <- ggplot(data = plot_landscape_data_baseline) +
                  facet_wrap(~region) +
                  theme_void() +
                  # Crop
                  geom_rect(
                    fill = "#f48b30",
                    aes( xmin = 0, xmax = 1, ymin = 0, ymax = crop_top )
                  ) +
                 geom_rect(
                    fill = "#ABE188",
                    aes(   xmin = 0,   xmax = forest_even_aged_right,   ymin = crop_top,   ymax = forest_top )
                  ) +
                  geom_rect(
                    fill = "#00A375",
                    aes(   xmin = forest_even_aged_right,   xmax = 1,   ymin = crop_top,   ymax = forest_top )
                  ) +
                  # Grasslands
                  geom_rect(
                    fill = "#8ccde8",
                    aes(  xmin = 0,  xmax = grass_low_right,  ymin = forest_top,  ymax = 1)
                  ) +
                  geom_rect(
                    fill = "#70a6ca",
                    aes(  xmin = grass_low_right,  xmax = grass_med_right,  ymin = plot_landscape$Crop + plot_landscape$Forest,  ymax = 1)
                  ) +
                  geom_rect(
                    fill = "#4d708a",
                    aes(  xmin = grass_med_right,  xmax = 1,  ymin = plot_landscape$Crop + plot_landscape$Forest,  ymax = 1))
              }
              ggsave(
                plot = landscape_box_baseline,
                file = paste(
                  "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/landscape_baseline",
                  "-by_region", by_region, "_forest", forest_class, "_constrained", crop_constrained, ".pdf", sep = ""
                ),
                width = ifelse(by_region == TRUE, 20, 6.5)
              )

              
              #### Plot all plots based on criteria
              
              for (criteria in c( "no_loser", "good_both",'best_no_loser_no_threat','best_natasso', 'best_diversity',
                                  #"best_no_loser", 
                                  "no_loser_no_threat", "deforestation", #"the_best", 
                                  #"the_best_no_loser",
                                  'both_best'
              )) {
                dir.create(paste( "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/", criteria, sep = ''))
                ### Plot: change in new landscape
                new_scenarios <- Community_MF_average[get(criteria) == 1, list(scenario = unique(scenario), 'keep_scenario' = TRUE), by = 'region']

                if (nrow(new_scenarios) > 0) {
               
                  
                  Community_services_full2 = merge.data.table(Community_services_full, new_scenarios, by = c('region','scenario'), all = T)
                  Community_services_full2[, value_relative := value_diff/value_baseline*100]
                  
                  plot_optimum_change <- Community_services_full2[keep_scenario == TRUE, list(
                                           value_relative = mean(value_relative)), by = c("scenario","variable", "region" )][ ,
                                           as.list(CI(value_relative, ci = 0.95)), 
                                           by = c("variable", "region" )] # calculate.ci(value_diff/value_baseline*100)
                  plot_optimum_change_abs <- Community_services_full2[keep_scenario == TRUE, list(
                    value = mean(value)), by = c("scenario","variable", "region" )][ ,
                                                                                             as.list(CI(value, ci = 0.95)), 
                                                                                                       by = c("variable", "region" )] # calculate.ci(value_diff/value_baseline*100)
                  plot_optimum_change = merge(plot_optimum_change, plot_optimum_change_abs[, list( variable, region, 'upper_abs' = upper,'mean_abs' = mean, 'lower_abs' = lower)], all = T)
                  plot_optimum_change = merge(plot_optimum_change,
                                              Community_services_full2[keep_scenario == TRUE, list(
                                                value_baseline = mean(value_baseline)), by = c("variable", "region" )],
                                              all = T
                                              )
                  plot_optimum_change[, is.MF := grepl('__MF', variable, perl = T)]
                  
                  plot_optimum_change[, variable := factor(
                    variable,
                    levels = c( "Ric", "Aesthetic", "Reg_id", "Leisure", "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting", "C_stock",
 "Nat_cons_asso__MF", "Research__MF", "Econ__MF", "Reg_dev_prog__MF", "Tourism__MF", "Locals__MF", "Press__MF", "Loc_her_asso__MF", "Policy_admin__MF",
 "Hunting__MF", "Landowner__MF", "Forestry__MF", "Quarrying__MF", "Agric__MF"
                    )
                  )]
                  stakeholder_palette <- c( "#F4882A", "#F69E51", "#4C622D", "#6C8B40", "#82A84D", "#258EBB", "#33A5D7", "#55B4DD", "#77C3E4", "#88CBE7", "#6E7B8B", "#818D9C", "#98A2AE", "#BAC1C9"
                  )

                  plot_optimum_change[, group := factor(
                    gsub("__MF", "", variable),
                    levels = c( "Quarrying", "Agric", "Hunting", "Forestry", "Landowner", "Tourism", "Loc_her_asso", "Policy_admin", "Press", "Locals", "Econ", "Nat_cons_asso", "Research", "Reg_dev_prog"
                    )
                  )]
                  plot_optimum_change <- plot_optimum_change[order(group, decreasing = T), ]
                  
                  gg_optimum_MF <- ggplot(
                    plot_optimum_change[grepl("MF", variable), ],
                    aes(  mean,  ymin = lower,  ymax = upper,  x = group,  fill = group)
                  ) +
                    geom_col() + facet_wrap(~region) + geom_errorbar() + theme_bw() +
                    ylab("Change in multifunctionality (%)") +
                    xlab("Groups") +
                    coord_flip() +
                    xlab("") +
                    scale_fill_manual(    breaks = levels(plot_optimum_change$group),    values = stakeholder_palette  ) +
                    theme(legend.position = "none")
                  
                  
                  gg_optimum_MF_abs <- ggplot(
                    plot_optimum_change[grepl("MF", variable), ],
                    aes(  mean_abs,  ymin = lower_abs,  ymax = upper_abs,  x = group,  fill = group)
                  ) + geom_col() + facet_wrap(~region) + geom_errorbar() + theme_bw() +
                    geom_point(aes(y = value_baseline, x = group), color = 'black') +
                    ylab("Total multifunctionality") +
                    xlab("Groups") +
                    coord_flip() +
                    xlab("") +
                    scale_fill_manual(    breaks = levels(plot_optimum_change$group),    values = stakeholder_palette  ) +
                    theme(legend.position = "none")
                  
                  gg_optimum_services <- ggplot(
                    plot_optimum_change[!grepl("MF", variable), ],
                    aes(  mean,  ymin = lower,  ymax = upper,  x = variable,  fill = variable)
                  ) +
                    facet_wrap(~region) + geom_col() + geom_errorbar() + theme_bw() + ylab("Change in service supply (%)") +
                    xlab("Services") + coord_flip() + xlab("") + scale_fill_manual(values = my_palette_services) +
                    theme(legend.position = "none")
                  
                  ggsave(  plot = gg_optimum_MF_abs,  file = paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/", criteria, "/gg_optimum_MF_abs", env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup", big_groups, "_forest", forest_class, "_", "constrained", crop_constrained, ".pdf", sep = ""  ),  width = ifelse(by_region == TRUE, 15, 4),  height = 5)
                  ggsave(  plot = gg_optimum_MF,  file = paste( "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/", criteria, "/gg_optimum_MF", env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup", big_groups, "_forest", forest_class, "_", "constrained", crop_constrained, ".pdf", sep = ""  ),  width = ifelse(by_region == TRUE, 15, 4),  height = 5)
                  ggsave(  plot = gg_optimum_services,  file = paste( "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/", criteria, "/gg_optimum_services", env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, "-biggroup", big_groups, "_forest", forest_class, "_", "constrained", crop_constrained, ".pdf", sep = ""  ),  width = ifelse(by_region == TRUE, 15, 4),  height = 5
                  )



                  ## Calculate average for scenarios fitting the criteria
                  Community_MF_average[, c("mean_MF","mean_harmony","Score","Crop","Grassland_low","Grassland_medium","Grassland_high","Forest_Deciduous","Forest_Mixed","Forest_Coniferous","Forest_even_aged","Forest_uneven_aged" ) :=
                                        lapply(.SD, function(x){as.numeric(x)}),
                                      .SDcols = c( "mean_MF", "mean_harmony", "Score", "Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged")
                  ]
                  Average <- Community_MF_average[get(criteria) == 1, lapply(.SD, mean),
                   by = region,
                   .SDcols = c( "mean_MF", "mean_harmony", "Score", "Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged"
                   )
                  ]
                  Average[, c("Criteria", "Description") := list(criteria, "Average")]
                  
                  plot_landscape <- Average[, list(
                    Grassland = (    Grassland_low + Grassland_medium + Grassland_high  ) / (    Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                    Crop = (Crop) / (   Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
                    Forest = (   Forest_Deciduous + Forest_Mixed + Forest_Coniferous ) / (   Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
                    Grassland_low = (Grassland_low) / (   Grassland_low + Grassland_medium + Grassland_high ),
                    Grassland_med = (Grassland_medium) / (   Grassland_low + Grassland_medium + Grassland_high ),
                    Grassland_high = (Grassland_high) / (   Grassland_low + Grassland_medium + Grassland_high ),
                    Forest_even_aged = (Forest_even_aged) / (   Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
                    Forest_uneven_aged = (Forest_uneven_aged) / (    Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                    Forest_Deciduous = (Forest_Deciduous) / (    Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                    Forest_Mixed = (Forest_Mixed) / (    Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                    Forest_Coniferous = (Forest_Coniferous) / (    Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  )
                  ),
                  by = region
                  ]


                  plot_landscape_data <- plot_landscape[, list(
                    region = region,
                    crop_top = Crop,
                    forest_top = Crop + Forest,
                    forest_decid_right = Forest_Deciduous,
                    forest_mixed_right = Forest_Deciduous +Forest_Mixed,
                    forest_even_aged_right = Forest_even_aged,
                    grass_low_right = Grassland_low,
                    grass_med_right = Grassland_low + Grassland_med
                  )
                  ]

                  if (forest_class == "Type") {
                    landscape_box <- ggplot(data = plot_landscape_data) +
                      facet_wrap(~region) +
                     theme_void() +
                      # Crop
                      geom_rect(
                        fill = "#f48b30",
                        aes(  xmin = 0,  xmax = 1,  ymin = 0,  ymax = crop_top)
                      ) +
                      # geom_emoji(aes(x= 0.5, y = plot_landscape$Crop/2), emoji = '1f33d') +
                      # Forests
                      geom_rect(
                        fill = "#c3d640",
                        aes(  xmin = 0,  xmax = forest_decid_right,  ymin = crop_top,  ymax = forest_top)
                      ) +
                      # geom_emoji(aes(x=  plot_landscape$Forest_Deciduous/2, y = plot_landscape$Crop +  plot_landscape$Forest/2), emoji = '1f333') +
                      geom_rect(
                        fill = "#9cc947",
                        aes(   xmin = forest_decid_right,   xmax = forest_mixed_right,   ymin = crop_top,   ymax = forest_top )
                      ) +
                      #  geom_emoji(aes(x= plot_landscape$Forest_Deciduous + plot_landscape$Forest_Mixed/2 , y = plot_landscape$Crop +  plot_landscape$Forest/3), emoji = '1f333') +
                      #  geom_emoji(aes(x= plot_landscape$Forest_Deciduous + plot_landscape$Forest_Mixed/2 , y = plot_landscape$Crop +  2*plot_landscape$Forest/3), emoji = '1f332') +
                      geom_rect(
                        fill = "#6c8b40",
                        aes(    xmin = forest_mixed_right,    xmax = 1,    ymin = crop_top,    ymax = forest_top  )
                      ) +
                      #   geom_emoji(aes(x= plot_landscape$Forest_Deciduous + plot_landscape$Forest_Mixed + plot_landscape$Forest_Coniferous/2, y = plot_landscape$Crop +  plot_landscape$Forest/2), emoji = '1f332') +

                      # Grasslands
                      geom_rect(
                        fill = "#8ccde8",
                        aes(    xmin = 0,    xmax = grass_low_right,    ymin = forest_top,    ymax = 1  )
                      ) +
                      geom_rect(
                        fill = "#70a6ca",
                        aes(   xmin = grass_low_right,   xmax = grass_med_right,   ymin = forest_top,   ymax = 1 )
                      ) +
                      geom_rect(
                        fill = "#4d708a",
                        aes( xmin = grass_med_right, xmax = 1, ymin = forest_top, ymax = 1)
                      ) # +
                    #   geom_emoji(aes(x = plot_landscape$Grassland_low/2, y = plot_landscape$Crop + plot_landscape$Forest + plot_landscape$Grassland/2), emoji = '1f3f5') +
                    #   geom_emoji(aes(x= plot_landscape$Grassland_low + plot_landscape$Grassland_med/2, y =  plot_landscape$Crop + plot_landscape$Forest + plot_landscape$Grassland/2), emoji = '1f3f5') +
                    #   geom_emoji(aes(x= plot_landscape$Grassland_low + plot_landscape$Grassland_med + plot_landscape$Grassland_high/2, y =  plot_landscape$Crop + plot_landscape$Forest + plot_landscape$Grassland/2), emoji = '1f69c')
                  }
                  if (forest_class == "Age") {
                    landscape_box <- ggplot(data = plot_landscape_data) +
                      facet_wrap(~region) +
                      theme_void() +
                      # Crop
                      geom_rect(
                        fill = "#f48b30",
                        aes( xmin = 0, xmax = 1, ymin = 0, ymax = crop_top )
                      ) +
                      # geom_emoji(aes(x= 0.5, y = plot_landscape$Crop/2), emoji = '1f33d') +
                      # Forests
                      # geom_rect(fill = '#c3d640', aes(xmin = 0, xmax = forest_decid_right, ymin = crop_top, ymax = forest_top) )+
                      # geom_emoji(aes(x=  plot_landscape$Forest_Deciduous/2, y = plot_landscape$Crop +  plot_landscape$Forest/2), emoji = '1f333') +
                      #  geom_rect(fill = '#9cc947', aes(xmin = forest_decid_right,  xmax =forest_mixed_right, ymin = crop_top, ymax = forest_top)) +
                      #   geom_emoji(aes(x= plot_landscape$Forest_Deciduous + plot_landscape$Forest_Mixed/2 , y = plot_landscape$Crop +  plot_landscape$Forest/3), emoji = '1f333') +
                      #  geom_emoji(aes(x= plot_landscape$Forest_Deciduous + plot_landscape$Forest_Mixed/2 , y = plot_landscape$Crop +  2*plot_landscape$Forest/3), emoji = '1f332') +
                      # geom_rect(fill = '#6c8b40', aes(xmin = forest_mixed_right, xmax = 1, ymin = crop_top, ymax = forest_top)) +
                      #   geom_emoji(aes(x= plot_landscape$Forest_Deciduous + plot_landscape$Forest_Mixed + plot_landscape$Forest_Coniferous/2, y = plot_landscape$Crop +  plot_landscape$Forest/2), emoji = '1f332') +
                      geom_rect(
                        fill = "#ABE188",
                        aes(   xmin = 0,   xmax = forest_even_aged_right,   ymin = crop_top,   ymax = forest_top )
                      ) +
                      geom_rect(
                        fill = "#00A375",
                        aes(   xmin = forest_even_aged_right,   xmax = 1,   ymin = crop_top,   ymax = forest_top )
                      ) +

                      # Grasslands
                      geom_rect(
                        fill = "#8ccde8",
                        aes(  xmin = 0,  xmax = grass_low_right,  ymin = forest_top,  ymax = 1)
                      ) +
                      geom_rect(
                        fill = "#70a6ca",
                        aes(  xmin = grass_low_right,  xmax = grass_med_right,  ymin = plot_landscape$Crop + plot_landscape$Forest,  ymax = 1)
                      ) +
                      geom_rect(
                        fill = "#4d708a",
                        aes(  xmin = grass_med_right,  xmax = 1,  ymin = plot_landscape$Crop + plot_landscape$Forest,  ymax = 1)
                      ) # +
                    #   geom_emoji(aes(x = plot_landscape$Grassland_low/2, y = plot_landscape$Crop + plot_landscape$Forest + plot_landscape$Grassland/2), emoji = '1f3f5') +
                    #   geom_emoji(aes(x= plot_landscape$Grassland_low + plot_landscape$Grassland_med/2, y =  plot_landscape$Crop + plot_landscape$Forest + plot_landscape$Grassland/2), emoji = '1f3f5') +
                    #   geom_emoji(aes(x= plot_landscape$Grassland_low + plot_landscape$Grassland_med + plot_landscape$Grassland_high/2, y =  plot_landscape$Crop + plot_landscape$Forest + plot_landscape$Grassland/2), emoji = '1f69c')
                  }
                  ggsave(
                    plot = landscape_box,
                    file = 
                      paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/", criteria, "/landscape_box",
                      env_corr, "-by_region", by_region, "scale_within", scale_within_land_use, "-SB", use_SB, "-weighted", weighted, 
                      "-biggroup", big_groups, "_forest", forest_class, "_", criteria, "_constrained", crop_constrained, ".pdf", sep = ""
                    ),
                    width = ifelse(by_region == TRUE, 20, 6.5)
                  )


                  ## Calculate confidence intervals for scenarios fitting the criteria
                  # The data is strongly not-normal so we need bootstrap confidence intervals
                  mean.fun <-
                    function(data, idx) {
                      df <- data[idx]
                      mean(df, na.rm = TRUE)
                    }
                  calculate.ci <- function(x) {
                    x <- x[is.finite(x)]
                    if (length(x[!is.na(x)]) < 3) {
                      upper.ci <- NA
                      ci <- -1
                    }
                    if (length(x[!is.na(x)]) >= 3) {
                      upper.ci <- try(boot.ci(boot(x, mean.fun, R = length(x) * 2), type = "basic")$basic[5])
                      if (class(upper.ci) == "try-error") {
                        ci <- -1 # ; #print('ERRORRR')
                      } else {
                        ci <- upper.ci - mean(x, na.rm = T)
                      }
                    }
                    return(ci)
                  }

                  Confidence_intervals <- Community_MF_average[get(criteria) == 1, lapply(.SD, calculate.ci),
                    by = region,
                    .SDcols = c("mean_MF","mean_harmony","Score","Crop","Grassland_low","Grassland_medium","Grassland_high","Forest_Deciduous","Forest_Mixed","Forest_Coniferous","Forest_even_aged","Forest_uneven_aged"
                    )
                  ]
                  Confidence_intervals <- Confidence_intervals[, lapply(.SD, function(x) {
                    y <- x
                    y[y == -1] <-
                      NA
                    return(y)
                  }), .SDcols = c( "region", "mean_MF", "mean_harmony", "Score", "Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged"
                  )]
                  Confidence_intervals[, c("Criteria", "Description") := list(criteria, "CI")]


                  Best_landscape_all_melt <- melt.data.table(
                    rbind(Average, Confidence_intervals),
                    id.vars = c("region", "Description")
                  )

                  if (nrow(Best_landscape_all_melt) > 0) {
                    Best_landscape_all_cast <- dcast.data.table(
                      Best_landscape_all_melt,
                      region + variable ~ Description,
                      value.var = "value"
                    )
                    Best_landscape_plus_baseline <- merge(
                      Baseline_composition_melt,
                      Best_landscape_all_cast,
                      by = c("region", "variable")
                    )
                    Best_landscape_plus_baseline[, c("Average", "CI") := lapply(.SD, as.numeric), .SDcols = c("Average", "CI")]
                    Best_landscape_plus_baseline[, Average_diff := Average - baseline]

                    if (forest_class == "Age") {
                      Best_landscape_plus_baseline <- Best_landscape_plus_baseline[!(
                        variable %in% c(
                          "Forest_Deciduous",
                          "Forest_Coniferous",
                          "Forest_Mixed"
                        )
                      ), ]
                    }
                    if (forest_class == "Type") {
                      Best_landscape_plus_baseline <- Best_landscape_plus_baseline[!(variable %in% c(
                        "Forest_even_aged",
                        "Forest_uneven_aged"
                      )), ]
                    }


                    Best_landscape_plus_baseline$variable <- droplevels(Best_landscape_plus_baseline$variable)
                    # Plot differences in landscape composition
                    gg_diff_LU <-
                      ggplot(
                        Best_landscape_plus_baseline[variable %in% c("Crop","Grassland_low","Grassland_medium","Grassland_high","Forest_Deciduous","Forest_Mixed","Forest_Coniferous","Forest_even_aged","Forest_uneven_aged"
                        ), ],
                        aes(
                          Average_diff / no_plots * 100,
                          x = variable,
                          fill = variable,
                          ymin = Average_diff / no_plots * 100 - CI / no_plots * 100,
                          ymax = Average_diff / no_plots * 100 + CI / no_plots * 100
                        )
                      ) +
                      facet_grid(. ~ region) + # facet_grid(Metric ~ region + Forest_age_scenario) +
                      ylab("Difference compare to the baseline") +
                      geom_col() +
                      scale_fill_manual(
                        name = "LU type",
                        values = c("darkorange","skyblue","skyblue3","skyblue4","olivedrab1","olivedrab3","olivedrab4","#ABE188","#00A375"
                        ),
                        breaks =
                          c( "Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged"
                          )
                      ) +
                      geom_errorbar() +
                      theme_bw() +
                      xlab("")

                    ggsave(
                      plot = gg_diff_LU,
                      file = paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/", criteria, "/gg_diff_LU",env_corr,"-by_region",by_region,"scale_within",scale_within_land_use,"-SB",use_SB,"-weighted",weighted,"-biggroup",big_groups,"_forest",forest_class,"_",criteria,"_constrained",crop_constrained,".pdf",sep = ""
                      ),
                      width = ifelse(by_region == TRUE, 16,10)
                    )

                    # Export best landscape composition to rerun whole analysis
                    mean_export <- Average[, .SD, .SDcols = c( "region", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged", "Crop"
                    )][, Scenario_description := paste( "Scenario_post_", env_corr, "_by_region", by_region, "scale_within", scale_within_land_use, "_SB", use_SB, "-weighted", weighted, "_biggroup", big_groups, "_forest", forest_class, "_", criteria, "_constrained", crop_constrained, sep = ""
                    )]
                    # print('n')
                    mean_no <- Community_MF_average[get(criteria) == 1, .N, by = region]
                    # print('o')
                    mean_export <- merge(mean_export, mean_no)[, Forest_classification := forest_class]
                    # print('p')
                    write.csv(
                      mean_export,
                        paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/", criteria, "/Scenario_post_",
                        env_corr, "by_region", by_region, "scale_within", scale_within_land_use, "_SB", use_SB, "-weighted", weighted, "_biggroup",
                        big_groups, "_forest", forest_class, "_", criteria, "_constrained", crop_constrained, ".csv", sep = ""
                      )
                    )
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
