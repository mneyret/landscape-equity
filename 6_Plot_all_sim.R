# -------------------------------------------------------------------------------------------
# This is part of the work used for the publication Neyret et al. Landscape management for multifunctionality and Equity. In revision for Nature Sustainability.
# by Margot Neyret

# In this script, we create the landscapes that will be used for the simulations

# Input: Calculated service, multifunctionality and equity tables for all landscape scenarios
# Output: Figures
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
library(ggcorrplot)
library(ggpubr)

setwd('~Landscape_composition')

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

for (crop_constrained in c(TRUE, FALSE
                           )) {
  for (by_region in c(#TRUE,
                    FALSE  
                    )) {
      # * --- Loop on environment correction ####
      for (env_corr in c("env_corr", ""
                         )) {
        # * --- Loop on SB ####
        for (use_SB in c(FALSE, TRUE
                         )) {

          #  *--- Loop on Forest classification ####
          for (forest_class in c("Type", "Age" 
                                 )) {
            #  *--- Loop on power weighting ####
            for (weighted in c(FALSE, TRUE 
                               )) {
              
              com_file <-
                paste( "Temporary_data/Community_average", 
                       env_corr, "-by_region", by_region,  "-SB", use_SB, "-weighted", weighted,
                       "_forest", forest_class, "_constrained", crop_constrained, ".csv", sep = ""
                )
              
              service_file <-
                paste( "Temporary_data/Community_services", 
                       env_corr, "-by_region", by_region,  "-SB", use_SB, "-weighted", weighted, 
                       "_forest", forest_class, "_constrained", crop_constrained, ".csv", sep = ""
                )
              
              service_file_full = paste(
                "Temporary_data/Community_services_full",
                env_corr, "-by_region", by_region, "-SB", use_SB, 
                "_forest", forest_class, "_constrained", crop_constrained, ".csv", sep = ""
              )
         
              if (file.exists(com_file)) {
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
              
              
              Community_MF_average[, equity_diff := - mean_gini_diff]
              Community_MF_average[, mean_equity := - mean_gini]
              Community_MF_average[, n_losers := - n_losers]
              
              # Decide maximum number of plots included in the best scenarios
              n_max = 15
              
              
              # Identification of focus scenarios: deforestation, good scenarios, best for biodiversity, best for conservation asso
              Community_MF_average[, c("good_MF", "good_H", "good_both") := list(
                mean_MF_diff > 0,
                equity_diff  >  0,
                mean_MF_diff > 0 & equity_diff > 0
              )]
              Community_MF_average[, no_loser := (n_losers == 0), by = c("scenario", "region")]
              Community_MF_average[Scenario_description == "Baseline", no_loser := FALSE]
              
              # Test of best MF + equity
              Community_MF_average[, MfEqScore := sqrt(mean_MF_diff) + sqrt(equity_diff)]
              Community_MF_average[good_both == TRUE, order_both := rank(-MfEqScore), by = region]
              Community_MF_average[good_both == TRUE & order_both <= n_max, both_best := TRUE]
              

              # No losers, no loss of vulnerable services
              Community_MF_average[, no_loser_no_threat := (Score01 == 0 & no_loser == TRUE
                ), by = c("scenario", "region")]
              
              # Best among no losers, no loss of vulnerable services
              Community_MF_average[no_loser_no_threat == TRUE, order_no_losers := rank(-mean_MF), by = c("region")]
              Community_MF_average[no_loser_no_threat == TRUE & order_no_losers <= n_max, best_no_loser_no_threat := TRUE, by = c("region")]
              
              # Deforestation scenarios
              max_defo <- Community_MF_average[Scenario_description == "Deforestation", ]
              Community_MF_average[Scenario_description == "Deforestation", deforestation := 1]

              # Highest biodiversity scenarios
              Richness_average = Service_average[variable == 'Ric', ]
              Richness_average[, order_div := (rank(-MEAN)), by = c("region")]
              Community_MF_average[scenario %in% Richness_average[order_div <= n_max, scenario], 
                                   best_diversity := TRUE, by = c("region")]
              
              Cstock_average = Service_average[variable == 'C_stock', ]
              Cstock_average[, order_div := (rank(-MEAN)), by = c("region")]
              Community_MF_average[scenario %in% Cstock_average[order_div <= 15, scenario], 
                                   best_Cstock := TRUE, by = c("region")]
              
              # Best for nat conservation asso
              NatAsso_average = Service_average[variable == 'Nat_cons_asso__MF', ]
              NatAsso_average[, order_div := (rank(-MEAN)), by = c("region")]
              Community_MF_average[scenario %in% NatAsso_average[order_div <= 10, scenario], 
                                   best_natasso := TRUE, by = c("region")]
              
              Community_MF_average_to_plot <- melt.data.table(
                Community_MF_average[, list(
                  Scenario_description,
                  mean_MF_diff,
                  equity_diff,
                  region,
                  n_losers,
                 # Score,
                  Score01,
                  both_best,
                  best_diversity,
                  best_Cstock,
                  best_no_loser_no_threat
                )],
                id.var = c("Scenario_description", "mean_MF_diff", "region", 'both_best','best_Cstock', 'best_diversity', 'best_no_loser_no_threat')
              )

              ## Extract pre-defined scenarios
              Community_MF_average_by_predef <-
                melt.data.table(
                  Community_MF_average[!is.na(Scenario_description) &
                    Scenario_description != "Baseline" &
                    !grepl("Scenario_post", Scenario_description),
                  lapply(.SD, mean),
                  .SDcols = c(
                    "mean_MF_diff",
                    "equity_diff",
                    "n_losers",
                  #  "Score",
                    "Score01"
                  ),
                  by = c("region", "Scenario_description")
                  ],
                  id.var = c("Scenario_description", "mean_MF_diff", "region")
                )



                Community_MF_average_by_predef[, Scenario_description := dplyr::recode(Scenario_description,
                  "Deforestation"                                               =  "1. Deforestation: 50% less forests",
                  "All forests are coniferous"                                  =  "2. All forests are coniferous",
                  "All forests are deciduous"                                   =  "3. All forests are deciduous",
                  "All forests are mixed"                                       =  "4. All forests are mixed",
                  "All grasslands are high int"                                 =  "5. All grasslands are high intensity",
                  "50% more high int grasslands"                                =  "6. 50% more high intensity grasslands",
                  "All mixed forests and low-intensity grasslands (same crops)" =  "7. All forests are mixed forests and all grasslands are low-intensity",
                  "Reforestation"                                               =  "8. Reforestation: 50% more forests",
                  "Same prop of each forest type"                               =  "9. Equal proportions of mixed, coniferous and deciduous forests",
                  "50% more low int grasslands"                                 =  "10. 50% more low intensity grasslands",
                  "Low-lui and med-lui grasslands become coniferous forests"    =  "11. Low- and medium-intensity grasslands become coniferous forests",
                  "All grasslands are low int"                                  =  "12. All grasslands are low intensity",
                  "Only coniferous forests and only high LUI grasslands"        =  "13. All forests are coniferous and all grasslands are high intensity",


                  "Add 50% grasslands"                                          =  "14. 50% more grasslands",
                  "33% each"                                                    =  "15. Equal proportions of grasslands, crops and forests",
                  "All forests are even-aged"                                   =  "16. All forests are even-aged",
                  "All forests are uneven-aged"                                 =  "19. All forests are uneven-aged",
                  "All forests are uneven-aged and grasslands low-intensity"    =  "17. All forests are uneven-aged and grasslands low-intensity",
                  "Low-lui and med-lui grasslands become uneven-aged forests"   =  "18. Low-lui and med-lui grasslands become uneven-aged forests",
                  "Low-lui and med-lui grasslands become mixed forests"         =  "19. Low- and medium-intensity grasslands become mixed forests"
                )]
                
                Community_MF_average_by_predef[, Scenario_description := factor(Scenario_description, levels = c(
                  "1. Deforestation: 50% less forests",
                  "2. All forests are coniferous",
                  "3. All forests are deciduous",
                  "4. All forests are mixed",
                  "5. All grasslands are high intensity",
                  "6. 50% more high intensity grasslands",
                  "7. All forests are mixed forests and all grasslands are low-intensity",
                  "8. Reforestation: 50% more forests",
                  "9. Equal proportions of mixed, coniferous and deciduous forests",
                  "10. 50% more low intensity grasslands",
                  "11. Low- and medium-intensity grasslands become coniferous forests",
                  "12. All grasslands are low intensity",
                  "13. All forests are coniferous and all grasslands are high intensity",
                  "14. 50% more grasslands",
                  "15. Equal proportions of grasslands, crops and forests",
                  "16. All forests are even-aged",
                  "19. All forests are uneven-aged",
                  "17. All forests are uneven-aged and grasslands low-intensity",
                  "18. Low-lui and med-lui grasslands become uneven-aged forests",
                  "19. Low- and medium-intensity grasslands become mixed forests"
                ))]

              Community_MF_average_by_predef <- Community_MF_average_by_predef[order(Scenario_description), ]

              palette_test <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628','#f781bf' )
              

              for (R in unique(Community_MF_average_to_plot$region)){
                min_MF = Community_MF_average_to_plot[(!is.na(both_best) | !is.na(best_diversity) | 
                                                      !is.na(best_no_loser_no_threat) |!is.na(Scenario_description)) & region == R,
                  min(mean_MF_diff)
                ]
                
                plot_big_plot = function(Community_MF_average_to_plot, Community_MF_average_by_predef){
                  
                hull_both_best <- Community_MF_average_to_plot[region == R & both_best == TRUE, .SD[chull(value, mean_MF_diff),], by = 'variable']
                hull_diversity <- Community_MF_average_to_plot[region == R & best_diversity == TRUE, .SD[chull(value, mean_MF_diff),], by = 'variable']
                hull_noloss <- Community_MF_average_to_plot[region == R & best_no_loser_no_threat == TRUE, .SD[chull(value, mean_MF_diff),], by = 'variable']
                
                big_plot_MF_H = ggplot(
                  Community_MF_average_to_plot[region == R,],
                  aes(y = value, x = mean_MF_diff)) 
                 if (nrow(hull_both_best)>0){ 
                   big_plot_MF_H = big_plot_MF_H + 
                     geom_polygon(data =  hull_both_best,  aes(fill = 'Best MF and equity'#, color = 'Best MF and equity'
                                                               ), color = NA, size = 5, alpha = 0.6)
                 }
                 if (nrow(hull_diversity)>0){  
                   big_plot_MF_H = big_plot_MF_H +
                     geom_polygon(data =  hull_diversity,  aes(fill = 'Best diversity'#, color =  'Best diversity'
                                                               ), color = NA, size = 5, alpha = 0.6)
                 }
                 if (nrow(hull_noloss)>0){    
                   big_plot_MF_H = big_plot_MF_H + 
                     geom_polygon(data =  hull_noloss ,    aes(fill =   'No loss'#, color = 'No loss'
                                                               ), color = NA, size = 5, alpha = 0.6)
                 }
                 
                 big_plot_MF_H = big_plot_MF_H  +
                   scale_colour_manual(name="Scenarios",
                                       values=c('Best MF and equity'="#91bfdb", 'Best diversity'="#7fbf7b", 'No loss'="#af8dc3"))+
                   scale_fill_manual(name="Scenarios",
                                     values=c('Best MF and equity'="#91bfdb", 'Best diversity'="#7fbf7b", 'No loss'="#af8dc3")) +
                   ylab('') + xlab('') +
                   new_scale_color() +
                   new_scale_fill()  +
                facet_wrap(~ variable, scales = "free_y") +
                geom_hline(aes(yintercept = 0), lwd = 0.3) +
                geom_vline(aes(xintercept = 0), lwd = 0.3) +
                geom_point(
                  size = 0.5,
                  alpha = 0.4,
                  shape = 16
                ) +
                theme_bw() + 
                new_scale_color() +
                new_scale_fill() +
                guides(fill=guide_legend(ncol=3), shape=guide_legend(ncol=3),  color=guide_legend(ncol=3)) +
                scale_color_manual(breaks =   c( "1. Deforestation: 50% less forests",
                                                 "2. All forests are coniferous",
                                                 "3. All forests are deciduous",
                                                 "4. All forests are mixed",
                                                 "5. All grasslands are high intensity",
                                                 "6. 50% more high intensity grasslands",
                                                 "7. All forests are mixed forests and all grasslands are low-intensity",
                                                 "8. Reforestation: 50% more forests",
                                                 "9. Equal proportions of mixed, coniferous and deciduous forests",
                                                 "10. 50% more low intensity grasslands",
                                                 "11. Low- and medium-intensity grasslands become coniferous forests",
                                                 "12. All grasslands are low intensity",
                                                 "13. All forests are coniferous and all grasslands are high intensity",
                                                 "14. 50% more grasslands",
                                                 "15. Equal proportions of grasslands, crops and forests",
                                                 "16. All forests are even-aged",
                                                 "19. All forests are uneven-aged",
                                                 "17. All forests are uneven-aged and grasslands low-intensity",
                                                 "18. Low-lui and med-lui grasslands become uneven-aged forests",
                                                 "19. Low- and medium-intensity grasslands become mixed forests"),   
                                  
                                   values = c(palette_test, palette_test, palette_test)) + 
                scale_fill_manual(breaks =   c("1. Deforestation: 50% less forests",
                                               "2. All forests are coniferous",
                                               "3. All forests are deciduous",
                                               "4. All forests are mixed",
                                               "5. All grasslands are high intensity",
                                               "6. 50% more high intensity grasslands",
                                               "7. All forests are mixed forests and all grasslands are low-intensity",
                                               "8. Reforestation: 50% more forests",
                                               "9. Equal proportions of mixed, coniferous and deciduous forests",
                                               "10. 50% more low intensity grasslands",
                                               "11. Low- and medium-intensity grasslands become coniferous forests",
                                               "12. All grasslands are low intensity",
                                               "13. All forests are coniferous and all grasslands are high intensity",
                                               "14. 50% more grasslands",
                                               "15. Equal proportions of grasslands, crops and forests",
                                               "16. All forests are even-aged",
                                               "19. All forests are uneven-aged",
                                               "17. All forests are uneven-aged and grasslands low-intensity",
                                               "18. Low-lui and med-lui grasslands become uneven-aged forests",
                                               "19. Low- and medium-intensity grasslands become mixed forests"), 
                                  values  = c(palette_test, rep("white", 8), rep('grey', 8))) +
                scale_shape_manual(breaks =   c("1. Deforestation: 50% less forests",
                                                "2. All forests are coniferous",
                                                "3. All forests are deciduous",
                                                "4. All forests are mixed",
                                                "5. All grasslands are high intensity",
                                                "6. 50% more high intensity grasslands",
                                                "7. All forests are mixed forests and all grasslands are low-intensity",
                                                "8. Reforestation: 50% more forests",
                                                "9. Equal proportions of mixed, coniferous and deciduous forests",
                                                "10. 50% more low intensity grasslands",
                                                "11. Low- and medium-intensity grasslands become coniferous forests",
                                                "12. All grasslands are low intensity",
                                                "13. All forests are coniferous and all grasslands are high intensity",
                                                "14. 50% more grasslands",
                                                "15. Equal proportions of grasslands, crops and forests",
                                                "16. All forests are even-aged",
                                                "19. All forests are uneven-aged",
                                                "17. All forests are uneven-aged and grasslands low-intensity",
                                                "18. Low-lui and med-lui grasslands become uneven-aged forests",
                                                "19. Low- and medium-intensity grasslands become mixed forests"), 
                                   values = rep(c(22, 23, 24), 8)) +

                geom_point(
                  data = Community_MF_average_by_predef[region == R & !is.na(Scenario_description)  & mean_MF_diff >= min_MF,],
                  aes(
                    color = Scenario_description,
                    shape = Scenario_description,
                    fill = Scenario_description
                  ),
                  size = 3,
                  lwd = 0.5,
                  stroke = 1
                ) +
                ylab("") +
                xlab("Multifunctionality difference compared to current landscape") +
               theme(
                 legend.title = element_blank(),
                 panel.spacing = unit(2, "lines"),
                 strip.background = element_blank(),
                 strip.text.x = element_blank(),
                 text = element_text(size=16)
               )  +
                   ylab('') + xlab('') +
                   new_scale_color() +
                   new_scale_fill() +
                   scale_colour_manual(name="Scenarios",
                                       values=c('Best MF and equity'="#4575b4", 'Best diversity'="#008837", 'No loss'="#7b3294"))+
                   scale_fill_manual(name="Scenarios",
                                     values=c('Best MF and equity'="#4575b4", 'Best diversity'="#008837", 'No loss'="#7b3294"))
                   
                 
                 if (nrow(hull_both_best)>0){ 
                   big_plot_MF_H = big_plot_MF_H + 
                     geom_point(data = Community_MF_average_to_plot[region == R & both_best == TRUE,],  aes(color = 'Best MF and equity', fill = 'Best MF and equity'))
                 }
                 if (nrow(hull_diversity)>0){  
                   big_plot_MF_H = big_plot_MF_H +
                     geom_point(data = Community_MF_average_to_plot[region == R & best_diversity == TRUE,],  aes(color = 'Best diversity', fill = 'Best diversity'))
                 }
                 if (nrow(hull_noloss)>0){    
                   big_plot_MF_H = big_plot_MF_H + 
                     geom_point(data = Community_MF_average_to_plot[region == R & best_no_loser_no_threat == TRUE,],aes(color = 'No loss', fill = 'No loss'))
                 }
                 return(big_plot_MF_H)
                 }
                   
                # If default, directly save the figure format (with reduced scales)

                
                # Convert to a ggplot and print
                
                if (R == 'All' & env_corr == 'env_corr' & by_region == FALSE  &
                    use_SB == FALSE & weighted == FALSE & forest_class == "Type" & crop_constrained == TRUE){
                
                Fig2wholerange = plot_big_plot(Community_MF_average_to_plot[variable == 'equity_diff',], Community_MF_average_by_predef[variable == 'equity_diff',])
                Fig2 = plot_big_plot(Community_MF_average_to_plot[variable == 'equity_diff' &  mean_MF_diff >= min_MF,], Community_MF_average_by_predef[variable == 'equity_diff',])
                Fig4 = plot_big_plot(Community_MF_average_to_plot[variable != 'equity_diff' &  mean_MF_diff >= min_MF,], Community_MF_average_by_predef[variable != 'equity_diff',])
                Fig4 = Fig4 + scale_y_reverse()
                
                dir.create(file.path(paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region,  "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, "/constrained", crop_constrained, "/", sep = "")), recursive = T) 
                
               ggsave(plot = Fig2wholerange + theme(legend.position = 'none'),
                file = paste( "Results/",R,"/env_corr", env_corr, "/by_region", by_region,  "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, "/constrained", crop_constrained, "/plot_MF_wholerange.pdf", sep = ""
                ),  width = 4.5, height = 3.8)
               ggsave(plot = Fig2 + theme(legend.position = 'none'),
                      file = paste( "Results/",R,"/env_corr", env_corr, "/by_region", by_region,  "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, "/constrained", crop_constrained, "/plot_MF.pdf", sep = ""
                      ),  width = 8, height = 9)
               ggsave(plot = Fig4 + theme(legend.position = 'none', panel.spacing = unit(5, "lines")),
                 file = paste( "Results/",R,"/env_corr", env_corr, "/by_region", by_region,  "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, "/constrained", crop_constrained, "/plot_loss.pdf", sep = ""
                 ),width = 9, height = 4)
                }
                
               # In all cases, save the non-cropped figure, all together, for SI 
                Fig2 =   plot_big_plot(Community_MF_average_to_plot[variable == 'equity_diff' & region == R,], Community_MF_average_by_predef[region == R & variable == 'equity_diff',])
                Fig4 =   plot_big_plot(Community_MF_average_to_plot[variable != 'equity_diff' & region == R,], Community_MF_average_by_predef[region == R & variable != 'equity_diff',])
                Fig4 =   Fig4+ scale_y_reverse()
                
                leg <- get_legend(Fig4)
                gg_leg = as_ggplot(leg)
                
                Fig2_4 = plot_grid(Fig2 + theme(legend.position = 'none'), Fig4 + theme(legend.position = 'none'), ncol = 2, rel_widths = c(1.05, 2))

                dir.create(file.path(paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region,  "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, "/constrained", crop_constrained, "/", sep = "")), recursive = T) 
                
                ggsave(plot = gg_leg,
                       file = paste( "Results/",R,"/env_corr", env_corr, "/by_region", by_region,  "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, "/constrained", crop_constrained, "/legend.pdf", sep = ""
                       ))
                
                ggsave(plot = Fig2_4  + theme(legend.position = 'none'),
                       file = paste( "Results/",R,"/env_corr", env_corr, "/by_region", by_region,  "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, "/constrained", crop_constrained, "/big_plot_MF.pdf", sep = ""
                       ),  width = 11, height = 4)
                }

              ### Service correlation matrix
              Services_data = Community_services_full[!(grepl('__MF', variable)) & variable != 'N', ]
              Services_data_cast = dcast.data.table(Services_data, replica + scenario~variable, value.var = 'value')
              Services_data_cast = Services_data_cast[,list('C stock' = C_stock, 'Leisure' = Leisure,  
                                                            'Aesthetic value' = Aesthetic,
                                                            'Regional identity' = Reg_id,
                                                            'Biodiversity' = Ric, 
                                                            'Foraging' = Harvesting, 
                                                            'Hunting' = Hunting,
                                                            'Livestock production' = Production_livestock,
                                                            'Food production'      = Production_food, 
                                                            'Energy production'    = Production_energy,  
                                                            'Timber production'    = Production_timber)]
              
              
              corr_mat <- round(cor(Services_data_cast), 1)
              cor_plot = ggcorrplot(corr_mat, method = "circle", type = "upper",
                         colors = c("#6D9EC1", "white", "#E46726"))
              
              ggsave(plot = cor_plot,
                     file = paste( "Results/",R,"/env_corr", env_corr, "/by_region", by_region, "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, "/constrained", crop_constrained, "/cor_plot.pdf", sep = ""
                     ),  width = 8, height = 8)
          
              
              
              
             
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
              
              Community_MF_average[, c('Forest_all', 'Grassland_all' ):= 
                                     list(Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged,
                                         Grassland_low + Grassland_medium + Grassland_high)]
              Community_MF_average[, c('prop_grass_low', 'prop_grass_med', 'prop_grass_high',
                                       'prop_for_dec', 'prop_for_mix', 'prop_for_con', 'prop_for_ea', 'prop_for_ua'):= 
                                     list(Grassland_low / Grassland_all,
                                          Grassland_medium / Grassland_all,
                                          Grassland_high / Grassland_all,
                                          Forest_Deciduous / Forest_all, 
                                          Forest_Mixed / Forest_all, 
                                          Forest_Coniferous / Forest_all,
                                          Forest_even_aged / Forest_all,
                                          Forest_uneven_aged/ Forest_all
                                          )]
              
              Baseline_composition <- Community_MF_average[Scenario_description == "Baseline", lapply(.SD, mean),
                by = region,
                .SDcols = c( "mean_MF", "mean_equity", "Score", "Crop", "Grassland_all", "Forest_all","Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged",'prop_grass_low', 'prop_grass_med', 'prop_grass_high',
                             'prop_for_dec', 'prop_for_mix', 'prop_for_con', 'prop_for_ea', 'prop_for_ua'
                )
              ]

              Baseline_composition_melt <- melt.data.table(
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
              
              plot_landscape_baseline <- dcast.data.table(Baseline_composition_melt, region~variable)[, list(
                Grassland = (Grassland_low + Grassland_medium + Grassland_high) / (Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                Crop = (Crop) / (   Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
                Forest = (   Forest_even_aged + Forest_uneven_aged + Forest_Deciduous + Forest_Mixed + Forest_Coniferous ) / (   Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
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
              
              for (R in unique(plot_landscape_data_baseline$region)){
              if (forest_class == "Type") {
                landscape_box_baseline <- ggplot(data = plot_landscape_data_baseline[region == R,]) +
                  
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
                landscape_box_baseline <- ggplot(data = plot_landscape_data_baseline[region == R,]) +
                  theme_void() +
                  # Crop
                  geom_rect(
                    fill = "#f48b30",
                    aes( xmin = 0, xmax = 1, ymin = 0, ymax = crop_top )
                  ) +
                 geom_rect(
                    fill = "#ABE188",
                    aes(xmin = 0,   xmax = forest_even_aged_right,   ymin = crop_top,   ymax = forest_top )
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
                    aes(  xmin = grass_low_right,  xmax = grass_med_right,  ymin = forest_top,  ymax = 1)
                  ) +
                  geom_rect(
                    fill = "#4d708a",
                    aes(  xmin = grass_med_right,  xmax = 1,  ymin = forest_top,  ymax = 1))
              }
              ggsave(
                plot = landscape_box_baseline,
                file = paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region, 
                              "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, 
                             "/constrained", crop_constrained, "/landscape_baseline.pdf", sep = ""
                ),
                height = 6,
                width = 6
              )
}
              
              
              
              # Check variation of MF and equity with % forest cover
              Community_MF_average[, Forest_tot := (Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged)/
                                     (Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged + Crop + Grassland_low + Grassland_medium + Grassland_high)]
              Community_MF_average_maxforest = melt.data.table(Community_MF_average[, list(scenario, Forest_tot, Multifunctionality = mean_MF, Equity = mean_equity, region)], id.vars = c('region', 'Forest_tot','scenario'))
              
              get_best = function(Data){
                model <- loess(value ~ Forest_tot, Data)
                xrange <- range(Data$Forest_tot)
                xseq <- seq(from=xrange[1], to=xrange[2], length=100)
                pred <- predict(model, newdata = data.frame(Forest_tot = xseq), se=TRUE)
                Fit = pred$fit
                max_fit = max(Fit)
                return(list(Forest_tot = xseq[Fit ==  max(Fit)],  value = max(Fit)))
              }
              Best_forest = Community_MF_average_maxforest[, get_best(.SD), .SDcols = c('value', 'Forest_tot'), by = c('variable','region') ]
              
              forest_plot = ggplot(Community_MF_average_maxforest, aes(value, x = Forest_tot)) + geom_jitter(alpha = 0.2, width = 0.01) + geom_smooth(color = 'darkgreen') + theme_bw() +
                facet_wrap(~variable, nrow = 2, scales = "free_y") +
                geom_vline(data = Best_forest, aes(xintercept = Forest_tot)) +
                geom_vline(data =  Baseline_composition_melt[, list(Forest_tot = sum(baseline[variable %in% c('Forest_Deciduous', 'Forest_Mixed','Forest_Coniferous','Forest_even_aged', 'Forest_uneven_aged')])/
                                                                      sum(baseline[variable %in% c('Forest_Deciduous', 'Forest_Mixed','Forest_Coniferous','Forest_even_aged', 'Forest_uneven_aged', 'Grassland_low', 'Grassland_medium','Grassland_high', 'Crop')])), by = region]
                           
                           , aes(xintercept = Forest_tot), lty = 2) +
                ylab('Value') + xlab('% Forest in the landscape') +
                annotate('label', x = 0.35, y = -Inf, label = 'Current\nlandscape', vjust = -1)+
                annotate('label', x = 0.57, y = -Inf, label = 'Optimal forest\ncover', vjust = -1)
              
              ggsave(
                plot = forest_plot,
                file = paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region, 
                              "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, 
                             "/constrained", crop_constrained, "/forest_plot.pdf", sep = ""
                ),
                height = 6,
                width = 6
              )
               
              
              #### Plot all plots based on criteria
              
              for (criteria in c(  'best_no_loser_no_threat', 'best_diversity',
                                  'both_best', 'best_Cstock'
              )) {
               #dir.create(paste( "Results/", criteria, sep = ''))
                ### Plot: change in new landscape
                new_scenarios <- Community_MF_average[get(criteria) == 1, list(scenario = unique(scenario), 'keep_scenario' = TRUE), by = 'region']

                if (nrow(new_scenarios) > 0) {
                  
                  Community_services_full2 = merge.data.table(Community_services_full, new_scenarios, by = c('region','scenario'), all.x = T)
                  Community_services_full2[, value_relative := value_diff/value_baseline*100]
                  
                  plot_optimum_change <- Community_services_full2[keep_scenario == TRUE, list(
                                           value_relative = mean(value_relative)), by = c("scenario","variable", "region" )][ ,
                                           as.list(CI(value_relative, ci = 0.95)), 
                                           by = c("variable", "region" )] # calculate.ci(value_diff/value_baseline*100)
                  plot_optimum_change_abs <- Community_services_full2[keep_scenario == TRUE, list(
                    value = mean(value)), by = c("scenario","variable", "region" )][ ,
                  
                                                                                                                                                              as.list(CI(value, ci = 0.95)), 
                                                                                                       by = c("variable", "region" )] # calculate.ci(value_diff/value_baseline*100)
                  plot_optimum_all = Community_services_full2[keep_scenario == TRUE, list(
                    value_relative = mean(value_relative), 
                    group = factor(gsub("__MF", "", variable),
                                                                          levels = c( "Quarrying", "Agric", "Hunting", "Forestry", "Landowner", "Tourism", "Loc_her_asso", "Policy_admin", "Press", "Locals", "Econ", "Nat_cons_asso", "Research", "Reg_dev_prog"
                                                                          ))), by = c("scenario","variable", "region" )]
                  plot_optimum_all[, Nscenarios := .N, by = c("variable", "region" )]
                  
                  plot_optimum_change = merge.data.table(plot_optimum_change, plot_optimum_change_abs[, list( variable, region, 'upper_abs' = upper,'mean_abs' = mean, 'lower_abs' = lower)], all = T)
                  plot_optimum_change = merge.data.table(plot_optimum_change,
                                              Community_services_full2[keep_scenario == TRUE, list(
                                                value_baseline = mean(value_baseline)), by = c("variable", "region" )],
                                              all = T
                                              )
                  plot_optimum_change[, is.MF := grepl('__MF', variable, perl = T)]
                  
                  plot_optimum_change = plot_optimum_change[variable != 'N',]
                  plot_optimum_change[, variable := factor(
                    variable,
                    levels = c( "Ric", "Aesthetic", "Reg_id", "Leisure", "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting", "C_stock",
 "Nat_cons_asso__MF", "Research__MF", "Econ__MF", "Reg_dev_prog__MF", "Tourism__MF", "Locals__MF", "Press__MF", "Loc_her_asso__MF", "Policy_admin__MF",
 "Hunting__MF", "Landowner__MF", "Forestry__MF", "Quarrying__MF", "Agric__MF"
                    )
                  )]

                  plot_optimum_change[, group := factor(
                    gsub("__MF", "", variable),
                    levels = c( "Quarrying", "Agric", "Hunting", "Forestry", "Landowner", "Tourism", "Loc_her_asso", "Policy_admin", "Press", "Locals", "Econ", "Nat_cons_asso", "Research", "Reg_dev_prog"
                    )
                  )]
                  plot_optimum_change <- plot_optimum_change[order(group, decreasing = T), ]
                  
                  for (R in unique(plot_optimum_change$region)){
                  
                  gg_optimum_MF <- ggplot(
                    plot_optimum_change[grepl("MF", variable) & plot_optimum_change$region == R, ],
                    aes(  mean,  ymin = lower,  ymax = upper,  x = group)
                  ) +
                    geom_col(fill = '#98A2AE') + geom_errorbar() + theme_bw() +
                    ylab("") +
                    xlab("") +
                    coord_flip() +
                   theme(legend.position = "none", 
                                     text = element_text(size=20),
                                     axis.ticks.y = element_blank(),
                                     axis.text.y = element_blank(), 
                                     panel.grid.minor =   element_blank())+ # Add points
                    geom_jitter(data = plot_optimum_all[region == R & grepl('__MF', variable),], aes(x = group, y = value_relative), inherit.aes = F, width = 0.1, alpha = 0.25, size = 0.9)+
                    annotate(geom= 'text', x = +Inf, y = Inf, label = paste('n = ', unique(plot_optimum_all[region == R,]$Nscenarios)),  vjust = 1.5, hjust = 1.5, size = 6)
                  
                  
                  
                  gg_optimum_services <- ggplot(
                    plot_optimum_change[!grepl("MF", variable) & variable != 'N' & plot_optimum_change$region == R, ],
                    aes(  mean,  ymin = lower,  ymax = upper,  x = variable,  fill = variable)
                  ) +
                     geom_col() + geom_errorbar() + theme_bw() + ylab("") +
                    xlab("") + coord_flip() + 
                    scale_fill_manual(
                      values = my_palette_services,
                      breaks = c("Ric","Aesthetic","Reg_id","Leisure","Production_food","Production_livestock","Production_timber","Production_energy","Harvesting","Hunting","C_stock")) +
                   scale_x_discrete(limits=rev)+
                    theme(legend.position = "none", 
                          text = element_text(size=20),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_blank(), 
                          panel.grid.minor =   element_blank()) + # Add points
                    geom_jitter(data = Community_services_full2[region == R & keep_scenario == TRUE & variable != 'N' & !grepl('_MF', variable), list(
                      value_relative = mean(value_relative)), by = c("scenario","variable", "region" )], aes(x = variable, y = value_relative), inherit.aes = F, width = 0.1, alpha = 0.25, size = 0.9)+
                    annotate(geom= 'text', x = +Inf, y = Inf, label = paste('n = ', unique(plot_optimum_all[region == R,]$Nscenarios)),  vjust = 1.5, hjust = 1.5, size = 6)
                  
                  
                 ggsave(  plot = gg_optimum_MF,  
                           file = paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region, 
                                         "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, 
                                        "/constrained", crop_constrained, "/gg_optimum_MF", criteria, ".pdf", sep = ""  ),  width =  5,  height = 5)
                 ggsave(  plot = gg_optimum_services,  
                           file = paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region, 
                                        "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, 
                                        "/constrained", crop_constrained, "/gg_optimum_services", criteria, ".pdf", sep = ""  ),  width =  5,  height = 5)
                
                  }


                  ## Calculate average for scenarios fitting the criteria
                  Community_MF_average[, c("mean_MF","mean_equity","Score","Crop","Grassland_low","Grassland_medium","Grassland_high","Forest_Deciduous","Forest_Mixed","Forest_Coniferous","Forest_even_aged","Forest_uneven_aged" ) :=
                                        lapply(.SD, function(x){as.numeric(x)}),
                                      .SDcols = c( "mean_MF", "mean_equity", "Score", "Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged")
                  ]
                  Community_MF_average[, Forest_all := Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged]
                  Community_MF_average[, Grassland_all := Grassland_low + Grassland_medium + Grassland_high]
                  
                  Average <- Community_MF_average[get(criteria) == 1, lapply(.SD, mean, na.rm = T),
                   by = region,
                   .SDcols = c( "mean_MF", "mean_equity", "Score", "Crop", 'Forest_all','Grassland_all', "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even_aged", "Forest_uneven_aged",'prop_grass_low', 'prop_grass_med', 'prop_grass_high',
                                'prop_for_dec', 'prop_for_mix', 'prop_for_con', 'prop_for_ea', 'prop_for_ua'
                                
                   )
                  ]
                  Average[, c("Criteria", "Description") := list(criteria, "Average")]
                  
                  plot_landscape <- Average[, list(
                    Grassland = (    Grassland_low + Grassland_medium + Grassland_high  ) / (    Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged  ),
                    Crop = (Crop) / (   Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
                    Forest = ( Forest_even_aged + Forest_uneven_aged +   Forest_Deciduous + Forest_Mixed + Forest_Coniferous ) / (   Crop + Grassland_low + Grassland_medium + Grassland_high + Forest_Deciduous + Forest_Mixed + Forest_Coniferous + Forest_even_aged + Forest_uneven_aged ),
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
                
                  for (R in unique(plot_landscape_data$region)){
                  # if (nrow(plot_landscape_data[region == R,])>0){  
                  if (forest_class == "Type") {
                    landscape_box <- ggplot(data = plot_landscape_data[region == R,]) +
                     
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
                      ) +                      geom_rect(
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
                        aes( xmin = grass_med_right, xmax = 1, ymin = forest_top, ymax = 1)
                      )                 
                    }
                  if (forest_class == "Age") {
                    landscape_box <- ggplot(data = plot_landscape_data[region == R,]) +
                      
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
                        aes(  xmin = grass_low_right,  xmax = grass_med_right,  ymin = forest_top,  ymax = 1)
                      ) +
                      geom_rect(
                        fill = "#4d708a",
                        aes(  xmin = grass_med_right,  xmax = 1,  ymin = forest_top,  ymax = 1)
                      ) 
                    }
                  ggsave(
                    plot = landscape_box,
                    file = 
                      paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region, 
                             "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, 
                            "/constrained", crop_constrained, "/landscape_box", criteria, ".pdf", sep = ""
                    ),
                    height = 6,
                    width = 6
                  )
                  }
                  }

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
                    .SDcols = c("mean_MF","mean_equity","Score","Crop","Forest_all","Grassland_all" ,'prop_grass_low', 'prop_grass_med', 'prop_grass_high',
                                'prop_for_dec', 'prop_for_mix', 'prop_for_con', 'prop_for_ea', 'prop_for_ua'
                    )
                  ] # UA = uneven aged, EA = even aged
                  Confidence_intervals <- Confidence_intervals[, lapply(.SD, function(x) {
                    y <- x
                    y[y == -1] <-
                      NA
                    return(y)
                  }), .SDcols = c( "region", "mean_MF", "mean_equity", "Score", "Crop","Forest_all","Grassland_all" , 'prop_grass_low', 'prop_grass_med', 'prop_grass_high',
                                   'prop_for_dec', 'prop_for_mix', 'prop_for_con', 'prop_for_ea', 'prop_for_ua'
                  )]
                  Confidence_intervals[, c("Criteria", "Description") := list(criteria, "CI")]


                  Best_landscape_all_melt <- melt.data.table(
                    rbind(Average[, .SD, .SDcols= c( "region", "mean_MF", "mean_equity", "Score", "Crop","Forest_all","Grassland_all" , 'prop_grass_low', 'prop_grass_med', 'prop_grass_high',
                                                               'prop_for_dec', 'prop_for_mix', 'prop_for_con', 'prop_for_ea', 'prop_for_ua', 'Criteria', 'Description'
                    )], Confidence_intervals),
                    id.vars = c("region", "Description", 'Criteria')
                  )

                  if (nrow(Best_landscape_all_melt) > 0) {
                    Best_landscape_all_cast <- dcast.data.table(
                      Best_landscape_all_melt,
                      region + variable ~ Description,
                      value.var = "value"
                    )
                    Best_landscape_plus_baseline <- merge.data.table(
                      Baseline_composition_melt,
                      Best_landscape_all_cast,
                      by = c("region", "variable")
                    )
                    Best_landscape_plus_baseline[, c("Average", "CI") := lapply(.SD, as.numeric), .SDcols = c("Average", "CI")]
                    Best_landscape_plus_baseline[, Average_diff := Average - baseline]

                    if (forest_class == "Age") {
                      Best_landscape_plus_baseline <- Best_landscape_plus_baseline[!(
                        variable %in% c(
                          'prop_for_dec', 'prop_for_mix', 'prop_for_con'
                        )
                      ), ]
                    }
                    if (forest_class == "Type") {
                      Best_landscape_plus_baseline <- Best_landscape_plus_baseline[!(variable %in% c(
                        'prop_for_ea', 'prop_for_ua'
                      )), ]
                    }

                    Best_landscape_plus_baseline$variable <- droplevels(Best_landscape_plus_baseline$variable)
                  }
                }
              }
            }
          }
        }
      }
    
  }
 

