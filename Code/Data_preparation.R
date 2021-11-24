# ------------------------------------------------------------------------- #
# In this file I  merge raw indicator data into one consistent ES dataframe,
# and import scenarios table into usable formats.
# ------------------------------------------------------------------------- #
library(ggplot2)
library(data.table)
library(tidyr)
library(readxl)
library(vegan)
library(mice)


setwd('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_simulation/')
# Parameters
set.seed(101)
for (by_region in c(TRUE, FALSE)) {
#by_region = FALSE
for (scale_within_land_use in c(TRUE, FALSE
                                )) {
#scale_within_land_use = TRUE
    print(scale_within_land_use)
    env_corr ='env_corr'
    for (env_corr in c("env_corr", "")) {
      
      # ###################### #
      ####    TO DO LIST    ####
      # ###################### #
      # Birds in crops?

      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      #####     PLOT DESCRIPTORS     #####
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      ### Load data
      Forest_classif <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/17706_New_forest_type_classification_of_all_forest_EPs_2008-2014_1.2.6/17706.txt")
     # Grassland_LUI <- fread('/Users/Margot/Desktop/Research/Senckenberg/Data/Environment/LUI_input_data/LUI_standardized_global.txt')
      Grassland_LUI <- fread("/Users/Margot/Desktop/Research/Senckenberg/Data/Environment/LUI_input_data/LUI_standardized_regional.txt", dec = ",")
      Grassland_LUI[, Year := gsub('separately\\(','', YEAR)][, Year := gsub('\\)','', Year)]
      Grassland_LUI[, Plot := ifelse(nchar(PLOTID)== 5, PLOTID, paste(substr(PLOTID, 1, 3), '0', substr(PLOTID, 4, 4 ), sep = ''))]
      
      Grassland_LUI = Grassland_LUI[as.numeric(Year) < 2016 & as.numeric(Year) > 2007, list(LUI_2008_2015 = mean(as.numeric(LUI))), by = Plot]

      Crop_data <- setDT(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Land-use_proportion220920.xlsx"))
      Plot_id_matching <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/20826_Basic\ Information\ of\ all\ Experimental\ Plots_EPs_1.7.5/20826.txt")

      # Classify forests in coniferous, deciduous, mixed
      Forest_classif[grepl("pure", StComp_G), Classif := ifelse(rowSums(sapply(c("Pa", "Ps"), function(x) {
        grepl(x, StComp_G)
      })) > 0, # If column StComp_G contains Pa and Ps,
      "Forest_Coniferous", # then coniferous
      "Forest_Deciduous" # Otherwise deciduous
      )]
      Forest_classif[!grepl("pure", StComp_G), Classif := "Forest_Mixed"]
      Forest_classif[, Main_sp := substr(StComp_Area, 1, 2)]
      Forest_classif[, c("Region", "Plot", "LU") := list(substr(Exploratory, 1, 1), EP, "Forest")]

      # Other forest classification in even- or uneven-aged
      Forest_classif[grepl("AC", Forest_type_in_detail), Classif2 := "Forest_even-aged"]
      Forest_classif[grepl("unmanaged", Forest_type_in_detail) | grepl("extensively_managed", Forest_type_in_detail) | grepl("selection", Forest_type_in_detail), Classif2 := "Forest_uneven-aged"]

      # Classify grasslands based on LUI quantiles
      Grassland_LUI[, c("Region", "Plot", "LU") := list(substr(Plot, 1, 1), Plot, "Grassland")]

      if (by_region == TRUE) {
        Grassland_LUI[, Classif := cut(LUI_2008_2015, breaks = quantile(LUI_2008_2015, c(0, 0.33, 0.66, 1)), labels = c("Grassland_low", "Grassland_medium", "Grassland_high"), include.lowest = TRUE), by = Region]
      }
      if (by_region == FALSE) {
        Grassland_LUI[, Classif := cut(LUI_2008_2015, breaks = quantile(LUI_2008_2015, c(0, 0.33, 0.66, 1)), labels = c("Grassland_low", "Grassland_medium", "Grassland_high"), include.lowest = TRUE)]
      }

      Grassland_LUI[, Classif2 := Classif]

      # Crops
      Crop_data <- Crop_data[!is.na(Crop_type), ]
      Crop_data[, Productivity := as.numeric(Market_value_euro_perT_without_tax) * as.numeric(Yield_t_per_ha)]
      Crop_data[, Proportion_of_all_crops := round(as.numeric(PROPORTION) * 2)]
      # This creates 1 line for each 1% of each crop, so that we end up with a proportional number of lines in each crop 
      Crop_classif <- Crop_data[rep(1:.N, Proportion_of_all_crops)] 

      Crop_classif[, c("Plot", "LU", "Classif", "Classif2") := list(paste("C", 1:600, sep = ""), "Crop", Land_cover, Land_cover)]

      Classif <- rbind(
        Forest_classif[, c("Region", "Plot", "LU", "Classif", "Classif2")],
        Grassland_LUI[, c("Region", "Plot", "LU", "Classif", "Classif2")],
        Crop_classif[, c("Region", "Plot", "LU", "Classif", "Classif2")]
      )

      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      ##### FUNCTIONS and scripts #####
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      scale01 <- function(x) {
        # Scales a variable between 0 and 1
        x <- as.numeric(x)
        max <- quantile(x, 0.975, na.rm = T)
        min <- min(x, na.rm = T)
        y <- (x - min) / (max - min)
        y[y < 0] <- 0
        y[y > 1] <- 1
        return(y)
      }

      # Calculates Acoustic diversity
      if (!exists("Acoustic_diversity_by_plot")) source(file = "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Acoustic_diversity.R")

      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      ##### CREATION OF INDICATOR DATASETs #####
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      # These are individual indicators that will be corrected for the environment at the next step (e.g. not including market values etc.)

      #### 0. Multi-LU indicators ####
      ## Charismatics
      Charismatic <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Grassland_data/Edible_culturally_important_plant_species.csv", sep = "\t", quote = "")[-1, ]
      colnames(Charismatic) <- gsub(" ", "_", colnames(Charismatic))
      charismatic_plants <- gsub(" ", "_", Charismatic[!is.na(Cultural_overall) & Cultural_overall != "", Species])
      # Removing common trees, except oak, bc double counts with forest type
      charismatic_plants <- charismatic_plants[!(charismatic_plants %in% c("Abies_alba", "Picea_abies"))]
      
      # Data from Sophie's survey
      Charismatic_birds <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Bird_cultural_value/bird_species_edited.xlsx"))
      Charismatic_birds_melt <- melt.data.table(Charismatic_birds[Species_German_Latin != "Bird species missing?"], id.vars = c("Species", "Species_German_Latin", "Bird_species_English"))
      Charismatic_birds_melt[, value_use := dplyr::recode(value, "1" = "NA", "2" = "0", "3" = "1", "4" = "5")]
      bird_scores <- Charismatic_birds_melt[, list(
        species_score = mean(as.numeric(value_use), na.rm = T),
        Species = gsub(" ", "_", Species)
      ), by = Species][order(Species), c(2, 3)]
      bird_scores <- rbind(bird_scores, list(species_score = mean(bird_scores[grepl("Parus", Species), species_score]), Species = "Parus_montanus"))
      most_charismatic_birds <- bird_scores[order(species_score, decreasing = T), Species][1:round(nrow(bird_scores) / 4)]
      
      ## Edibles 
      Edible = data.table(read_excel('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/updated_edible_species.xlsx'))
      
      all_edible_plants <- unique(c(Edible$Plants_other, Edible$Plants_top))
      top_edible_plants <- unique(c(Edible$Plants_top))

      all_edible_fungi <- unique(c(Edible$Fungi_other, Edible$Fungi_top))
      top_edible_fungi <- unique(c(Edible$Fungi_top))
      
      # Abundances and richness for plants and birds
      Abundance_forests <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/Forest_autotrophs_birds.txt")
      Abundance_forests[, Species := gsub("_$", "", Species)]
      Abundance_grasslands <- fread("/Users/Margot/Desktop/Research/Senckenberg/Data/Abundances/Dataset_clean.txt")
      Abundance_grasslands <- Abundance_grasslands[Group_broad %in% c("Plant", "Birds"), ]
      Abundance_grasslands = Abundance_grasslands[Group_broad == 'Plant' & Year %in% c('2009', '2010', '2011', '2012', '2013', '2014', '2015') | Group_broad == "Birds" &  Year %in% c('2008', '2009', '2010', '2011', '2012', '2013', '2014'),]
      Abundance_grasslands_forests0 <- rbind(
        Abundance_grasslands[, c("Species", "Plot", "Year", "Group_broad", "value")],
        Abundance_forests[, c("Species", "Plot", "Year", "Group_broad", "value")]
      )
      Abundance_grasslands_forests0[, Group_broad := tolower(gsub("s", "", Group_broad))]
      Abundance_grasslands_forests <- Abundance_grasslands_forests0[Year < 2016, list(value = sum(value, na.rm = T)), by = c("Species", "Plot", "Group_broad")]
      Abundance_grasslands_forests[Species == "Delichon_urbica", Species := "Delichon_urbicum"]
      Abundance_grasslands_forests[, Region := substr(Plot, 1, 1)]
      Abundance_grasslands_forests01 <- copy(Abundance_grasslands_forests)
      Abundance_grasslands_forests01[value > 0, value := 1]
      ab_birds <- dcast(Abundance_grasslands_forests01[Species %in% most_charismatic_birds, ], Plot ~ Species, value.var = "value", fill = 0)

      Ric= dcast.data.table(Abundance_grasslands_forests[value>0, .N, by = c('Plot', 'Group_broad', 'Region')],
                            Plot + Region ~ Group_broad, value.var = 'N')
      Ric_LUI = merge.data.table(Grassland_LUI, Ric, by = 'Plot')

      Edible_charismatic_richness <- Abundance_grasslands_forests[value > 0,
        list(
          Cover_edible = sum(value[Species %in% all_edible_plants]) + sum(value[Species %in% top_edible_plants]),
          Charismatic_plants = sum(value[Species %in% charismatic_plants]),
          Plant_richness = length(unique(Species[Group_broad == "plant"])),
          Bird_richness = length(unique(Species[Group_broad == "bird"])),
          Uniqueness_juniperus = ("Juniperus_communis" %in% Species)
        ),
        by = c("Plot", "Region")
      ]

      # Abundances and richness for fungi
      Fungi_ab0 <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/24286_Abundant\ fungi_2011_relative_abundance_1.1.7/24286.txt")
      Fungi_ab <- Fungi_ab0[Abundance > 0, ]
      Fungi_species_info <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/24306_Abundant_fungi_2011_taxonomic_look_uptable_1.1.8/24306.txt")

      all_edible_fungi = all_edible_fungi[!is.na(all_edible_fungi)]
      Fungi_species_edible <- Fungi_species_info[Species %in% all_edible_fungi, ]
      Fungi_ab <- merge.data.table(Fungi_ab, Fungi_species_info[, .SD, .SDcols = c("Species", "OTU")], by = c("OTU"))
      Fungi_ab[, Plot := ifelse(nchar(Plotid) == 5, Plotid, paste(substr(Plotid, 1, 3), 0, substr(Plotid, 4, 4), sep = ""))]
      Fungi_ab_plot <- Fungi_ab[, list(Abundance = sum(Abundance)), by = c("Species", "Plot")]
      Fungi_ab_plot[Abundance >= 1 & Species %in% all_edible_fungi, Abundance := 1]
      Fungi_ab_plot[Abundance >= 1 & Species %in% top_edible_fungi, Abundance := 2]

      Fungi_cast <- dcast.data.table(Fungi_ab_plot, Plot ~ Species, value.var = "Abundance", fill = 0)
      Fungi_edible_cast <- Fungi_cast[, .SD, .SDcols = c("Plot", all_edible_fungi)]

      # Export edible fungi abundance
      fwrite(Fungi_edible_cast, "data/Edible_fungi_presence.csv")

      Edible_charismatic_richness <- merge.data.table(Edible_charismatic_richness,
        data.table("Plot" = Fungi_edible_cast$Plot, "Fungi_richness" = rowSums(Fungi_edible_cast[, -1])),
        by = "Plot"
      )

      #### 1. Forest ####
      # Full vegetation records
      Forest_veg_records = fread('/Users/Margot/Desktop/Research/Senckenberg/Data/Traits/Plants/Species_lists/25886_Vegetation_Records_for_151_Forest_EPs_2009-2018_1.5.4/25886.txt')
      Forest_veg_charac  = Forest_veg_records[Year < 2016, list(Shrub_cover_sqrt = sqrt(sum(Cover[Layer == 'S']))), by = list(Plot = Useful_EPPlotID) ]
      
      # Tweaking to match variable names
      ES_forests <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/Timber_Prices/24367_Raw_data_Multiple_forest_attributes__1.1.16/24367.txt")[, c('Plot', 'Plot0', 'Exploratory', 'Trees_C_storage')]
      ES_forests[, c("Plot_ID", "Plot", "Exploratory") := list(Plot, Plot0, substr(Exploratory, 1, 1))]

      Openness_forests <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/25146_Canopy_openness_of_forest_EPs_2014_1.2.8/25146.txt")
      Openness_forests[, Plot := ifelse(nchar(EP) == 5, EP, paste(substr(EP, 1, 3), 0, substr(EP, 4, 4), sep = ""))]

      Forest_structure <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/17687_Stand_structure_and_composition_on_all_forest_EPs_2008-2014_1.4.8/17687.xlsx"))

      Moss_cover <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/4141_Bryophyte\ diversity\ in\ forests_1.6.8_PublicDataset/4141.txt")
      Moss_cover$PlotID <- Moss_cover$Plotid
      Moss_cover <- merge.data.table(Moss_cover, Plot_id_matching[, c("EP_PlotID", "PlotID")], by = "PlotID")
      Moss_cover[, Plot := ifelse(nchar(EP_PlotID) == 5, EP_PlotID, paste(substr(EP_PlotID, 1, 3), 0, substr(EP_PlotID, 4, 4), sep = ""))]

      #### Calculating timber production based on volume and market value of each species + total biomass for C stocks and Wood for energy
      Timber_volume <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/22907_Stand_composition_based_on_2nd_forest_inventory_on_all_forest_EPs_2014_2018_1.7.8/22907.txt")
      Timber_increment <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/22868_4_Dataset/22868_4_data.csv")
      Wood_use <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/Wood_use.xlsx", skip = 1))
      # from https://www.destatis.de/DE/Themen/Branchen-Unternehmen/Landwirtschaft-Forstwirtschaft-Fischerei/Wald-Holz/Tabellen/holzeinschlag-deutschland.html
      Timber_price <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/Timber_market_prices_incl_lumber_and_pulpwood.xlsx"))
      # Get prices here: http://www.wald-prinz.de/holzpreise-und-holzpreisentwicklung-fichte/383
      # Firewood: https://fbg-amberg.de/brennholz/brennholzboerse
      # https://fbg-amberg.de/holzvermarktung/holzpreise

      
    # New calculation after nov 2021  
      
      # Objective: Timber: have 2 indices, one for standing stocks and one for growth
      # Firewood: 1 index based on increment only to be comparable with annual crops  
      
      Forest_structure[, Plot := EP]
      Timber_increment[, c("Exploratory", "Plot", 'Timber_Prod_tot') := list(strsplit(Exploratory, 1, 1), EP, Timber_Prod)]
      
      Timber_volume[, c("Exploratory", "Plot", 'Timber_volume') := list(strsplit(Exploratory, 1, 1), EP, VOL)]
      Timber_volume[, Timber_volume_prop := VOL/sum(VOL), by = EP]
      
      # Merge volume and increment, and weight increment by each species' volume
      Timber_all = merge.data.table(Timber_volume, Timber_increment[, list(Timber_Prod_tot, Plot)])
      Timber_all[, Timber_Prod_weighted := Timber_Prod_tot*Timber_volume_prop]
      
      # We will need, for each species, the proportion of the wood or wood increment that will go to timber, energy, or industry wood
      Wood_use_cast = dcast.data.table(Wood_use[Species != 'Insgesamt' & Wood_type != 'Total',], Species ~ Wood_type, value.var = 'Proportions_to_use')
      
      # Usable species based on https://www.lignum.ch/files/images/Downloads_francais/Shop/20010_Bois_de_chez_nous.pdf
      unused_species = c("Sorbus_aucuparia"    ,  "Salix_caprea", "Malus_sylvestris"  , "Sorbus_aria"   ,       
                         "Alnus_spec"          ,  "Salix_spec"  , "Sorbus_torminalis" , "Pyrus_pyraster",       
                         "Robinia_pseudoacacia",  "Carya_ovata" , "Aesculus_hippocastanum")
      
      # Simplify species classification
      Timber_all[grepl('Quercus', Species), Species2 := 'Quercus_sp' ]
      Timber_all[Species %in% c('Picea_abies','Pinus_sylvestris', 'Fagus_sylvatica'),
                 Species2 := Species]
      Timber_all[Species %in% c('Prunus_serotina', 'Prunus_avium', 'Acer_spec', 'Acer_platanoides', 'Acer_pseudoplatanus', 'Acer_campestre', "Populus_tremula", 'Populus_nigra', 'Tilia_platyphyllos', 'Tilia_cordata', 'Tilia_spec','Ulmus_glabra','Ulmus_spec', 'Fraxinus_excelsior', 'Betula_spec','Betula_pendula', 'Carpinus_betulus'),
                 Species2 := 'Other_hardwood' ]
      Timber_all[Species %in% c('Larix_spec','Larix_decidua'),
                 Species2 := 'Larix_decidua']
      Timber_all[Species %in% c('Pseudotsuga_menziesii','Abies_alba'),
                 Species2 := 'Other_softwood']
      
      Timber_all[Species2 == 'Quercus_sp', c('Prop_energy', 'Prop_indus', 'Prop_logs') := Wood_use_cast[Species == 'Oak', list(Energy_wood, Industrial_wood, Logs)] ]
      Timber_all[Species2 %in% c('Other_hardwood', 'Fagus_sylvatica'), c('Prop_energy', 'Prop_indus', 'Prop_logs') := Wood_use_cast[Species == 'Beech_other_hardwood', list(Energy_wood, Industrial_wood, Logs)] ]
      Timber_all[Species2 %in% c('Larix_decidua', 'Pinus_sylvestris'), c('Prop_energy', 'Prop_indus', 'Prop_logs') := Wood_use_cast[Species == 'Pine_larch', list(Energy_wood, Industrial_wood, Logs)] ]
      Timber_all[Species2 %in% c('Other_softwood', 'Picea_abies'), c('Prop_energy', 'Prop_indus', 'Prop_logs') := Wood_use_cast[Species == 'Spruce_fir_Douglas_other_softwoods', list(Energy_wood, Industrial_wood, Logs)] ]
      
      Timber_all[is.na(Species2), Species2 := 'Unused' ]
      
      # Add firewood prices based on https://fbg-amberg.de/brennholz/brennholzboerse
      Timber_all[Species2 %in% c('Fagus_sylvatica', 'Quercus_sp'), Price_firewood := 54 ]
      Timber_all[Species2 == 'Other_hardwood', Price_firewood := 50.5 ]
      Timber_all[Species2 %in% c('Other_softwood', 'Picea_abies', 'Larix_decidua', 'Pinus_sylvestris'), Price_firewood := 40]
      
      # For simplicity we consider the price for industrial wood is the same for all species
      Timber_all[Species2 != "Unused", Price_industry :=  40 ]
      
      # Then we calculate the price by plot by species based on the size of the tree
      Mean_dbh <- merge.data.table(merge.data.table(Forest_structure[MTS != "X", list(d50 = max(MTS_d50, MTS_d100,  MTS_dg  , na.rm = T)), by = list(Plot = Plot, "Species" = MTS)],
                                                    Forest_structure[ATS1 != "X", list(d50 = max(ATS1_d50,ATS1_d100, ATS1_dg, na.rm = T)),  by = list(Plot = Plot, "Species" = ATS1)],   all = TRUE),
                                                    Forest_structure[ATS2 != "X", list(d50 = max(ATS2_d50,ATS2_d100, ATS2_dg, na.rm = T)),  by = list(Plot = Plot, "Species" = ATS2)],
                                                     all = TRUE
                                                   )
      Mean_dbh$Species <- dplyr::recode(Mean_dbh$Species,
                                         "Pa"  = "Picea_abies",
                                         "Fs"  = "Fagus_sylvatica",
                                         "oHS" = "Other_hardwood",
                                         "Ps"  = "Pinus_sylvestris",
                                         "oSS" = "Other_softwood",
                                         "Qs"  = "Quercus_sp",
                                         "Ld"  = "Larix_decidua"
                                       )
      Dbh_based_price <- merge.data.table(Mean_dbh, Timber_price, by = "Species", all = TRUE)[!is.na(Plot), ]
      Dbh_based_price[, Price_timber := ifelse(d50 < 10, NA, 
       ifelse(d50 < 25, 40, # If too small same as industry wood
             ifelse(d50 < 30, max(from25, Industrial, na.rm = T),
               ifelse(d50 < 35, max(from30, Industrial, na.rm = T),
                 ifelse(d50 < 40, max(from35, Industrial, na.rm = T),
                   ifelse(d50 < 50, max(from40, Industrial, na.rm = T),
                     ifelse(d50 < 60, max(from50, Industrial, na.rm = T),
                       max(from60, Industrial, na.rm = T)
                     )
                   )
                 )
               )
             )
         )
      ), by = 1:nrow(Dbh_based_price)]
      
      # Merge the timber price with the main dataset
      Timber_all = merge.data.table(Timber_all, Dbh_based_price[,list(Species2 = Species, Plot, Price_timber) ], by = c('Species2', 'Plot'), all = T)
      
      # Now we calculate, for each species and each plot, the standing volume-, increment-weighted price for industrial and timber wood, firewood increment
      
      Timber_all[, c('Total_timber_volume', 'Total_timber_increment', 'Total_firewood_increment') := list(Timber_volume*Prop_indus*Prop_indus/100 + Timber_volume*Prop_logs*Price_timber/100,
                                                                                                          Timber_Prod_weighted*Prop_indus*Prop_indus/100 + Timber_Prod_weighted*Prop_logs*Price_timber/100,
                                                                                                          Timber_Prod_weighted*Prop_energy*Price_firewood/100)]
      # MAI is in cubic meter per hectare and year so can directly calculate by prices bu cubic meter
      
      
      ### We also need t have wood densities to measure C stocks
      # Densities (kg/m3) based on https://www.icp-forests.org/pdf/TRLII2003.pdf. 
       # Species with #* have no data so were given average broadleave/conifer densities or of another species in the same genus
       densities = list( "Abies_alba"              = 410,
                         "Acer_campestre"         = 590,
                         "Acer_platanoides"        = 590,
                         "Acer_pseudoplatanus"   = 640,
                         "Acer_spec"              = 590,
                         "Aesculus_hippocastanum"  = 595, #*
                         "Alnus_spec"             = 510,
                         "Betula_pendula"          = 610,
                         "Betula_spec"           = 610,#*
                         "Carpinus_betulus"       = 790,
                         "Carya_ovata"            = 595, #*
                         "Fagus_sylvatica"       = 680,
                         "Fraxinus_excelsior"     = 600,
                         "Larix_decidua"          = 550,
                         "Larix_spec"             = 550,#*
                         "Malus_sylvestris"       = 700,#*
                         "Picea_abies"           = 400,
                         "Pinus_sylvestris"       = 490,
                         "Populus_nigra"          = 410,
                         "Populus_tremula"        = 410,
                         "Prunus_avium"           = 550,
                         "Prunus_serotina"       = 550,
                         "Pseudotsuga_menziesii"  = 470,
                         "Pyrus_pyraster"         = 700,#*
                         "Quercus_spec"           = 600,
                         "Robinia_pseudoacacia"   = 740,
                         "Salix_caprea"          = 330,
                         "Salix_spec"             = 330,#*
                         "Sorbus_aria"         = 730,
                         "Sorbus_aucuparia"       = 730,
                         "Sorbus_torminalis"      = 730,
                         "Tilia_cordata"         = 490,
                         "Tilia_platyphyllos"     = 490,
                         "Tilia_spec"            = 490,#*
                         "Ulmus_glabra"           = 640,
                         "Ulmus_spec"  = 640#*
       )
       Densities_df = data.table('Species' = names(densities), 'Density' = as.numeric(densities))
       # Merge with main dataset
       Timber_all = merge.data.table(Timber_all, Densities_df, by = 'Species', all = T)
       # C is approx 50% of the biomass. Volume is in m3 ha-1 and density is in kg/m3 so we get kg/ha.
       # Soil C is in kg/m2 so we need to convert it by a factor 10000
       Timber_all[, Tree_C := ((VOL*Density)/2)/ 10000]

       ### Now we can calculate plot-level values by summing each species' value
       Timber_data_by_plot = Timber_all[, list('Tree_C' = sum(Tree_C),
                                               'Firewood' = sum(Total_firewood_increment),
                                               'Timber_increment' = sum(Total_timber_increment),
                                               'Timber_volume' = sum(Total_timber_volume),
                                               'Uniqueness' = sum(BA[grepl('Carpinus', Species) | Species== 'Fagus_sylvatica'], na.rm = T)/sum(BA, na.rm = T)), 
                                        by = 'Plot']
      
    # Previous calculation 
    # # We need to weight timber price based on the average DBH of each species.
    # Forest_structure[, Plot := EP]
    # Mean_dbh <- merge.data.table(merge.data.table(Forest_structure[, list(d100 = mean(MTS_dg)), by = list(Plot = Plot, "Species" = MTS)],
    #   Forest_structure[, list(d100 = mean(ATS1_dg)), by = list(Plot = Plot, "Species" = ATS1)],
    #   all = TRUE
    # ),
    # Forest_structure[, list(d100 = mean(ATS2_dg)), by = list(Plot = Plot, "Species" = ATS2)],
    # all = TRUE
    # )[Species != "X", ]
    # Mean_dbh$Species <- dplyr::recode(Mean_dbh$Species,
    #   "Pa" = "Picea_abies",
    #   "Fs" = "Fagus_sylvatica",
    #   "oHS" = "Hardwood",
    #   "Ps" = "Pinus_sylvestris",
    #   "oSS" = "Softwood",
    #   "Qs" = "Quercus_spec",
    #   "Ld" = "Larix_decidua"
    # )
    # Dbh_based_price <- merge.data.table(Mean_dbh, Timber_price, by = "Species", all = TRUE)[!is.na(Plot), ]
    # Dbh_based_price[, Price_timber := ifelse(d100 < 10, NA,
    #   ifelse(d100 < 15, Industrial,
    #     ifelse(d100 < 20, max(from15, Industrial, na.rm = T),
    #       ifelse(d100 < 25, max(from20, Industrial, na.rm = T),
    #         ifelse(d100 < 30, max(from25, Industrial, na.rm = T),
    #           ifelse(d100 < 35, max(from30, Industrial, na.rm = T),
    #             ifelse(d100 < 40, max(from35, Industrial, na.rm = T),
    #               ifelse(d100 < 50, max(from40, Industrial, na.rm = T),
    #                 ifelse(d100 < 60, max(from50, Industrial, na.rm = T),
    #                   max(from60, Industrial, na.rm = T)
    #                 )
    #               )
    #             )
    #           )
    #         )
    #       )
    #     )
    #   )
    # ), by = 1:nrow(Dbh_based_price)]
    # 
    # 
    # ### Measure tree C stock
    # # Densities (kg/m3) based on https://www.icp-forests.org/pdf/TRLII2003.pdf. 
    # # Species with #* have no data so were given average broadleave/conifer densities or of another species in the same genus
    # densities = list( "Abies_alba"              = 410,
    #                   "Acer_campestre"         = 590,
    #                   "Acer_platanoides"        = 590,
    #                   "Acer_pseudoplatanus"   = 640,
    #                   "Acer_spec"              = 590,
    #                   "Aesculus_hippocastanum"  = 595, #*
    #                   "Alnus_spec"             = 510,
    #                   "Betula_pendula"          = 610,
    #                   "Betula_spec"           = 610,#*
    #                   "Carpinus_betulus"       = 790,
    #                   "Carya_ovata"            = 595, #*
    #                   "Fagus_sylvatica"       = 680,
    #                   "Fraxinus_excelsior"     = 600,
    #                   "Larix_decidua"          = 550,
    #                   "Larix_spec"             = 550,#*
    #                   "Malus_sylvestris"       = 700,#*
    #                   "Picea_abies"           = 400,
    #                   "Pinus_sylvestris"       = 490,
    #                   "Populus_nigra"          = 410,
    #                   "Populus_tremula"        = 410,
    #                   "Prunus_avium"           = 550,
    #                   "Prunus_serotina"       = 550,
    #                   "Pseudotsuga_menziesii"  = 470,
    #                   "Pyrus_pyraster"         = 700,#*
    #                   "Quercus_spec"           = 600,
    #                   "Robinia_pseudoacacia"   = 740,
    #                   "Salix_caprea"          = 330,
    #                   "Salix_spec"             = 330,#*
    #                   "Sorbus_aria"         = 730,
    #                   "Sorbus_aucuparia"       = 730,
    #                   "Sorbus_torminalis"      = 730,
    #                   "Tilia_cordata"         = 490,
    #                   "Tilia_platyphyllos"     = 490,
    #                   "Tilia_spec"            = 490,#*
    #                   "Ulmus_glabra"           = 640,
    #                   "Ulmus_spec"  = 640#*
    # )
    # # Turn it into a dataframe to merge later
    # Densities_df = data.table('Species' = names(densities), 'Density' = as.numeric(densities))
    # 
    # # Calculate volume per species per plot
    # Volume_per_species_per_plot = Timber_prod[, list(VOL = sum(VOL)), by = list(Species, 'Plot' = EP)]
    # # Merge with density
    # Volume_per_species_per_plot = merge.data.table(Volume_per_species_per_plot, Densities_df, by = 'Species')
    # 
    # # Calculate corresponding biomass and C
    # # Volume is in m3 ha-1 and density is in kg/m3 so we get kg/ha
    # # C is approx 50% of the biomass
    # Volume_per_species_per_plot[, Tree_C := (VOL*Density)/2]
    # # Total C and Biomass per plot
    # Tree_C_per_plot = Volume_per_species_per_plot[, list(Trees_C_storage = sum(Tree_C)), by = Plot]
    # 
    # # Merge all the data into one single dataframe
    # Timber_data <- merge.data.table(Timber_prod, Dbh_based_price[, list(Species, Plot, "Price_timber" = Price_timber, "Price_industry" = Industrial, "Price_firewood" = Firewood)], by = c("Species", "Plot"), all.x = TRUE)

    # # Note: I calculate separately market value and volume here, because the wood volume and biomass will be corrected for environment but not the market value (i.e. tot timber production)
    # # I hypothesise that 80% of the marketable species can be used as timber and the rest is used for firewood?
    # Timber_data_plot <- Timber_data[, list(
    #   Wood_volume = sum(VOL), # Total wood volume in m3/ha
    #   Timber_volume = 0.8 * sum(VOL[!is.na(Price_timber)]), # Wood volume of marketable species in m3/ha
    #   Firewood_volume = sum(VOL) - 0.8 * sum(VOL[!is.na(Price_timber)]),
    #   Market_value = weighted.mean(Price_timber, VOL, na.rm = T), # Plot-specific market value, weighted by the volume of each wood
    #   Uniqueness_hornbeam = sum(grepl("Carpinus", Species)), # Proportion of hornbeam
    #   Uniqueness_main_fagus = sum(Species[BA == max(BA)] == "Fagus_sylvatica") # Is Fagus the main species?
    # ),
    # by = Plot
    # ][order(Plot), ]
    # Timber_data_plot[is.na(Market_value), Market_value := 0]
 
    # Timber_data_plot = merge.data.table(Timber_data_plot, Tree_C_per_plot, by = 'Plot')
      
      # Merge
      ES_forests <- merge.data.table(ES_forests, Openness_forests[, c("Plot", "openness_mean")])
      ES_forests <- merge.data.table(ES_forests, Edible_charismatic_richness)
      ES_forests <- merge.data.table(ES_forests, Moss_cover[, c("Cover_bryophytes", "Plot")], all.x = T)
      ES_forests <- merge.data.table(ES_forests, Timber_data_by_plot[, c(
        "Plot", "Timber_volume", 'Timber_increment', 'Firewood',  'Tree_C', 'Uniqueness'
      )])
      
      ES_forests <- merge.data.table(ES_forests, Forest_veg_charac)
      
   #   if (correct_tree_C == TRUE){
    #  ES_forests[, Trees.C_storage := Trees_C_storage*(1-Firewood_volume/Wood_volume)*5/4]} # + 25% C for tree roots
      
      ES_forests <- merge.data.table(ES_forests, Forest_classif[, c("Plot", "Classif")])
      ES_forests <- merge.data.table(ES_forests, Acoustic_diversity_by_plot[, .SD, .SDcols = c("ADI", "NDSI", "Plot")], all.x = T)

      # ES_forests[, mean(Timber_volume), by = c("Classif", "Region")]
      # ES_forests[, mean(Market_value), by = c("Classif", "Region")]
      # ES_forests[, mean(Market_value * Timber_volume), by = c("Classif", "Region")]

      # Fill missing values
      ES_forests <- ES_forests[, mice::complete(mice(.SD, m = 10, seed = 101)), .SDcols = colnames(ES_forests)[!(colnames(ES_forests) %in% c("Exploratory"))], by = Exploratory]
      

      
      #### 2. Grassland ####
      ES_grasslands <- read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Grassland_data/ES_data.xlsx")
      ES_grasslands <- data.table(ES_grasslands[, colnames(ES_grasslands) != "Org_C_stock_maybe2006?"])
      ES_grasslands <- data.table(merge(ES_grasslands, Acoustic_diversity_by_plot[, .SD, .SDcols = c("ADI", "NDSI", "Plot")], by = "Plot", all.x = TRUE))

      # Cover of edible and charismatic species
      ES_grasslands <- merge.data.table(ES_grasslands[, c("Plot", "Exploratory", "Total_flower_cover", "Productivity", "ADI", "NDSI", "butterfly_abundance", "Soil.C.stock_2011")],
        Edible_charismatic_richness,
        all.x = T
      )

      # Impute missing values
      ES_grasslands <- ES_grasslands[, mice::complete(mice(.SD, m = 10, seed = 101)), .SDcols = colnames(ES_grasslands)[colnames(ES_grasslands) != "Exploratory"], by = Exploratory]
      ES_grasslands$Uniqueness_juniperus <- as.numeric(ES_grasslands$Uniqueness_juniperus)
      ES_grasslands <- merge.data.table(ES_grasslands, Classif)

      #### 3. Crops ####
      ES_crops <- data.table(Crop_classif)

      ## Crop plant communities simulations ##
      arable_weeds <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_weeds_to_use.txt")
      arable_weeds[, Species := Species_correct]
      arable_birds = fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_birds_to_use.txt")
      
      Abundance_crops <- data.table(Crop_classif[, c("Plot", "LU", "Region")])
      
      simulate_crop_weeds <- function(species_table, community_type = FALSE) {
        if (community_type == TRUE){
          com_type = sample(species_table$Community_level_code, 1)
          species_table = species_table[Community_level_code == com_type,]
          print(nrow(species_table))
        }
        
        if (is.null(species_table$Cover)) {species_table$Cover = 1 }
        
        species_table[, c('Cover', 'Frequency') := list(as.numeric(Cover), as.numeric(Frequency))]
        species_table2 = species_table[, list(Cover = mean(Cover), Frequency = mean(Frequency))
                                      , by = Species]
        
        species_table2[, tmp := sample(1:100, nrow(species_table2), replace = TRUE)]
        species_table2[, keep := tmp < Frequency]
        cover <- species_table2[keep == TRUE, Cover]
        names(cover) <- species_table2[keep == TRUE, Species]
        return(cover)
      }
      for (i in 1:600) {
        weeds <- simulate_crop_weeds(arable_weeds)
        birds <- simulate_crop_weeds(arable_birds)
        for (wi in 1:length(weeds)){
          Abundance_crops[i, (names(weeds[wi])) := wi]}
        for (wi in 1:length(birds)){
          Abundance_crops[i, (names(birds[wi])) := wi]}
      }
      
      Abundance_crops <- melt.data.table(Abundance_crops, id.vars = c("Plot", "LU", "Region"), variable.name = "Species", value.name = "value")
      Abundance_crops[Species %in% arable_weeds$Species, Group_broad := "plant"]
      Abundance_crops[Species %in% arable_birds$Species, Group_broad := "bird"]
      
      Abundance_crops[is.na(value), value := 0]

      Abundance_crops[, Species := gsub(" ", "_", Species)]

      #### 4. Abundances ####
      All_abundances <- rbind(
        Abundance_grasslands_forests[, c("Species", "Plot", "Group_broad", "value", "Region")],
        Abundance_crops[, c("Species", "Plot", "Group_broad", "value", "Region")]
      )

      # Corrections for species names
      all_sp <- unique(All_abundances$Species)
      all_genera <- unique(sapply(all_sp, function(x) {
        unlist(strsplit(x, "_"))[[1]]
      }))

      All_abundances[, Species := dplyr::recode(Species,
        "Achillea_millefolium" = "Achillea_millefolium_aggr.",
        "Alchemilla_sp" = "Alchemilla_vulgaris_aggr.",
        "Bromus_horderaceus" = "Bromus_hordeaceus_aggr.incl_B_commutatus",
        "Capsella_bursa_pastoris" = "Capsella_bursa-pastoris",
        "Carex_muricata_aggr" = "Carex_muricata_aggr.",
        "Euphrasia_sp_cf" = "Euphrasia_rostkoviana_aggr.",
        "Festuca_rubra" = "Festuca_rubra_aggr.",
        "Galium_mollugo_agg" = "Galium_mollugo_aggr.",
        "Hordeum_vulgare" = "Hordeum_vulgare_aggr.",
        "Leucanthemum_vulgare_agg" = "Leucanthemum_vulgare_aggr.",
        "Oenothera_biennis" = "Oenothera_biennis_agg",
        "Persicaria_lapathifolium" = "Persicaria_lapathifolia",
        "Phleum_pratense" = "Phleum_pratense_aggr.",
        "Platanthera_bifolia" = "Platanthera_bifolia_agg",
        "Poa_pratensis_agg" = "Poa_pratensis_aggr.",
        "Primula_veris" = "Primula_elatior_veris_agg",
        "Ranunculus_auricomus" = "Ranunculus_auricomus_agg",
        "Rosa_canina_agg" = "Rosa_canina_aggr.",
        "Trifolium_cf_montanum" = "Trifolium_montanum",
        "Vicia_sativa" = "Vicia_sativa_aggr.",
        "Vicia_tetrasperma" = "Vicia_tetrasperma_aggr.",
        "Vicia_cracca" = "Vicia_cracca_aggr.",
        "Bacidina_arnoldiana" = "Bacidina_arnoldiana_agg",
        "Cladonia_cf__ramulosa" = "Cladonia_ramulosa",
        "Cladonia_pyxidata_ssp__chlorophaea" = "Cladonia_pyxidata_agg",
        "Lecanora_cf__chlarotera" = "Lecanora_chlarotera",
        "Lecanora_cf__conizaeoides" = "Lecanora_conizaeoides",
        "Lecanora_cf__symmicta" = "Lecanora_symmicta",
        "Lepraria_cf__incana" = "Lepraria_incana",
        "Micarea_cf__prasina" = "Micarea_prasina",
        "Opegrapha_varia_var__varia" = "Opegrapha_varia",
        "Physcia_cf__aipolia" = "Physcia_aipolia",
        "Placynthiella_cf__dasaea" = "Placynthiella_dasaea",
        "Usnea_cf__filipendula" = "Usnea_filipendula",
        "Amblystegium_cf_subtile" = "Amblystegium_subtile",
        "Brachythecium_cf_starkei" = "Brachythecium_starkei",
        "Brachythecium_cf_velutinum" = "Brachythecium_velutinum",
        "Bryum_capillare" = "Bryum_capillare_agg",
        "Orthotrichum_cf_affine" = "Orthotrichum_affine",
        "Plagiomnium_affine" = "Plagiomnium_affine_agg"
      )]
      All_abundances <- All_abundances[!(Species %in% c(
        "Unknown_species", "Asteraceae sp", "Caryophyllaceae sp", "Brassicaceae_sp", "Cf_eurhynchium",
        "Cf_platygyrium_repens", "Cf_pohlia", "Cf_ptychodium", "Orchidacea_sp"
      )), ]

      All_abundances_aggr <- All_abundances[, list(value = sum(as.numeric(value), na.rm = T)), by = c("Species", "Plot", "Group_broad")]
      All_abundances_aggr <- merge.data.table(Classif, All_abundances_aggr, all = T)

      all_plants <- as.character(unique(All_abundances_aggr[Group_broad == "plant", ]$Species))
      all_birds <- as.character(unique(All_abundances_aggr[Group_broad == "bird", ]$Species))

      All_abundances_cast <- data.table(dcast(All_abundances_aggr, Plot + Region + LU + Classif ~ Species, value.var = "value", fill = 0))

      fwrite(All_abundances_cast[, .SD, .SDcols = c("Plot", all_plants)], "data/Plants_abundance.csv")
      fwrite(All_abundances_cast[, .SD, .SDcols = c("Plot", most_charismatic_birds)], "data/Birds_charism_abundance.csv")
      fwrite(All_abundances_cast[, .SD, .SDcols = c("Plot", all_birds)], "data/Birds_abundance.csv")


      #### Finish Crop output###
      # NB: crops have simulated plant communities
      
      Edible_charismatic_crops <- Abundance_crops[, list(
        Plant_richness = specnumber(value[Species %in% all_plants]),
        Bird_richness = specnumber(value[Species %in% all_birds]),
        Fungi_richness = 0,
        Cover_edible = sum(value[Species %in% all_edible_plants]) + sum(value[Species %in% top_edible_plants]),
        Charismatic_plants = sum(value[Species %in% charismatic_plants])
      ), by = "Plot"]
      ES_crops <- merge.data.table(ES_crops, Edible_charismatic_crops, by = "Plot")

      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      ##### ENVIRONMENTAL CORECTION #####
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      # Soil and TWI data
      All_soil_data <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/GP\ Soils\ and\ Carbon\ stocks\ master\ sheet\ with\ terrain.xlsx", sheet = 1))
      All_soil_data[rw == 3515825 & hw == 5360120 & id == "A2277", PlotID := "A2277"]
      All_soil_data <- merge(All_soil_data, Plot_id_matching[ActivePlot == "yes", c("EP_PlotID", "PlotID")], by = "PlotID")
      All_soil_data$Core_depth <- All_soil_data[, "Core depth (cm)"]
      All_soil_data_f <- All_soil_data[, list(Exploratory,
        Plot = ifelse(nchar(EP_PlotID) == 5, EP_PlotID, paste(substr(EP_PlotID, 1, 3), 0, substr(EP_PlotID, 4, 4), sep = "")),
        LU = substr(EP_PlotID, 3, 3),
        Core_depth = as.numeric(Core_depth),
        elevation = as.numeric(elevation),
        TWI = as.numeric(TWI)
      )][order(LU, Plot), ]
      Texture <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/14686_MinSoil_2011_Soil_Texture_1.9.13/14686.txt")
      Texture[, Plot := ifelse(nchar(EP_Plotid) == 5, EP_Plotid, paste(substr(EP_Plotid, 1, 3), 0, substr(EP_Plotid, 4, 4), sep = ""))][order(Plot), ]
      Texture <- Texture[, list(prop_clay = mean((Clay) / (Clay + Fine_Silt + Medium_Silt + Coarse_Silt + Fine_Sand + Medium_Sand + Coarse_Sand), na.rm = T)), by = Plot]
      All_soil_data_f <- merge.data.table(All_soil_data_f, Texture)

      pH_2011 = fread('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/14447_8_Dataset/14447_8_data.csv')
      pH_2011[, c("Plot", "pH_2011") := list(
        ifelse(nchar(EP_Plotid) == 5, EP_Plotid, paste(substr(EP_Plotid, 1, 3), 0, substr(EP_Plotid, 4, 4), sep = "")),
       (pH_1 +pH_2)/2), by = EP_Plotid]
      
      pH_2014 = fread('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/19067_3_Dataset/19067_3_data.csv')
      pH_2014[, c("Plot", "pH_2014") := list(
        ifelse(nchar(EP_Plotid) == 5, EP_Plotid, paste(substr(EP_Plotid, 1, 3), 0, substr(EP_Plotid, 4, 4), sep = "")),
        (pH_1 +pH_2)/2), by = EP_Plotid]
      
      CN_2011 = fread('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/17086_3_Dataset/17086_3_data.csv')
      CN_2011[, c("Plot", "Cstock_2011") := list(
        ifelse(nchar(EP_Plotid) == 5, EP_Plotid, paste(substr(EP_Plotid, 1, 3), 0, substr(EP_Plotid, 4, 4), sep = "")),
        as.numeric(OC_stock)
      )]
      CN_2011[Cstock_2011 < 1000, Cstock_2011 := NA]
      
      CN_2014 <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/20266_MinSoil_2014_CN_stocks_2.1.4/20266.xlsx"))
      CN_2014[, c("Plot", "Cstock_2014") := list(
        ifelse(nchar(EP_Plotid) == 5, EP_Plotid, paste(substr(EP_Plotid, 1, 3), 0, substr(EP_Plotid, 4, 4), sep = "")),
        as.numeric(OC_stock)
      )]
      
      Forest_grasslands <- merge.data.table(CN_2011[, .SD, .SDcols = c("Plot", "Cstock_2011")], CN_2014[, .SD, .SDcols = c("Plot", "Cstock_2014")], by = "Plot")
      Forest_grasslands <- merge.data.table(Forest_grasslands, pH_2011[, .SD, .SDcols = c("Plot", "pH_2011")], by = "Plot")
      Forest_grasslands <- merge.data.table(Forest_grasslands, pH_2014[, .SD, .SDcols = c("Plot", "pH_2014")], by = "Plot")
      
      Forest_grasslands[, C.stock := mean(c(Cstock_2011, Cstock_2014), na.rm = T), by = 1:300]
      Forest_grasslands[, pH := mean(c(pH_2011, pH_2014), na.rm = T), by = 1:300]
      
      All_soil_data_f <- merge.data.table(All_soil_data_f, Forest_grasslands[, .SD, .SDcols = c("Plot", "pH", "C.stock")], by = "Plot")

      Temp =   fread("/Users/Margot/Desktop/Research/Senckenberg/Data/Environment/Ta_200_2008_2018_3a7aa69b37636254/plots.csv")
      Precip =   fread("/Users/Margot/Desktop/Research/Senckenberg/Data/Environment/precipitation_radolan_95a5c5f55a798133/plots.csv")
      Climate_data <- merge.data.table(Temp[, plotID := gsub('f', '', plotID)], Precip, by = c('plotID', 'datetime'))
      Climate_data[, Plot := plotID]
      Climate <- Climate_data[datetime>=2008, list("Mean_Temp" = mean(Ta_200, na.rm = T), "Mean_precip" = mean(precipitation_radolan, na.rm = T)), by = list("Plot" = plotID)]

      env_final <- merge.data.table(All_soil_data_f, Climate, all.x = T)
      env_final[, colnames(env_final) := mice::complete(mice(.SD, m = 10, seed = 101)), .SDcols = colnames(env_final), by = c("Exploratory", "LU")]

      # Adds the C stocks to service dataframes
      ES_forests <- merge.data.table(ES_forests, env_final[, .SD, .SDcols = c("Plot", "C.stock")])
      ES_grasslands <- merge.data.table(ES_grasslands, env_final[, .SD, .SDcols = c("Plot", "C.stock")], by = "Plot")


      ### Environmental correction ###
      ES_grasslands[, c("sqrtTotal_flower_cover", "sqrtbutterfly_abundance", "sqrtCover_edible", "sqrtCharismatic_plants") :=
        lapply(.SD, sqrt), .SDcols = c("Total_flower_cover", "butterfly_abundance", "Cover_edible", "Charismatic_plants")]
      ES_forests[, c("sqrtCover_edible", "sqrtCharismatic_plants", "sqrtCover_bryophytes") :=
        lapply(.SD, sqrt), .SDcols = c("Cover_edible", "Charismatic_plants", "Cover_bryophytes")]

      
      if (by_region == TRUE) {
        ES_grasslands_corr <- cbind(
          ES_grasslands[, .SD, .SDcols = c("Plot", "Classif", "Uniqueness_juniperus", "Plant_richness", "Bird_richness", "Fungi_richness")],
          ES_grasslands[,
                        lapply(.SD, function(x) {
                          x <- as.numeric(x)
                          r <- .BY[[1]]
                          mod <- lm(x ~ ., data = env_final[Exploratory == r & LU == "G", c("Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")])
                          pred_val <- predict(mod, env_final[Exploratory == r & LU == "G", lapply(.SD, mean), .SDcols = c("Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")]) + residuals(mod)
                          print(length(pred_val))
                          return(pred_val)
                        }),
                        by = Exploratory,
                        .SDcols = c("Total_flower_cover", "sqrtTotal_flower_cover", "sqrtbutterfly_abundance", "sqrtCover_edible", "sqrtCharismatic_plants", "ADI", "NDSI", "butterfly_abundance", "Productivity", "C.stock", "Cover_edible", "Charismatic_plants", "Plant_richness")
          ]
        )
        ES_forests_corr <- cbind(
          ES_forests[, list(Plot, Classif, Plant_richness, Bird_richness, Fungi_richness)],
          ES_forests[,
                     lapply(.SD, function(x) {
                       r <- .BY[[1]]
                       x <- as.numeric(x)
                       mod <- lm(x ~ ., data = env_final[Exploratory == r & LU == "W", c("Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")])
                       pred_val <- predict(mod, env_final[Exploratory == r & LU == "W", lapply(.SD, mean), .SDcols = c("Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")]) + residuals(mod)
                       return(pred_val)
                     }),
                     by = Exploratory,
                     .SDcols = c('Shrub_cover_sqrt', "sqrtCover_edible", 'Uniqueness', "sqrtCharismatic_plants", "C.stock", "sqrtCover_bryophytes", "Cover_bryophytes", "ADI",  "openness_mean", "Cover_edible", "Charismatic_plants", "Plant_richness", "Bird_richness", "Timber_volume",'Timber_increment', "Firewood", "Trees_C_storage")
          ]
        )
      }
      if (by_region == FALSE) {
        ES_grasslands_corr <- cbind(
          ES_grasslands[, .SD, .SDcols = c("Plot", "Classif", "Uniqueness_juniperus", "Plant_richness", "Bird_richness", "Fungi_richness")],
          ES_grasslands[,
                        lapply(.SD, function(x) {
                          x <- as.numeric(x)
                          r <- .BY[[1]]
                          mod <- lm(x ~ ., data = env_final[LU == "G", c("Exploratory", "Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")])
                          pred_val <- predict(mod, cbind(env_final[LU == "G", ]$Exploratory, env_final[LU == "G", lapply(.SD, mean), .SDcols = c("Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")])) + residuals(mod)
                          print(length(pred_val))
                          return(pred_val)
                        }),
                        .SDcols = c("Total_flower_cover", "sqrtTotal_flower_cover", "sqrtbutterfly_abundance", "sqrtCover_edible", "sqrtCharismatic_plants", "ADI", "NDSI", "butterfly_abundance", "Productivity", "C.stock", "Cover_edible", "Charismatic_plants", "Plant_richness")
          ]
        )
        ES_forests_corr <- cbind(
          ES_forests[, list(Plot,  Classif,  Plant_richness, Bird_richness, Fungi_richness)],
          ES_forests[,
                     lapply(.SD, function(x) {
                       r <- .BY[[1]]
                       x <- as.numeric(x)
                       mod <- lm(x ~ ., data = env_final[LU == "W", c("Exploratory", "Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")])
                       pred_val <- predict(mod, cbind(env_final[LU == "W", ]$Exploratory, env_final[LU == "W", lapply(.SD, mean), .SDcols = c("Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")])) + residuals(mod)
                       return(pred_val)
                     }),
                     .SDcols =  c('Shrub_cover_sqrt', "sqrtCover_edible", 'Uniqueness', "sqrtCharismatic_plants", "C.stock", "sqrtCover_bryophytes", "Cover_bryophytes", "ADI",  "openness_mean", "Cover_edible", "Charismatic_plants", "Plant_richness", "Bird_richness", "Timber_volume",'Timber_increment', "Firewood", "Trees_C_storage")
          ]
        )
      }

      ES_grasslands[, lapply(.SD, function(x) {
        list(min(x), mean(x), max(x))
      }), .SDcols = c("Total_flower_cover", "ADI", "NDSI", "butterfly_abundance", "Productivity", "C.stock", "Cover_edible", "Charismatic_plants", "Plant_richness")]
      ES_grasslands_corr[, lapply(.SD, function(x) {
        list(min(x, na.rm = TRUE), mean(x), max(x))
      }), .SDcols = c("sqrtTotal_flower_cover", "sqrtbutterfly_abundance", "sqrtCover_edible", "sqrtCharismatic_plants", "Total_flower_cover", "ADI", "NDSI", "butterfly_abundance", "Productivity", "C.stock", "Cover_edible", "Charismatic_plants", "Plant_richness")]
      ES_grasslands_corr[, c("sqrtCover_edible", "sqrtCharismatic_plants", "sqrtTotal_flower_cover", "sqrtbutterfly_abundance") := lapply(.SD, function(x) {
        x[x < 0] <- 0
        return(x)
      }), .SDcols = c("sqrtCover_edible", "sqrtCharismatic_plants", "sqrtTotal_flower_cover", "sqrtbutterfly_abundance")]

      ES_forests[, lapply(.SD, function(x) {
        list(min(x), mean(x), max(x))
      }), .SDcols = c( 'Shrub_cover_sqrt', "sqrtCover_edible", 'Uniqueness', "sqrtCharismatic_plants", "C.stock", "sqrtCover_bryophytes", "Cover_bryophytes", "ADI",  "openness_mean", "Cover_edible", "Charismatic_plants", "Plant_richness", "Bird_richness", "Timber_volume",'Timber_increment', "Firewood", "Trees_C_storage","Plant_richness", "Bird_richness")]
      ES_forests_corr[, lapply(.SD, function(x) {
        list(min(x, na.rm = TRUE), mean(x), max(x))
      }), .SDcols = c('Shrub_cover_sqrt', "sqrtCover_edible", 'Uniqueness', "sqrtCharismatic_plants", "C.stock", "sqrtCover_bryophytes", "Cover_bryophytes", "ADI",  "openness_mean", "Cover_edible", "Charismatic_plants", "Plant_richness", "Bird_richness", "Timber_volume",'Timber_increment', "Firewood", "Trees_C_storage","Plant_richness", "Bird_richness")]


      # We consider that corrected values < 0 are = 0. This is a  safe assumption for all except Tree C storage which varies a bit more with the environment.
      ES_forests_corr[, c('Shrub_cover_sqrt', "sqrtCover_edible", 'Uniqueness', "sqrtCharismatic_plants", "C.stock", "sqrtCover_bryophytes", "Cover_bryophytes", "ADI",  "openness_mean", "Cover_edible", "Charismatic_plants",   "Timber_volume",'Timber_increment', "Firewood", "Trees_C_storage","Plant_richness", "Bird_richness") := lapply(.SD, function(x) {
        x[x < 0] <- 0
        return(x)
      }), .SDcols = c('Shrub_cover_sqrt', "sqrtCover_edible", 'Uniqueness', "sqrtCharismatic_plants", "C.stock", "sqrtCover_bryophytes", "Cover_bryophytes", "ADI",  "openness_mean", "Cover_edible", "Charismatic_plants",  "Timber_volume",'Timber_increment', "Firewood", "Trees_C_storage","Plant_richness", "Bird_richness")]


      if (env_corr == "env_corr") {
        ES_grasslands_use <- ES_grasslands_corr
        ES_forests_use <- ES_forests_corr
      }
      if (env_corr != "env_corr") {
        ES_grasslands_use <- ES_grasslands
        ES_forests_use <- ES_forests
      }

      # For later analyses I will need to separate datasets:
      # One will include plot-level ecosystem services
      # the other will have abundance data for plants and birds species for calculation of landscape gamma diversity

      # %%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      ##### PLOT-LEVEL SERVICES #####
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%% #

      ES_grasslands_use[, Region := substr(Plot, 1, 1)]
      ES_forests_use[, Region := substr(Plot, 1, 1)]

      #### Provisioning services ####
      ## Food/ fodder production ##
      # !!!!!!!!!! Problem with estimated productivity < 0. #As a quick fix now, I add the minimum to all values but won't work in the long-term
      ES_grasslands_use[, Productivity := Productivity]
      # ------ in grasslands
      ES_grasslands_use[, Production_livestock := Productivity * 12.3] # !!!! Need to check price / unit. Productivity is dt/ha, price is about 123€/t according to agrarheute
      ES_grasslands_use[, Production_food := 0]
      # ------ in forests:  No food
      ES_forests_use[, Production_food := 0]
      ES_forests_use[, Production_livestock := 0]
      # ------ in crops: corresponds to productivity of the given crop
      ES_crops[, Production_food := Productivity]
      ES_crops[, Production_livestock := 0]
      ES_crops[Crop_type %in% "Maize", c("Production_food", "Production_livestock") := list(0, Productivity * 0.6)]
      ES_crops[Crop_type %in% "Oilseed rape", Production_food := 0]
      ES_crops[Crop_type %in% "Alfalfa", c("Production_food", "Production_livestock") := list(0, Productivity)]

      ## Timber production ##
      ES_grasslands_use[, Production_timber := 0] # No timber in grasslands
      ES_forests_use[, Production_timber := (scale01(Timber_volume)+scale01(Timber_increment))/2] 
      ES_crops[, Production_timber := 0]

      ## Energy production ##
      # ------ in grasslands: no energy
      ES_grasslands_use[, Production_energy := 0]
      # ------ in forests:  No food
      ES_forests_use[, Production_energy := as.numeric(Firewood)]
      # ------ in crops
      ES_crops[, Production_energy := 0]
      ES_crops[Crop_type %in% "Maize", Production_energy := 0.4 * Productivity]
      ES_crops[Crop_type %in% "Oilseed rape", Production_energy := Productivity]


      #### Cultural services ####
      ## Aesthetic ##
      # Aesthetic is divided into two classes: naturalness and diversity
      # ------ in forests
      ES_forests_use[, c("Aesth_diversity_ADI") := ADI]

      if (by_region == TRUE) {
        ES_forests_use[, Aesthetic_naturalness := scale01(sqrtCover_bryophytes) + scale01(openness_mean), by = Region]
        #  ES_forests_use[, Aesthetic_naturalness := scale01(Aesthetic_naturalness), by = Region]
      }
      if (by_region == FALSE) {
        ES_forests_use[, Aesthetic_naturalness := scale01(sqrtCover_bryophytes) + scale01(openness_mean)]
        # ES_forests_use[, Aesthetic_naturalness := scale01(Aesthetic_naturalness)]
      }

      # ------ in grasslands
      ES_grasslands_use[, c("Aesth_diversity_ADI") := ADI]
      if (by_region == TRUE) {
        ES_grasslands_use[, Aesthetic_naturalness := scale01(sqrtTotal_flower_cover) + scale01(sqrtbutterfly_abundance) + scale01(NDSI), by = Region]
      }
      if (by_region == FALSE) {
        ES_grasslands_use[, Aesthetic_naturalness := scale01(sqrtTotal_flower_cover) + scale01(sqrtbutterfly_abundance) + scale01(NDSI)]
      }
      
      # ------ in crops: mean of 5% lowest lowest values
      if (by_region == TRUE) {
        ES_grasslands_use[, c('rankADI', 'rankNaturalness') := list(rank(Aesth_diversity_ADI), rank(Aesthetic_naturalness)), by = Region]

        Crop_ADI_Naturalness <- ES_grasslands_use[, list(ADI = mean(Aesth_diversity_ADI[rankADI <=3]),
                                                         Naturalness = mean(Aesthetic_naturalness[rankNaturalness <=3])), by = Region]
        ES_crops[, c('Aesth_diversity_ADI','Aesthetic_naturalness') :=
           sapply(Region, function(x) {
            Crop_ADI_Naturalness[Region == x, list(ADI, Naturalness)]}), by = 1:nrow(ES_crops)]
      }
      if (by_region == FALSE) {
        ES_grasslands_use[, c('rankADI', 'rankNaturalness') := list(rank(Aesth_diversity_ADI), rank(Aesthetic_naturalness))]
        
        Crop_ADI_Naturalness <- ES_grasslands_use[, list(ADI = mean(Aesth_diversity_ADI[rankADI <=3]),
                                                         Naturalness = mean(Aesthetic_naturalness[rankNaturalness <=3]))]
        ES_crops[, c('Aesth_diversity_ADI','Aesthetic_naturalness') := 
                     Crop_ADI_Naturalness[, list(ADI, Naturalness)]]
      }
      
      ## Regional ID ##
      # Aesthetic is divided into two classes: naturalness and diversity
      # ------ in forests
      ES_forests_use[, c("Aesth_uniqueness_charismatic_plants") := sqrtCharismatic_plants]
      ES_forests_use[, Reg_ID_habitat := Uniqueness]
      # ------ in grasslands
      ES_grasslands_use[, "Aesth_uniqueness_charismatic_plants" := sqrtCharismatic_plants]
      ES_grasslands_use[, Reg_ID_habitat := (Uniqueness_juniperus)]
      # ------ in crops
      ES_crops[, Aesth_uniqueness_charismatic_plants := sqrt(Charismatic_plants)]
      ES_crops[, Reg_ID_habitat := 0]

      ## Harvesting ##
      # ----- grasslands
      ES_grasslands_use[, Harvesting_plants := sqrtCover_edible]
      # ----- forests
      ES_forests_use[, Harvesting_plants := sqrtCover_edible]
      # ----- crops
      ES_crops[, Harvesting_plants := sqrt(Cover_edible)]

      ## Hunting ##
      # indicator: suitability of habitat (depending on forest type) and on tree size (dbh class based on SCH class)
      
      # ----- crops
      ES_crops[, Hunting_habitat_deer := 0]

      # ----- grasslands
      ES_grasslands_use[, Hunting_habitat_deer := 0]

      # ----- forests
      ES_forests_use[, shrub_cover_suitability :=
        as.numeric(cut(Shrub_cover_sqrt, c(min(Shrub_cover_sqrt), quantile( Shrub_cover_sqrt, c(0.33, 0.66)), max(Shrub_cover_sqrt)), include.lowest = TRUE, labels = c(3, 2, 1)))]
      ES_forests_use[, forest_type_suitability :=
        ifelse(Classif == "Forest_Deciduous", 1,
          ifelse(Classif == "Forest_Mixed", 2,
            ifelse(Classif == "Forest_Coniferous", 3, NA)
          )
        )]
      

      if (by_region == TRUE)  ES_forests_use[, Hunting_habitat_deer := scale01(forest_type_suitability) + scale01(shrub_cover_suitability), by = Region]
      if (by_region == FALSE) ES_forests_use[, Hunting_habitat_deer := scale01(forest_type_suitability) + scale01(shrub_cover_suitability)]

      #### Regulating services ####
      ## C stock ##
      # ----- grasslands
      ES_grasslands_use[, C_stock := as.numeric(C.stock)]
      # ----- forestshttps://docs.google.com/forms/d/e/1FAIpQLScjTFGAbJ6naZHV6G0s-ZYCI6NXTVRyuYmryCnPQ8ZntMAv9A/viewform?fbclid=IwAR3avDmXy52iwRAx9mXlcU53x4WgUqsyZtYaYF__ZrlUbbEsvp2jW9xMy7w
      ES_forests_use[, C_stock := (Trees_C_storage + C.stock)] 
      # ----- crops: Fill C stock as 80% of grassland values
      if (by_region == TRUE) {
        Crop_C_stock <- ES_grasslands_use[, list(C_stock = 0.8 * mean(C_stock, na.rm = T)), by = Region]
        ES_crops[LU == "Crop", C_stock := sapply(Region, function(x) {
          Crop_C_stock[Region == x, C_stock]
        }), by = .I]
      }
      if (by_region == FALSE) {
        ES_crops[LU == "Crop", C_stock := ES_grasslands_use[, list(C_stock = 0.72 * mean(C_stock, na.rm = T))]]
      }



      #### Putting all data together ####
      indicators_plot <- c(
        'Hunting_habitat_deer',
        "Production_food",
        "Production_livestock",
        "Production_energy",
        "Production_timber",
        "Harvesting_plants",
        "C_stock",
        "Aesthetic_naturalness", 
        "Aesth_diversity_ADI", # LU diversity calculated at landscape level
        # Conservation calculated at landscape level
        "Reg_ID_habitat", "Aesth_uniqueness_charismatic_plants" #' Reg_ID', birds added at landscape level
      )
      all_indicators <- c(
        "Ric_plants", "Ric_birds", "Aest_diversity_landscape", "Aesthuniqueness_charismatic_birds",
        "Harvesting_mushrooms",
        "Leisure_forests", "Leisure_grasslands",
        indicators_plot
      )
      no_indicators_plots <- length(indicators_plot)

      All_ES_data <- rbind(
        ES_grasslands_use[, .SD, .SDcols = c(indicators_plot, "Plant_richness", "Bird_richness", "Fungi_richness", "Plot")],
        ES_forests_use[, .SD, .SDcols = c(indicators_plot, "Plant_richness", "Bird_richness", "Fungi_richness", "Plot")],
        ES_crops[, .SD, .SDcols = c(indicators_plot, "Plant_richness", "Bird_richness", "Fungi_richness", "Plot")]
      )

      All_ES_data_classif <- merge.data.table(Classif,
        All_ES_data,
        by = "Plot", all = TRUE
      )

        All_ES_data_classif2 = copy(All_ES_data_classif)
        
      if (scale_within_land_use == TRUE) {
        # For richness (for plot-level analyses only) as well as cover of edible and charismatic plants, we rescale between 0 and 1 independently for forests and grasslands
        All_ES_data_classif2[, c("Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, as.numeric), .SDcols = c("Plant_richness", "Bird_richness", "Fungi_richness")]

        if (by_region == TRUE) {
          All_ES_data_classif2[LU == "Forest", c("Harvesting_plants",  "Aesth_uniqueness_charismatic_plants","Aesthetic_naturalness", "Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, scale01),
                                     .SDcols = c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants","Aesthetic_naturalness", "Plant_richness", "Bird_richness", "Fungi_richness"), by = Region]
          
          All_ES_data_classif2[LU != "Forest", c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Aesthetic_naturalness","Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, scale01),
                                     .SDcols = c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Aesthetic_naturalness","Plant_richness", "Bird_richness", "Fungi_richness"), by = Region]
          
        }
        if (by_region == FALSE) {
          All_ES_data_classif2[LU == "Forest", c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants","Aesthetic_naturalness", "Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, scale01),
                                      .SDcols = c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants","Aesthetic_naturalness", "Plant_richness", "Bird_richness", "Fungi_richness")]
          
          All_ES_data_classif2[LU != "Forest", c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Aesthetic_naturalness","Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, scale01),
                                     .SDcols = c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants","Aesthetic_naturalness", "Plant_richness", "Bird_richness", "Fungi_richness")]
          
        }
      }

      if (by_region == FALSE) {
        All_ES_data_classif2$Region <- "All"
      }
      fwrite(All_ES_data_classif2[, .SD, .SDcols = colnames(All_ES_data_classif2)[!(colnames(All_ES_data_classif2) %in% c('Plant_richness', 'Bird_richness', 'Fungi_richness'))]], 
             paste("data/All_ES_data_classif_", env_corr, "_region", by_region, "_scale_within", scale_within_land_use, ".csv", sep = ""))
    }
  }
}


# test
#a1 = melt(fread('data/All_ES_data_classif_env_corr_regionFALSE_scale_withinFALSE.csv'), id.vars = c('Plot', 'Region',        'LU',          'Classif',           'Classif2'))[, Data := "a1"]
#a2 = melt(fread('data/All_ES_data_classif__regionFALSE_scale_withinTRUE.csv'), id.vars = c('Plot', 'Region',        'LU',          'Classif',           'Classif2'))[, Data := "a2"]
#a3 = melt(fread('data/All_ES_data_classif_env_corr_regionFALSE_scale_withinTRUE.csv') , id.vars = c('Plot', 'Region',        'LU',          'Classif',           'Classif2'))[, Data := "a3"]#

#a4 = melt(fread('data/All_ES_data_classif__regionTRUE_scale_withinFALSE.csv'), id.vars = c('Plot', 'Region',        'LU',          'Classif',           'Classif2'))[, Data := "a4"]
#a6 = melt(fread('data/All_ES_data_classif__regionTRUE_scale_withinTRUE.csv'), id.vars = c('Plot', 'Region',        'LU',          'Classif',           'Classif2'))[, Data := "a6"]
#a5 = melt(fread('data/All_ES_data_classif_env_corr_regionTRUE_scale_withinFALSE.csv'), id.vars = c('Plot', 'Region',        'LU',          'Classif',           'Classif2'))[, Data := "a5"]
#a7 = melt(fread('data/All_ES_data_classif_env_corr_regionTRUE_scale_withinTRUE.csv'), id.vars = c('Plot', 'Region',        'LU',          'Classif',           'Classif2'))[, Data := "a7"]

#all = rbindlist(list(a1, a2, a3, a4, a5, a6, a7))
#all[, value := scale01(value), by = c('variable', 'Data' )]
#all_cast = dcast(all,  Plot + Region+ LU + Classif +  Classif2 + variable ~ Data, value.var = 'value')

#ggplot(all_cast[LU != 'Crop',], aes(a4, a5, color = Region, shape = LU)) + geom_point() + facet_wrap(~variable)


All_ES_plot <- melt.data.table(All_ES_data_classif, id.vars = c("Plot", "Region", "LU", "Classif", "Classif2"))
All_ES_plot <- rbind(All_ES_plot, All_ES_plot[LU == "Forest", ][, Classif := Classif2])
All_ES_plot[LU == "Crop", Classif := "Crop"]
All_ES_plot[, value := scale01(value), by = "variable"]
All_ES_plot[, Classif := factor(Classif, levels = c("Crop", "Grassland_high", "Grassland_medium", "Grassland_low", "Forest_Coniferous", "Forest_Mixed", "Forest_Deciduous", "Forest_even-aged", "Forest_uneven-aged"))]

All_ES_plot[, pretty_variables := dplyr::recode(variable,
                                         'Production_food' = "Food production",
                                         'Production_livestock' = "Livestock production",
                                         'Production_energy' = "Energy production",
                                         'Production_timber' = "Timber production",
                                         'Harvesting_plants' = "Foraging\n(edible plants)",
                                         'C_stock' = "Carbon stocks",
                                         'Hunting' = "Hunting",
                                         'Aesthetic_naturalness' = "Aesthetic\n(naturalness)",
                                         'Aesth_diversity_ADI' = "Aesthetic\n(acoustic diversity)",
                                         'Reg_ID_habitat' = "Regional identity\n(cultural habitat)",
                                         'Aesth_uniqueness_charismatic_plants' = "Aesthetic\n(charismatic plants)",
                                         'Plant_richness' = "Conservation\n(plant richness)",
                                         'Bird_richness' = "Conservation\n(bird richness)",
                                         'Fungi_richness' = "Foraging\n(edible fungi richness)",
                                         'Hunting_habitat_deer' = "Hunting\n(Suitability of forests\n for deers)"
                                         )]

All_ES_plot_copy = copy(All_ES_plot)
All_ES_plot_copy[LU != 'Forest' & variable %in% c('Hunting_habitat_deer', 'Production_timber'), value:=NA]
All_ES_plot_copy[LU != 'Crop' & variable %in% c('Production_food'), value:=NA]

All_ES_plot_copy[LU == 'Forest' & variable %in% c('Production_livestock'), value:=NA]
All_ES_plot_copy[LU == 'Grassland' & variable %in% c('Production_energy'), value:=NA]


gg_ES = ggplot(All_ES_plot_copy, aes(
  y = value, x = Classif, fill = Classif  , color = Classif
)) +
  geom_boxplot(position = position_dodge(width = 1), alpha = 0.5) +
  geom_jitter(alpha = 0.3, shape = 21, width = 0.2, size = 0.5, lwd = 0.2) +
  facet_grid(pretty_variables~Region) +
  #facet_wrap(~pretty_variables, ncol = 2) +
  theme_bw() +
  scale_fill_manual(
    values = c("#8ccde8", "#70a6ca", "#4d708a", "#c3d640", "#6c8b40", "#9cc947", "#ABE188", "#00A375", "#f48b30"),
    breaks = c("Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even-aged", "Forest_uneven-aged", "Crop")
  ) + 
  scale_color_manual(
    values = c("#8ccde8", "#70a6ca", "#4d708a", "#c3d640", "#6c8b40", "#9cc947", "#ABE188", "#00A375", "#f48b30"),
    breaks = c("Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even-aged", "Forest_uneven-aged", "Crop")
  ) +
  scale_x_discrete(breaks = c("Crop", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Coniferous", 
                              "Forest_Mixed",       "Forest_Deciduous",   "Forest_even-aged",   "Forest_uneven-aged"),
                   labels= c("Crop", "Grassland\nlow int.", "Grassland\nmedium int.", "Grassland\nhigh int.", 
                             "Forest\nConiferous", "Forest\nMixed", "Forest\nDeciduous",  "Forest\nEven-aged", "Forest\nUneven-aged" ))+
  
 # scale_color_manual(
#    values = c("darkslategray", "navyblue", "purple4"),
#    breaks = c("A", "H", "S")
#  ) +
  theme(legend.position = "bottom") +
  ylab('Service indicator value (scaled)') + xlab('Land-use type and managemend')+
  theme(legend.position = 'none')

ggsave(plot = gg_ES, '/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Results/GG_ES_reg.png', width = 10, height = 40)

#All_ES_data_classif2 <- copy(All_ES_data_classif_not_scaled)

# if (plot_level_tests = TRUE){
All_ES_data_classif2[, colnames(All_ES_data_classif2)[6:19] := lapply(.SD, as.numeric), .SDcols = colnames(All_ES_data_classif2)[6:19]]
All_ES_data_classif2[, colnames(All_ES_data_classif2)[c(6:19)] := lapply(.SD, function(x) {

  Max <- quantile(x, 0.975, na.rm = F)
  Min <- quantile(x, 0.025, na.rm = F)
  y <- (x - Min) / (Max - Min)
  y[y < 0] <- 0
  y[y > 1] <- 1
  return(y)
}), .SDcols = colnames(All_ES_data_classif)[c(6:19)], by = "Region"]

All_ES_scaled_MF <- cbind(
  All_ES_data_classif2[, c(1:5)],
  All_ES_data_classif2[, list(
    Ric = (Plant_richness + Bird_richness) / 2,
    Hunting = Hunting,
    Harvesting = (Harvesting_plants) / 2,
    Production_food = Production_food,
    Production_livestock = Production_livestock,
    Production_timber = Production_timber,
    Production_energy = Production_energy,
    Aesthetic = (Aesthetic_naturalness + (0.5 + Aesth_diversity_ADI) / 2) / 2,
    Reg_id = (Aesth_uniqueness_charismatic_plants),
    C_stock = C_stock
  )]
)
#Demand <- fread("data/Demand.csv")
groups <- data.table(Group = unique(Demand$Group))
whole_data <- data.table(groups[, All_ES_scaled_MF[], by = Group])
List_scenario_MF <- apply(Demand, 1, function(x) {
  # print(x)
  x <- as.list(x)
  Weights <- lapply(x[3:13], as.numeric)
  group <- x$Group
  Average_with_MF <- whole_data[
    Group == group,
    list(
      Region = Region,
      Plot = Plot, LU = LU, Classif = Classif,
      Group = group,
      Ric = Weights$Ric * Ric / sum(unlist(Weights)),
      Hunting = Weights$Hunting * Hunting / sum(unlist(Weights)),
      Harvesting = Weights$Harvesting * Harvesting / sum(unlist(Weights)),
      Production_food = Weights$Production_food * Production_food / sum(unlist(Weights)),
      Production_livestock = Weights$Production_livestock * Production_livestock / sum(unlist(Weights)),
      Production_energy = Weights$Production_energy * Production_energy / sum(unlist(Weights)),
      Reg_id = Weights$Reg_id * Reg_id / sum(unlist(Weights)),
      Leisure = Weights$Leisure * 0.5 / sum(unlist(Weights)),
      Production_timber = Weights$Production_timber * Production_timber / sum(unlist(Weights)),
      Aesthetic = Weights$Aesthetic * Aesthetic / sum(unlist(Weights)),
      C_stock = Weights$C_stock * C_stock / sum(unlist(Weights))
    )
  ]
  Average_with_MF[, MF := sum(Ric, Hunting, Harvesting, Production_food, Production_livestock, Production_energy, Reg_id, Leisure, Production_timber, Aesthetic, C_stock),
    by = 1:nrow(Average_with_MF)
  ]
  return(Average_with_MF)
})

MF_melt <- melt(rbindlist(List_scenario_MF), id.vars = c("Region", "Group", "Plot", "LU", "Classif"))
MF_melt[LU == "Crop", c("Classif", "Classif2") := list("Crop", "Crop")]
MF_average <- MF_melt[variable != "MF", list(value = mean(value)), by = c("Region", "LU", "Classif", "variable")]
MF_average[, variable := factor(variable, levels = c(
  "Production_timber", "Ric", "Aesthetic", "Reg_id", "Leisure",
  "Production_food", "Production_livestock", "Production_energy", "Harvesting", "Hunting",
  "C_stock"
)[11:1])]
my_palette_services <- c(
  "lightsteelblue1", "lightsteelblue2", "lightsteelblue3", "lightsteelblue4",
  "burlywood1", "sandybrown", "lightsalmon1", "darksalmon", "lightsalmon3", "salmon4",
  "paleturquoise4"
)[11:1]
ggplot(
  MF_average # [!(variable %in% c('Ric', 'Leisure')),]
  , aes(value, x = Classif, fill = variable)
) +
  facet_wrap(~Region, ncol = 1) +
  theme_bw() +
  geom_col() +
  scale_fill_manual(values = my_palette_services)

MF_melt[variable == "MF", mean(value), by = c("Region", "LU", "Classif", "variable")]

# }


library(zoo)

All_env_data_classif <- merge.data.table(Classif[, c("Region", "Plot", "LU")],
  env_final[, c("Plot", "Core_depth", "pH", "elevation", "TWI", "prop_clay", "Mean_Temp", "Mean_precip")],
  by = c("Plot"), all = TRUE
)
All_env_data_classif$Core_depth <- as.numeric(All_env_data_classif$Core_depth)
All_env_data_classif = merge.data.table(All_env_data_classif, Grassland_LUI[, c('Plot', 'LUI_2008_2015')], all.x = T)
#All_env_data_classif[, c('Core_depth','pH', 'elevation', 'TWI', 'prop_clay', 'Mean_Temp', 'Mean_precip') :=
#                       lapply(.SD, na.aggregate),
#                     by = Region, .SDcols = c('pH', 'elevation', 'TWI', 'prop_clay', 'Mean_Temp', 'Mean_precip')]
All_env_data_classif[, c("Core_depth", "pH", "elevation", "TWI", "prop_clay", "Mean_Temp", "Mean_precip") :=
  lapply(.SD, function(x) {
    x[is.na(x)] <- rnorm(length(x[is.na(x)]), mean(x, na.rm = T), sd(x, na.rm = T) / 5)
    return(x)
  }),
by = Region, .SDcols = c("Core_depth", "pH", "elevation", "TWI", "prop_clay", "Mean_Temp", "Mean_precip")
]

All_env_data_classif[is.na(LUI_2008_2015), LUI_2008_2015 := 0]
fwrite(All_env_data_classif, "data/environments_new.csv")



env_matrix <- as.matrix(All_env_data_classif[, -c(1:3, 11)])
rownames(env_matrix) <- All_env_data_classif$Plot

env_pca <- dudi.pca(env_matrix[All_env_data_classif$LU != "Crop", ], scannf = FALSE, nf = 3)

ind.sup.coord <- suprow(env_pca, env_matrix[All_env_data_classif$LU == "Crop", ])$lisup

env_pca <- rbind(env_pca$li, ind.sup.coord)
env_pca$Plot <- rownames(env_pca)
fwrite(env_pca, "data/environments_pca.csv")



















# %%%%%%%%%%%%%%%%% #
##### Scenarios #####
# %%%%%%%%%%%%%%%%% #

# Scenarios for land use
Scenarios_land_use <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Scenarios/Scenarios_management2.xlsx"))
scenarios_land_use <- unique(Scenarios_land_use$Scenario_name)
no_scenarios_land_use <- length(scenarios_land_use)

# DEMAND AND POWER
Demand <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Stakeholders\ weightings/Data_Socudes270720.xlsx", sheet = 3))[!is.na(ID) & cluster != 'NA',]

Demand[, Stakeholder := dplyr::recode(Group, 
                                      'Agriculture' = "Agric",
                                      "Economy" = "Econ",
                                      'Forestry' = "Forestry",
                                      "Hunting"= "Hunting" ,
                                      'Landowner'= "Landowner" ,
                                      "Local heritage association"  = "Loc_her_asso",
                                      'Locals' = "Locals",
  "Nature conservation associations" = "Nat_cons_asso" ,
  "Policy & administration" = "Policy_admin",
  "Press" = "Press" ,
  "Quarrying" = "Quarrying",
  "Regional development programmes"= "Reg_dev_prog",
  'Research' = "Research" ,
  'Tourism' = "Tourism"
)]

Demand[, cluster := dplyr::recode(cluster, 
                               "1" = "Conservation" ,
                               "2" = "Forest" ,
                               "3" = "Open_land",
                               "4" = "Cultural"
)]
colnames(Demand) <- c('ID', 'Cluster', 
  "Group",  "Production_livestock","Production_food", "Ric",
  "C_stock", "Production_timber", "Production_energy", "REMOVE",
  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting", 'Stakeholder'
)

Scenarios_demand <- Demand[, .SD, .SDcols = colnames(Demand)[!(colnames(Demand) %in% c("REMOVE", 'ID', 'Group'))]]
Scenarios_demand[, c("Production_livestock","Production_food", "Ric",
                     "C_stock", "Production_timber", "Production_energy",  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting") := 
                   lapply(.SD, as.numeric), 
          .SDcols = c("Production_livestock","Production_food", "Ric",
                 "C_stock", "Production_timber", "Production_energy",  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting")]
Scenarios_demand[, c("Production_livestock","Production_food", "Ric","C_stock", "Production_timber", "Production_energy",  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting") ] =
  Scenarios_demand[, c("Production_livestock","Production_food", "Ric","C_stock", "Production_timber", "Production_energy",  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting") ] /
  rowSums(Scenarios_demand[, c("Production_livestock","Production_food", "Ric","C_stock", "Production_timber", "Production_energy",  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting") ])

Scenarios_demand_melt0 <- melt.data.table(Scenarios_demand, variable.name = "Service_name", value.name = "current_demand", id.vars = c('Cluster', 'Stakeholder'))

Demand_cluster = Scenarios_demand_melt0[, list(Demand =mean(current_demand), Category = 'Cluster'), by = list('Group' =Cluster,Service_name)]
Demand_stakeholder = Scenarios_demand_melt0[, list(Demand =mean(current_demand), Category = 'Stakeholder'), by = list('Group' =Stakeholder,Service_name)]
Demand_all = rbind(Demand_cluster, Demand_stakeholder)
Demand_all_cast = dcast(rbind(Demand_cluster, Demand_stakeholder), Category+Group ~Service_name, value.var = 'Demand')


my_palette_services <- c("lightsteelblue1","lightsteelblue2","lightsteelblue3","lightsteelblue4","burlywood1","sandybrown","lightsalmon1","darksalmon","lightsalmon3","salmon4","paleturquoise4")[11:1]
Demand_all[, Group := factor(Group,
                           levels = c( "Hunting",  "Forestry", "Landowner", "Econ", "Nat_cons_asso", "Research", "Reg_dev_prog", "Quarrying", "Agric", "Tourism", "Loc_her_asso", "Policy_admin", "Press", "Locals"
                           ) )] 
Demand_all[, Service_name := factor(
  Service_name,
  levels = c( "Ric", "Aesthetic", "Reg_id", "Leisure", "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting", "C_stock")[11:1])]
demand_plot = ggplot(Demand_all[Category == 'Stakeholder',], aes(Demand *100, x = Group, fill = Service_name)) +
  geom_col() + theme_bw() + theme(legend.position ='bottom') +
  scale_fill_manual(values = my_palette_services, name = 'Service',
                    breaks = c( "Ric", "Aesthetic", "Reg_id", "Leisure", "Production_food", "Production_livestock", "Production_timber", "Production_energy", "Harvesting", "Hunting", "C_stock"),
                    labels = c( "Biodiversity protection", "Landscape aesthetic", "Regional identity", "Leisure", "Food production", "Livestock production", "Timber production", "Energy production", "Foraging", "Hunting", "Climate regulation")) +
  xlab('Stakeholder group') + ylab('Relative priority (%)') +
  scale_x_discrete(breaks=c( "Hunting",  "Forestry", "Landowner", "Econ", "Nat_cons_asso", "Research", "Reg_dev_prog", "Quarrying", "Agric", "Tourism", "Loc_her_asso", "Policy_admin", "Press", "Locals"),
                   labels=c( "Hunting",  "Forestry\nsector", "Land\nowner", "Local\neconomy", "Nature\ncons. asso.", "Scientific\nresearch", "Regional dev.\nprog.", "Quarrying\nsector", "Agricultural\nsector", "Tourism\nsector", "Local\nheritage\nasso.", "Policy and\npublic admin", "Local\npress", "Local\nresidents"))

ggsave(demand_plot,file =  '/Users/Margot/Desktop/Research/Senckenberg/Documents/Papers/Landscape_P4/Demand_plot.pdf', height = 5, width = 10)

#All_ES_data_classif = fread(paste("data/All_ES_data_classif", env_corr, "-region", by_region, "-scale_within", scale_within_land_use, ".csv", sep = ""))

All_ES_data_classif[, c('Ric', 'Harvesting', 'Aesthetic', 'Reg_id', 'Leisure')
                        :=
                         list( scale01(Plant_richness) + scale01(Bird_richness),
                          scale01(Harvesting_plants) ,
                          scale01(Aesthetic_naturalness) + scale01(Aesth_diversity_ADI),
                          scale01(Reg_ID_habitat) + scale01(Aesth_uniqueness_charismatic_plants),
                          NA)
                    ]



# Power
Power <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Stakeholders\ weightings/Power_interest_groups.xlsx"))
Power[, Group := dplyr::recode(Group,
  "Agriculture" = "Agric",
  "Economy" = "Econ",
  "Local heritage association" = "Loc_her_asso",
  "Nature conservation associations" = "Nat_cons_asso", "Policy & administration" = "Policy_admin",
  "Regional development programmes" = "Reg_dev_prog"
)]
Power[, Region := dplyr::recode(Region, "SA" = "A", "SCH" = "S", "HAI" = "H")]

demand_power <- merge.data.table(Demand_all_cast, Power[, c("Region", "Group", "Perceived_influence")], by = "Group", all.x =  T)
demand_power_all_regions <- demand_power[, lapply(.SD, mean), by = c("Group", "Category")]
