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


# Parameters

for (by_region in c(TRUE, FALSE)) {
  for (scale_within_land_use in c(TRUE, FALSE)) {
    for (env_corr in c("env_corr", "")) {
      # ###################### #
      ####    TO DO LIST    ####
      # ###################### #
      # Decide if/how to transform data (especially w forests). Correct before/after scaling etc
      # Birds in crops

      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      #####     PLOT DESCRIPTORS     #####
      # %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% #
      ### Load data
      Forest_classif <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/17706_New_forest_type_classification_of_all_forest_EPs_2008-2014_1.2.6/17706.txt")
      Grassland_LUI <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Grassland_data/LUI_standardized.csv", dec = ",")
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
      Grassland_LUI[, c("Region", "Plot", "LU") := list(substr(Plot_id1, 1, 1), Plot_id1, "Grassland")]

      if (by_region == TRUE) {
        Grassland_LUI[, Classif := cut(LUI_2007to12, breaks = quantile(LUI_2007to12, c(0, 0.33, 0.66, 1)), labels = c("Grassland_low", "Grassland_medium", "Grassland_high"), include.lowest = TRUE), by = Region]
      }
      if (by_region == FALSE) {
        Grassland_LUI[, Classif := cut(LUI_2007to12, breaks = quantile(LUI_2007to12, c(0, 0.33, 0.66, 1)), labels = c("Grassland_low", "Grassland_medium", "Grassland_high"), include.lowest = TRUE)]
      }

      Grassland_LUI[, Classif2 := Classif]

      # Crops
      Crop_data <- Crop_data[!is.na(Crop_type), ]
      Crop_data[, Productivity := as.numeric(Market_value_euro_perT_without_tax) * as.numeric(Yield_t_per_ha)]
      Crop_data[, Proportion_of_all_crops := round(as.numeric(PROPORTION) * 2)]
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
        x <- as.numeric(x)
        max <- quantile(x, 0.975, na.rm = T)
        min <- quantile(x, 0.025, na.rm = T)
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
      # Redlist
      # Redlist_species = fread('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Data/Final_data/regional_redlist.csv')
      # Redlist_species[Species == 'Polygala_comosa_agg', Species := 'Polygala_comosa_aggr.']
      # Redlist_species[, region := dplyr::recode(Region, 'Thuringen' = 'H', 'BW' = 'A', 'Brandenburg'= 'S')]
      # Edibles and charismatics
      Edible_charismatic <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Grassland_data/Edible_culturally_important_plant_species.csv", sep = "\t", quote = "")[-1, ]
      colnames(Edible_charismatic) <- gsub(" ", "_", colnames(Edible_charismatic))
      all_edibles_plants <- gsub(" ", "_", Edible_charismatic[!is.na(Edible) & Edible != "", Species])
      common_edibles_plants <- gsub(" ", "_", Edible_charismatic[!is.na(Commonly_harvested) & Commonly_harvested != "", Species])
      charismatic_plants <- gsub(" ", "_", Edible_charismatic[!is.na(Cultural_overall) & Cultural_overall != "", Species])

      # Removing large trees, except oak
      charismatic_plants <- charismatic_plants[!(charismatic_plants %in% c("Abies_alba", "Picea_abies"))]

      # Charismatic_birds_Pete_Leo = data.table(read_excel('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Bird_cultural_value/Birds\ cultural\ value.xlsx'))
      # charismatic_birdsPL = Charismatic_birds_Pete_Leo[!is.na(Uniqueness_Pete) & !is.na(Uniqueness_Leo), gsub(' ', '_', Species)]
      Charismatic_birds <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Bird_cultural_value/bird_species_edited.xlsx"))
      Charismatic_birds_melt <- melt.data.table(Charismatic_birds[Species_German_Latin != "Bird species missing?"], id.vars = c("Species", "Species_German_Latin", "Bird_species_English"))
      Charismatic_birds_melt[, value_use := dplyr::recode(value, "1" = "NA", "2" = "0", "3" = "1", "4" = "5")]
      bird_scores <- Charismatic_birds_melt[, list(
        species_score = mean(as.numeric(value_use), na.rm = T),
        Species = gsub(" ", "_", Species)
      ), by = Species][order(Species), c(2, 3)]
      bird_scores <- rbind(bird_scores, list(species_score = mean(bird_scores[grepl("Parus", Species), species_score]), Species = "Parus_montanus"))
      most_charismatic_birds <- bird_scores[order(species_score, decreasing = T), Species][1:round(nrow(bird_scores) / 4)]

      # Abundances and richness for plants and birds
      Abundance_forests <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/Forest_autotrophs_birds.txt")
      Abundance_forests[, Species := gsub("_$", "", Species)]
      Abundance_grasslands <- fread("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Data/Raw_data/New_synthesis_data/Dataset_clean.txt")
      Abundance_grasslands <- Abundance_grasslands[Group_broad %in% c("Plant", "Birds"), ]
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

      # calc_redlist = function(Species, value, Region){
      #  r = as.character(Region)
      #  redlist_sp = Redlist_species[region == r,]$Species
      #  tot = sum(value[Species %in% redlist_sp])
      #  return(tot)
      # }

      Edible_charismatic_richness <- Abundance_grasslands_forests[value > 0,
        list(
          Cover_edible = sum(value[Species %in% all_edibles_plants]) + sum(value[Species %in% common_edibles_plants]),
          # Cover_mushrooms = sum(value[Species %in% all_edibles_mushrooms]),
          # Redlist = calc_redlist(Species, value, Region),
          Charismatic_plants = sum(value[Species %in% charismatic_plants]),
          Plant_richness = length(unique(Species[Group_broad == "plant"])),
          Bird_richness = length(unique(Species[Group_broad == "bird"])),
          Uniqueness_juniperus = ("Juniperus_communis" %in% Species)
        ),
        by = c("Plot", "Region")
      ]
      # Edible_charismatic_richness = merge(Edible_charismatic_richness, CWM_bird_scores)

      # Abundances and richness for fungi
      Fungi_ab0 <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/24286_Abundant\ fungi_2011_relative_abundance_1.1.7/24286.txt")
      Fungi_ab <- Fungi_ab0[Abundance > 0, ]
      Fungi_species_info <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/24306_Abundant_fungi_2011_taxonomic_look_uptable_1.1.8/24306.txt")
      Edibles_mushrooms <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/Forest_edible_fungi_species.csv", sep = "\t")
      Edibles_mushrooms[, Edible_fungi_species := gsub(" ", "_", Edible_fungi_species)][order(Edible_fungi_species), ]

      # Add some to complete with grassland species
      edible_list <- unique(c(Edibles_mushrooms$Edible_fungi_species, c(
        "Amanita_fulva", "Amanita_rubescens", "Amanita_vaginata", "Auricularia_auricula_judae", "Coprinellus_micaceus",
        "Hydnum_rufescens", "Hygrocybe_chlorophana", "Laccaria_amethystina", "Laccaria_laccata", "Lactarius_camphoratus", "Lepista_saeva", "Lycoperdon_perlatum",
        "Lycoperdon_pyriforme", "Phallus_impudicus", "Russula_cyanoxantha", "Russula_ochroleuca", "Russula_virescens"
      )))

      Fungi_species_edible <- Fungi_species_info[Species %in% edible_list, ]
      Fungi_ab <- merge(Fungi_ab, Fungi_species_info[, .SD, .SDcols = c("Species", "OTU")], by = c("OTU"))
      Fungi_ab[, Plot := ifelse(nchar(Plotid) == 5, Plotid, paste(substr(Plotid, 1, 3), 0, substr(Plotid, 4, 4), sep = ""))]
      Fungi_ab_plot <- Fungi_ab[, list(Abundance = sum(Abundance)), by = c("Species", "Plot")]
      Fungi_ab_plot[Abundance > 1, Abundance := 1]

      Fungi_cast <- dcast.data.table(Fungi_ab_plot, Plot ~ Species, value.var = "Abundance", fill = 0)
      Fungi_edible_cast <- Fungi_cast[, .SD, .SDcols = c("Plot", unique(Fungi_species_edible$Species))]

      # Export edible fungi abundance
      fwrite(Fungi_edible_cast, "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Edible_fungi_presence.csv")



      Edible_charismatic_richness <- merge(Edible_charismatic_richness,
        data.table("Plot" = Fungi_edible_cast$Plot, "Fungi_richness" = apply(Fungi_edible_cast[, -1], 1, specnumber)),
        by = "Plot"
      )


      #### 1. Forest ####
      # Tweaking to match variable names

      ES_forests <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/ALL_forest_EFES.xlsx"))
      ES_forests[, c("Plot_ID", "Plot", "Exploratory") := list(Plot, Plot0, substr(Exploratory, 1, 1))]

      Openness_forests <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/25146_Canopy_openness_of_forest_EPs_2014_1.2.8/25146.txt")
      Openness_forests[, Plot := ifelse(nchar(EP) == 5, EP, paste(substr(EP, 1, 3), 0, substr(EP, 4, 4), sep = ""))]

      Forest_structure <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/17687_Stand_structure_and_composition_on_all_forest_EPs_2008-2014_1.4.8/17687.xlsx"))

      Moss_cover <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/4141_Bryophyte\ diversity\ in\ forests_1.6.8_PublicDataset/4141.txt")
      Moss_cover$PlotID <- Moss_cover$Plotid
      Moss_cover <- merge(Moss_cover, Plot_id_matching[, c("EP_PlotID", "PlotID")], by = "PlotID")
      Moss_cover[, Plot := ifelse(nchar(EP_PlotID) == 5, EP_PlotID, paste(substr(EP_PlotID, 1, 3), 0, substr(EP_PlotID, 4, 4), sep = ""))]

      #### Calculating timber production based on volume and market value of each species + total biomass for C stocks and Wood for energy
      Timber_prod <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/22907_Stand_composition_based_on_2nd_forest_inventory_on_all_forest_EPs_2014_2018_1.7.8/22907.txt")
      # Timber_price_old = fread('/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/Timber_market_prices_updated.txt')
      Timber_price <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/Timber_market_prices_incl_lumber_and_pulpwood.txt", fill = TRUE)
      # Get new prices here: http://www.wald-prinz.de/holzpreise-und-holzpreisentwicklung-fichte/383
      # Firewood: https://fbg-amberg.de/brennholz/brennholzboerse
      # https://fbg-amberg.de/holzvermarktung/holzpreise

      Timber_density <- data.table(
        "Species" = c(
          "Picea_abies", "Pinus_sylvestris", "Fagus_sylvatica", "Quercus_spec",
          "Acer_pseudoplatanus", "Fraxinus_excelsior", "Tilia_spec", "Populus_tremula", "Salix_caprea", "Carpinus_betulus"
        ), # completed with Holz handbuch and https://cedarstripkayak.wordpress.com/lumber-selection/162-2/
        "Density" = c(0.43, 0.49, 0.66, 0.68, 0.5, 0.71, 0.56, 0.42, 0.5, 0.67)
      ) # Density in 10^3 kg/m3
      Timber_prod[, c("Exploratory", "Plot") := list(strsplit(Exploratory, 1, 1), EP)]

      # We need to weight timber price based on the average DBH of each species.
      Forest_structure[, Plot := EP]
      Mean_dbh <- merge(merge(Forest_structure[, list(d100 = mean(MTS_dg)), by = list(Plot = Plot, "Species" = MTS)],
        Forest_structure[, list(d100 = mean(ATS1_dg)), by = list(Plot = Plot, "Species" = ATS1)],
        all = TRUE
      ),
      Forest_structure[, list(d100 = mean(ATS2_dg)), by = list(Plot = Plot, "Species" = ATS2)],
      all = TRUE
      )[Species != "X", ]
      Mean_dbh$Species <- dplyr::recode(Mean_dbh$Species,
        "Pa" = "Picea_abies",
        "Fs" = "Fagus_sylvatica",
        "oHS" = "Hardwood",
        "Ps" = "Pinus_sylvestris",
        "oSS" = "Softwood",
        "Qs" = "Quercus_spec",
        "Ld" = "Larix_decidua"
      )
      Dbh_based_price <- merge(Mean_dbh, Timber_price, by = "Species", all = TRUE)[!is.na(Plot), ]
      Dbh_based_price[, Price_timber := ifelse(d100 < 10, NA,
        ifelse(d100 < 15, Industrial,
          ifelse(d100 < 20, max(from15, Industrial, na.rm = T),
            ifelse(d100 < 25, max(from20, Industrial, na.rm = T),
              ifelse(d100 < 30, max(from25, Industrial, na.rm = T),
                ifelse(d100 < 35, max(from30, Industrial, na.rm = T),
                  ifelse(d100 < 40, max(from35, Industrial, na.rm = T),
                    ifelse(d100 < 50, max(from40, Industrial, na.rm = T),
                      ifelse(d100 < 60, max(from50, Industrial, na.rm = T),
                        max(from60, Industrial, na.rm = T)
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ), by = 1:nrow(Dbh_based_price)]

      # Merge all the data into one single dataframe
      Timber_data <- merge.data.table(Timber_prod, Dbh_based_price[, list(Species, Plot, "Price_timber" = Price_timber, "Price_industry" = Industrial, "Price_firewood" = Firewood)], by = c("Species", "Plot"), all.x = TRUE)
      Timber_data <- merge.data.table(Timber_data, Timber_density, by = "Species", all.x = TRUE)

      # Note: I calculate separately market value and volume here, because the wood volume and biomass will be corrected for environment but not the market value (i.e. tot timber production)
      # I hypothesise that 80% of the marketable species can be used as timber and the rest is used for firewood?
      Timber_data_plot <- Timber_data[, list(
        Wood_volume = sum(VOL), # Total wood volume in m3/ha
        Timber_volume = 0.8 * sum(VOL[!is.na(Price_timber)]), # Wood volume of marketable species in m3/ha
        Firewood_volume = sum(VOL) - 0.8 * sum(VOL[!is.na(Price_timber)]),
        Market_value = weighted.mean(Price_timber, VOL, na.rm = T), # Plot-specific market value, weighted by the volume of each wood
        Biomass = sum(VOL) * weighted.mean(Density, VOL, na.rm = T), # Density for known species weighted by the volume of those species in the plot
        Uniqueness_hornbeam = sum(grepl("Carpinus", Species)),
        Uniqueness_main_fagus = sum(Species[BA == max(BA)] == "Fagus_sylvatica")
      ),
      by = Plot
      ][order(Plot), ]
      Timber_data_plot[is.na(Market_value), Market_value := 0]


      # Merge
      ES_forests <- merge.data.table(ES_forests[, c("Plot", "Exploratory", "Trees.C_storage")], Forest_structure[, c("Plot", "dbh_MW_EP")])
      ES_forests <- merge.data.table(ES_forests, Openness_forests[, c("Plot", "openness_mean")])
      ES_forests <- merge.data.table(ES_forests, Edible_charismatic_richness)
      ES_forests <- merge.data.table(ES_forests, Moss_cover[, c("Cover_bryophytes", "Plot")], all.x = T)
      ES_forests <- merge.data.table(ES_forests, Timber_data_plot[, c(
        "Plot", "Market_value", "Timber_volume", # 'Tree.C_stock',
        "Firewood_volume"
      )])
      ES_forests <- merge.data.table(ES_forests, Forest_classif[, c("Plot", "Classif")])
      ES_forests <- merge.data.table(ES_forests, Acoustic_diversity_by_plot[, .SD, .SDcols = c("ADI", "NDSI", "Plot")], all.x = T)

      ES_forests[, mean(Timber_volume), by = c("Classif", "Region")]
      ES_forests[, mean(Market_value), by = c("Classif", "Region")]
      ES_forests[, mean(Market_value * Timber_volume), by = c("Classif", "Region")]

      # Fill missing values
      ES_forests <- cbind(
        ES_forests[, mice::complete(mice(.SD, m = 10)), .SDcols = colnames(ES_forests)[!(colnames(ES_forests) %in% c("Exploratory", "Market_value"))], by = Exploratory],
        Timber_data_plot[, c("Market_value", "Uniqueness_hornbeam", "Uniqueness_main_fagus")]
      )

      #### 2. Grassland ####
      ES_grasslands <- read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Grassland_data/ES_data.xlsx")
      ES_grasslands <- data.table(ES_grasslands[, colnames(ES_grasslands) != "Org_C_stock_maybe2006?"])
      ES_grasslands <- data.table(merge(ES_grasslands, Acoustic_diversity_by_plot[, .SD, .SDcols = c("ADI", "NDSI", "Plot")], by = "Plot", all.x = TRUE))

      # Cover of edible and charismatic species
      ES_grasslands <- merge(ES_grasslands[, c("Plot", "Exploratory", "Total_flower_cover", "Productivity", "ADI", "NDSI", "butterfly_abundance", "Soil.C.stock_2011")],
        Edible_charismatic_richness,
        all.x = T
      )


      # Impute missing values
      ES_grasslands <- ES_grasslands[, mice::complete(mice(.SD, m = 10)), .SDcols = colnames(ES_grasslands)[colnames(ES_grasslands) != "Exploratory"], by = Exploratory]
      ES_grasslands$Uniqueness_juniperus <- as.numeric(ES_grasslands$Uniqueness_juniperus)
      ES_grasslands <- merge(ES_grasslands, Classif)


      #### 3. Crops ####
      ES_crops <- data.table(Crop_classif)

      ## Crop plant communities simulations ##
      arable_weeds <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Agricultural_data/Arable_weeds_to_use.txt")

      Abundance_crops <- data.table(Crop_classif[, c("Plot", "LU", "Region")])
      simulate_crop_weeds <- function(weed_table) {
        weed_table[, tmp := sample(1:100, nrow(weed_table), replace = TRUE)]
        weed_table[, keep := tmp < Frequency]
        spe <- weed_table[keep == TRUE, unique(Species)]
        return(spe)
      }
      for (i in 1:600) {
        spe <- simulate_crop_weeds(arable_weeds)
        Abundance_crops[i, (spe) := 1]
      }
      Abundance_crops <- melt.data.table(Abundance_crops, id.vars = c("Plot", "LU", "Region"), variable.name = "Species", value.name = "value")
      Abundance_crops$Group_broad <- "plant"
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
      All_abundances_aggr <- merge(Classif, All_abundances_aggr, all = T)

      all_plants <- as.character(unique(All_abundances_aggr[Group_broad == "plant", ]$Species))
      all_birds <- as.character(unique(All_abundances_aggr[Group_broad == "bird", ]$Species))

      All_abundances_cast <- data.table(dcast(All_abundances_aggr, Plot + Region + LU + Classif ~ Species, value.var = "value", fill = 0))

      fwrite(All_abundances_cast[, .SD, .SDcols = c("Plot", all_plants)], "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Plants_abundance.csv")
      fwrite(All_abundances_cast[, .SD, .SDcols = c("Plot", most_charismatic_birds)], "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Charismatic_birds_abundance.csv")
      fwrite(All_abundances_cast[, .SD, .SDcols = c("Plot", all_birds)], "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Birds_abundance.csv")


      #### Finish Crop output###

      Edible_charismatic_crops <- Abundance_crops[, list(
        Plant_richness = specnumber(value[Species %in% all_plants]),
        Bird_richness = specnumber(value[Species %in% all_birds]),
        Fungi_richness = 0,
        Cover_edible = sum(value[Species %in% all_edibles_plants]) * 10,
        Charismatic_plants = sum(value[Species %in% charismatic_plants]) * 10
      ), by = "Plot"]
      ES_crops <- merge(ES_crops, Edible_charismatic_crops, by = "Plot")

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
      All_soil_data_f <- merge(All_soil_data_f, Texture)

      Grassland_env <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/may2019_grassland_functions.csv")
      Grassland_use <- Grassland_env[, c("Plot", "pH", "Cstock_2011") := list(Plotn, PH, Soil.C.stock)][, .SD, .SDcols = c("Plot", "pH", "Cstock_2011")]

      Forest_env <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Forest_data/24367_Raw_data_forest_attributes_of_forest\ EPs_Multiple_forest_attributes__1.1.16/24367.txt")
      Forest_use <- Forest_env[, c("Plot", "pH", "Cstock_2011") := list(Plot0, pH, Soil_C_storage)][, .SD, .SDcols = c("Plot", "pH", "Cstock_2011")]

      Forest_grasslands <- rbind(Grassland_use, Forest_use)

      CN_2014 <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/20266_MinSoil_2014_CN_stocks_2.1.4/20266.xlsx"))
      CN_2014[, c("Plot", "Cstock_2014") := list(
        ifelse(nchar(EP_Plotid) == 5, EP_Plotid, paste(substr(EP_Plotid, 1, 3), 0, substr(EP_Plotid, 4, 4), sep = "")),
        as.numeric(OC_stock)
      )]
      Forest_grasslands <- merge(Forest_grasslands, CN_2014[, .SD, .SDcols = c("Plot", "Cstock_2014")], by = "Plot")
      Forest_grasslands[, C.stock := mean(c(Cstock_2011, Cstock_2014), na.rm = T), by = 1:300]

      All_soil_data_f <- merge(All_soil_data_f, Forest_grasslands[, .SD, .SDcols = c("Plot", "pH", "C.stock")], by = "Plot")

      Climate_data <- fread("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Environmental_data_for_correction/climate_data/plots.csv")
      Climate_data[, Plot := plotID]
      Climate <- Climate_data[, list("Mean_Temp" = mean(Ta_200, na.rm = T), "Mean_precip" = mean(precipitation_radolan, na.rm = T)), by = "Plot"]

      env_final <- merge.data.table(All_soil_data_f, Climate)
      env_final[, colnames(env_final) := mice::complete(mice(.SD, m = 10)), .SDcols = colnames(env_final), by = c("Exploratory", "LU")]

      # Adds the C stocks to service dataframes
      ES_forests <- merge(ES_forests, env_final[, .SD, .SDcols = c("Plot", "C.stock")])
      ES_grasslands <- merge(ES_grasslands, env_final[, .SD, .SDcols = c("Plot", "C.stock")], by = "Plot")


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
          ES_forests[, list(Plot, Market_value, Classif, Uniqueness_hornbeam, Uniqueness_main_fagus, Plant_richness, Bird_richness, Fungi_richness)],
          ES_forests[,
            lapply(.SD, function(x) {
              r <- .BY[[1]]
              x <- as.numeric(x)
              mod <- lm(x ~ ., data = env_final[Exploratory == r & LU == "W", c("Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")])
              pred_val <- predict(mod, env_final[Exploratory == r & LU == "W", lapply(.SD, mean), .SDcols = c("Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")]) + residuals(mod)
              return(pred_val)
            }),
            by = Exploratory,
            .SDcols = c("sqrtCover_edible", "sqrtCharismatic_plants", "C.stock", "sqrtCover_bryophytes", "Cover_bryophytes", "ADI", "dbh_MW_EP", "openness_mean", "Cover_edible", "Charismatic_plants", "Plant_richness", "Bird_richness", "Timber_volume", "Firewood_volume", "Trees.C_storage")
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
          ES_forests[, list(Plot, Market_value, Classif, Uniqueness_hornbeam, Uniqueness_main_fagus, Plant_richness, Bird_richness, Fungi_richness)],
          ES_forests[,
            lapply(.SD, function(x) {
              r <- .BY[[1]]
              x <- as.numeric(x)
              mod <- lm(x ~ ., data = env_final[LU == "W", c("Exploratory", "Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")])
              pred_val <- predict(mod, cbind(env_final[LU == "W", ]$Exploratory, env_final[LU == "W", lapply(.SD, mean), .SDcols = c("Mean_Temp", "Mean_precip", "elevation", "Core_depth", "pH", "prop_clay", "TWI")])) + residuals(mod)
              return(pred_val)
            }),
            .SDcols = c("sqrtCover_edible", "sqrtCharismatic_plants", "C.stock", "sqrtCover_bryophytes", "Cover_bryophytes", "ADI", "dbh_MW_EP", "openness_mean", "Cover_edible", "Charismatic_plants", "Plant_richness", "Bird_richness", "Timber_volume", "Firewood_volume", "Trees.C_storage")
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
      }), .SDcols = c("sqrtCover_edible", "sqrtCover_bryophytes", "sqrtCharismatic_plants", "C.stock", "dbh_MW_EP", "openness_mean", "Cover_edible", "Charismatic_plants", "Plant_richness", "Bird_richness", "Timber_volume", "Firewood_volume", "Trees.C_storage")]
      ES_forests_corr[, lapply(.SD, function(x) {
        list(min(x, na.rm = TRUE), mean(x), max(x))
      }), .SDcols = c("sqrtCover_edible", "sqrtCover_bryophytes", "sqrtCharismatic_plants", "C.stock", "dbh_MW_EP", "openness_mean", "Cover_edible", "Charismatic_plants", "Plant_richness", "Bird_richness", "Timber_volume", "Firewood_volume", "Trees.C_storage")]


      # We consider that corrected values < 0 are = 0. This is a very safe assumption for all except Timber volumes which varies a bit more with the environment.
      ES_forests_corr[, c("sqrtCover_edible", "sqrtCharismatic_plants", "Timber_volume", "Trees.C_storage") := lapply(.SD, function(x) {
        x[x < 0] <- 0
        return(x)
      }), .SDcols = c("sqrtCover_edible", "sqrtCharismatic_plants", "Timber_volume", "Trees.C_storage")]


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
      ES_grasslands_use[, Production_livestock := Productivity * 11] # !!!! Need to check price / unit. Productivity is dt/ha, price is about 110€/t
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
      ES_forests_use[, Production_timber := Timber_volume * Market_value] # !!!! Will need to make it better with specific species market values
      ES_crops[, Production_timber := 0]

      ## Energy production ##
      # ------ in grasslands: no energy
      ES_grasslands_use[, Production_energy := 0]
      # ------ in forests:  No food
      ES_forests_use[, Production_energy := as.numeric(Firewood_volume) * 45 / 35]
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
      # ------ in crops
      ES_crops[, Aesthetic_naturalness := 0]
      ES_crops[, Aesth_diversity_ADI := 0]

      ## Regional ID ##
      # Aesthetic is divided into two classes: naturalness and diversity
      # ------ in forests
      ES_forests_use[, c("Aesth_uniqueness_charismatic_plants") := sqrtCharismatic_plants]
      ES_forests_use[, Reg_ID_habitat := (Uniqueness_hornbeam + Uniqueness_main_fagus) / 2]
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
      # ----- grasslands
      ES_grasslands_use$Hunting <- 0
      # ----- forests
      ES_forests_use[, tree_size_suitability :=
        as.numeric(cut(dbh_MW_EP, c(min(dbh_MW_EP), quantile(ES_forests[Exploratory == "S", dbh_MW_EP], c(0.33, 0.66)), max(dbh_MW_EP)), include.lowest = TRUE, labels = c(3, 2, 1)))]
      ES_forests_use[, forest_type_suitability :=
        ifelse(Classif == "Forest_Deciduous", 1,
          ifelse(Classif == "Forest_Mixed", 2,
            ifelse(Classif == "Forest_Coniferous", 3, NA)
          )
        )]
      if (by_region == TRUE) ES_forests_use[, Hunting := scale01(forest_type_suitability) + scale01(tree_size_suitability), by = Region]
      if (by_region == FALSE) ES_forests_use[, Hunting := scale01(forest_type_suitability) + scale01(tree_size_suitability)]
      # ----- crops
      ES_crops[, Hunting := 0]

      #### Regulating services ####
      ## C stock ##
      # ----- grasslands
      ES_grasslands_use[, C_stock := as.numeric(C.stock)]
      # ----- forestshttps://docs.google.com/forms/d/e/1FAIpQLScjTFGAbJ6naZHV6G0s-ZYCI6NXTVRyuYmryCnPQ8ZntMAv9A/viewform?fbclid=IwAR3avDmXy52iwRAx9mXlcU53x4WgUqsyZtYaYF__ZrlUbbEsvp2jW9xMy7w
      ES_forests_use[, C_stock := (Trees.C_storage + C.stock)] # !!!! Need to check the + 1/3 for root C, and where does the C stock comes from
      # ----- crops: Fill C stock as 80% of grassland values
      if (by_region == TRUE) {
        Crop_C_stock <- ES_grasslands_use[, list(C_stock = 0.8 * mean(C_stock, na.rm = T)), by = Region]
        ES_crops[LU == "Crop", C_stock := sapply(Region, function(x) {
          Crop_C_stock[Region == x, C_stock]
        }), by = .I]
      }
      if (by_region == FALSE) {
        ES_crops[LU == "Crop", C_stock := ES_grasslands_use[, list(C_stock = 0.8 * mean(C_stock, na.rm = T))]]
      }



      #### Putting all data together ####
      indicators_plot <- c(
        "Production_food",
        "Production_livestock",
        "Production_energy",
        "Production_timber",
        "Harvesting_plants",
        "C_stock",
        "Hunting",
        "Aesthetic_naturalness", "Aesth_diversity_ADI", # LU diversity calculated at landscape level
        # Conservation calculated at landscape level
        "Reg_ID_habitat", "Aesth_uniqueness_charismatic_plants" #' Reg_ID', birds added at landscape level
      )
      all_indicators <- c(
        "Ric_plants", "Ric_birds", "Aest_diversity_landscape", "Aesth_uniqueness_charismatic_birds",
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

      All_ES_data_classif <- merge(Classif,
        All_ES_data,
        by = "Plot", all = TRUE
      )


      if (scale_within_land_use == TRUE) {
        # For richness (for plot-level analyses only) as well as cover of edible and charismatic plants, we rescale between 0 and 1 independently for forests and grasslands
        All_ES_data_classif[, c("Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, as.numeric), .SDcols = c("Plant_richness", "Bird_richness", "Fungi_richness")]

        if (by_region == TRUE) {
          All_ES_data_classif[LU == "Forest", c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, scale01),
            .SDcols = c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Plant_richness", "Bird_richness", "Fungi_richness"), by = Region
          ]
          All_ES_data_classif[LU != "Forest", c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, scale01),
            .SDcols = c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Plant_richness", "Bird_richness", "Fungi_richness"), by = Region
          ]
        }
        if (by_region == FALSE) {
          All_ES_data_classif[LU == "Forest", c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, scale01),
            .SDcols = c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Plant_richness", "Bird_richness", "Fungi_richness")
          ]
          All_ES_data_classif[LU != "Forest", c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Plant_richness", "Bird_richness", "Fungi_richness") := lapply(.SD, scale01),
            .SDcols = c("Harvesting_plants", "Aesth_uniqueness_charismatic_plants", "Plant_richness", "Bird_richness", "Fungi_richness")
          ]
        }
      }

      if (by_region == FALSE) {
        All_ES_data_classif$Region <- "All"
      }
      fwrite(All_ES_data_classif, paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/All_ES_data_classif", env_corr, "-region", by_region, "-scale_within", scale_within_land_use, ".csv", sep = ""))
    }
  }
}





All_ES_plot <- melt(All_ES_data_classif, id.vars = c("Plot", "Region", "LU", "Classif", "Classif2"))
All_ES_plot <- rbind(All_ES_plot, All_ES_plot[LU == "Forest", ][, Classif := Classif2])
All_ES_plot[LU == "Crop", Classif := "Crop"]
All_ES_plot[, value := scale(value), by = "variable"]
All_ES_plot[, Classif := factor(Classif, levels = c("Crop", "Grassland_high", "Grassland_medium", "Grassland_low", "Forest_Coniferous", "Forest_Mixed", "Forest_Deciduous", "Forest_even-aged", "Forest_uneven-aged"))]
ggplot(All_ES_plot, aes(
  y = value, x = Classif, fill = Classif # , color = Region
)) +
  geom_boxplot(position = position_dodge(width = 1)) +
  geom_jitter(alpha = 0.3, shape = 21, position = position_dodge(width = 1), size = 0.5, lwd = 0.2) +
  facet_wrap(~variable, ncol = 2) +
  theme_bw() +
  scale_fill_manual(
    values = c("palegreen1", "palegreen3", "palegreen4", "cadetblue1", "cadetblue3", "cadetblue4", "lightsteelblue3", "lightsteelblue4", "tan1"),
    breaks = c("Grassland_low", "Grassland_medium", "Grassland_high", "Forest_Deciduous", "Forest_Mixed", "Forest_Coniferous", "Forest_even-aged", "Forest_uneven-aged", "Crop")
  ) +
  scale_color_manual(
    values = c("darkslategray", "navyblue", "purple4"),
    breaks = c("A", "H", "S")
  ) +
  theme(legend.position = "bottom")

All_ES_data_classif2 <- copy(All_ES_data_classif)

# if (plot_level_tests = TRUE){
All_ES_data_classif2[, colnames(All_ES_data_classif2)[6:19] := lapply(.SD, as.numeric), .SDcols = colnames(All_ES_data_classif2)[6:19]]
All_ES_data_classif2[, colnames(All_ES_data_classif2)[c(6:9, 10:15)] := lapply(.SD, function(x) {
  Max <- quantile(x, 0.975)
  Min <- quantile(x, 0.025)
  y <- (x - Min) / (Max - Min)
  y[y < 0] <- 0
  y[y > 1] <- 1
  print(Max)
  print(Min)
  print(y)
  return(y)
}), .SDcols = colnames(All_ES_data_classif)[c(6:9, 10:15)], by = "Region"]

All_ES_scaled_MF <- cbind(
  All_ES_data_classif2[, c(1:5)],
  All_ES_data_classif2[, list(
    Ric = (Plant_richness + Bird_richness) / 2,
    Hunting = Hunting,
    Harvesting = (Harvesting_plants + Fungi_richness) / 2,
    Production_food = Production_food,
    Production_livestock = Production_livestock,
    Production_timber = Production_timber,
    Production_energy = Production_energy,
    Aesthetic = (Aesthetic_naturalness + (0.5 + Aesth_diversity_ADI) / 2) / 2,
    #  (Leisure_grasslands + Leisure_forests)/2,
    Reg_id = (Aesth_uniqueness_charismatic_plants),
    C_stock = C_stock
  )]
)
Demand <- fread("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Demand.csv")
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

All_env_data_classif <- merge(Classif[, c("Region", "Plot", "LU")],
  env_final[, c("Plot", "Core_depth", "pH", "elevation", "TWI", "prop_clay", "Mean_Temp", "Mean_precip")],
  by = c("Plot"), all = TRUE
)
All_env_data_classif$Core_depth <- as.numeric(All_env_data_classif$Core_depth)
# All_env_data_classif[, c('Core_depth','pH', 'elevation', 'TWI', 'prop_clay', 'Mean_Temp', 'Mean_precip') :=
#                       lapply(.SD, na.aggregate),
#                     by = Region, .SDcols = c('pH', 'elevation', 'Cstock0.10', 'TWI', 'prop_clay', 'Mean_Temp', 'Mean_precip')]
All_env_data_classif[, c("Core_depth", "pH", "elevation", "TWI", "prop_clay", "Mean_Temp", "Mean_precip") :=
  lapply(.SD, function(x) {
    x[is.na(x)] <- rnorm(length(x[is.na(x)]), mean(x, na.rm = T), sd(x, na.rm = T) / 5)
    return(x)
  }),
by = Region, .SDcols = c("Core_depth", "pH", "elevation", "TWI", "prop_clay", "Mean_Temp", "Mean_precip")
]
fwrite(All_env_data_classif, "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/All_env_data.csv")



env_matrix <- as.matrix(All_env_data_classif[, -c(1:3)])
rownames(env_matrix) <- All_env_data_classif$Plot

env_pca <- dudi.pca(env_matrix[All_env_data_classif$LU != "Crop", ], scannf = FALSE, nf = 3)

ind.sup.coord <- suprow(env_pca, env_matrix[All_env_data_classif$LU == "Crop", ])$lisup

env_pca <- rbind(env_pca$li, ind.sup.coord)
env_pca$Plot <- rownames(env_pca)
fwrite(env_pca, "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Env_pca.csv")






















# %%%%%%%%%%%%%%%%% #
##### Scenarios #####
# %%%%%%%%%%%%%%%%% #

# Scenarios for land use
Scenarios_land_use <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Scenarios/Scenarios_management2.xlsx"))
scenarios_land_use <- unique(Scenarios_land_use$Scenario_name)
no_scenarios_land_use <- length(scenarios_land_use)

# DEMAND AND POWER
# Demand <- read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Stakeholders\ weightings/Data_Socudes270720.xlsx", sheet = 'DEMAND PER STAKEHOLDERS GROUP')
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
                     "C_stock", "Production_timber", "Production_energy",  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting") := lapply(.SD, as.numeric), .SDcols = c("Production_livestock","Production_food", "Ric",
                 "C_stock", "Production_timber", "Production_energy",  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting")]
Scenarios_demand[, c("Production_livestock","Production_food", "Ric",
                     "C_stock", "Production_timber", "Production_energy",  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting")  := lapply(.SD, function(x){x/rowSums(.SD)})
                 , .SDcols = c("Production_livestock","Production_food", "Ric",
                                                                        "C_stock", "Production_timber", "Production_energy",  "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting")]

Scenarios_demand_melt0 <- melt.data.table(Scenarios_demand, variable.name = "Service_name", value.name = "current_demand", id.vars = c('Cluster', 'Stakeholder'))

Demand_cluster = Scenarios_demand_melt0[, list(Demand =mean(current_demand), Category = 'Cluster'), by = list('Group' =Cluster,Service_name)]
Demand_stakeholder = Scenarios_demand_melt0[, list(Demand =mean(current_demand), Category = 'Stakeholder'), by = list('Group' =Stakeholder,Service_name)]
Demand_all = rbind(Demand_cluster, Demand_stakeholder)
Demand_all_cast = dcast(rbind(Demand_cluster, Demand_stakeholder), Category+Group ~Service_name, value.var = 'Demand')


dpca_demand = dudi.pca(df = Demand[, lapply(.SD, as.numeric), .SDcols = c("Production_livestock", "Production_food", "Ric", "C_stock", "Production_timber", "Production_energy", "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting")], scannf = FALSE, nf = 4)
fviz_pca(dpca)

All_ES_data_classif = fread(paste("/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/All_ES_data_classif", env_corr, "-region", by_region, "-scale_within", scale_within_land_use, ".csv", sep = ""))

All_ES_data_classif[, c('Ric', 'Harvesting', 'Aesthetic', 'Reg_id', 'Leisure')
                        :=
                         list( scale01(Plant_richness) + scale01(Bird_richness),
                          scale01(Harvesting_plants) ,
                          scale01(Aesthetic_naturalness) + scale01(Aesth_diversity_ADI),
                          scale01(Reg_ID_habitat) + scale01(Aesth_uniqueness_charismatic_plants),
                          NA)
                    ]
dpca_supply_plot = dudi.pca(df = All_ES_data_classif[, .SD, .SDcols = c("Production_livestock", "Production_food", "Ric", "C_stock", "Production_timber", "Production_energy", "Harvesting", "Aesthetic", "Reg_id", "Hunting")], scannf = FALSE, nf = 3)
dpca_supply_landscape =
  dudi.pca(df = service_data[replica < 100, .SD, .SDcols = c("Production_livestock", "Production_food", "Ric", "C_stock", "Production_timber", "Production_energy", "Harvesting", "Leisure", "Aesthetic", "Reg_id", "Hunting")], scannf = FALSE, nf = 3)                                 

test = rbind(
cbind(service = rownames(dpca_demand$co), data.table(dpca_demand$co), what = 'demand'),
cbind(service = rownames(dpca_supply_landscape$co), data.table(dpca_supply_landscape$co), what = 'landscape_supply'),
cbind(service = rownames(dpca_supply_plot$co), data.table(dpca_supply_plot$co), what = 'plot_supply'), fill = T)

test2 = dcast(melt(test), service ~what+variable)

corr(test2)


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
demand_power_all_regions<- demand_power[, lapply(.SD, mean), by = c("Group", 'Category'), .SDcols = colnames(demand_power)[c(3:13, 15)]][, Region := "All"]

demand_power_combined <- rbind(demand_power, demand_power_all_regions)
fwrite(demand_power_combined, "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Demand_Power.csv")


# Availability
Availability <- data.table(read_excel("/Users/Margot/Dropbox/P4_BEF-Up_SoCuDES/Stakeholders\ weightings/Data_availability.xlsx"))
Availability[, Group := dplyr::recode(Group,
  "Agriculture" = "Agric",
  "Economy" = "Econ",
  "Local heritage associations" = "Loc_her_asso",
  "Nature conservation association" = "Nat_cons_asso", "Policy & Administration" = "Policy_admin",
  "Regional development programs" = "Reg_dev_prog"
)]
Availability <- Availability[, -2]
colnames(Availability) <- c(
  "Region", "Group", "Production_livestock", "Production_food", "Ric", "C_stock",
  "Production_timber", "Production_energy", "Energy_techno", "Harvesting", "Aesthetic", "Reg_id", "Leisure", "Hunting"
)
Availability_melt <- melt(Availability, id.vars = c("Group", "Region"))
Availability_melt[, value := as.numeric(value)]
Availability_melt[, available_enough := ifelse(value > 2, "available", "not_available")]

ggplot(Availability_melt, aes(y = value, x = Group, fill = Region)) +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.5) +
  facet_wrap(~variable, ncol = 3) +
  geom_boxplot()

ggplot(Availability_melt, aes(y = value, x = variable, fill = Region)) +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.5) +
  geom_boxplot()

Availability2 <- dcast.data.table(Availability_melt[!is.na(available_enough), .N, by = c("Group", "Region", "variable", "available_enough")], Group + Region + variable ~ available_enough, value.var = "N", fill = 0)
Availability2[, count := rowSums(.SD, na.rm = T), .SDcols = c("available", "not_available")]

# For overall availability perception
Availability_of_ES <- Availability2[, list(endangered = sum(not_available) / sum(count)), by = c("variable", "Region")]
# Availability_of_ES = Availability_of_ES[endangered == TRUE & variable != 'Energy_techno' ,]

Availability_of_ES_overall <- Availability2[, list(endangered = sum(not_available) / sum(count)), by = c("variable")][, Region := "All"]
# Availability_of_ES_overall = Availability_of_ES_overall[endangered == TRUE & variable != 'Energy_techno' ,][,Region := 'All']



fwrite(
  rbind(Availability_of_ES, Availability_of_ES_overall),
  "/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/Temporary_data/Availability.csv"
)


# Detailed per group

Availability2[, prop_strict := ifelse(count > 2, ifelse(is.na(available), 0, available / count), NA)]
Availability2[, prop := ifelse(is.na(available), 0, available / count)]
Availability2[, prop01 := ifelse(prop > 0.5, 1, 0)]
Availability2_per_group <- Availability2[, list(
  prop = mean(prop, na.rm = T),
  prop_strict = mean(prop_strict > 0.5, na.rm = T),
  prop01 = mean(prop01, na.rm = T)
), by = c("Group", "Region")]

Availability_to_use <- dcast(Availability2, Group + Region ~ variable, value.var = "prop01")
colnames(Availability_to_use) <- c(
  "Group", "Region", "Production_livestock", "Production_food", "Ric", "C_stock",
  "Production_timber", "Production_energy", "", "Harvesting",
  "Aesthetic", "Reg_id", "Leisure", "Hunting"
)
Availability_to_use[is.na(Availability_to_use)] <- -1
