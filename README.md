# Landscape for multifunctionality and equity

This is the code necessary to replicate the results from Neyret et al. Landscape management strategies for multifunctionality and social equity. 

1. Data acquisition Users should first acquire the data as indicated in 1_Dagtaset_list and store the datasets in the Raw_Data/Data_to_load subfolder
Remaining data is already provided in Raw_Data/Other

2. 2_Data preparation.R: pulls all the raw data together into an Plot x Ecosystem services matrix. Also formats availability and priority data. Stores resulting datasets in Temporary_data.

3. 3_Create_landscape_sim.R determines the composition of scenarios ranging the whole range of possible landscape compositions. Stores resulting datasets in Temporary_data.

4. 4_Landscape_simulations. Randomise the plots and calculate landscape-level ecosystem services. This relies on Rust code written by Guillaume Fraux and available here: https://github.com/mneyret/Landscape_simulation.
Stores resulting datasets in Temporary_data. !!! Time consuming, make sure to run only for default parameters for a start.

5. 5_Calculate_MF: calculate multifunctionality for all generated landscapes. Stores resulting datasets in Temporary_data, and a few plots in Results/. !!! Time consuming, make sure to run only for default parameters for a start.

6. 6_Plot_all_sim: generates all other plots and stores them in Results/

Note on parameters and file organisation:
There are a number of parameters used for sensitivity analyses. Default parameters are used for the main results shown in the manuscript
         - env_corr: are indicators corrected for environmental drivers? (default: env_corr = 'env_corr', means corrected, "" means not corrected)
         - by_region: should the optimisation should be done within (TRUE) or across regions (FALSE, default).
         - use_SB: use supply-benefit relationship (default = FALSE)
         - weighted: weight multifunctionality by stakeholder power (default = FALSE)
         - forest_class: 'Type' if forests are classified in coniferous/deciduous/mixed, "Age" if classified as even-aged/uneven-aged/
         - crop_constrained: is crop proportion fixed to its baseline value? (default = TRUE)
         - (not used) scale_within_land_use: some services are not directly comparable across land uses, for instance cultural 
             or edible plant cover between forest and grasslands due to different sampling areas. We scale these services 
             within, rather than across, land uses to make them comparable (TRUE, default). 
 The parameters are included in all the intermediate data and figure file names to keep track of which are used. In particular, the plots will be saved with the following path (with R designing the region. R = 'All' if by_region = FALSE, or 'A', 'H', 'S' if by_region = TRUE. 
> paste("Results/",R,"/env_corr", env_corr, "/by_region", by_region,  "/SB", use_SB, "/weighted", weighted,  "/forest", forest_class, "/constrained", crop_constrained, "/", sep = "")
All plots for default parameters can thus be found in Results/All/env_correnv_corr/by_regionFALSE/SBFALSE/weightedFALSE/forestType/constrainedTRUE/
