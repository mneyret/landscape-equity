library(lme4)
library(ggeffects)
library(MuMIn)
library(mice)
library(data.table)
library(ggtern)
library(vegan)
library(rsq)
library(BiodiversityR)
library(reshape2)
library(dplyr)
library(tidyr)
library(sp)
library(readr)
library(RColorBrewer)
library(ade4)
library(geometry)
library(readxl)
library(car)
library(emmeans)
library(Hmisc) # this package has implemented a cor function calculating both r and p.  
library(corrplot)
library(ggcorrplot)
library(cowplot)
library(psych)
library(readxl)
library(readr)
library(data.table)
library(multcomp)
library(ggnomics)







#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#  Correct productivity: use 2009, not 2008 !













setwd('/Users/Margot/Desktop/Research/Senckenberg/Project_Sophie_P4/Landscape_composition/Code/')
set.seed(101)

saveplots = F
col2 <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582",
                           "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE",
                           "#4393C3", "#2166AC", "#053061"))
col2 <- colorRampPalette(c('#a50026',
  '#d73027',
  '#f46d43',
  '#fdae61',
  '#fee090',
  '#ffffbf',
  '#e0f3f8',
  '#abd9e9',
  '#74add1',
  '#4575b4',
  '#313695'))


source(file = 'Functions.R')


test_default = function(isdefault, choiceiftrue, choiceiffalse){
  if (isdefault) return(choiceiftrue)
  else return(choiceiffalse)
}

for (lui_class_method in c('quantile_30', 'quantile_20')){
  
  for (environmental_correction in c(0,1)) {
    if (lui_class_method == "quantile_30"){options_plots =  c(7, 10, 13)}
    if (lui_class_method == 'quantile_20'){options_plots = 7}

    for (no_plots in options_plots) {
      if (lui_class_method == "quantile_30" & no_plots == 10){options_method_wt_ls =  c('mean', 'max')}
      else {options_method_wt_ls = 'mean'}
      
      for (method_within_landscape in options_method_wt_ls) {
        if (lui_class_method == "quantile_30" & no_plots == 10 & method_within_landscape == 'mean'){
                   options_method =  c('Threshold','minimise_trade_offs','Average', 'Threshold_perc')}
        else {options_method = 'Threshold'}

        for (method in options_method){
          if (method == 'Threshold'){
           if (method_within_landscape == 'mean' & no_plots == 10 & lui_class_method == "quantile_30"){
             options_threshold =  c(0.4, 0.5, 0.6) }
           else {options_threshold = 0.5}}
          if (method == 'minimise_trade_offs'){
           if (method_within_landscape == 'mean' & no_plots == 10 &  lui_class_method == "quantile_30"){
            options_threshold =  c(0.15, 0.25, 0.35) }
           else {options_threshold = 0.25}}
          if (method == 'Average'){
            options_threshold = NA}
          else {options_threshold = 0.5}}
          if (method == 'Threshold_perc'){
            options_threshold = 0.7}

          for (threshold_to_use in options_threshold) {
            
            print(environmental_correction) 
            print(no_plots)
            print(lui_class_method) 
            print(method)
            print(method_within_landscape)
            print(threshold_to_use)
            
            for (lui_class_method in c('quantile_30', 'quantile_20')){
              for (environmental_correction in c(0,1)) {
                if (lui_class_method == "quantile_30"){options_plots =  c(7, 10, 13)}
                if (lui_class_method == 'quantile_20'){options_plots = 7}
                
                for (no_plots in options_plots) {
                  
                for (method_within_landscape in c('mean', 'max')) {
                  
             
source(file = 'Data_preparation.R')
source(file = 'Plot_scale.R')
source(file = 'Create_landscape_sim.R')
source(file = 'Calculate_MF.R')
# source(file = 'All_plots.R')

  
  
   }}}}}}}}

