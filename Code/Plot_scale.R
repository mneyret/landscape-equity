es_data <- es_data_raw

# Extract correlation matrix
corrtest_mat = corr.test(es_data[, c(4:8, 11:13)])
a = matrix(paste(round(corrtest_mat$r, 2), 
                 ifelse(corrtest_mat$p> 0.05, '', 
                        ifelse(corrtest_mat$p> 0.01, '*',
                               ifelse(corrtest_mat$p> 0.001, '**', '***'
                               )))), nrow = 8)
colnames(a) = rownames(a) = colnames(corrtest_mat$r)
#View(a)

if(environmental_correction == TRUE){
  es_env_melt = melt(es_data[env_data, on = 'Plot'], measure.vars = services_all)
  form = as.formula(paste("value ~", paste(env_variables, collapse="+")))
  es_env_melt[, value_cor := list(residuals(lm(form, .SD))), by = list(Exploratory, variable) ]
  es_data[, (services_all) := dcast.data.table(es_env_melt, Plot ~ variable, value.var = 'value_cor')[, ..services_all]]
}



data_plot  <- es_data[, .SD, .SDcols=-c('Org_C_stock_0')]

#data_plot[, (services_all) := lapply(.SD, function(x){(x-min(x))/(max(x)-min(x))}), by = Exploratory, .SDcols = services_all ]
data_plot = data_plot[, `:=` (Aesthetic = (Total_flower_cover + butterfly_abundance + ric_birds)/3, 
                              Richness =(ric_tot + is.redlist)/2)]

data_plot[, c('luiclass', 'LUI_2007to12') ]= env_data[, c('luiclass', 'LUI_2007to12') ]
data_plot$Exploratory = factor(data_plot$Exploratory)

data_plot_melt = melt(data_plot, id.var = c("Plot", "exploratory", "Exploratory", "luiclass",  'LUI_2007to12'))

data_plot_melt[, c('Region', 'pretty_variables') :=   list(Exploratory, variable)]
levels(data_plot_melt$Region) = c('SW', 'C', 'N')
data_plot_melt$pretty_variables = factor(data_plot_melt$pretty_variables, levels = c('Richness','ric_tot', 'is.redlist',  'Tot_protein','Productivity', 'Nshoot_2009_2001',
                                                                                     'Aesthetic', 'Total_flower_cover', 'butterfly_abundance', 'ric_birds', 'Org_C_stock') )
levels(data_plot_melt$pretty_variables) =
                       c('Conservation value' ,  
                         'Plant species richness',
                         'Cover by redlist species' , 
                       'Productivity' , 
                       'Biomass production', 
                       'Plant protein content', 
                       'Aesthetic value' ,
                       'Flower cover' , 
                       'Butterfly abundance' , 
                       'Bird family richness',  
                       'C stock' )
     

data_plot_letters = data_plot_melt[, model_letters(value, luiclass), by = list(pretty_variables, Region)]
to_print = data_plot_melt[, list(mean = mean(value), sd = sd(value)), by = list(pretty_variables, Region, luiclass)]

to_print = merge(data_plot_letters, to_print)
to_print[, to_print := paste(round(mean, 1), " +/- ", round(sd, 1), ' (', gsub(' ', '', Letter), ')', sep = "")]

#View(dcast(to_print, pretty_variables+luiclass ~ Region))


gg_box = ggplot(data_plot_melt, aes( value, x = Region, fill = luiclass)) + 
  facet_wrap(~ pretty_variables, nrow = 4) +
  theme_bw() +
  geom_text(data = data_plot_letters, aes(x = Region, y = 1.1, label = Letter), position = position_dodge(width = 0.8)) +
  scale_fill_brewer(palette = "Accent", name = 'Land use intensity', labels = c('Low', 'Medium', 'High')) +
  xlab('Region') + ylab('Standardised value') +
  geom_boxplot(position = position_dodge2(width = 1))

if(saveplots){ggsave(plot = gg_box,  paste(c('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Results/Sensitivity analyses/boxplot', environmental_correction,'_lui', lui_class_method, '.pdf'), collapse = ""), width = 10)}

#data_plot_stars = data_plot_melt[, model_letters(value, env, Exploratory, type = 'continuous'), 
#                                 by = list(variable)]
#gg_continuous =  ggplot(data_plot_melt[data_plot_melt$env > -2], aes( value, x = env, fill = Exploratory, color = Exploratory)) + 
#  facet_wrap(variable~  luiclass) +
#  theme_bw() +
#  scale_fill_brewer(palette = "Accent") +
#  scale_color_brewer(palette = "Accent") +
#  geom_smooth(span = 0.8, method = 'lm') + geom_point()

#if(saveplots){ggsave(plot = gg_continuous, '/Users/Margot/Desktop/Research/Senckenberg/Analysis/Results/lmplot_raw.pdf', width = 10)}



