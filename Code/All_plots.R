
# Start decomment
### Correlations ####
if (method != 'Threshold_perc'){
  
library(svglite)
 get_corr = function(data){
  colnames(data) = c('Production', 'C stocks', 'Aesthetic value', 'Conservation value')
  M<-corr.test(data, use = "pairwise")
  M$r[lower.tri(M$r, diag = TRUE)] = NA
  tmp = melt(M$r, value.name = 'correlation')
  M$p[lower.tri(M$p, diag = TRUE)] = NA
  tmp_p = melt(M$p, value.name = 'p.value')
  return(list(correlation = tmp$correlation, p.value = tmp_p$p.value, Var1 =tmp_p$Var1, Var2 = tmp$Var2 ))
}

data_average[,  corr.test(.SD),   .SDcols = c( 'Prod','C_stock', 'Aesthetic', 'Ric')]

all_corr = data_average[,  get_corr(.SD),   by= Explo, .SDcols = c( 'Prod','C_stock', 'Aesthetic', 'Ric')]
all_corr = all_corr[!is.na(all_corr$correlation),]
all_corr[, list(Var1 =  factor(Var1, levels = c('Production', 'C stocks', 'Aesthetic value')),
                Var2 = factor(Var2, levels = c( 'C stocks', 'Aesthetic value','Conservation value')))]

all_corr$Region = rep(c('South-West', 'Central', 'North'), each = 6)
all_corr_plot = ggplot(all_corr, aes(x = Var1, y = Var2, fill = correlation, size = abs(correlation))) +
  geom_point(shape = 21, color = 'black')  +
  facet_wrap(~ Region, nrow = 1) + ylab('') + xlab('') +
  geom_point(data = all_corr[all_corr$p.value > 0.05,], shape = 4, size = 5) +
  scale_fill_gradientn(limits = c(-0.8, 0.8), colors = col2(50), breaks = seq(-0.6, 0.6, 0.3), name = "Correlation") +
  scale_size(range = c(2, 10), name = "Correlation", breaks = seq(0, 0.5, 0.1), guide = F) + theme_bw() +
  theme(legend.position = 'bottom', legend.direction = 'horizontal', axis.text.x = element_text(angle = 45, hjust = 1))

if(saveplots){
  ggsave(plot = all_corr_plot, paste(c('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Results/Sensitivity analyses/corrplot_envcorr', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '.pdf'), collapse = ""), width = 7, height = 3.5)
}

}

add_padding = function(x, padding = 0.1){
  y1 = scale(x, center = TRUE, scale = TRUE)
  y2 = (1+ padding)*y1
  y = y2*sd(x) + mean(x)
  return(as.numeric(y))
}

interpolate_tern = function(data_to_plot, tern = TRUE){
 # Grid creation
    if(tern){
  mygrid = data.frame(rbind(
    expand.grid(
      x = seq(0, 1, length.out = 100),
      y = seq(0, 1, length.out = 100),
      Region = unique(data_to_plot$Region),
      pretty_variables = unique(data_to_plot$pretty_variables),
      value = NA
    )))
  mygrid$high = mygrid$x - mygrid$y*tan(pi/6)
  mygrid$medium = mygrid$y/(tan(pi/3)*0.5)
  mygrid$low = 1 - mygrid$high  - mygrid$medium
  }

  if(!tern){
    mygrid = data.frame(rbind(
      expand.grid(
        lui_mean = seq(1, 3, length.out = 200),
        lui_cv = seq(0, 0.7, length.out = 200),
        Explo = c('A', 'H', 'S'),
        Region = unique(data_to_plot$Region),
        pretty_variables = unique(data_to_plot$pretty_variables),
        value = NA,
        in_range = NA
      )))
    data_to_plot[, lui_mean_padded := add_padding(lui_mean, 0.1),
                 by = Region][, lui_cv_padded := add_padding(lui_cv, 0.1), by = Region]

        ch <- data_to_plot[, .SD[c(chull(.SD), chull(.SD)[1])], .SDcols = c('lui_mean_padded', 'lui_cv_padded'), by = Region]
    polyA <- SpatialPolygons(list(Polygons(list(Polygon(ch[ch$Region == 'South-West', 2:3])),1)))
    polyH <- SpatialPolygons(list(Polygons(list(Polygon(ch[ch$Region == 'Central', 2:3])),1)))
    polyS <- SpatialPolygons(list(Polygons(list(Polygon(ch[ch$Region == 'North', 2:3])),1)))

    mygrid_poly= mygrid
    coordinates(mygrid_poly) <- ~ lui_mean+lui_cv

    mygrid[mygrid$Region == 'South-West',]$in_range <- over(mygrid_poly[mygrid$Region == 'South-West',], polyA)
    mygrid[mygrid$Region == 'Central',]$in_range <- over(mygrid_poly[mygrid$Region == 'Central',], polyH)
    mygrid[mygrid$Region == 'North',]$in_range <- over(mygrid_poly[mygrid$Region == 'North',], polyS)
  }

  mygrid = merge(mygrid, unique(data_to_plot[, c('pretty_variables', 'nservices')]), by = 'pretty_variables')
  rlist = expand.grid(Region = unique(data_to_plot$Region), pretty_variables = unique(data_to_plot$pretty_variables), r2 = NA, r = NA, range = NA)
  rlist = merge(rlist, unique(data_to_plot[, c('pretty_variables', 'nservices')]), by = 'pretty_variables')

  # Fill grid with model interpolation

  for (Exp in unique(data_to_plot$Region)){
    for (var in unique(data_to_plot$pretty_variables)){
     if (tern){
      modelbetalin = glm(value ~ poly(low, high, degree = 2), data = data_to_plot[pretty_variables == var &  Region == Exp & !is.na(value)], family = "binomial")
      mygrid[mygrid$Region == Exp & mygrid$pretty_variables ==var,]$value = predict(modelbetalin,  mygrid[mygrid$Region == Exp & mygrid$pretty_variables ==var,], type="response")
      mygrid = mygrid[mygrid$high >= 0,]
      mygrid = mygrid[mygrid$low >= 0,]
      }
      if (!tern){
        modelbetalin = glm(value ~ poly(lui_mean, lui_cv, degree = 2), data = data_to_plot[pretty_variables == var &  Region == Exp & !is.na(value)], family = "binomial")
        mygrid[mygrid$Region == Exp & mygrid$pretty_variables ==var,]$value = predict(modelbetalin,  mygrid[mygrid$Region == Exp & mygrid$pretty_variables ==var,], type="response")
        mygrid <- mygrid[!is.na(mygrid$in_range),]
        }

      rlist[rlist$Region == Exp& rlist$pretty_variable ==var,'r2'] = as.character(round(r.squaredLR(modelbetalin)[1], 2))
      rlist[rlist$Region == Exp& rlist$pretty_variable ==var,'range'] = diff(quantile(range(mygrid[mygrid$Region == Exp & mygrid$pretty_variables == var,]$value, na.rm = T), c(0.025, 0.975)))    #range(mygrid[mygrid$Region == Exp & mygrid$pretty_variables == var,]$value, na.rm = T))
    }}

  rlist$r = paste("R^2==",rlist$r2 , sep = '')
  return(list(grid = mygrid, R2 = rlist[, c('pretty_variables','Region','r','r2', 'nservices')], range = rlist[, c('pretty_variables','Region','range', 'nservices')] ))
}


 outplot_tern = function(mygrid, rlist, tern = TRUE){
   if(!tern){
     mygrid$x = mygrid$lui_mean
     mygrid$y = mygrid$lui_cv
   }

     gg =   ggplot(mygrid, aes(z = value, x= x, y = y, fill = value, color = value)) +
       geom_raster() +  theme_bw() +
       facet_nested(cols = vars(pretty_variables), rows = vars(nservices, Region), switch = 'y') +
       scale_fill_gradientn(colors = col2(20), name = "Multifunctionality", limits=c(0.0,1.0)) +
       scale_color_gradientn(colors = col2(20), name = "Multifunctionality", limits=c(0.0,1.0))

     if(tern){
       gg = gg + geom_text(data = data.frame(lab = c('%l', '%m', '%h'), x = c(0, 0.62, 0.95), y = c(-0.05, 0.85, -0.05)),
                 aes(x = x, y =y, label = lab), inherit.aes = F, size = 3, fontface = "italic")  +
         theme(panel.background = element_blank(),
               panel.border = element_blank(),
               axis.title=element_blank(),
               axis.text=element_blank(),
               axis.ticks=element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank(),
               aspect.ratio=1) +
       geom_text(data = rlist, aes(x = 0.2, y = 0.93, label = r), inherit.aes = F, parse = TRUE, size = 4) +
       geom_path(data = data.frame(x = c(0, 0.5, 1, 0, 0.5), y = c(0, 0.86, 0, 0, 0.86)), aes(x = x, y= y), inherit.aes = F, lwd = 0.7) }
     else(
       gg = gg + scale_y_continuous(position = "right") +
         theme( aspect.ratio=1) + ylab('LUI coefficient of variation') +xlab('LUI mean') + xlim(c(0.8, 3))+
         geom_text(data = rlist, aes(x = 1.1, y = 0.73, label = r), inherit.aes = F, parse = TRUE, size = 4)
     )
     return(gg)
      }

 plots <- lapply(unique(data_average_melt$nservices), function(o) {
  grido = interpolate_tern(data_average_melt[data_average_melt$nservices == o,])
  outplot_tern(grido$grid, grido$R2)
 })

 grobs = lapply(plots, ggplotGrob)
 gg = grid.arrange(grobs = grobs, ncol = 1,  align = "v", axis = "l")
 ggsave(plot = gg, filename =  paste(c('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Results/Sensitivity analyses/TERN', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '2.pdf'), collapse = ""), width = 10, height = 15)

 if (method == 'Threshold' & no_plots == 10 & lui_class_method == 'luiquantile_30' ){
 data_average_melt[, lui_cv := lui_sd/lui_mean]
 plots <- lapply(unique(data_average_melt$nservices), function(o) {
   grido = interpolate_tern(data_average_melt[data_average_melt$nservices == o,], tern = F)
   outplot_tern(grido$grid, grido$R2, tern = F)
 })
 grobs = lapply(plots, ggplotGrob)
 gg = grid.arrange(grobs = grobs, ncol = 1,  align = "v", axis = "l")
 ggsave(plot = gg, filename =  paste(c('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Results/Sensitivity analyses/MEAN_SD', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '2.pdf'), collapse = ""), width = 10, height = 12)
}

#### Multifunctionality range depending on ... ####

 
Ranges =  interpolate_tern(data_average_melt, tern = TRUE)$range
Ranges$range = as.numeric(Ranges$range)
data_plots2 = setDT(merge(data_plot, env_data))
data_plots2$Region = factor(data_plots2$Exploratory, levels = c('A', 'H', 'S'))
levels(data_plots2$Region) =  c('South-West', 'Central', 'North')
Ranges = Ranges[order(Ranges$nservices),]

Ranges$Richness    = ifelse(grepl('Cons', Ranges$pretty_variables), 1, 0) + ifelse(grepl('All', Ranges$pretty_variables), 1, 0) 
Ranges$Tot_protein = ifelse(grepl('Prod', Ranges$pretty_variables), 1, 0) + ifelse(grepl('All', Ranges$pretty_variables), 1, 0) 
Ranges$Aesthetic   = ifelse(grepl('Aest', Ranges$pretty_variables), 1, 0) + ifelse(grepl('All', Ranges$pretty_variables), 1, 0) 
Ranges$Org_C_stock = ifelse(grepl('C', Ranges$pretty_variables), 1, 0)    + ifelse(grepl('All', Ranges$pretty_variables), 1, 0) 
Ranges$nvar = NA
Ranges$mean_abs  = NA
Ranges$SRV  = NA
Ranges$SRM = NA
Ranges$coeff_ratio = NA
Ranges$corr_LUI = NA
Ranges$cor_max= NA
for (i in 1:nrow(Ranges)){
  explo = Ranges[i,]$Region
  a = Ranges[i, c("Tot_protein", 'Richness', 'Aesthetic', 'Org_C_stock')]
  b = unlist(a); b = names(b[b>0])
 
  # Number of services
  Ranges$nvar[i] = sum(a)
  
  # Correlation among the 4 main ES
  M<-cor(data.frame(data_plots2[data_plots2$Region == explo, c("Aesthetic", "Richness", "Tot_protein","Org_C_stock")]))
  M[ upper.tri(M, diag = TRUE)] = NA
  Ranges$meancor[i] = mean(M[b,b], na.rm = T)
  
  M_lui<-cor(data.frame(data_plots2[data_plots2$Region == explo, c("Aesthetic", "Richness", "Tot_protein","Org_C_stock", 'LUI_2007to12')]))[5,]
  Ranges$SRV[i] = abs(var(M_lui[b]))
  Ranges$SRM[i] = mean(M_lui[b])
  
  # Correlation between ES and LUI / correlation between ES and main env factor
  if (i < 13){
    v = as.character(Ranges$pretty_variables[i])
    if( v == "C stock"){ v = "Org_C_stock"}
    if( v == "Conservation"){ v = "Richness"}
    
    M2 = cor(data.frame(data_plots2[Region == explo, c(v,"LUI_2007to12", env_variables), with=FALSE]))[,1]
    Ranges$corr_LUI[i] = M2[2]
    Ranges$cor_max[i] = names(M2[M2 == max(M2[3:8])])
    Ranges$corr_ratio[i] = abs(M2[2])/sum(abs(M2[3:8]))
    
    model_coeff = numeric(9)
    names(model_coeff) = c("LUI_2007to12", env_variables)
    for (k in names(model_coeff)){
      mod = lm(scale(unlist(data_plots2[data_plots2$Region == explo, ..v]))~ scale(unlist(data_plots2[data_plots2$Region == explo, ..k])))
      model_coeff[k] = coefficients(mod)[2]
      Ranges$coeff_ratio[i] = abs(model_coeff[1])/max(abs(model_coeff[2:9]))
    }
    }}


ggplot(Ranges, aes(range, x = pretty_variables, fill = Region)) + geom_col(position = position_dodge())



#print('Corr_ratio:')
print(summary(lm(range~coeff_ratio, Ranges[1:12,])))
pcor = paste(summary(lm(range~coeff_ratio, Ranges[1:12,]))$coefficients[2,c(1, 2, 4) ], collapse = "--")
gg_ranges_corr_ratio = ggplot(Ranges[1:12,], aes(x = coeff_ratio, y = range, color = Region)) + 
  geom_smooth( aes(x = coeff_ratio, y = range), method = 'lm', inherit.aes = FALSE, color = 'black', alpha = 0.2) +
  geom_point(size = 3) + 
  ylab('Ecosystem service range') + xlab('Relative strength of LUI effect') +
  scale_color_brewer(palette = "Accent", name = "Region")+
  theme_bw() +
  theme(legend.position = 'none') 

if(saveplots){
  ggsave(plot = gg_ranges_corr_ratio, paste(c('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Results/Sensitivity analyses/ggrangescorrratio', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '.pdf'), collapse = ""), width = 5, height = 3.5)
}

#print('Corr_SRV:')
print(summary(lm(range~SRV, Ranges )))
psrv = paste(summary(lm(range~SRV, Ranges))$coefficients[2,c(1, 2, 4) ], collapse = "--")

gg_ranges_srv = ggplot(Ranges, aes(x = SRV, y = range, color = Region)) + 
  geom_smooth( aes(x = SRV, y = range), method = 'lm', inherit.aes = FALSE, color = 'black', alpha = 0.2) +
  geom_point(size = 3) + 
  ylab('Multifunctionaliy range') + xlab('Service response variance') +
  scale_color_brewer(palette = "Accent", name = "Region")+
  theme_bw() +
  theme(legend.position = 'none') 
if(saveplots){
  ggsave(plot = gg_ranges_srv, paste(c('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Results/Sensitivity analyses/ggrangessrv', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '.pdf'), collapse = ""), width = 5, height = 3.5)
}

#print('Corr_nvar:')
print(summary(lm(range~nvar, Ranges )))
pnvar = paste(summary(lm(range~nvar, Ranges))$coefficients[2,c(1, 2, 4) ], collapse = "--")

gg_ranges_nES = ggplot(Ranges, aes(x = nvar, range, color = Region, fill = Region)) + 
  geom_smooth(data = Ranges, aes(x = nvar, range), color = "black", method = 'lm', alpha = 0.2, inherit.aes = F) +
  geom_point(size = 3) +
  ylab('Multifunctionality range') + xlab('Number of ecosystem services included') + 
  scale_fill_brewer(palette = "Accent", name = "Region") +
  scale_color_brewer(palette = "Accent", name = "Region")+
  theme_bw() + 
  theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    legend.position = 'none' ) 

if(saveplots){
  ggsave(plot = gg_ranges_nES, paste(c('/Users/Margot/Desktop/Research/Senckenberg/Project_Landscape_MF/Landscape_composition/Results/Sensitivity analyses/ggrangesnES', environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '.pdf'), collapse = ""), width = 5, height = 3.5, dpi = 800,   bg = "transparent")
}

print(paste(c(environmental_correction,'_lui', lui_class_method, '_plots', no_plots, '_', method_within_landscape, '_method', method, threshold_to_use, '_', pcor, '_', psrv, '_', pnvar), collapse = ""))
