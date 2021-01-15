Scenarios_list = Scenarios_land_use
scenario_name = 'UnevenAged_forests'

create_new_combinations_all <- function(Scenarios_list, scenario_no, data) {
  categories <- unlist(Scenarios_list[scenario_no == sc, 1:9])
  categories <- categories[categories>0]
  # We're also trying to do the analysis with even- and uneven-aged forests
  use_alternative_classif = FALSE
  if('Forest_even-aged' %in% names(categories) | 'Forest_uneven-aged' %in% names(categories)){  use_alternative_classif = TRUE}
  plots_to_keep <- c()
  error = NA
  # print(categories)
  for (i in names(categories)) {
    newplots = try(sample(which(All_ES_data_region$Classif == i | All_ES_data_region$Classif2 == i |  All_ES_data_region$LU == i),
                          categories[i]), silent=T)
    if (class(newplots) == "try-error"){
    newplots = sample(which(All_ES_data_region$Classif == i | All_ES_data_region$Classif2 == i |  All_ES_data_region$LU == i), 
                          categories[i], replace = TRUE)
    error = paste("Not enough", i, "plots, reusing same plot", sep = ' ')
            }
     plots_to_keep <- c(plots_to_keep, newplots)
      }
  return(list(plots_to_keep = sort(plots_to_keep), error = error))
}


create_new_combinations <- function(Scenarios_list, scenario_name, no_plots, data, region) {
  categories <- unlist(Scenarios_list[Scenario_name == scenario_name & Region == region, 6:(ncol(Scenarios_list) - 1)])
  categories <- categories[!is.na(categories) & categories != 'NA']
  categories <- round(categories * no_plots / 100)
  # We're also trying to do the analysis with even- and uneven-aged forests
  use_alternative_classif = FALSE
  if('Forest_even-aged' %in% names(categories) | 'Forest_uneven-aged' %in% names(categories)){  use_alternative_classif = TRUE}
 
  plots_to_keep <- c()
  # print(categories)
  for (i in names(categories)) {
    if (i %in% c("Forest", "Grassland")) { # If it is a LU type
      #  print(paste('LU type:', as.character(i)))
      LUtype <- gsub("_any", "", i)
      newplots = try(data[LU == LUtype, sample(Plot, categories[i])])
      if (class(newplots) == "try-error"){
        newplots = data[LU == LUtype, sample(Plot, categories[i], replace = TRUE)]
        print("Warning: Not enough plots to build landscape, reusing the same plot")
      }
      plots_to_keep <- c(plots_to_keep, newplots)
    }
    else {
      if (i %in% c("Crop")) { # If it is a LU type
        plots_to_keep <- c(plots_to_keep, data[LU == 'Crop', sample(Plot, categories[i], replace = TRUE)])
      }
      else {
        
        if (i %in% c("Forest_Mixed", "Forest_Coniferous", "Forest_Deciduous", "Grassland_low", "Grassland_medium", "Grassland_high", "Forest_even-aged", "Forest_uneven-aged")) { # If it is a LU management
          
          if (  use_alternative_classif == FALSE){ # Use normal classification
            newplots = try(data[Classif == i, sample(Plot, categories[i])])
          if (class(newplots) == "try-error"){
            newplots = data[Classif == i, sample(Plot, categories[i], replace = TRUE)]
            print("Warning: Not enough plots to build landscape, reusing the same plot")
          }}
          if (  use_alternative_classif == TRUE){ # Use normal classification
            newplots = try(data[Classif2 == i, sample(Plot, categories[i])])
            if (class(newplots) == "try-error"){
              newplots = data[Classif2 == i, sample(Plot, categories[i], replace = TRUE)]
              print("Warning: Not enough plots to build landscape, reusing the same plot")
            }}
          
          
          plots_to_keep <- c(plots_to_keep, newplots)
        }
        else {
          if (grepl("NO", i)) {
            LUtype <- unlist(strsplit(i, "_"))[1]
            LU_management_no <- gsub("NO_", "", i)
            newplots = try(data[LU == LUtype & Classif != LU_management_no, sample(Plot, categories[i])])
            if (class(newplots) == "try-error"){
              newplots = data[LU == LUtype & Classif != LU_management_no, sample(Plot, categories[i], replace = TRUE)]
              print("Warning: Not enough plots to build landscape, reusing the same plot")
            }
            plots_to_keep <- c(plots_to_keep, newplots)
          }
        }
      }
    }
  }
  return(sort(plots_to_keep))
}

classify_LUI <- function(raw_data, method = "quantile_30") {
  lui_class <- matrix(ncol = 3, nrow = 50)
  if (method == "quantile_30") {
    for (i in unique(raw_data$exploratory)) {
      raw_env_data_exploratory <- raw_data[raw_data$exploratory == i, ]
      lui_class[, i] <- cut(raw_env_data_exploratory$LUI_2007to12, breaks = quantile(raw_env_data_exploratory$LUI_2007to12, c(0, 0.33, 0.66, 1)))
      lui_class[is.na(lui_class)] <- 1
    }
  }
  if (method == "quantile_20") {
    for (i in unique(raw_data$exploratory)) {
      raw_env_data_exploratory <- raw_data[raw_data$exploratory == i, ]
      lui_class[, i] <- cut(raw_env_data_exploratory$LUI_2007to12, breaks = quantile(raw_env_data_exploratory$LUI_2007to12, c(0, 0.2, 0.4, 0.6, 0.8, 1)))
      lui_class[is.na(lui_class[, i]), i] <- 1
      lui_class[lui_class[, i] %in% c(2, 4), i] <- NA
      lui_class[, i] <- factor(lui_class[, i])
    }
  }
  return(factor(c(lui_class[, 1], lui_class[, 2], lui_class[, 3])))
}

model_letters <- function(value, luiclass, Exploratory, type = "group") {
  if (type == "group") {
    mod <- lm(value ~ luiclass)
    l <- cld(emmeans(mod, "luiclass"), Letters = LETTERS)
    return(list(luiclass = l$luiclass, Letter = tolower(l$.group)))
  }
  else {
    mod <- lm(value ~ luiclass * Exploratory)
    l <- summary(emtrends(mod, var = "luiclass", ~Exploratory), null = 0, infer = c(TRUE, TRUE))
    l$stars <- ifelse(l$p.value > 0.05, "n.s.", ifelse(l$p.value > 0.01, "*", ifelse(l$p.value > 0.001, "**", "***")))
    return(list(Exploratory = l$Exploratory, stars = tolower(l$stars)))
  }
}

scale_average_threshold <- function(x, threshold = NA, method) {
  if (method == "Average") {
    max <- quantile(x, 0.975, na.rm = TRUE)
    min <- quantile(x, 0.025, na.rm = TRUE)
    y <- (x - min) / (max - min)
    y[y > 1] <- 1
    y[y < 0] <- 0
  }
  if (method == "Threshold") {
    real_tresh <- quantile(x, threshold, na.rm = TRUE)
    y <- ifelse(x >= real_tresh, 1, 0)
  }
  if (method == "Threshold_perc") {
    max <- quantile(x, 0.975, na.rm = TRUE)
    real_tresh <- max * threshold
    y <- ifelse(x >= real_tresh, 1, 0)
  }
  return(y)
}

find_range <- function(value, low, med, ternary = TRUE) {
  library(compositions)

  mygrid <- data.frame(rbind(
    expand.grid(
      low = seq(0, 1, length.out = 100),
      med = seq(0, 1, length.out = 100)
    )
  ))
  mygrid$high <- 1 - mygrid$low - mygrid$med
  mygrid <- cbind(mygrid, ilr(mygrid))


  if (ternary == TRUE) {
    ilr_dat <- ilr(data.frame(low, med, 1 - low - med))
  }
  else {
    ilr_dat <- data.frame("V1" = low, "V2" = med)
  }

  model <- lm(value ~ poly(V1, V2, degree = 2), ilr_dat)
  mygrid$value <- predict(model, mygrid)
  mygrid$value[mygrid$value > 1 | mygrid$value < 0] <- NA

  return(diff(quantile(mygrid$value, probs = c(0.05, 0.95), na.rm = T)))
}
