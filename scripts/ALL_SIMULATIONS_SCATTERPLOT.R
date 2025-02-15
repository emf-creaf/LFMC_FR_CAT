library(tidyverse)
library(patchwork)
library(ggpubr)

### Brute force path to the floder with the simulations results list (.RDS objects)
path <- file.path("results","SIMULATIONS_DF")

### Load the simulation results list (Choose all data or only summer) 

#data_list <- readRDS(file.path(path,"OUTLIER3_TOP_LFMC_SIM_DATA_LIST.rds")) #All simulations
data_list <- readRDS(file.path(path,"SUMMER_OUTLIER3_TOP_LFMC_SIM_DATA_LIST.rds")) #Filtered only summer period (July to September)

### We performent diferent combinations of data input. Choose the desired combination for the plot:

    # Cheatsheet:
  
    #   LAI data:    MODIS (MODIS sensor)             or MEDFATE (Allometric estimations)
    #   Meteo data:  ERA5  (ERA5 satellite)           or INTER   (interpolated data (only Catalan network))
    #   Soil data:   FALSE (soil data from soligrids) or TRUE    (Soilgrids + rock content Modification)

### Regex expression to match the simulation name string

pattern <- "(?=.*MODIS)(?=.*ERA5)(?=.*FALSE)" 

### Filter the list with the desired simulation type:

matching_names <- str_detect(names(data_list), pattern) 
filtered_data_list <- data_list[matching_names]

### Bind the diferent simulations (diferent species and sites) into a single data frame

df <- bind_rows(filtered_data_list)
clean_df <- df |> 
  na.omit() |> #remove NA (keep only the days with measured data)
  filter(!is_outlier) #remove ouliers

### Scatter plot

scatter <- ggplot(clean_df, aes(x = LFMC.MODELED, y = LFMC.MEASURED)) +
  geom_point(alpha = .3, size = 1.5, shape = 21, fill = "black", stroke = NA) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # stat_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  scale_x_continuous(limits= c(0, max(clean_df$LFMC.MODELED,clean_df$LFMC.MEASURED)), 
                     expand = c(0,0)) +
  scale_y_continuous(limits= c(0,max(clean_df$LFMC.MODELED,clean_df$LFMC.MEASURED)),
                     expand = c(0,0)) +
  coord_equal(clip = "off") +
  labs(title = "LFMC modeled vs observed") +
  theme(legend.position = "bottom", 
        legend.title = element_blank(),
        aspect.ratio = 1,
        text = element_text(size = 15))

### Save plot

plot_path <- file.path("results","scatter") # Brute force path

ggsave(filename = file.path(plot_path,"scatter.png"), plot = scatter, width = 190, height = 190, units = "mm")

##### PLOT WITH EVALUATION INDICES ####

### Function to calculate the indices 
evalstats <- function(obs, pred) {
  sel_complete <- !(is.na(obs) | is.na(pred))
  n<- sum(sel_complete)
  
  n_obs<- sum(!is.na(obs))
  n_pred<- sum(!is.na(pred))
  
  obs <- obs[sel_complete]
  pred <- pred[sel_complete]
  E <- pred - obs
  Bias <- mean(E)
  #Bias.rel <- 100 * Bias / abs(mean(obs))
  MAE <- mean(abs(E)) #Mean absolute error
  #MAE.rel <- 100 * MAE / abs(mean(obs))
  r <- cor(obs, pred)
  r2<- r^2
  #NSE <- 1 - (sum((obs - pred) ^ 2) / sum((obs - mean(obs)) ^ 2)) #Nash–Sutcliffe model efficiency coefficient (NSE)
  #NSE.abs <- 1 - (sum(abs(obs - pred)) / sum(abs(obs - mean(obs))))
  return(
    data.frame(
      n_obs = n_obs,
      n_pred = n_pred,
      n = n,
      Bias = Bias,
      #Bias.rel = Bias.rel,
      MAE = MAE,
      #MAE.rel = MAE.rel,
      #r = r,
      r2 = r2#,
      #NSE = NSE,
      #NSE.abs = NSE.abs
    )
  )
}

### Data frame with the stats

stats <- evalstats(df$LFMC.MEASURED,df$LFMC.MODELED)

### Convert the stats data frame to a ggpubr table

stats_table <- ggtexttable(round(stats,3),
                           rows = NULL,
                           theme = ttheme(base_style = "light", base_size = 12))

### Create a final plot with the scatter + the table (patchwork)

scatter_stats <- (scatter + stat_smooth(method = "lm", se = FALSE, color = "red", linewidth = .5))/stats_table + plot_layout(heights = c(0.8,0.2))

ggsave(filename = file.path(plot_path,"scatter_stats.png"), plot = scatter_stats, width = 190, height = 190, units = "mm")

