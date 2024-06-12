library(tidyverse)
library(patchwork)
library(ggh4x)
library(ggpubr)
#################################CAT_FR_SITES###################################
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
sites<-CAT_FR_SITES$site_name
#sites<-CAT_FR_SITES$site_name[CAT_FR_SITES$source=="CAT"]
#################################LFMC_DATA######################################

CAT_LFMC<-read.csv("data/CAT_LFMC.csv")
CAT_LFMC$date<-as.Date(CAT_LFMC$date)

FR_LFMC<-read.csv("data/FR_LFMC.csv")
FR_LFMC$date<-as.Date(FR_LFMC$date,format = "%d/%m/%Y")

#################################STATISTICS#####################################

#PATTERN<-"2012"

#PATTERN<- ".*MODIS.*INTER.*FALSE.*"

#PATTERN<- ".*(leaf|fine)"

PATTERN<- ".*(TRUE|FALSE)"

#####################READ SIM_FILES#############################################

# files_path1<-list.files(paste0("data/PLOTS_SIMULATIONS/", sites), pattern = paste0(PATTERN,"\\.RDS$"), recursive = TRUE, full.names = TRUE)
# files_name1<-basename(files_path1)
# 
# sim_list<-list()
# for (i in 1:length(files_path1)) {
#   sim_list[[files_name1[i]]]<-readRDS(files_path1[i])
# }

#####################READ DATA_FILES############################################

files_path2<-list.files(paste0("data/PLOTS_SIMULATIONS/", sites), pattern = paste0(PATTERN,"\\.csv$"), recursive = TRUE, full.names = TRUE)
files_name2<-basename(files_path2) %>% sub("\\.csv$", "", .)

# files_pathrm<-dir(paste0("data/PLOTS_SIMULATIONS/", sites), pattern = PATTERN, recursive = TRUE, full.names = TRUE)
# unlink(files_pathrm, recursive = T)
# 
# for (path in files_pathrm) {
#   # Get the directory path
#   dir_path <- dirname(path)
#   # Remove the directory if it's empty
#   if (length(list.files(dir_path, full.names = TRUE)) == 0) {
#     unlink(dir_path, recursive = TRUE)
#   }
# }

simulations<-list()
for (i in 1:length(files_path2)) {
  simulations[[files_name2[i]]]<-read.csv(files_path2[i])
}

######EVALSTATS########

evalstats <- function(obs, pred) {
  sel_complete = !(is.na(obs) | is.na(pred))
  n<- sum(sel_complete)
  
  n_obs<- sum(!is.na(obs))
  n_pred<- sum(!is.na(pred))
  
  obs <- obs[sel_complete]
  pred <- pred[sel_complete]
  E <- pred - obs
  Bias <- mean(E)
  Bias.rel <- 100 * Bias / abs(mean(obs))
  MAE <- mean(abs(E)) #Mean absolute error
  MAE.rel <- 100 * MAE / abs(mean(obs))
  r <- cor(obs, pred)
  r2<- r^2
  NSE <- 1 - (sum((obs - pred) ^ 2) / sum((obs - mean(obs)) ^ 2)) #Nash–Sutcliffe model efficiency coefficient (NSE)
  NSE.abs <- 1 - (sum(abs(obs - pred)) / sum(abs(obs - mean(obs))))
  return(
    list(
      n_obs = n_obs,
      n_pred = n_pred,
      n = n,
      Bias = Bias,
      Bias.rel = Bias.rel,
      MAE = MAE,
      MAE.rel = MAE.rel,
      r = r,
      r2 = r2,
      NSE = NSE,
      NSE.abs = NSE.abs
    )
  )
}

#####################EVALUATION STATISTICS######################################

process_simulation <- function(SIMULATION, FILENAME, DF_TYPE = "ALL") {
  SIMULATION$date <- as.Date(SIMULATION$date)
  
  split <- strsplit(FILENAME, "_")[[1]]
  site_name <- split[1]
  years <- as.numeric(strsplit(split[2], "-")[[1]][1]):as.numeric(strsplit(split[2], "-")[[1]][2])
  sp <- split[3]
  lai <- split[4]
  meteo <- split[5]
  soil_mod <- split[6]
  type <- "SINGLE"
  changes <- split[7]
  
  source <- CAT_FR_SITES[CAT_FR_SITES$site_name == site_name,]$source
  
  cat(" #######################################", "\n",
      "site_name:", site_name, "\n",
      "years:", years, "\n",
      "sp:", sp, "\n",
      "LAI:", lai, "\n",
      "meteo:", meteo, "\n",
      "soil_mod:", soil_mod, "\n",
      "#######################################\n"
  )
  
  if (source == "CAT") {
    FILTERED_LFMC <- CAT_LFMC %>%
      rename(date = date, site = LocalityName, specie = sp_correct_name, LFMC = LFMC) %>%
      filter(if(type == "SINGLE") specie == sp else TRUE) %>% 
      filter(site == site_name, year(date) >= years[1]) %>% 
      select(date, LFMC, specie)
  } else if (source == "FR") {
    FILTERED_LFMC <- FR_LFMC %>%
      rename(date = date, site = SiteCode, specie = sp_correct_name, LFMC = RobustLFMC) %>%
      filter(if(type == "SINGLE") specie == sp else TRUE) %>% 
      filter(site == site_name, year(date) >= years[1]) %>% 
      select(date, LFMC, specie)
  }
  
  MERGED_LFMC_ALL <- SIMULATION %>%
    full_join(FILTERED_LFMC, by = c("date" = "date", "species" = "specie"), suffix = c(".MODELED", ".MEASURED"))
  
  if (DF_TYPE == "summer") {
    MERGED_LFMC_ALL <- MERGED_LFMC_ALL %>%
      filter(month(date) >= 6 & month(date) <= 9)
  }
  
  if (DF_TYPE == "outlier") {
    med <- median(MERGED_LFMC_ALL$LFMC.MEASURED, na.rm = TRUE) #median 
    mad <- mad(MERGED_LFMC_ALL$LFMC.MEASURED, na.rm = TRUE) #Median absolute deviation
    threshold <- 3 * mad
    
    #create  "is_outlier" column and 
    MERGED_LFMC_ALL <- MERGED_LFMC_ALL %>%
      mutate(is_outlier = (LFMC.MEASURED > (med + threshold)) | (LFMC.MEASURED < (med - threshold))) %>%
      mutate(LFMC.MEASURED = ifelse(is_outlier, NA, LFMC.MEASURED)) 
      
  }
  
  result_M <- data.frame(
    SITE_NAME = site_name,
    YEARS = paste0(years[1], "-", years[length(years)]),
    TYPE = type,
    SP = sp,
    LAI = lai,
    METEO = meteo,
    SOIL = soil_mod,
    LFMC_TYPE = "MEASURED",
    SOURCE = source,
    CHANGES = changes) %>%
    mutate(!!!c(evalstats(MERGED_LFMC_ALL$LFMC.MEASURED, MERGED_LFMC_ALL$LFMC.MODELED))) 
  
  result_R <- data.frame(
    SITE_NAME = site_name,
    YEARS = paste0(years[1], "-", years[length(years)]),
    TYPE = type,
    SP = sp,
    LAI = lai,
    METEO = meteo,
    SOIL = soil_mod,
    LFMC_TYPE = "RODRIGO",
    SOURCE = source,
    CHANGES = changes) %>%
    mutate(!!!c(evalstats(MERGED_LFMC_ALL$LFMC.MEASURED, MERGED_LFMC_ALL$LFMC_rodrigo)))
  
  result <- rbind(result_M, result_R)
  
  return(list(result = result, merged_data = MERGED_LFMC_ALL))
}


process_all_simulations <- function(SIMULATIONS, DATA_TYPE) {
  stats_df <- data.frame()
  data_list <- list()
  
  for (i in 1:length(SIMULATIONS)) {
    filename <- names(SIMULATIONS[i])
    result <- process_simulation(SIMULATION = SIMULATIONS[[i]], FILENAME = filename, DF_TYPE = DATA_TYPE)
    stats_df <- rbind(stats_df, result$result)
    data_list[[i]] <- result$merged_data
    if (DATA_TYPE == "summer") {
      names(data_list)[i] <- paste0(filename, "_SUMMER")
    } else if (DATA_TYPE == "outlier") {
      names(data_list)[i] <- paste0(filename, "_OUTLIER")
    } else {
      names(data_list)[i] <- filename
    }
  }
  
  return(list(stats_df = stats_df, data_list = data_list))
}

data<- process_all_simulations(simulations, DATA_TYPE = "all")
summer_data<- process_all_simulations(simulations, DATA_TYPE = "summer")
outlier_data<- process_all_simulations(simulations, DATA_TYPE = "outlier")

stats_df<-data$stats_df %>% 
  arrange(LFMC_TYPE)
summer_stats_df<-summer_data$stats_df %>% 
  arrange(LFMC_TYPE)
outlier_stats_df<-outlier_data$stats_df %>% 
  arrange(LFMC_TYPE)

# dir.create("data/SIMULATIONS_DF/", showWarnings = F)
# write.csv(stats_df, "data/SIMULATIONS_DF/LFMC_SIM_STATS.csv", row.names = F)
# write.csv(summer_stats_df, "data/SIMULATIONS_DF/SUMMER_LFMC_SIM_STATS.csv", row.names = F)
# write.csv(outlier_stats_df, "data/SIMULATIONS_DF/OUTLIER_LFMC_SIM_STATS.csv", row.names = F)


data_list<-data$data_list
summer_data_list<-summer_data$data_list
outlie_data_list<-outlier_data$data_list

saveRDS(data_list, "data/SIMULATIONS_DF/LFMC_SIM_DATA_LIST.rds")
saveRDS(summer_data_list, "data/SIMULATIONS_DF/SUMMER_LFMC_SIM_DATA_LIST.rds")
saveRDS(outlie_data_list, "data/SIMULATIONS_DF/OUTLIER_LFMC_SIM_DATA_LIST.rds")
#####################PLOTS######################################################

func_time_plot <- function(DATA_LIST) {
  time_plot <- list()
  
  for (i in seq_along(DATA_LIST)) {
    time_p <- ggplot(data = DATA_LIST[[i]], aes(x = date)) +
      geom_line(aes(y = LFMC.MODELED, colour = "LFMC.MODELED")) +
      geom_line(aes(y = LFMC_rodrigo, colour = "LFMC.RODRIGO")) +
      geom_point(aes(y = LFMC.MEASURED, colour = "LFMC.MEASURED")) +
      geom_line(data = na.omit(DATA_LIST[[i]]), aes(y = LFMC.MEASURED, colour = "LFMC.MEASURED")) +
      scale_color_manual(values = c("LFMC.MODELED" = "black", "LFMC.RODRIGO" = "blue", "LFMC.MEASURED" = "red")) +
      theme_classic() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      xlab("DATE") +
      ylab("LFMC") +
      labs(title = names(DATA_LIST)[i]) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(limits = c(0, 200))
    
    time_plot[[i]] <- time_p
    names(time_plot)[i] <- names(DATA_LIST)[i]
  }
  
  return(time_plot)
}

func_scatter_plot <- function(DATA_LIST) {
  scatter_plot <- list()
  
  for (i in seq_along(DATA_LIST)) {
    scatter_p <- ggplot(data = DATA_LIST[[i]], aes(x = LFMC.MODELED, y = LFMC.MEASURED)) +
      geom_point() +
      stat_smooth(method = "lm") +
      theme_classic() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      xlim(c(0, max(max(DATA_LIST[[i]]$LFMC.MODELED, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
      ylim(c(0, max(max(DATA_LIST[[i]]$LFMC.MODELED, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title = names(DATA_LIST)[i])
    
    scatter_plot[[i]] <- scatter_p
    names(scatter_plot)[i] <- names(DATA_LIST)[i]
  }
  
  return(scatter_plot)
}

func_scatter_plotR <- function(DATA_LIST) {
  scatter_plotR <- list()
  
  for (i in seq_along(DATA_LIST)) {
    scatter_pR <- ggplot(data = DATA_LIST[[i]], aes(x = LFMC_rodrigo, y = LFMC.MEASURED)) +
      geom_point() +
      stat_smooth(method = "lm") +
      theme_classic() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      xlim(c(0, max(max(DATA_LIST[[i]]$LFMC_rodrigo, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
      ylim(c(0, max(max(DATA_LIST[[i]]$LFMC_rodrigo, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title = names(DATA_LIST)[i])
    
    scatter_plotR[[i]] <- scatter_pR
    names(scatter_plotR)[i] <- names(DATA_LIST)[i]
  }
  
  return(scatter_plotR)
}

time_plot<-func_time_plot(data_list)
scatter_plot<-func_scatter_plot(data_list)
scatter_plotR<-func_scatter_plotR(data_list)

# summer_time_plot<-func_time_plot(summer_data_list)
# summer_scatter_plot<-func_scatter_plot(summer_data_list)
# summer_scatter_plotR<-func_scatter_plotR(summer_data_list)

#################################SAVE ALL THE SIMULATIONS STATISTICS AS .RData####

#rm(list = setdiff(ls(),c("stats_df_MEASURED_MODELED","data_list","sim_list", "scatter_plot", "time_plot")))

#####################DISPLAY PLOTS##############################################

plots_site_sp<-function(index,TIME_PLOT,SCATTER_PLOT,SCATTER_PLOTR,STATS_DF){
  
  p1 <- TIME_PLOT[[index]] + labs(title = NULL)
  p2 <- SCATTER_PLOT[[index]] + labs(title = NULL)
  p3 <- SCATTER_PLOTR[[index]] + labs(title = NULL)
  
  t1 <- ggtexttable(round(STATS_DF[index, 11:21],3), rows = NULL, theme = ttheme("light"))
  t2 <- ggtexttable(round(STATS_DF[(index+length(time_plot)), 11:21],3), rows = NULL, theme = ttheme("light"))
  
  #t1 <- ggtexttable(round(summer_stats_df[index, 11:21],3), rows = NULL, theme = ttheme("light"))
  #t2 <- ggtexttable(round(summer_stats_df[(index+length(time_plot)), 11:21],3), rows = NULL, theme = ttheme("light"))
  
  plot_design <- p1 / (p2 | p3) / (t1 | t2) +
    plot_layout(heights = c(0.35, 0.55, 0.1)) +
    plot_annotation(title = gsub(".csv", "", names(scatter_plot)[index]))
}

print(plots_site_sp(1,time_plot,scatter_plot,scatter_plotR,stats_df))

dir<-c("data/SIMULATIONS_PLOTS/")
#dir<-c("data/SIMULATIONS_PLOTS/SUMMER/")

for (i in 1:length(time_plot)) {
  plot<-plots_site_sp(i,time_plot,scatter_plot,scatter_plotR,stats_df)
  plotname<-gsub(".csv", "", names(time_plot[i]))
  ggsave(filename = paste0(dir,plotname,".png"), plot = plot, width = 1920, height = 1080, units = "px", dpi = 96)
}

# for (i in 1:length(summer_time_plot)) {
#   plot<-plots_site_sp(i,summer_time_plot,summer_scatter_plot,summer_scatter_plotR,summer_stats_df)
#   plotname<-gsub(".csv", "", names(summer_time_plot[i]))
#   ggsave(filename = paste0(dir,plotname,".png"), plot = plot, width = 1920, height = 1080, units = "px", dpi = 96)
# }

#################SECOND LEVEL ANALISIS #################################################################  

stats_df<-read.csv("data/SIMULATIONS_DF/LFMC_SIM_STATS.csv")

#stats_df2<-read.csv("data/SIMULATIONS_DF/SUMMER_LFMC_SIM_STATS.csv")

plot<-stats_df %>%
  mutate(death = n_pred < 4000) %>%
  filter(LFMC_TYPE == "MEASURED") %>% 
  ggplot(aes(x = interaction(LAI,SOIL,METEO), y = n_pred, color = death)) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip() +
  scale_color_manual(values = c("TRUE" = "#FF5733", "FALSE" = "#83FF79"))+
  theme(legend.position = "bottom",
        text = element_text(size = 15))+
  facet_wrap(~SP)

# stats_df %>%
#   mutate(death = n_pred < 4000) %>%
#   filter(LFMC_TYPE == "MEASURED") %>% 
#   filter(SP == "Genista cinerea") %>% 
#   ggplot(aes(x = interaction(LAI,SOIL,METEO), y = n_pred, color = death)) +
#   geom_jitter() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   coord_flip() +
#   scale_color_manual(values = c("TRUE" = "#FF5733", "FALSE" = "#83FF79"))+
#   theme(legend.position = "bottom",
#         text = element_text(size = 15))+
#   facet_wrap(~SP+SITE_NAME)

death<-stats_df %>%
  mutate(death = n_pred < 4000) %>% 
  filter(death) %>% 
  ggplot(aes(x = interaction(SP), y = n_pred, color = death)) +
  geom_jitter() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip() +
  scale_color_manual(values = c("TRUE" = "#FF5733", "FALSE" = "#83FF79"))+
  theme(legend.position = "none",
        text = element_text(size =15))

plots <- list(
  plot = plot,
  death = death
)

dir<-c("data/STATS_PLOTS/SIM/")

for (i in names(plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

################################################################################
stats_df<-read.csv("data/SIMULATIONS_DF/LFMC_SIM_STATS.csv")
#stats_df<-read.csv("data/SIMULATIONS_DF/SUMMER_LFMC_SIM_STATS.csv")
outlier_stats_df<-read.csv("data/SIMULATIONS_DF/OUTLIER_LFMC_SIM_STATS.csv")

df<- stats_df %>% 
  mutate(LAI = ifelse(LAI == "MEDFATE", "Allometric", ifelse(LAI == "MODIS", "Modis", LAI)),
         METEO = ifelse(METEO == "ERA5", "ERA5_Land", ifelse(METEO == "INTER", "Interpolated", METEO)),
         SOIL = ifelse(SOIL == "FALSE", "SoilGrids", ifelse(SOIL == "TRUE", "RC modification", SOIL)),
         LFMC_TYPE = ifelse(LFMC_TYPE == "MEASURED", "Modeled LFMC", ifelse(LFMC_TYPE == "RODRIGO", "Semi-Mechanistic LFMC", LFMC_TYPE))) %>% 
  filter(n_pred > 4000 & n > 20)

# df<-stats_df %>% 
#   mutate(LAI = ifelse(LAI == "MEDFATE", "Allometric", ifelse(LAI == "MODIS", "Modis", LAI)),
#          METEO = ifelse(METEO == "ERA5", "ERA5_Land", ifelse(METEO == "INTER", "Interpolated", METEO)),
#          SOIL = ifelse(SOIL == "FALSE", "SoilGrids", ifelse(SOIL == "TRUE", "RC modification", SOIL)),
#          LFMC_TYPE = ifelse(LFMC_TYPE == "MEASURED", "Modeled LFMC", ifelse(LFMC_TYPE == "RODRIGO", "Semi-Mechanistic LFMC", LFMC_TYPE))) %>% 
#   filter(n_pred > 1300 & n > 20)

boxplot <- function(df, yy, nest_levels = c("LAI","METEO","SOIL")) {
  ggplot(df, aes(x = LFMC_TYPE, y = .data[[yy]], color = LFMC_TYPE)) +
    geom_boxplot() +
    geom_jitter(alpha = 0.3) +
    #facet_nested(~ LAI + METEO + SOIL, labeller=label_both) +
    facet_nested(as.formula(paste("~", paste(nest_levels, collapse = " + "))), labeller = label_both) +
    theme_classic() +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.spacing = unit(0, "lines"),
      strip.background = element_rect(fill = NA, colour = "black", linewidth = 0.5),
      panel.border = element_rect(fill = NA, colour = "black", linewidth = 0.5),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
}

#ALL####

r2<-df %>%
  boxplot(., "r2") +
  labs(title = "r2") +
  ylim(0,1)

# r2_all_LAI<-df %>%
#   boxplot(., "r2", c("LAI")) +
#   labs(title = "r2") +
#   ylim(0,1)
# 
# r2_all_METEO<-df %>%
#   boxplot(., "r2", c("METEO")) +
#   labs(title = "r2") +
#   ylim(0,1)
# 
# r2_all_SOIL<-df %>%
#   boxplot(., "r2", c("SOIL")) +
#   labs(title = "r2") +
#   ylim(0,1)

Bias<- df %>%
  boxplot(., "Bias") +
  labs(title = "Bias") +
  ylim(-60,60)

MAE<-df %>%
  boxplot(., "MAE") +
  labs(title = "MAE") +
  ylim(0,60)

###CAT####

CAT_r2<-df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "r2") +
  labs(title = "CAT_r2") +
  ylim(0,1)

CAT_Bias<- df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "Bias") +
  labs(title = "CAT_Bias") +
  ylim(-60,60)

CAT_MAE<-df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "MAE") +
  labs(title = "CAT_MAE") +
  ylim(0,60)

###FR####

FR_r2<-df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "r2") +
  labs(title = "FR_r2") +
  ylim(0,1)

FR_Bias<- df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "Bias") +
  labs(title = "FR_Bias") +
  ylim(-60,60)

FR_MAE<-df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "MAE") +
  labs(title = "FR_MAE") +
  ylim(0,60)

######COMBINED PLOTS####

# r2_bias_MAE<-(
#   (r2 + 
#      theme(legend.position = "none"))
#   /
#     (Bias + 
#        theme(legend.position = "none",
#              strip.background = element_blank(),
#              strip.text = element_blank()))
#   /
#     (MAE + 
#        theme(strip.background = element_blank(), 
#              strip.text = element_blank())) 
# ) + plot_annotation(title = "r2_bias_MAE")
# 
# CAT_r2_bias_MAE<-(
#   (CAT_r2 + 
#      theme(legend.position = "none"))
#   /
#     (CAT_Bias + 
#        theme(legend.position = "none",
#              strip.background = element_blank(),
#              strip.text = element_blank()))
#   /
#     (CAT_MAE + 
#        theme(strip.background = element_blank(), 
#              strip.text = element_blank()))
# ) + plot_annotation(title = "CAT_r2_bias_MAE")
# 
# FR_r2_bias_MAE<-(
#   (FR_r2 + 
#      theme(legend.position = "none"))
#   /
#     (FR_Bias + 
#        theme(legend.position = "none",
#              strip.background = element_blank(),
#              strip.text = element_blank())) 
#   /
#     (FR_MAE + 
#        theme(strip.background = element_blank(), 
#              strip.text = element_blank()))
# ) + plot_annotation(title = "FR_r2_bias_MAE")
# 
# CAT_FR_r2<-(
#   (CAT_r2 + 
#      theme(legend.position = "none"))
#   / 
#     (FR_r2)
# ) + plot_annotation(title = "CAT_FR_r2")
# 
# CAT_FR_Bias<-(
#   (CAT_Bias + 
#      theme(legend.position = "none"))
#   / 
#     (FR_Bias)
# ) + plot_annotation(title = "CAT_FR_Bias")
# 
# CAT_FR_MAE<-(
#   (CAT_MAE + 
#      theme(legend.position = "none"))
#   / 
#     (FR_MAE)
# ) + plot_annotation(title = "CAT_FR_MAE")

#########SAVE####

plots <- list(
  A_r2 = r2,
  A_Bias = Bias,
  A_MAE = MAE,
  #A_r2_bias_MAE = r2_bias_MAE,
  B_CAT_r2 = CAT_r2,
  B_CAT_Bias = CAT_Bias,
  B_CAT_MAE = CAT_MAE,
  #B_CAT_r2_bias_MAE = CAT_r2_bias_MAE,
  C_FR_r2 = FR_r2,
  C_FR_Bias = FR_Bias,
  C_FR_MAE = FR_MAE,
  #C_FR_r2_bias_MAE = FR_r2_bias_MAE,
  #D_CAT_FR_r2 = CAT_FR_r2,
  #D_CAT_FR_Bias = CAT_FR_Bias,
  #D_CAT_FR_MAE = CAT_FR_MAE
)

dir<-c("data/STATS_PLOTS/ALL/")
#dir<-c("data/STATS_PLOTS/SUMMER/")

for (i in names(plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}


# #####ONLY ROSMARINUS##########################
# 
# #ALL
# 
# ROS_r2<-df %>%
#   filter(SP == "Rosmarinus officinalis") %>% 
#   boxplot(., "r2") +
#   labs(title = "ROS_r2") +
#   ylim(0,1)
# 
# ROS_Bias<- df %>%
#   filter(SP == "Rosmarinus officinalis") %>% 
#   boxplot(., "Bias") +
#   labs(title = "ROS_Bias") +
#   ylim(-60,60)
# 
# ROS_MAE<-df %>%
#   filter(SP == "Rosmarinus officinalis") %>% 
#   boxplot(., "MAE") +
#   labs(title = "ROS_MAE") +
#   ylim(0,60)
# 
# ###CAT
# 
# ROS_CAT_r2<-df %>%
#   filter(SP == "Rosmarinus officinalis") %>% 
#   filter(SOURCE == "CAT") %>% 
#   boxplot(., "r2") +
#   labs(title = "ROS_CAT_r2") +
#   ylim(0,1)
# 
# 
# ROS_CAT_Bias<- df %>%
#   filter(SP == "Rosmarinus officinalis") %>% 
#   filter(SOURCE == "CAT") %>% 
#   boxplot(., "Bias") +
#   labs(title = "ROS_CAT_Bias") +
#   ylim(-60,60)
# 
# ROS_CAT_MAE<-df %>%
#   filter(SP == "Rosmarinus officinalis") %>% 
#   filter(SOURCE == "CAT") %>% 
#   boxplot(., "MAE") +
#   labs(title = "ROS_CAT_MAE") +
#   ylim(0,60)
# 
# ###FR
# 
# ROS_FR_r2<-df %>%
#   filter(SP == "Rosmarinus officinalis") %>% 
#   filter(SOURCE == "FR") %>% 
#   boxplot(., "r2") +
#   labs(title = "ROS_FR_r2") +
#   ylim(0,1)
# 
# 
# ROS_FR_Bias<- df %>%
#   filter(SP == "Rosmarinus officinalis") %>% 
#   filter(SOURCE == "FR") %>% 
#   boxplot(., "Bias") +
#   labs(title = "ROS_FR_Bias") +
#   ylim(-60,60)
# 
# ROS_FR_MAE<-df %>%
#   filter(SP == "Rosmarinus officinalis") %>% 
#   filter(SOURCE == "FR") %>% 
#   boxplot(., "MAE") +
#   labs(title = "ROS_FR_MAE") +
#   ylim(0,60)
# 
# ######COMBINED PLOTS####
# 
# ROS_r2_bias_MAE<-(
#   (ROS_r2 + 
#      theme(legend.position = "none"))
#   /
#     (ROS_Bias + 
#        theme(legend.position = "none",
#              strip.background = element_blank(),
#              strip.text = element_blank()))
#   /
#     (ROS_MAE + 
#        theme(strip.background = element_blank(), 
#              strip.text = element_blank()))
# ) + plot_annotation(title = "ROS_r2_bias_MAE")
# 
# ROS_CAT_r2_bias_MAE<-(
#   (ROS_CAT_r2 + 
#      theme(legend.position = "none"))
#   /
#     (ROS_CAT_Bias + 
#        theme(legend.position = "none",
#              strip.background = element_blank(),
#              strip.text = element_blank()))
#   /
#     (ROS_CAT_MAE + 
#        theme(strip.background = element_blank(), 
#              strip.text = element_blank()))
# ) + plot_annotation(title = "ROS_CAT_r2_bias_MAE")
# 
# ROS_FR_r2_bias_MAE<-(
#   (ROS_FR_r2 + 
#      theme(legend.position = "none"))
#   /
#     (ROS_FR_Bias + 
#        theme(legend.position = "none",
#              strip.background = element_blank(),
#              strip.text = element_blank()))
#   /
#     (ROS_FR_MAE + 
#        theme(strip.background = element_blank(), 
#              strip.text = element_blank()))
# ) + plot_annotation(title = "ROS_FR_r2_bias_MAE")
# 
# ROS_CAT_FR_r2<-(
#   (ROS_CAT_r2 + 
#      theme(legend.position = "none"))
#   / 
#     (ROS_FR_r2)
# ) + plot_annotation(title = "ROS_CAT_FR_r2")
# 
# ROS_CAT_FR_Bias<-(
#   (ROS_CAT_Bias + 
#      theme(legend.position = "none"))
#   / 
#     (ROS_FR_Bias)
# ) + plot_annotation(title = "ROS_CAT_FR_Bias")
# 
# ROS_CAT_FR_MAE<-(
#   (ROS_CAT_MAE + 
#      theme(legend.position = "none"))
#   / 
#     (ROS_FR_MAE)
# ) + plot_annotation(title = "ROS_CAT_FR_MAE")
# 
# 
# plots <- list(
#   A_ROS_r2 = ROS_r2,
#   A_ROS_Bias = ROS_Bias,
#   A_ROS_MAE = ROS_MAE,
#   A_ROS_r2_bias_MAE = ROS_r2_bias_MAE,
#   B_ROS_CAT_r2 = ROS_CAT_r2,
#   B_ROS_CAT_Bias = ROS_CAT_Bias,
#   B_ROS_CAT_MAE = ROS_CAT_MAE,
#   B_ROS_CAT_r2_bias_MAE = ROS_CAT_r2_bias_MAE,
#   C_ROS_FR_r2 = ROS_FR_r2,
#   C_ROS_FR_Bias = ROS_FR_Bias,
#   C_ROS_FR_MAE = ROS_FR_MAE,
#   C_ROS_FR_r2_bias_MAE = ROS_FR_r2_bias_MAE,
#   D_ROS_CAT_FR_r2 = ROS_CAT_FR_r2,
#   D_ROS_CAT_FR_Bias = ROS_CAT_FR_Bias,
#   D_ROS_CAT_FR_MAE = ROS_CAT_FR_MAE
# )
# 
# dir<-c("data/STATS_PLOTS/ROS/")
# #dir<-c("data/STATS_PLOTS/SUMMER/ROS/")
# 
# for (i in names(plots)) {
#   ggsave(filename = paste0(dir, i, ".png"), plot = plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
# }
