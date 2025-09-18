library(tidyverse)
library(patchwork)
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

# files_path1<-list.files(paste0("results/SIMULATION_RESULTS/", sites), pattern = paste0(PATTERN,"\\.RDS$"), recursive = TRUE, full.names = TRUE)
# files_name1<-basename(files_path1)
# 
# sim_list<-list()
# for (i in 1:length(files_path1)) {
#   sim_list[[files_name1[i]]]<-readRDS(files_path1[i])
# }

#####################READ DATA_FILES############################################

files_path2<-list.files(paste0("results/SIMULATION_RESULTS/", sites), pattern = paste0(PATTERN,"\\.csv$"), recursive = TRUE, full.names = TRUE)
files_name2<-basename(files_path2) %>% sub("\\.csv$", "", .)

# files_pathrm<-dir(paste0("results/SIMULATION_RESULTS/", sites), pattern = PATTERN, recursive = TRUE, full.names = TRUE)
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

##SINGLE FILE#########

# file<-read.csv("results/SIMULATION_RESULTS/Badalona/Badalona_2012-2022_Cistus monspeliensis_MODIS_ERA5_FALSE/Badalona_2012-2022_Cistus monspeliensis_MODIS_ERA5_FALSE.csv")
# simulations<-list("Badalona_2012-2022_Cistus monspeliensis_MODIS_ERA5_FALSE" = file)

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
  #Bias.rel <- 100 * Bias / abs(mean(obs))
  MAE <- mean(abs(E)) #Mean absolute error
  #MAE.rel <- 100 * MAE / abs(mean(obs))
  r <- cor(obs, pred)
  r2<- r^2
  #NSE <- 1 - (sum((obs - pred) ^ 2) / sum((obs - mean(obs)) ^ 2)) #Nash–Sutcliffe model efficiency coefficient (NSE)
  #NSE.abs <- 1 - (sum(abs(obs - pred)) / sum(abs(obs - mean(obs))))
  return(
    list(
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


#####################EVALUATION STATISTICS######################################

# SIMULATION <- read.csv("results/SIMULATION_RESULTS/D06S2/D06S2_2012-2022_MEASURED_MEDFATE_ERA5_FALSE/D06S2_2012-2022_Cistus monspeliensis_MEDFATE_ERA5_FALSE.csv")
# FILENAME <- "D06S2_2012-2022_Cistus monspeliensis_MEDFATE_ERA5_FALSE"
# DF_TYPE <- c("SUMMER","outlier_top")
# TH <- 3

process_simulation <- function(SIMULATION, FILENAME, DF_TYPE = c("ALL"), TH) {
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
  
  if ("summer" %in% DF_TYPE) {
    MERGED_LFMC_ALL <- MERGED_LFMC_ALL %>%
      filter(month(date) >= 6 & month(date) <= 9)
  }
  
  if ("outlier" %in% DF_TYPE) {
    med <- median(MERGED_LFMC_ALL$LFMC.MEASURED, na.rm = TRUE) #median 
    mad <- mad(MERGED_LFMC_ALL$LFMC.MEASURED, na.rm = TRUE) #Median absolute deviation
    threshold <- TH * mad
    
    MERGED_LFMC_ALL_OUTLIER <- MERGED_LFMC_ALL %>%
      mutate(is_outlier = (LFMC.MEASURED > (med + threshold)) | (LFMC.MEASURED < (med - threshold)))
    
    MERGED_LFMC_ALL <- MERGED_LFMC_ALL %>%
      mutate(is_outlier = (LFMC.MEASURED > (med + threshold)) | (LFMC.MEASURED < (med - threshold))) %>%
      mutate(LFMC.MEASURED = ifelse(is_outlier, NA, LFMC.MEASURED)) 
  }
  
  if ("outlier_top" %in% DF_TYPE) {
    med <- median(MERGED_LFMC_ALL$LFMC.MEASURED, na.rm = TRUE) #median 
    mad <- mad(MERGED_LFMC_ALL$LFMC.MEASURED, na.rm = TRUE) #Median absolute deviation
    threshold <- TH * mad
    
    MERGED_LFMC_ALL_OUTLIER <- MERGED_LFMC_ALL %>% 
      mutate(is_outlier = (LFMC.MEASURED > (med + threshold)))
    
    MERGED_LFMC_ALL <- MERGED_LFMC_ALL %>% 
      mutate(is_outlier = (LFMC.MEASURED > (med + threshold))) %>% 
      mutate(LFMC.MEASURED = ifelse(is_outlier, NA, LFMC.MEASURED))
  }
  
  # if ("summer" %in% DF_TYPE) {
  #   MERGED_LFMC_ALL <- MERGED_LFMC_ALL %>%
  #     filter(month(date) >= 6 & month(date) <= 9)
  #   
  #   if ("outlier" %in% DF_TYPE | "outlier_top" %in% DF_TYPE){
  #     MERGED_LFMC_ALL_OUTLIER <- MERGED_LFMC_ALL_OUTLIER %>% 
  #       filter(month(date) >= 6 & month(date) <= 9)
  #   }
  # }
  
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
  
  if ("outlier" %in% DF_TYPE | "outlier_top" %in% DF_TYPE) {
    return(list(result = result, merged_data = MERGED_LFMC_ALL_OUTLIER))
  } else {
  return(list(result = result, merged_data = MERGED_LFMC_ALL))
  }
}


process_all_simulations <- function(SIMULATIONS, DATA_TYPE, TH) {
  stats_df <- data.frame()
  data_list <- list()
  
  for (i in 1:length(SIMULATIONS)) {
    filename <- names(SIMULATIONS[i])
    result <- process_simulation(SIMULATION = SIMULATIONS[[i]], FILENAME = filename, DF_TYPE = DATA_TYPE, TH)
    stats_df <- rbind(stats_df, result$result)
    data_list[[i]] <- result$merged_data
    if (all(DATA_TYPE == "summer")) {
      names(data_list)[i] <- paste0(filename, "_SUMMER")
    } else if (all(DATA_TYPE == "outlier")) {
      names(data_list)[i] <- paste0(filename, "_OUTLIER_", TH)
    } else if (all(DATA_TYPE == "outlier_top")) {
      names(data_list)[i] <- paste0(filename, "_OUTLIER_TOP_", TH)
    } else if (all(DATA_TYPE == c("summer", "outlier")) | all(DATA_TYPE == c("outlier", "summer"))){
      names(data_list)[i] <- paste0(filename, "_SUMMER_OUTLIER_", TH)
    } else if (all(DATA_TYPE == c("summer", "outlier_top")) | all(DATA_TYPE == c("outlier_top", "summer"))){
      names(data_list)[i] <- paste0(filename, "_SUMMER_OUTLIER_TOP_", TH)
    } else {
      names(data_list)[i] <- filename
    }
  }
  
  return(list(stats_df = stats_df, data_list = data_list))
}

data<- process_all_simulations(simulations, DATA_TYPE = "all")
summer_data <- process_all_simulations(simulations, DATA_TYPE = "summer")
outlier3_top_data<- process_all_simulations(simulations, DATA_TYPE = "outlier_top", TH = 3)
summer_outlier3_top_data<- process_all_simulations(simulations, DATA_TYPE = c("summer", "outlier"), TH = 3)

stats_df<-data$stats_df %>%
  arrange(LFMC_TYPE)
summer_stats_df <- summer_data$stats_df %>% 
  arrange(LFMC_TYPE)
outlier3_top_stats_df<-outlier3_top_data$stats_df %>%
  arrange(LFMC_TYPE)
summer_outlier3_top_stats_df<-summer_outlier3_top_data$stats_df %>%
  arrange(LFMC_TYPE)

dir.create("results/SIMULATIONS_DF/", showWarnings = F)
write.csv(stats_df, "results/SIMULATIONS_DF/LFMC_SIM_STATS.csv", row.names = F)
write.csv(summer_stats_df, "results/SIMULATIONS_DF/SUMMER_LFMC_SIM_STATS.csv", row.names = F)
write.csv(outlier3_top_stats_df, "results/SIMULATIONS_DF/OUTLIER3_TOP_LFMC_SIM_STATS.csv", row.names = F)
write.csv(summer_outlier3_top_stats_df, "results/SIMULATIONS_DF/SUMMER_OUTLIER3_TOP_LFMC_SIM_STATS.csv", row.names = F)

data_list<-data$data_list
summer_data_list <- summer_data$data_list
outlier3_top_data_list<-outlier3_top_data$data_list
summer_outlier3_top_data_list<-summer_outlier3_top_data$data_list

saveRDS(data_list, "results/SIMULATIONS_DF/LFMC_SIM_DATA_LIST.rds")
saveRDS(summer_data_list, "results/SIMULATIONS_DF/SUMMER_LFMC_SIM_DATA_LIST.rds")
saveRDS(outlier3_top_data_list, "results/SIMULATIONS_DF/OUTLIER3_TOP_LFMC_SIM_DATA_LIST.rds")
saveRDS(summer_outlier3_top_data_list, "results/SIMULATIONS_DF/SUMMER_OUTLIER3_TOP_LFMC_SIM_DATA_LIST.rds")

################################################################################

############################load saved data#####################################

################################################################################

# stats_df<- read.csv("results/SIMULATIONS_DF/LFMC_SIM_STATS.csv")
# summer_stats_df <- read.csv("results/SIMULATIONS_DF/SUMMER_LFMC_SIM_STATS.csv")
# outlier3_top_stats_df<- read.csv("results/SIMULATIONS_DF/OUTLIER3_TOP_LFMC_SIM_STATS.csv")
summer_outlier3_top_stats_df<- read.csv("results/SIMULATIONS_DF/SUMMER_OUTLIER3_TOP_LFMC_SIM_STATS.csv")

# data_list<- readRDS("results/SIMULATIONS_DF/LFMC_SIM_DATA_LIST.rds")
# summer_data_list <- readRDS("results/SIMULATIONS_DF/SUMMER_LFMC_SIM_DATA_LIST.rds")
outlier3_top_data_list<- readRDS("results/SIMULATIONS_DF/OUTLIER3_TOP_LFMC_SIM_DATA_LIST.rds")
summer_outlier3_top_data_list<- readRDS("results/SIMULATIONS_DF/SUMMER_OUTLIER3_TOP_LFMC_SIM_DATA_LIST.rds")

#####################PLOTS######################################################

func_time_plot <- function(DATA_LIST, detect_outliers = FALSE, remove_outlier = FALSE) {
  time_plot <- list()
  
  for (i in seq_along(DATA_LIST)) {
    time_p <- ggplot(data = DATA_LIST[[i]], aes(x = date)) +
      geom_line(aes(y = LFMC.MODELED, colour = "Fully-mechanistic LFMC")) +
      geom_line(aes(y = LFMC_rodrigo, colour = "Semi-mechanistic LFMC"))
    
    if (detect_outliers) {
      if (!remove_outlier) {
        time_p <- time_p + 
          geom_point(data = DATA_LIST[[i]] %>% filter(is_outlier), aes(y = LFMC.MEASURED, colour = "outlier"))
      }
      time_p <- time_p +
        geom_point(data = DATA_LIST[[i]] %>% filter(!is_outlier), aes(y = LFMC.MEASURED, colour = "Measured LFMC")) +
        geom_line(data = DATA_LIST[[i]] %>% filter(!is_outlier) %>% na.omit(), aes(y = LFMC.MEASURED, colour = "Measured LFMC"))
    } else {
      time_p <- time_p +
        geom_point(data = DATA_LIST[[i]], aes(y = LFMC.MEASURED, colour = "Measured LFMC")) +
        geom_line(data = DATA_LIST[[i]] %>% na.omit(), aes(y = LFMC.MEASURED, colour = "Measured LFMC"))
    }
    
    time_p <- time_p +
      scale_color_manual(values = c("Fully-mechanistic LFMC" = "black", "Semi-mechanistic LFMC" = "blue", "Measured LFMC" = "red", "outlier" = "orange")) +
      theme_classic() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      xlab("DATE") +
      ylab("LFMC") +
      labs(title = names(DATA_LIST)[i]) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(limits = c(0, 220), expand = c(0,0))
    
    time_plot[[i]] <- time_p
    names(time_plot)[i] <- names(DATA_LIST)[i]
  }
  
  return(time_plot)
}

func_time_plot_summer <- function(DATA_LIST, detect_outliers = FALSE, remove_outlier = FALSE) {
  time_plot <- list()
  
  for (i in seq_along(DATA_LIST)) {
    DATA_LIST[[i]] <- DATA_LIST[[i]] %>% 
      mutate(is.summer = month(date) >= 6 & month(date) <= 9) %>% 
      mutate(summer_group = cumsum(lag(is.summer, default = FALSE) != is.summer)) %>%
      mutate(summer_group = ifelse(is.summer, summer_group, NA))
  }
  
  for (i in seq_along(DATA_LIST)) {
    time_p <- ggplot(data = DATA_LIST[[i]], aes(x = date)) +
      geom_line(aes(y = LFMC.MODELED, colour = "Fully-mechanistic LFMC")) +
      geom_line(aes(y = LFMC_rodrigo, colour = "Semi-mechanistic LFMC"))
    
    if (detect_outliers) {
      if (!remove_outlier) {
        time_p <- time_p + 
          geom_point(data = DATA_LIST[[i]] %>% filter(is_outlier & is.summer), aes(y = LFMC.MEASURED, colour = "outlier"))
      }
      time_p <- time_p +
        geom_point(data = DATA_LIST[[i]] %>% filter(!is_outlier & is.summer), aes(y = LFMC.MEASURED, colour = "Measured LFMC")) +
        geom_line(data = DATA_LIST[[i]] %>% filter(!is_outlier & is.summer) %>% na.omit(), aes(y = LFMC.MEASURED, group = summer_group, colour = "Measured LFMC"))
    } else {
      time_p <- time_p +
        geom_point(data = DATA_LIST[[i]] %>% filter(is.summer), aes(y = LFMC.MEASURED, colour = "Measured LFMC")) +
        geom_line(data = DATA_LIST[[i]] %>% filter(is.summer) %>% na.omit(), aes(y = LFMC.MEASURED, group = summer_group, colour = "Measured LFMC"))
    }
    
    time_p <- time_p +
      scale_color_manual(values = c("Fully-mechanistic LFMC" = "black", "Semi-mechanistic LFMC" = "blue", "Measured LFMC" = "red", "outlier" = "orange")) +
      theme_classic() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      xlab("DATE") +
      ylab("LFMC") +
      labs(title = names(DATA_LIST)[i]) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      scale_y_continuous(limits = c(0, 220), expand = c(0,0))
    
    time_plot[[i]] <- time_p
    names(time_plot)[i] <- names(DATA_LIST)[i]
  }
  
  return(time_plot)
}

func_scatter_plot <- function(DATA_LIST, detect_outliers = FALSE, remove_outlier = FALSE, LFMC_TYPE = "MODELED") {
  scatter_plot <- list()
  
  for (i in seq_along(DATA_LIST)) {
    x_variable <- if (LFMC_TYPE == "MODELED") {
      "LFMC.MODELED"
    } else if (LFMC_TYPE == "RODRIGO") {
      "LFMC_rodrigo"
    }
    
    base_plot <- ggplot(data = DATA_LIST[[i]], aes_string(x = x_variable, y = "LFMC.MEASURED")) +
      theme_classic() +
      theme(legend.position = "bottom", legend.title = element_blank()) +
      # xlim(c(0, 220)) +
      # ylim(c(0, 220)) +
      # xlim(c(0, max(max(DATA_LIST[[i]]$x_variable, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
      # ylim(c(0, max(max(DATA_LIST[[i]]$x_variable, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title = names(DATA_LIST)[i]) +
      scale_x_continuous(limits = c(0, 220), expand = c(0,0)) +
      scale_y_continuous(limits = c(0, 220), expand = c(0,0))
    
    if (detect_outliers) {
      if (remove_outlier) {
        scatter_p <- base_plot +
          geom_point(data = DATA_LIST[[i]] %>% filter(!is_outlier), colour = "black") #alpha = .3, stroke = 1, size = 2
      } else {
        scatter_p <- base_plot +
          geom_point(aes(colour = is_outlier)) +
          scale_color_manual(values = c("FALSE" = "black", "TRUE" = "orange"))
      }
    } else {
      scatter_p <- base_plot +
        geom_point()
    }
    
    scatter_p <- scatter_p +
      stat_smooth(method = "lm", se = FALSE)
    
    scatter_plot[[i]] <- scatter_p
    names(scatter_plot)[i] <- names(DATA_LIST)[i]
  }
  
  return(scatter_plot)
}

#print(func_scatter_plot(summer_outlier3_top_data_list[220], detect_outliers = TRUE, remove_outlier = F, LFMC_TYPE = "MODELED"))


# print(func_time_plot(outlier3_top_data_list[1], mark_outlier = TRUE))
# print(func_time_plot_summer(outlier3_top_data_list[1], mark_outlier = TRUE))
# print(func_time_plot_summer(summer_outlier3_top_data_list[1], mark_outlier = TRUE))

time_plot<-func_time_plot_summer(outlier3_top_data_list, detect_outliers = TRUE, remove_outlier = TRUE) ##NOO SUMMER DATA LIST; (FUNCIÓ és la SUMMER)
scatter_plot<-func_scatter_plot(summer_outlier3_top_data_list, detect_outliers = TRUE, remove_outlier = TRUE, LFMC_TYPE = "MODELED")
scatter_plotR<-func_scatter_plot(summer_outlier3_top_data_list, detect_outliers = TRUE, remove_outlier = TRUE, LFMC_TYPE = "RODRIGO")

#####################DISPLAY PLOTS##############################################

plots_site_sp<-function(index,TIME_PLOT,SCATTER_PLOT,SCATTER_PLOTR,STATS_DF,TITLE = TRUE, TYPE = "ALL"){
  
  p1 <- TIME_PLOT[[index]] + 
    labs(title = "A") +
    theme(text=element_text(size=19),
          axis.title = element_text(size=15),
          legend.position = "top")
  
  p2 <- SCATTER_PLOT[[index]] + 
    labs(title = "B") +
    xlab("Process-based LFMC") +
    ylab("Measured LFMC") +
    theme(legend.position="none",
          aspect.ratio = 1,
          text = element_text(size=19),
          axis.title = element_text(size=15))
  
  p3 <- SCATTER_PLOTR[[index]] + 
    labs(title = "C") +
    xlab("Semi-mechanistic LFMC") +
    ylab("Measured LFMC") +
    theme(legend.position="none",
          aspect.ratio = 1,
          text = element_text(size=19),
          axis.title = element_text(size=15))
  
  PB_STATS_DF <- STATS_DF %>% 
    filter(LFMC_TYPE == "MEASURED")
  
  SM_STATS_DF <- STATS_DF %>% 
    filter(LFMC_TYPE == "RODRIGO")
  
  t1 <- ggtexttable(round(PB_STATS_DF[index, 13:16],3),
                    rows = NULL,
                    theme = ttheme(base_style = "light", base_size = 19))
  t2 <- ggtexttable(round(SM_STATS_DF[index, 13:16],3),
                    rows = NULL,
                    theme = ttheme(base_style = "light", base_size = 19))
  
  PB_LFMC <- p2/t1 + plot_layout(heights = c(0.8,0.2))
  
  SM_LFMC  <- p3/t2 + plot_layout(heights = c(0.8,0.2))
  
  if (TYPE == "ALL") {
    
    plot_design <- p1 / (PB_LFMC | SM_LFMC)  +
      plot_layout(heights = c(0.4, 0.6))
    
    if (TITLE){
      plot_design <- plot_design +
        plot_annotation(title = gsub(".csv", "", names(SCATTER_PLOT)[index]))
    }
  }
  
  if (TYPE == "PB") {
    
    p1$layers <- p1$layers[-2]
    
    plot_design <- p1 / PB_LFMC  +
      plot_layout(heights = c(0.4, 0.6))
    
    if (TITLE){
      plot_design <- plot_design +
        plot_annotation(title = gsub(".csv", "", names(SCATTER_PLOT)[index]))
    }
    
  }
  
  if (TYPE == "SM") {
    
    p1$layers <- p1$layers[-1]
    
    plot_design <- p1 / SM_LFMC  +
      plot_layout(heights = c(0.4, 0.6))
    
    if (TITLE){
      plot_design <- plot_design +
        plot_annotation(title = gsub(".csv", "", names(SCATTER_PLOT)[index]))
    }
  }
  
  return(plot_design)
}

# a <- plots_site_sp(220,time_plot,scatter_plot,scatter_plotR,summer_outlier3_top_stats_df,TITLE = FALSE)
# dir<-c("results/")
# dir.create(dir, showWarnings = FALSE, recursive = TRUE)
# 
# ggsave(filename = paste0(dir,"aabaa",".tiff"), plot = a, width = 190, height = 190, units = "mm", dpi = 50, scale = 1.5)

######SAVE ALL#######

dir<-c("results/SIMULATIONS_PLOTS/ALL/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(time_plot)) {
  plot<-plots_site_sp(i,time_plot,scatter_plot,scatter_plotR,summer_outlier3_top_stats_df, TITLE = FALSE)
  plotname<-gsub(".csv", "", names(time_plot[i]))
  filename <- paste0(dir,i,"_",376+i,"_",plotname,".png")
  ggsave(filename = filename, plot = plot, width = 190, height = 190, units = "mm", dpi = 50, scale = 1.5)
}

###########GOOD_BAD############################################################

weight_mae <- 0.5
weight_bias <- 0.3
weight_r2 <- 0.2

GOOD_BAD_DF <- summer_outlier3_top_stats_df %>% 
  mutate(
    MAE_normalized = (MAE - min(MAE)) / (max(MAE) - min(MAE)),
    Bias_normalized = (abs(Bias) - min(abs(Bias))) / (max(abs(Bias)) - min(abs(Bias))),
    r2_normalized = (r2 - min(r2)) / (max(r2) - min(r2)),
    
    SCORE = (weight_mae * (1 - MAE_normalized)) +
      (weight_bias * (1 - Bias_normalized)) +
      (weight_r2 * r2_normalized),
    
    SCORE_normalized = (SCORE - min(SCORE)) / (max(SCORE) - min(SCORE))
  ) %>% 
  select(-SCORE)

good <- quantile(GOOD_BAD_DF$SCORE_normalized, 0.75)
bad <- quantile(GOOD_BAD_DF$SCORE_normalized, 0.25)

GOOD_BAD_DF <- GOOD_BAD_DF %>%
  mutate(
    class = case_when(
      SCORE_normalized > good ~ "Good",
      SCORE_normalized < bad ~ "Bad",    
      TRUE ~ "Medium"                     
    )
  )

#####SAVE GOOD BAD###########

dir_base <- "results/SIMULATIONS_PLOTS/GOOD_BAD/"

for (i in 1:length(time_plot)) {
  plot_PB <- plots_site_sp(i, time_plot, scatter_plot, scatter_plotR, summer_outlier3_top_stats_df, TITLE = FALSE, TYPE = "PB")
  plot_SM <- plots_site_sp(i, time_plot, scatter_plot, scatter_plotR, summer_outlier3_top_stats_df, TITLE = FALSE, TYPE = "SM")
  plotname <- gsub(".csv", "", names(time_plot[i]))
  
  GOOD_BAD_PB_DF <- GOOD_BAD_DF %>% 
    filter(LFMC_TYPE == "MEASURED")
  
  GOOD_BAD_SM_DF <- GOOD_BAD_DF %>%
    filter(LFMC_TYPE == "RODRIGO")
  
  GOOD_BAD_PB <- GOOD_BAD_PB_DF$class[i]
  GOOD_BAD_SM <- GOOD_BAD_SM_DF$class[i]
  
  dir_PB <- file.path(dir_base, GOOD_BAD_PB)
  dir_SM <- file.path(dir_base, GOOD_BAD_SM)
  
  dir.create(dir_PB, showWarnings = FALSE, recursive = TRUE)
  dir.create(dir_SM, showWarnings = FALSE, recursive = TRUE)
  
  filename_PB <- paste0(dir_PB, "/",i,"_",plotname, "_PB_", GOOD_BAD_PB, ".png")
  ggsave(filename = filename_PB, plot = plot_PB, width = 190, height = 190, units = "mm", dpi = 50, scale = 1.5)
  
  filename_SM <- paste0(dir_SM, "/",376+i,"_",plotname, "_SM_", GOOD_BAD_SM, ".png")
  ggsave(filename = filename_SM, plot = plot_SM, width = 190, height = 190, units = "mm", dpi = 50, scale = 1.5)
}
