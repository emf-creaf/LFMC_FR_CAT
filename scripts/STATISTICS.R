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

process_simulation <- function(SIMULATION, FILENAME, DF_TYPE = "ALL", TH) {
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
    threshold <- TH * mad
    
    MERGED_LFMC_ALL_OUTLIER <- MERGED_LFMC_ALL %>%
      mutate(is_outlier = (LFMC.MEASURED > (med + threshold)) | (LFMC.MEASURED < (med - threshold)))
    
    MERGED_LFMC_ALL <- MERGED_LFMC_ALL %>%
      mutate(is_outlier = (LFMC.MEASURED > (med + threshold)) | (LFMC.MEASURED < (med - threshold))) %>%
      mutate(LFMC.MEASURED = ifelse(is_outlier, NA, LFMC.MEASURED)) 
      
  }
  
  if (DF_TYPE == "outlier_top") {
    med <- median(MERGED_LFMC_ALL$LFMC.MEASURED, na.rm = TRUE) #median 
    mad <- mad(MERGED_LFMC_ALL$LFMC.MEASURED, na.rm = TRUE) #Median absolute deviation
    threshold <- TH * mad
    
    MERGED_LFMC_ALL_OUTLIER <- MERGED_LFMC_ALL %>% 
      mutate(is_outlier = (LFMC.MEASURED > (med + threshold)))
    
    MERGED_LFMC_ALL <- MERGED_LFMC_ALL %>% 
      mutate(is_outlier = (LFMC.MEASURED > (med + threshold))) %>% 
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
  
  if (DF_TYPE == "outlier" | DF_TYPE == "outlier_top") {
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
    if (DATA_TYPE == "summer") {
      names(data_list)[i] <- paste0(filename, "_SUMMER")
    } else if (DATA_TYPE == "outlier") {
      names(data_list)[i] <- paste0(filename, "_OUTLIER_", TH)
    } else if (DATA_TYPE == "outlier_top") {
      names(data_list)[i] <- paste0(filename, "_OUTLIER_TOP_", TH)
    } else {
      names(data_list)[i] <- filename
    }
  }
  
  return(list(stats_df = stats_df, data_list = data_list))
}

data<- process_all_simulations(simulations, DATA_TYPE = "all")
summer_data<- process_all_simulations(simulations, DATA_TYPE = "summer")
outlier3_data<- process_all_simulations(simulations, DATA_TYPE = "outlier", TH = 3)
outlier3_top_data<- process_all_simulations(simulations, DATA_TYPE = "outlier_top", TH = 3)
outlier2.5_data<- process_all_simulations(simulations, DATA_TYPE = "outlier", TH = 2.5)
outlier2.5_top_data<- process_all_simulations(simulations, DATA_TYPE = "outlier_top", TH = 2.5)

stats_df<-data$stats_df %>% 
  arrange(LFMC_TYPE)
summer_stats_df<-summer_data$stats_df %>% 
  arrange(LFMC_TYPE)
outlier3_stats_df<-outlier3_data$stats_df %>% 
  arrange(LFMC_TYPE)
outlier3_top_stats_df<-outlier3_top_data$stats_df %>% 
  arrange(LFMC_TYPE)
outlier2.5_stats_df<-outlier2.5_data$stats_df %>%
  arrange(LFMC_TYPE)
outlier2.5_top_stats_df<-outlier2.5_top_data$stats_df %>% 
  arrange(LFMC_TYPE)


dir.create("results/SIMULATIONS_DF/", showWarnings = F)
write.csv(stats_df, "results/SIMULATIONS_DF/LFMC_SIM_STATS.csv", row.names = F)
write.csv(summer_stats_df, "results/SIMULATIONS_DF/SUMMER_LFMC_SIM_STATS.csv", row.names = F)
write.csv(outlier3_stats_df, "results/SIMULATIONS_DF/OUTLIER3_LFMC_SIM_STATS.csv", row.names = F)
write.csv(outlier3_top_stats_df, "results/SIMULATIONS_DF/OUTLIER3_TOP_LFMC_SIM_STATS.csv", row.names = F)
write.csv(outlier2.5_stats_df, "results/SIMULATIONS_DF/OUTLIER2.5_LFMC_SIM_STATS.csv", row.names = F)
write.csv(outlier2.5_top_stats_df, "results/SIMULATIONS_DF/OUTLIER2.5_TOP_LFMC_SIM_STATS.csv", row.names = F)


data_list<-data$data_list
summer_data_list<-summer_data$data_list
outlier3_data_list<-outlier3_data$data_list
outlier3_top_data_list<-outlier3_top_data$data_list
outlier2.5_data_list<-outlier2.5_data$data_list
outlier2.5_top_data_list<-outlier2.5_top_data$data_list


saveRDS(data_list, "results/SIMULATIONS_DF/LFMC_SIM_DATA_LIST.rds")
saveRDS(summer_data_list, "results/SIMULATIONS_DF/SUMMER_LFMC_SIM_DATA_LIST.rds")
saveRDS(outlier3_data_list, "results/SIMULATIONS_DF/OUTLIER3_LFMC_SIM_DATA_LIST.rds")
saveRDS(outlier3_top_data_list, "results/SIMULATIONS_DF/OUTLIER3_TOP_LFMC_SIM_DATA_LIST.rds")
saveRDS(outlier2.5_data_list, "results/SIMULATIONS_DF/OUTLIER2.5_LFMC_SIM_DATA_LIST.rds")
saveRDS(outlier2.5_top_data_list, "results/SIMULATIONS_DF/OUTLIER2.5_TOP_LFMC_SIM_DATA_LIST.rds")


#load saved data####

stats_df<- read.csv("results/SIMULATIONS_DF/LFMC_SIM_STATS.csv")
summer_stats_df<- read.csv("results/SIMULATIONS_DF/SUMMER_LFMC_SIM_STATS.csv")
outlier3_stats_df<- read.csv("results/SIMULATIONS_DF/OUTLIER3_LFMC_SIM_STATS.csv")
outlier3_top_stats_df<- read.csv("results/SIMULATIONS_DF/OUTLIER3_TOP_LFMC_SIM_STATS.csv")
outlier2.5_stats_df<- read.csv("results/SIMULATIONS_DF/OUTLIER2.5_LFMC_SIM_STATS.csv")
outlier2.5_top_stats_df<- read.csv("results/SIMULATIONS_DF/OUTLIER2.5_TOP_LFMC_SIM_STATS.csv")


data_list<- readRDS("results/SIMULATIONS_DF/LFMC_SIM_DATA_LIST.rds")
summer_data_list<- readRDS("results/SIMULATIONS_DF/SUMMER_LFMC_SIM_DATA_LIST.rds")
outlier3_data_list<- readRDS("results/SIMULATIONS_DF/OUTLIER3_LFMC_SIM_DATA_LIST.rds")
outlier3_top_data_list<- readRDS("results/SIMULATIONS_DF/OUTLIER3_TOP_LFMC_SIM_DATA_LIST.rds")
outlier2.5_data_list<- readRDS("results/SIMULATIONS_DF/OUTLIER2.5_LFMC_SIM_DATA_LIST.rds")
outlier2.5_top_data_list<- readRDS("results/SIMULATIONS_DF/OUTLIER2.5_TOP_LFMC_SIM_DATA_LIST.rds")

#####################PLOTS######################################################

func_time_plot <- function(DATA_LIST, mark_outlier = FALSE) {
  time_plot <- list()
  
  for (i in seq_along(DATA_LIST)) {
    if (mark_outlier) {
      time_p <- ggplot(data = DATA_LIST[[i]], aes(x = date)) +
        geom_line(aes(y = LFMC.MODELED, colour = "LFMC.MODELED")) +
        geom_line(aes(y = LFMC_rodrigo, colour = "LFMC.RODRIGO")) +
        geom_point(aes(y = LFMC.MEASURED, colour = is_outlier)) +
        geom_line(data = DATA_LIST[[i]] %>% filter(!is_outlier) %>% na.omit(), aes(y = LFMC.MEASURED, colour = "LFMC.MEASURED")) +
        scale_color_manual(values = c("LFMC.MODELED" = "black", "LFMC.RODRIGO" = "blue", "LFMC.MEASURED" = "red", "FALSE" = "red", "TRUE" = "green")) +
        theme_classic() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        xlab("DATE") +
        ylab("LFMC") +
        labs(title = names(DATA_LIST)[i]) +
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") 
      #scale_y_continuous(limits = c(0, 200))
      
    } else {
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
        scale_x_date(date_breaks = "1 year", date_labels = "%Y") 
      #scale_y_continuous(limits = c(0, 200))
    }
    
    time_plot[[i]] <- time_p
    names(time_plot)[i] <- names(DATA_LIST)[i]
  }
  
  return(time_plot)
}

func_scatter_plot <- function(DATA_LIST, mark_outlier = FALSE) {
  scatter_plot <- list()
  
  for (i in seq_along(DATA_LIST)) {
    if (mark_outlier) {
      scatter_p <- ggplot(data = DATA_LIST[[i]], aes(x = LFMC.MODELED, y = LFMC.MEASURED)) +
        geom_point(aes(colour = is_outlier)) +
        stat_smooth(method = "lm") +
        theme_classic() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        xlim(c(0, max(max(DATA_LIST[[i]]$LFMC.MODELED, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
        ylim(c(0, max(max(DATA_LIST[[i]]$LFMC.MODELED, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
        labs(title = names(DATA_LIST)[i])
    } else {
      scatter_p <- ggplot(data = na.omit(DATA_LIST[[i]]), aes(x = LFMC.MODELED, y = LFMC.MEASURED)) +
        geom_point() +
        stat_smooth(method = "lm") +
        theme_classic() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        xlim(c(0, max(max(DATA_LIST[[i]]$LFMC.MODELED, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
        ylim(c(0, max(max(DATA_LIST[[i]]$LFMC.MODELED, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        labs(title = names(DATA_LIST)[i])
    }
    
    scatter_plot[[i]] <- scatter_p
    names(scatter_plot)[i] <- names(DATA_LIST)[i]
  }
  
  return(scatter_plot)
}

func_scatter_plotR <- function(DATA_LIST, mark_outlier = FALSE) {
  scatter_plotR <- list()
  
  for (i in seq_along(DATA_LIST)) {
    if (mark_outlier) {
      scatter_pR <- ggplot(data = DATA_LIST[[i]], aes(x = LFMC_rodrigo, y = LFMC.MEASURED)) +
        geom_point(aes(colour = is_outlier)) +
        stat_smooth(method = "lm") +
        theme_classic() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        xlim(c(0, max(max(DATA_LIST[[i]]$LFMC_rodrigo, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
        ylim(c(0, max(max(DATA_LIST[[i]]$LFMC_rodrigo, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
        labs(title = names(DATA_LIST)[i])
    } else {
      scatter_pR <- ggplot(data = na.omit(DATA_LIST[[i]]), aes(x = LFMC_rodrigo, y = LFMC.MEASURED)) +
        geom_point() +
        stat_smooth(method = "lm") +
        theme_classic() +
        theme(legend.position = "bottom", legend.title = element_blank()) +
        xlim(c(0, max(max(DATA_LIST[[i]]$LFMC_rodrigo, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
        ylim(c(0, max(max(DATA_LIST[[i]]$LFMC_rodrigo, na.rm = TRUE), max(DATA_LIST[[i]]$LFMC.MEASURED, na.rm = TRUE), na.rm = TRUE))) +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        labs(title = names(DATA_LIST)[i])
    }
    
    scatter_plotR[[i]] <- scatter_pR
    names(scatter_plotR)[i] <- names(DATA_LIST)[i]
  }
  
  return(scatter_plotR)
}

time_plot<-func_time_plot(data_list, mark_outlier = FALSE)
scatter_plot<-func_scatter_plot(data_list, mark_outlier = FALSE)
scatter_plotR<-func_scatter_plotR(data_list, mark_outlier = FALSE)

summer_time_plot<-func_time_plot(summer_data_list, mark_outlier = FALSE)
summer_scatter_plot<-func_scatter_plot(summer_data_list, mark_outlier = FALSE)
summer_scatter_plotR<-func_scatter_plotR(summer_data_list, mark_outlier = FALSE)

outlier3_time_plot<-func_time_plot(outlier3_data_list, mark_outlier = TRUE)
outlier3_scatter_plot<-func_scatter_plot(outlier3_data_list, mark_outlier = TRUE)
outlier3_scatter_plotR<-func_scatter_plotR(outlier3_data_list, mark_outlier = TRUE)

outlier3_top_time_plot<-func_time_plot(outlier3_top_data_list, mark_outlier = TRUE)
outlier3_top_scatter_plot<-func_scatter_plot(outlier3_top_data_list, mark_outlier = TRUE)
outlier3_top_scatter_plotR<-func_scatter_plotR(outlier3_top_data_list, mark_outlier = TRUE)

outlier2.5_time_plot<-func_time_plot(outlier2.5_data_list, mark_outlier = TRUE)
outlier2.5_scatter_plot<-func_scatter_plot(outlier2.5_data_list, mark_outlier = TRUE)
outlier2.5_scatter_plotR<-func_scatter_plotR(outlier2.5_data_list, mark_outlier = TRUE)

outlier2.5_top_time_plot<-func_time_plot(outlier2.5_top_data_list, mark_outlier = TRUE)
outlier2.5_top_scatter_plot<-func_scatter_plot(outlier2.5_top_data_list, mark_outlier = TRUE)
outlier2.5_top_scatter_plotR<-func_scatter_plotR(outlier2.5_top_data_list, mark_outlier = TRUE)

#####################DISPLAY PLOTS##############################################

plots_site_sp<-function(index,TIME_PLOT,SCATTER_PLOT,SCATTER_PLOTR,STATS_DF){
  
  p1 <- TIME_PLOT[[index]] + labs(title = NULL)
  p2 <- SCATTER_PLOT[[index]] + labs(title = NULL)
  p3 <- SCATTER_PLOTR[[index]] + labs(title = NULL)
  
  t1 <- ggtexttable(round(STATS_DF[index, 11:21],3), rows = NULL, theme = ttheme("light"))
  t2 <- ggtexttable(round(STATS_DF[(index+length(TIME_PLOT)), 11:21],3), rows = NULL, theme = ttheme("light"))
  
  #t1 <- ggtexttable(round(summer_stats_df[index, 11:21],3), rows = NULL, theme = ttheme("light"))
  #t2 <- ggtexttable(round(summer_stats_df[(index+length(time_plot)), 11:21],3), rows = NULL, theme = ttheme("light"))
  
  plot_design <- p1 / (p2 | p3) / (t1 | t2) +
    plot_layout(heights = c(0.35, 0.55, 0.1)) +
    plot_annotation(title = gsub(".csv", "", names(SCATTER_PLOT)[index]))
}

# print(plots_site_sp(1,time_plot,scatter_plot,scatter_plotR,stats_df))
# print(plots_site_sp(1,summer_time_plot,summer_scatter_plot,summer_scatter_plotR,summer_stats_df))
# print(plots_site_sp(1,outlier_time_plot,outlier_scatter_plot,outlier_scatter_plotR,outlier_stats_df))
# print(plots_site_sp(1,outlier_top_time_plot,outlier_top_scatter_plot,outlier_top_scatter_plotR,outlier_top_stats_df))

dir<-c("results/SIMULATIONS_PLOTS/ALL/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(time_plot)) {
  plot<-plots_site_sp(i,time_plot,scatter_plot,scatter_plotR,stats_df)
  plotname<-gsub(".csv", "", names(time_plot[i]))
  ggsave(filename = paste0(dir,plotname,".png"), plot = plot, width = 1920, height = 1080, units = "px", dpi = 96)
}

dir<-c("results/SIMULATIONS_PLOTS/SUMMER/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(summer_time_plot)) {
  plot<-plots_site_sp(i,summer_time_plot,summer_scatter_plot,summer_scatter_plotR,summer_stats_df)
  plotname<-gsub(".csv", "", names(summer_time_plot[i]))
  ggsave(filename = paste0(dir,plotname,".png"), plot = plot, width = 1920, height = 1080, units = "px", dpi = 96)
}

dir<-c("results/SIMULATIONS_PLOTS/OUTLIER3/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(outlier3_time_plot)) {
  plot<-plots_site_sp(i,outlier3_time_plot,outlier3_scatter_plot,outlier3_scatter_plotR,outlier3_stats_df)
  plotname<-gsub(".csv", "", names(outlier3_time_plot[i]))
  ggsave(filename = paste0(dir,plotname,".png"), plot = plot, width = 1920, height = 1080, units = "px", dpi = 96)
}

dir<-c("results/SIMULATIONS_PLOTS/OUTLIER3_TOP/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(outlier3_top_time_plot)) {
  plot<-plots_site_sp(i,outlier3_top_time_plot,outlier3_top_scatter_plot,outlier3_top_scatter_plotR,outlier3_top_stats_df)
  plotname<-gsub(".csv", "", names(outlier3_top_time_plot[i]))
  ggsave(filename = paste0(dir,plotname,".png"), plot = plot, width = 1920, height = 1080, units = "px", dpi = 96)
}

dir<-c("results/SIMULATIONS_PLOTS/OUTLIER2.5/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(outlier2.5_time_plot)) {
  plot<-plots_site_sp(i,outlier2.5_time_plot,outlier2.5_scatter_plot,outlier2.5_scatter_plotR,outlier2.5_stats_df)
  plotname<-gsub(".csv", "", names(outlier2.5_time_plot[i]))
  ggsave(filename = paste0(dir,plotname,".png"), plot = plot, width = 1920, height = 1080, units = "px", dpi = 96)
}

dir<-c("results/SIMULATIONS_PLOTS/OUTLIER2.5_TOP/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:length(outlier2.5_top_time_plot)) {
  plot<-plots_site_sp(i,outlier2.5_top_time_plot,outlier2.5_top_scatter_plot,outlier2.5_top_scatter_plotR,outlier2.5_top_stats_df)
  plotname<-gsub(".csv", "", names(outlier2.5_top_time_plot[i]))
  ggsave(filename = paste0(dir,plotname,".png"), plot = plot, width = 1920, height = 1080, units = "px", dpi = 96)
}

#################SECOND LEVEL ANALISIS #################################################################  

# stats_df<-read.csv("results/SIMULATIONS_DF/LFMC_SIM_STATS.csv")
# 
# plot<-stats_df %>%
#   mutate(death = n_pred < 4000) %>%
#   filter(LFMC_TYPE == "MEASURED") %>% 
#   ggplot(aes(x = interaction(LAI,SOIL,METEO), y = n_pred, color = death)) +
#   geom_jitter() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   coord_flip() +
#   scale_color_manual(values = c("TRUE" = "#FF5733", "FALSE" = "#83FF79"))+
#   theme(legend.position = "bottom",
#         text = element_text(size = 15))+
#   facet_wrap(~SP)
# 
# # stats_df %>%
# #   mutate(death = n_pred < 4000) %>%
# #   filter(LFMC_TYPE == "MEASURED") %>% 
# #   filter(SP == "Genista cinerea") %>% 
# #   ggplot(aes(x = interaction(LAI,SOIL,METEO), y = n_pred, color = death)) +
# #   geom_jitter() +
# #   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
# #   coord_flip() +
# #   scale_color_manual(values = c("TRUE" = "#FF5733", "FALSE" = "#83FF79"))+
# #   theme(legend.position = "bottom",
# #         text = element_text(size = 15))+
# #   facet_wrap(~SP+SITE_NAME)
# 
# death<-stats_df %>%
#   mutate(death = n_pred < 4000) %>% 
#   filter(death) %>% 
#   ggplot(aes(x = interaction(SP), y = n_pred, color = death)) +
#   geom_jitter() +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
#   coord_flip() +
#   scale_color_manual(values = c("TRUE" = "#FF5733", "FALSE" = "#83FF79"))+
#   theme(legend.position = "none",
#         text = element_text(size =15))
# 
# plots <- list(
#   plot = plot,
#   death = death
# )
# 
# dir<-c("results/STATS_PLOTS/SIM/")
# dir.create(dir, showWarnings = FALSE, recursive = TRUE)
# 
# for (i in names(plots)) {
#   ggsave(filename = paste0(dir, i, ".png"), plot = plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
# }

##########BOXPLOTS######################################################################

boxplot <- function(DF, yy, facet = TRUE, nest_levels = c("LAI","METEO","SOIL")) {
  g <- ggplot(DF, aes(x = LFMC_TYPE, y = .data[[yy]], color = LFMC_TYPE)) +
    geom_boxplot(outliers = FALSE) +
    geom_jitter(alpha = 0.3) +
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
    ) + 
    stat_summary(
      fun.data = function(y) {
        return(data.frame(y = median(y), label = round(median(y), 2)))
      },
      geom = "text",
      vjust = -0.5,
      color = "black"
    )
  
  if (facet) {
    g <- g + facet_nested(as.formula(paste("~", paste(nest_levels, collapse = " + "))), labeller = label_both)
  }
  
  return(g)
}

#ALL DATA####

stats_df<-read.csv("results/SIMULATIONS_DF/LFMC_SIM_STATS.csv")

stats_df<- stats_df %>% 
  mutate(LAI = ifelse(LAI == "MEDFATE", "Allometric", ifelse(LAI == "MODIS", "Modis", LAI)),
         METEO = ifelse(METEO == "ERA5", "ERA5_Land", ifelse(METEO == "INTER", "Interpolated", METEO)),
         SOIL = ifelse(SOIL == "FALSE", "SoilGrids", ifelse(SOIL == "TRUE", "RC modification", SOIL)),
         LFMC_TYPE = ifelse(LFMC_TYPE == "MEASURED", "Modeled LFMC", ifelse(LFMC_TYPE == "RODRIGO", "Semi-Mechanistic LFMC", LFMC_TYPE))) %>% 
  filter(n > 20)

r2<-stats_df %>%
  boxplot(., "r2") +
  labs(title = "r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

Bias<-stats_df %>%
  boxplot(., "Bias") +
  labs(title = "Bias") +
  ylim(-60,60)

MAE<-stats_df %>%
  boxplot(., "MAE") +
  labs(title = "MAE") +
  ylim(0,60)

###SAVE PLOTS####

plots <- list(
  r2 = r2,
  Bias = Bias,
  MAE = MAE
)

dir<-c("results/STATS_PLOTS/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in names(plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

###DIFERENT FACETING DATA####

#LFMC TYPE####

R2_LFMC<-stats_df %>%
  boxplot(., "r2", facet = FALSE) +
  labs(title = "R2_LFMC") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

Bias_LFMC<-stats_df %>%
  boxplot(., "Bias", facet = FALSE) +
  labs(title = "Bias_LFMC") +
  ylim(-60,60)

MAE_LFMC<-stats_df %>%
  boxplot(., "MAE", facet = FALSE) +
  labs(title = "MAE_LFMC") +
  ylim(0,60)

#ONLY LAI####

R2_LAI<-stats_df %>%
  boxplot(., "r2", nest_levels = c("LAI")) +
  labs(title = "R2_LAI") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

Bias_LAI<-stats_df %>%
  boxplot(., "Bias", nest_levels = c("LAI")) +
  labs(title = "Bias_LAI") +
  ylim(-60,60)

MAE_LAI<-stats_df %>%
  boxplot(., "MAE", nest_levels = c("LAI")) +
  labs(title = "MAE_LAI") +
  ylim(0,60)

#ONLY METEO####

R2_METEO<-stats_df %>%
  boxplot(., "r2", nest_levels = c("METEO")) +
  labs(title = "R2_METEO") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

Bias_METEO<-stats_df %>%
  boxplot(., "Bias", nest_levels = c("METEO")) +
  labs(title = "Bias_METEO") +
  ylim(-60,60)

MAE_METEO<-stats_df %>%
  boxplot(., "MAE", nest_levels = c("METEO")) +
  labs(title = "MAE_METEO") +
  ylim(0,60)

#ONLY SOIL####

R2_SOIL<-stats_df %>%
  boxplot(., "r2", nest_levels = c("SOIL")) +
  labs(title = "R2_SOIL") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

Bias_SOIL<-stats_df %>%
  boxplot(., "Bias", nest_levels = c("SOIL")) +
  labs(title = "Bias_SOIL") +
  ylim(-60,60)

MAE_SOIL<-stats_df %>%
  boxplot(., "MAE", nest_levels = c("SOIL")) +
  labs(title = "MAE_SOIL") +
  ylim(0,60)

#SAVE PLOTS####

plots <- list(
  R2_LFMC = R2_LFMC,
  Bias_LFMC = Bias_LFMC,
  MAE_LFMC = MAE_LFMC,
  R2_LAI = R2_LAI,
  Bias_LAI = Bias_LAI,
  MAE_LAI = MAE_LAI,
  R2_METEO = R2_METEO,
  Bias_METEO = Bias_METEO,
  MAE_METEO = MAE_METEO,
  R2_SOIL = R2_SOIL,
  Bias_SOIL = Bias_SOIL,
  MAE_SOIL = MAE_SOIL
)

dir<-c("results/STATS_PLOTS/INDIVIDUAL/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in names(plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

##CAT AND FR####

###CAT####

CAT_r2<-stats_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "r2") +
  labs(title = "CAT_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

CAT_Bias<-stats_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "Bias") +
  labs(title = "CAT_Bias") +
  ylim(-60,60)

CAT_MAE<-stats_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "MAE") +
  labs(title = "CAT_MAE") +
  ylim(0,60)

###FR####

FR_r2<-stats_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "r2") +
  labs(title = "FR_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

FR_Bias<-stats_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "Bias") +
  labs(title = "FR_Bias") +
  ylim(-60,60)

FR_MAE<-stats_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "MAE") +
  labs(title = "FR_MAE") +
  ylim(0,60)


#SAVE PLOTS####

plots <- list(
  CAT_r2 = CAT_r2,
  CAT_Bias = CAT_Bias,
  CAT_MAE = CAT_MAE,
  FR_r2 = FR_r2,
  FR_Bias = FR_Bias,
  FR_MAE = FR_MAE
)

dir<-c("results/STATS_PLOTS/CAT_FR/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in names(plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

#SUMMER DATA####

summer_stats_df<-read.csv("results/SIMULATIONS_DF/SUMMER_LFMC_SIM_STATS.csv")

summer_stats_df<-summer_stats_df %>% 
  mutate(LAI = ifelse(LAI == "MEDFATE", "Allometric", ifelse(LAI == "MODIS", "Modis", LAI)),
         METEO = ifelse(METEO == "ERA5", "ERA5_Land", ifelse(METEO == "INTER", "Interpolated", METEO)),
         SOIL = ifelse(SOIL == "FALSE", "SoilGrids", ifelse(SOIL == "TRUE", "RC modification", SOIL)),
         LFMC_TYPE = ifelse(LFMC_TYPE == "MEASURED", "Modeled LFMC", ifelse(LFMC_TYPE == "RODRIGO", "Semi-Mechanistic LFMC", LFMC_TYPE))) %>% 
  filter(n > 20)

summer_df<- summer_stats_df %>% 
  mutate(LAI = ifelse(LAI == "MEDFATE", "Allometric", ifelse(LAI == "MODIS", "Modis", LAI)),
         METEO = ifelse(METEO == "ERA5", "ERA5_Land", ifelse(METEO == "INTER", "Interpolated", METEO)),
         SOIL = ifelse(SOIL == "FALSE", "SoilGrids", ifelse(SOIL == "TRUE", "RC modification", SOIL)),
         LFMC_TYPE = ifelse(LFMC_TYPE == "MEASURED", "Modeled LFMC", ifelse(LFMC_TYPE == "RODRIGO", "Semi-Mechanistic LFMC", LFMC_TYPE))) %>% 
  filter(n_pred > 1300 & n > 20)

summer_r2<-summer_df %>%
  boxplot(., "r2") +
  labs(title = "summer_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

summer_Bias<- summer_df %>%
  boxplot(., "Bias") +
  labs(title = "summer_Bias") +
  ylim(-60,60)

summer_MAE<-summer_df %>%
  boxplot(., "MAE") +
  labs(title = "summer_MAE") +
  ylim(0,60)

###CAT####

summer_CAT_r2<-summer_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "r2") +
  labs(title = "summer_CAT_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

summer_CAT_Bias<- summer_df %>% 
  filter(SOURCE == "CAT") %>% 
  boxplot(., "Bias") +
  labs(title = "summer_CAT_Bias") +
  ylim(-60,60)

summer_CAT_MAE<-summer_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "MAE") +
  labs(title = "summer_CAT_MAE") +
  ylim(0,60)

###FR####

summer_FR_r2<-summer_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "r2") +
  labs(title = "summer_FR_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

summer_FR_Bias<- summer_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "Bias") +
  labs(title = "summer_FR_Bias") +
  ylim(-60,60)

summer_FR_MAE<-summer_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "MAE") +
  labs(title = "summer_FR_MAE") +
  ylim(0,60)

###SAVE PLOTS####

summer_plots <- list(
  summer_r2 = summer_r2,
  summer_Bias = summer_Bias,
  summer_MAE = summer_MAE,
  summer_CAT_r2 = summer_CAT_r2,
  summer_CAT_Bias = summer_CAT_Bias,
  summer_CAT_MAE = summer_CAT_MAE,
  summer_FR_r2 = summer_FR_r2,
  summer_FR_Bias = summer_FR_Bias,
  summer_FR_MAE = summer_FR_MAE
)

dir<-c("results/STATS_PLOTS/SUMMER/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in names(summer_plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = summer_plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

##outlier3 DATA####

outlier3_stats_df<-read.csv("results/SIMULATIONS_DF/outlier3_LFMC_SIM_STATS.csv")

outlier3_df<- outlier3_stats_df %>% 
  mutate(LAI = ifelse(LAI == "MEDFATE", "Allometric", ifelse(LAI == "MODIS", "Modis", LAI)),
         METEO = ifelse(METEO == "ERA5", "ERA5_Land", ifelse(METEO == "INTER", "Interpolated", METEO)),
         SOIL = ifelse(SOIL == "FALSE", "SoilGrids", ifelse(SOIL == "TRUE", "RC modification", SOIL)),
         LFMC_TYPE = ifelse(LFMC_TYPE == "MEASURED", "Modeled LFMC", ifelse(LFMC_TYPE == "RODRIGO", "Semi-Mechanistic LFMC", LFMC_TYPE))) %>% 
  filter(n > 20)

outlier3_r2<-outlier3_df %>%
  boxplot(., "r2") +
  labs(title = "outlier3_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier3_Bias<- outlier3_df %>%
  boxplot(., "Bias") +
  labs(title = "outlier3_Bias") +
  ylim(-60,60)

outlier3_MAE<-outlier3_df %>%
  boxplot(., "MAE") +
  labs(title = "outlier3_MAE") +
  ylim(0,60)

###CAT####

outlier3_CAT_r2<-outlier3_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "r2") +
  labs(title = "outlier3_CAT_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier3_CAT_Bias<- outlier3_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "Bias") +
  labs(title = "outlier3_CAT_Bias") +
  ylim(-60,60)

outlier3_CAT_MAE<-outlier3_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "MAE") +
  labs(title = "outlier3_CAT_MAE") +
  ylim(0,60)

###FR####

outlier3_FR_r2<-outlier3_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "r2") +
  labs(title = "outlier3_FR_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier3_FR_Bias<- outlier3_df %>%
  filter(SOURCE == "FR") %>%
  boxplot(., "Bias") +
  labs(title = "outlier3_FR_Bias") +
  ylim(-60,60)

outlier3_FR_MAE<-outlier3_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "MAE") +
  labs(title = "outlier3_FR_MAE") +
  ylim(0,60)

###SAVE####

outlier3_plots <- list(
  outlier3_r2 = outlier3_r2,
  outlier3_Bias = outlier3_Bias,
  outlier3_MAE = outlier3_MAE,
  outlier3_CAT_r2 = outlier3_CAT_r2,
  outlier3_CAT_Bias = outlier3_CAT_Bias,
  outlier3_CAT_MAE = outlier3_CAT_MAE,
  outlier3_FR_r2 = outlier3_FR_r2,
  outlier3_FR_Bias = outlier3_FR_Bias,
  outlier3_FR_MAE = outlier3_FR_MAE
)

dir<-c("results/STATS_PLOTS/outlier3/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in names(outlier3_plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = outlier3_plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

#outlier3_TOP DATA####

outlier3_top_stats_df<-read.csv("results/SIMULATIONS_DF/outlier3_TOP_LFMC_SIM_STATS.csv")

outlier3_top_df<- outlier3_top_stats_df %>% 
  mutate(LAI = ifelse(LAI == "MEDFATE", "Allometric", ifelse(LAI == "MODIS", "Modis", LAI)),
         METEO = ifelse(METEO == "ERA5", "ERA5_Land", ifelse(METEO == "INTER", "Interpolated", METEO)),
         SOIL = ifelse(SOIL == "FALSE", "SoilGrids", ifelse(SOIL == "TRUE", "RC modification", SOIL)),
         LFMC_TYPE = ifelse(LFMC_TYPE == "MEASURED", "Modeled LFMC", ifelse(LFMC_TYPE == "RODRIGO", "Semi-Mechanistic LFMC", LFMC_TYPE))) %>% 
  filter(n > 20)

outlier3_top_r2<-outlier3_top_df %>%
  boxplot(., "r2") +
  labs(title = "outlier3_top_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier3_top_Bias<- outlier3_top_df %>%
  boxplot(., "Bias") +
  labs(title = "outlier3_top_Bias") +
  ylim(-60,60)

outlier3_top_MAE<-outlier3_top_df %>%
  boxplot(., "MAE") +
  labs(title = "outlier3_top_MAE") +
  ylim(0,60)

###CAT####

outlier3_top_CAT_r2<-outlier3_top_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "r2") +
  labs(title = "outlier3_top_CAT_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier3_top_CAT_Bias<- outlier3_top_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "Bias") +
  labs(title = "outlier3_top_CAT_Bias") +
  ylim(-60,60)

outlier3_top_CAT_MAE<-outlier3_top_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "MAE") +
  labs(title = "outlier3_top_CAT_MAE") +
  ylim(0,60)

###FR####

outlier3_top_FR_r2<-outlier3_top_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "r2") +
  labs(title = "outlier3_top_FR_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier3_top_FR_Bias<- outlier3_top_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "Bias") +
  labs(title = "outlier3_top_FR_Bias") +
  ylim(-60,60)

outlier3_top_FR_MAE<-outlier3_top_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "MAE") +
  labs(title = "outlier3_top_FR_MAE") +
  ylim(0,60)

###SAVE####

outlier3_top_plots <- list(
  outlier3_top_r2 = outlier3_top_r2,
  outlier3_top_Bias = outlier3_top_Bias,
  outlier3_top_MAE = outlier3_top_MAE,
  outlier3_top_CAT_r2 = outlier3_top_CAT_r2,
  outlier3_top_CAT_Bias = outlier3_top_CAT_Bias,
  outlier3_top_CAT_MAE = outlier3_top_CAT_MAE,
  outlier3_top_FR_r2 = outlier3_top_FR_r2,
  outlier3_top_FR_Bias = outlier3_top_FR_Bias,
  outlier3_top_FR_MAE = outlier3_top_FR_MAE
)

dir<-c("results/STATS_PLOTS/outlier3_TOP/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in names(outlier3_top_plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = outlier3_top_plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

#outlier2.5 DATA####

outlier2.5_stats_df<-read.csv("results/SIMULATIONS_DF/outlier2.5_LFMC_SIM_STATS.csv")

outlier2.5_df<- outlier2.5_stats_df %>% 
  mutate(LAI = ifelse(LAI == "MEDFATE", "Allometric", ifelse(LAI == "MODIS", "Modis", LAI)),
         METEO = ifelse(METEO == "ERA5", "ERA5_Land", ifelse(METEO == "INTER", "Interpolated", METEO)),
         SOIL = ifelse(SOIL == "FALSE", "SoilGrids", ifelse(SOIL == "TRUE", "RC modification", SOIL)),
         LFMC_TYPE = ifelse(LFMC_TYPE == "MEASURED", "Modeled LFMC", ifelse(LFMC_TYPE == "RODRIGO", "Semi-Mechanistic LFMC", LFMC_TYPE))) %>% 
  filter(n > 20)

outlier2.5_r2<-outlier2.5_df %>%
  boxplot(., "r2") +
  labs(title = "outlier2.5_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier2.5_Bias<- outlier2.5_df %>%
  boxplot(., "Bias") +
  labs(title = "outlier2.5_Bias") +
  ylim(-60,60)

outlier2.5_MAE<-outlier2.5_df %>%
  boxplot(., "MAE") +
  labs(title = "outlier2.5_MAE") +
  ylim(0,60)

###CAT####

outlier2.5_CAT_r2<-outlier2.5_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "r2") +
  labs(title = "outlier2.5_CAT_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier2.5_CAT_Bias<- outlier2.5_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "Bias") +
  labs(title = "outlier2.5_CAT_Bias") +
  ylim(-60,60)

outlier2.5_CAT_MAE<-outlier2.5_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "MAE") +
  labs(title = "outlier2.5_CAT_MAE") +
  ylim(0,60)

###FR####

outlier2.5_FR_r2<-outlier2.5_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "r2") +
  labs(title = "outlier2.5_FR_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier2.5_FR_Bias<- outlier2.5_df %>%
  filter(SOURCE == "FR") %>%
  boxplot(., "Bias") +
  labs(title = "outlier2.5_FR_Bias") +
  ylim(-60,60)

outlier2.5_FR_MAE<-outlier2.5_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "MAE") +
  labs(title = "outlier2.5_FR_MAE") +
  ylim(0,60)

###SAVE####

outlier2.5_plots <- list(
  outlier2.5_r2 = outlier2.5_r2,
  outlier2.5_Bias = outlier2.5_Bias,
  outlier2.5_MAE = outlier2.5_MAE,
  outlier2.5_CAT_r2 = outlier2.5_CAT_r2,
  outlier2.5_CAT_Bias = outlier2.5_CAT_Bias,
  outlier2.5_CAT_MAE = outlier2.5_CAT_MAE,
  outlier2.5_FR_r2 = outlier2.5_FR_r2,
  outlier2.5_FR_Bias = outlier2.5_FR_Bias,
  outlier2.5_FR_MAE = outlier2.5_FR_MAE
)

dir<-c("results/STATS_PLOTS/outlier2.5/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in names(outlier2.5_plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = outlier2.5_plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

#outlier2.5_TOP DATA####

outlier2.5_top_stats_df<-read.csv("results/SIMULATIONS_DF/outlier2.5_TOP_LFMC_SIM_STATS.csv")

outlier2.5_top_df<- outlier2.5_top_stats_df %>% 
  mutate(LAI = ifelse(LAI == "MEDFATE", "Allometric", ifelse(LAI == "MODIS", "Modis", LAI)),
         METEO = ifelse(METEO == "ERA5", "ERA5_Land", ifelse(METEO == "INTER", "Interpolated", METEO)),
         SOIL = ifelse(SOIL == "FALSE", "SoilGrids", ifelse(SOIL == "TRUE", "RC modification", SOIL)),
         LFMC_TYPE = ifelse(LFMC_TYPE == "MEASURED", "Modeled LFMC", ifelse(LFMC_TYPE == "RODRIGO", "Semi-Mechanistic LFMC", LFMC_TYPE))) %>% 
  filter(n > 20)

outlier2.5_top_r2<-outlier2.5_top_df %>%
  boxplot(., "r2") +
  labs(title = "outlier2.5_top_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier2.5_top_Bias<- outlier2.5_top_df %>%
  boxplot(., "Bias") +
  labs(title = "outlier2.5_top_Bias") +
  ylim(-60,60)

outlier2.5_top_MAE<-outlier2.5_top_df %>%
  boxplot(., "MAE") +
  labs(title = "outlier2.5_top_MAE") +
  ylim(0,60)

###CAT####

outlier2.5_top_CAT_r2<-outlier2.5_top_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "r2") +
  labs(title = "outlier2.5_top_CAT_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier2.5_top_CAT_Bias<- outlier2.5_top_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "Bias") +
  labs(title = "outlier2.5_top_CAT_Bias") +
  ylim(-60,60)

outlier2.5_top_CAT_MAE<-outlier2.5_top_df %>%
  filter(SOURCE == "CAT") %>% 
  boxplot(., "MAE") +
  labs(title = "outlier2.5_top_CAT_MAE") +
  ylim(0,60)

###FR####

outlier2.5_top_FR_r2<-outlier2.5_top_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "r2") +
  labs(title = "outlier2.5_top_FR_r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

outlier2.5_top_FR_Bias<- outlier2.5_top_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "Bias") +
  labs(title = "outlier2.5_top_FR_Bias") +
  ylim(-60,60)

outlier2.5_top_FR_MAE<-outlier2.5_top_df %>%
  filter(SOURCE == "FR") %>% 
  boxplot(., "MAE") +
  labs(title = "outlier2.5_top_FR_MAE") +
  ylim(0,60)

###SAVE####

outlier2.5_top_plots <- list(
  outlier2.5_top_r2 = outlier2.5_top_r2,
  outlier2.5_top_Bias = outlier2.5_top_Bias,
  outlier2.5_top_MAE = outlier2.5_top_MAE,
  outlier2.5_top_CAT_r2 = outlier2.5_top_CAT_r2,
  outlier2.5_top_CAT_Bias = outlier2.5_top_CAT_Bias,
  outlier2.5_top_CAT_MAE = outlier2.5_top_CAT_MAE,
  outlier2.5_top_FR_r2 = outlier2.5_top_FR_r2,
  outlier2.5_top_FR_Bias = outlier2.5_top_FR_Bias,
  outlier2.5_top_FR_MAE = outlier2.5_top_FR_MAE
)

dir<-c("results/STATS_PLOTS/outlier2.5_TOP/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in names(outlier2.5_top_plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = outlier2.5_top_plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

# ###MERGED PLOTS####
# 
# all_r2<-(
#   (r2 +
#      theme(legend.position = "none"))
#   /
#     (summer_r2 +
#        theme(legend.position = "none",
#              strip.background = element_blank(),
#              strip.text = element_blank()))
#   /
#     (outlier_r2 +
#        theme(strip.background = element_blank(),
#              strip.text = element_blank()))
# ) + plot_annotation(title = "all_r2")
# 
# 
# 
# all_Bias<-(
#   (Bias +
#      theme(legend.position = "none"))
#   /
#     (summer_Bias +
#        theme(legend.position = "none",
#              strip.background = element_blank(),
#              strip.text = element_blank()))
#   /
#     (outlier_Bias +
#        theme(strip.background = element_blank(),
#              strip.text = element_blank()))
# ) + plot_annotation(title = "all_Bias")
# 
# all_MAE<-(
#   (MAE +
#      theme(legend.position = "none"))
#   /
#     (summer_MAE +
#        theme(legend.position = "none",
#              strip.background = element_blank(),
#              strip.text = element_blank()))
#   /
#     (outlier_MAE +
#        theme(strip.background = element_blank(),
#              strip.text = element_blank()))
# ) + plot_annotation(title = "all_MAE")
# 
# ###SAVE####
# 
# all_plots <- list(
#   all_r2 = all_r2,
#   all_Bias = all_Bias,
#   all_MAE = all_MAE
# )
# 
# dir<-c("results/STATS_PLOTS/ALL/")
# dir.create(dir, showWarnings = FALSE, recursive = TRUE)
# 
# for (i in names(all_plots)) {
#   ggsave(filename = paste0(dir, i, ".png"), plot = all_plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
# }


#FACET WITH SPECIES####

#ALL DATA####

SP_r2<-stats_df %>%
  boxplot(., "r2", nest_levels = c("SP")) +
  labs(title = "r2") +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.1))

SP_Bias<- stats_df %>%
  boxplot(., "Bias", nest_levels = c("SP")) +
  labs(title = "Bias") +
  ylim(-60,60)

SP_MAE<-stats_df %>%
  boxplot(., "MAE", nest_levels = c("SP")) +
  labs(title = "MAE") +
  ylim(0,60)

###SAVE####

SP_plots <- list(
  SP_r2 = SP_r2,
  SP_Bias = SP_Bias,
  SP_MAE = SP_MAE
)

dir<-c("results/STATS_PLOTS/SP/")
dir.create(dir, showWarnings = FALSE, recursive = TRUE)

for (i in names(SP_plots)) {
  ggsave(filename = paste0(dir, i, ".png"), plot = SP_plots[[i]], device = "png", width = 1920, height = 1080, units = "px", dpi = 130)
}

