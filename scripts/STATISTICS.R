library(tidyverse)
library(gridExtra)
library(ggpubr)
library(patchwork)

#################################CAT_FR_SITES###################################
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
sites<-CAT_FR_SITES$site_name

#################################LFMC_DATA######################################

CAT_LFMC<-read.csv("data/CAT_LFMC.csv")
CAT_LFMC$date<-as.Date(CAT_LFMC$date)

FR_LFMC<-read.csv("data/FR_LFMC.csv")
FR_LFMC$date<-as.Date(FR_LFMC$date,format = "%d/%m/%Y")

#################################FUNCTIONS######################################

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

ggplotRegression <- function (fit, abline=F, caption = NULL) {
  
  g<-ggplot(fit$model, aes(x = fit$model[,2], y = fit$model[,1])) + 
    geom_point() +
    stat_smooth(method = "lm") +
    labs(title = paste(
      " y = ", round(fit$coef[[2]],5), "x + ", round(fit$coef[[1]],5), "\n",
      "Adj R2 = ", round(summary(fit)$adj.r.squared,5), "\n",
      "p =", signif(summary(fit)$coef[2,4],5)))+
    theme_classic()+
    xlab(names(fit$model)[2])+
    ylab(names(fit$model)[1])
  
  if (abline==T) {
    g<-g+
      xlim(c(-0, max(max(fit$model[,2]),max(fit$model[,1]))))+
      ylim(c(-0, max(max(fit$model[,2]),max(fit$model[,1]))))+
      geom_abline(intercept = 0, slope = 1, linetype= "dashed")
  }
  
  if (!is.null(caption)) {
    g<-g+
      labs(caption = caption)
  }
  
  print(g)
}


#################################STATISTICS#####################################
#####################READ DATA_FILES############################################

#PATTERNS :

#"ALL_FILTERED.*MODIS.*"
#"SINGLE.*MODIS.*"
#"SINGLE.*MEDFATE.*"

PATTERN<-"SINGLE.*MEDFATE.*"

files_path2<-list.files(paste0("data/PLOTS/", sites), pattern = paste0(PATTERN,"\\.csv$"), recursive = TRUE, full.names = TRUE)
files_name2<-basename(files_path2)

simulations<-list()
for (i in 1:length(files_path2)) {
  simulations[[files_name2[i]]]<-read.csv(files_path2[i])
}


#####################EVALUATION STATISTICS######################################

stats_df_MEASURED_MODELED<-data.frame()
stats_df_MEASURED_RODRIGO<-data.frame()
data_list<-list()
for (i in 1:length(simulations)) {
  
  simulations[[i]]$date<-as.Date(simulations[[i]]$date)
  
  #EXTRACT SIMULATION PARAMETERS INFO####
  
  filename <-names(simulations[i])
  
  #SPLIT THE FILENAME
  split <- strsplit(filename, "_")[[1]]
  
  #ASSIGN THE SPLIT TO THE VARIABLES
  site_name <- split[1]
  years <- as.numeric(strsplit(split[2], "-")[[1]][1]):as.numeric(strsplit(split[2], "-")[[1]][2])
  if (split[3]=="SINGLE"){
    type <- split[3]
    sp <-  split[4]
    
  } else {
    type <- paste0(split[3],"_",split[4])
    sp <- "ALL"
  }
  default_control <- split[5]
  lai <- split[6]
  meteo <- gsub(".csv", "", split[7])
  
  #PRINT THE VARIABLES NAMES
  cat(" #######################################", "\n",
      "site_name:", site_name, "\n",
      "years:", years, "\n",
      "type:", type, "\n",
      "sp:", sp, "\n",
      "default_control:", default_control, "\n",
      "lai:", lai, "\n",
      "meteo:", meteo, "\n",
      "#######################################"
  )
  
  #STATISTICS####
  
  source<-CAT_FR_SITES[CAT_FR_SITES$site_name == site_name,]$source
  
  
  if (source == "CAT") {
    
    FILTERED_LFMC <- CAT_LFMC %>%
      rename(date = date, 
             site = LocalityName , 
             specie = sp_correct_name, 
             LFMC = LFMC) %>%
      filter(if(type == "SINGLE") specie == sp else TRUE) %>% 
      filter(site == site_name,
             year(date) >= years[1]) %>% 
      select(date,LFMC,specie)
  }
  
  if (source == "FR")  {
    
    FILTERED_LFMC <- FR_LFMC %>%
      rename(date = date, 
             site = SiteCode , 
             specie = sp_correct_name, 
             LFMC = RobustLFMC) %>%
      filter(if(type == "SINGLE") specie == sp else TRUE) %>% 
      filter(site == site_name,
             year(date) >= years[1]) %>% 
      select(date,LFMC,specie)
  }
  
  #MERGE LFMC####
  
  MERGED_LFMC_ALL <- simulations[[i]] %>%
    full_join(FILTERED_LFMC, by = c("date" = "date", "species" = "specie"), suffix = c(".MODELED", ".MEASURED"))
  
  
  #SIMULATION DATA LIST####
  
  data_list[[i]]<-MERGED_LFMC_ALL
  names(data_list)[i]<- filename
  
  if (!type == "SINGLE") {
    
    a<-split(MERGED_LFMC_ALL,MERGED_LFMC_ALL$species)
    
    for (j in 1:length(a)) {
      result <- as.data.frame(evalstats(a[[j]]$LFMC.MEASURED,a[[j]]$LFMC.MODELED))
      result$SITE_NAME <- site_name
      result$YEARS <- paste0(years[1],"-",years[length(years)])
      result$TYPE <- type
      result$SP <- names(a)[j]
      result$CONTROL <- default_control
      result$LAI <- lai
      result$METEO <- meteo
      
      stats_df_MEASURED_MODELED <- rbind(stats_df_MEASURED_MODELED, result)
      
      result2 <- as.data.frame(evalstats(a[[j]]$LFMC.MEASURED,a[[j]]$LFMC_rodrigo))
      result2$SITE_NAME <- site_name
      result2$YEARS <- paste0(years[1],"-",years[length(years)])
      result2$TYPE <- type
      result2$SP <- names(a)[j]
      result2$CONTROL <- default_control
      result2$LAI <- lai
      result2$METEO <- meteo
      
      stats_df_MEASURED_RODRIGO <- rbind(stats_df_MEASURED_RODRIGO, result2)
      
    }
    
  } else if (type == "SINGLE"){
    #EVALUTATION STATISTICS DATA.FRAME####
    result <- as.data.frame(evalstats(MERGED_LFMC_ALL$LFMC.MEASURED,MERGED_LFMC_ALL$LFMC.MODELED))
    result$SITE_NAME <- site_name
    result$YEARS <- paste0(years[1],"-",years[length(years)])
    result$TYPE <- type
    result$SP <- sp
    result$CONTROL <- default_control
    result$LAI <- lai
    result$METEO <- meteo
    
    stats_df_MEASURED_MODELED <- rbind(stats_df_MEASURED_MODELED, result)
    
    result2 <- as.data.frame(evalstats(MERGED_LFMC_ALL$LFMC.MEASURED,MERGED_LFMC_ALL$LFMC_rodrigo))
    result2$SITE_NAME <- site_name
    result2$YEARS <- paste0(years[1],"-",years[length(years)])
    result2$TYPE <- type
    result2$SP <- sp
    result2$CONTROL <- default_control
    result2$LAI <- lai
    result2$METEO <- meteo
    
    stats_df_MEASURED_RODRIGO <- rbind(stats_df_MEASURED_RODRIGO, result2)
  }
}

#####################PLOTS######################################################

time_plot<-list()
scatter_plot<-list()
scatter_plotR<-list()
for (i in 1:length(data_list)) {
  
  #TIME SERIES PLOT
  
  time_p<-ggplot(data = data_list[[i]], aes(x = date))+
    geom_line(aes(y = LFMC.MODELED, colour = "LFMC.MODELED")) +
    geom_line(aes(y = LFMC_rodrigo, colour = "LFMC.RODRIGO")) +
    geom_point(aes(y = LFMC.MEASURED, colour = "LFMC.MEASURED")) +
    geom_line(data = na.omit(data_list[[i]]), aes(y = LFMC.MEASURED, colour = "LFMC.MEASURED")) +
    scale_color_manual(values = c("LFMC.MODELED" = "black", "LFMC.RODRIGO" = "blue", "LFMC.MEASURED" = "red")) +
    theme_classic()+
    theme(legend.position = "bottom", legend.title = element_blank())+
    xlab("DATE")+
    ylab("LFMC")+
    labs(title = names(data_list[i]))+
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  if (!type == "SINGLE") {
    time_p<-time_p+facet_wrap(~species)
  }
  
  time_plot[[i]]<-time_p
  names(time_plot)[i]<-names(data_list[i])
  
  #SCATTER PLOT
  
  scatter_p<-ggplot(data = data_list[[i]], aes(x = LFMC.MODELED, y = LFMC.MEASURED)) +
    geom_point()+
    stat_smooth(method = "lm") +
    theme_classic()+
    theme(legend.position = "bottom",legend.title = element_blank())+
    xlim(c(0, max(max(data_list[[i]]$LFMC.MODELED, na.rm = T),max(data_list[[i]]$LFMC.MEASURED, na.rm = T), na.rm = T)))+
    ylim(c(0, max(max(data_list[[i]]$LFMC.MODELED, na.rm = T),max(data_list[[i]]$LFMC.MEASURED, na.rm = T), na.rm = T)))+
    geom_abline(intercept = 0, slope = 1, linetype= "dashed")+
    labs(title = names(data_list[i]))
  
  if (!type == "SINGLE") {
    scatter_p<-scatter_p+facet_wrap(~species)
  }
  
  scatter_plot[[i]]<-scatter_p
  names(scatter_plot)[i]<-names(data_list[i])
  
  #SCATTER PLOT RODRIGO
  
  scatter_pR<-ggplot(data = data_list[[i]], aes(x = LFMC_rodrigo, y = LFMC.MEASURED)) +
    geom_point()+
    stat_smooth(method = "lm") +
    theme_classic()+
    theme(legend.position = "bottom",legend.title = element_blank())+
    xlim(c(0, max(max(data_list[[i]]$LFMC_rodrigo, na.rm = T),max(data_list[[i]]$LFMC.MEASURED, na.rm = T), na.rm = T)))+
    ylim(c(0, max(max(data_list[[i]]$LFMC_rodrigo, na.rm = T),max(data_list[[i]]$LFMC.MEASURED, na.rm = T), na.rm = T)))+
    geom_abline(intercept = 0, slope = 1, linetype= "dashed")+
    labs(title = names(data_list[i]))
  
  if (!type == "SINGLE") {
    scatter_pR<-scatter_pR+facet_wrap(~species)
  }
  
  scatter_plotR[[i]]<-scatter_pR
  names(scatter_plotR)[i]<-names(data_list[i])
}

# subtitle = paste0("n = ",signif(stats_df_MEASURED_MODELED[i,"n"],3),
#                   " Bias = ",signif(stats_df_MEASURED_MODELED[i,"Bias"],3),
#                   " Bias.rel = ",signif(stats_df_MEASURED_MODELED[i,"Bias.rel"],3),
#                   " MAE = ",signif(stats_df_MEASURED_MODELED[i,"MAE"],3),
#                   " MAE.rel = ",signif(stats_df_MEASURED_MODELED[i,"MAE.rel"],3),
#                   " r = ",signif(stats_df_MEASURED_MODELED[i,"r"],3),
#                   " r2 = ",signif(stats_df_MEASURED_MODELED[i,"r2"],3),
#                   " NSE = ",signif(stats_df_MEASURED_MODELED[i,"NSE"],3),
#                   " NSE.abs = ",signif(stats_df_MEASURED_MODELED[i,"NSE.abs"],3)), width = 5

#################################SAVE ALL THE SIMULATIONS STATISTICS AS .RData####

#rm(list = setdiff(ls(),c("stats_df_MEASURED_MODELED","data_list","sim_list", "scatter_plot", "time_plot")))

#####################FIT########################################################

# # Fit the linear regression model
# model <- lm(LFMC.MEASURED ~ LFMC.MODELED, data = data_list[[i]])
# summary(model)
# ggplotRegression(model,abline = T, caption = names(data_list[i]))


#####################READ SIM_FILES#############################################

files_path1<-list.files(paste0("data/PLOTS/", sites), pattern = paste0(PATTERN,"\\.RDS$"), recursive = TRUE, full.names = TRUE)
files_name1<-basename(files_path1)

sim_list<-list()
for (i in 1:length(files_path1)) {
  sim_list[[files_name1[i]]]<-readRDS(files_path1[i])
}

# #####################SAVE ALL THE SIMULATION DATA###############################
# 
# if (type == "SINGLE"){
#   name<-paste(paste0(years[1],"-",years[length(years)]),type,"MEASURED",default_control,lai,meteo, sep = "_")
#   
# } else {
#   name<-paste(paste0(years[1],"-",years[length(years)]),type,default_control,lai,meteo, sep = "_")
# }
# 
# #save(stats_df_MEASURED_MODELED,data_list,sim_list, scatter_plot, time_plot, file = paste0("data/SIMULATIONS/",name,".RData"))

#####################DISPLAY PLOTS##############################################


plots_site_sp<-function(index){
  
  p1 <- time_plot[[index]] + labs(title = NULL)
  p2 <- scatter_plot[[index]] + labs(title = NULL)
  p3 <- scatter_plotR[[index]] + labs(title = NULL)
  
  t1 <- ggtexttable(round(stats_df_MEASURED_MODELED[index, 1:11],3), rows = NULL, theme = ttheme("light"))
  t2 <- ggtexttable(round(stats_df_MEASURED_RODRIGO[index, 1:11],3), rows = NULL, theme = ttheme("light"))
  
  plot_design <- p1 / (p2 | p3) / (t1 | t2) +
    plot_layout(heights = c(0.35, 0.55, 0.1)) +
    plot_annotation(title = gsub(".csv", "", names(scatter_plot)[index]))
}

dir<-paste0("data/SIMULATIONS_PLOTS/",years[1],"-",years[length(years)],"_",type,"_",lai,"/")

for (i in 1:length(time_plot)) {
  plot<-plots_site_sp(i)
  plotname<-gsub(".csv", "", names(time_plot[i]))
  ggsave(filename = paste0(dir,"/",plotname,".jpg"), plot = plot, width = 1920, height = 1080, units = "px", dpi = 96)
}



