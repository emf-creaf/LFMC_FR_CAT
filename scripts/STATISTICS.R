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

#####################READ SIM_FILES#############################################

#PATTERNS :

#"ALL_FILTERED.*MODIS.*\\.RDS$"### 
#"SINGLE.*MODIS.*\\.RDS$"###
#"SINGLE.*MEDFATE.*\\.RDS$"

PATTERN1<-"SINGLE.*MODIS.*\\.RDS$" 

files_path1<-list.files(paste0("data/PLOTS/", sites), pattern = PATTERN1, full.names = TRUE)
files_name1<-list.files(paste0("data/PLOTS/", sites), pattern = PATTERN1)

sim_list<-list()
for (i in 1:length(files_path1)) {
  sim_list[[files_name1[i]]]<-readRDS(files_path1[i])
}

#####################READ DATA_FILES############################################

#PATTERNS :

#"ALL_FILTERED.*MODIS.*\\.csv$"### 
#"SINGLE.*MODIS.*\\.csv$"###
#"SINGLE.*MEDFATE.*\\.csv$"

PATTERN2<-"SINGLE.*MODIS.*\\.csv$" 

files_path2<-list.files(paste0("data/PLOTS/", sites), pattern = PATTERN2, full.names = TRUE)
files_name2<-list.files(paste0("data/PLOTS/", sites), pattern = PATTERN2)

simulations<-list()
for (i in 1:length(files_path2)) {
  simulations[[files_name2[i]]]<-read.csv(files_path2[i])
}


#####################EVALUATION STATISTICS######################################

df<-data.frame()
data_list<-list()
for (i in 1:length(simulations)) {
  
  SIM<-simulations[[i]]
  SIM$date<-as.Date(SIM$date)
  
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
             date >= paste0(years[1], "-01-01")) %>% 
      select(date,LFMC)
    
    MERGED_LFMC_ALL<-merge(SIM,FILTERED_LFMC, by = "date", suffixes = c(".MODELED",".MEASURED"), all = T)
  }
  
  if (source == "FR")  {
    
    FILTERED_LFMC <- FR_LFMC %>%
      rename(date = date, 
             site = SiteCode , 
             specie = sp_correct_name, 
             LFMC = RobustLFMC) %>%
      filter(if(type == "SINGLE") specie == sp else TRUE) %>% 
      filter(site == site_name,
             date >= paste0(years[1], "-01-01")) %>% 
      select(date,LFMC)
    
    MERGED_LFMC_ALL<-merge(SIM,FILTERED_LFMC, by = "date", suffixes = c(".MODELED",".MEASURED"), all = T)
  }
  
  #SIMULATION DATA LIST####
  
  data_list[[i]]<-MERGED_LFMC_ALL
  names(data_list)[i]<- filename
  
  #EVALUTATION STATISTICS DATA.FRAME####
  result <- as.data.frame(evalstats(MERGED_LFMC_ALL$LFMC.MEASURED,MERGED_LFMC_ALL$LFMC.MODELED))
  result$SITE_NAME <- site_name
  result$YEARS <- paste0(years[1],"-",years[length(years)])
  result$TYPE <- type
  result$SP <- sp
  result$CONTROL <- default_control
  result$LAI <- lai
  result$METEO <- meteo
  
  df <- rbind(df, result)
}

#####################PLOTS######################################################

time_plot<-list()
scatter_plot<-list()
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
  
  time_plot[[i]]<-time_p
  names(time_plot)[i]<-names(data_list[i])
  
  #SCATTER PLOT
  
  scatter_p<-ggplot(data = data_list[[i]], aes(x = LFMC.MODELED, y = LFMC.MEASURED)) +
    geom_point(aes(colour = " "))+
    stat_smooth(method = "lm") +
    scale_color_manual(values = c(" " = "black"))+
    theme_classic()+
    theme(legend.position = "bottom",legend.title = element_blank())+
    xlim(c(0, max(max(data_list[[i]]$LFMC.MODELED, na.rm = T),max(data_list[[i]]$LFMC.MEASURED, na.rm = T), na.rm = T)))+
    ylim(c(0, max(max(data_list[[i]]$LFMC.MODELED, na.rm = T),max(data_list[[i]]$LFMC.MEASURED, na.rm = T), na.rm = T)))+
    geom_abline(intercept = 0, slope = 1, linetype= "dashed")+
    labs(title = names(data_list[i]))
  
  scatter_plot[[i]]<-scatter_p
  names(scatter_plot)[i]<-names(data_list[i])
}

# subtitle = paste0("n = ",signif(df[i,"n"],3),
#                   " Bias = ",signif(df[i,"Bias"],3),
#                   " Bias.rel = ",signif(df[i,"Bias.rel"],3),
#                   " MAE = ",signif(df[i,"MAE"],3),
#                   " MAE.rel = ",signif(df[i,"MAE.rel"],3),
#                   " r = ",signif(df[i,"r"],3),
#                   " r2 = ",signif(df[i,"r2"],3),
#                   " NSE = ",signif(df[i,"NSE"],3),
#                   " NSE.abs = ",signif(df[i,"NSE.abs"],3)), width = 5

#################################SAVE ALL THE SIMULATIONS STATISTICS AS .RData##

if (type == "SINGLE"){
  name<-paste(paste0(years[1],"-",years[length(years)]),type,"MEASURED",default_control,lai,meteo, sep = "_")
  
} else {
  name<-paste(paste0(years[1],"-",years[length(years)]),type,default_control,lai,meteo, sep = "_")
}

save(df,data_list,sim_list, scatter_plot, time_plot, file = paste0("data/SIMULATIONS/",name,".RData"))

rm(list = setdiff(ls(),c("df","data_list","sim_list", "scatter_plot", "time_plot")))

#####################DISPLAY PLOTS##############################################

i<-75

grid.arrange(
  time_plot[[i]] + labs(title = NULL),
  scatter_plot[[i]] + labs(title = NULL),
  ggtexttable(round(df[i, 1:11],3), rows = NULL, theme = ttheme("light")),
  layout_matrix = rbind(c(1, 2), c(1, 2), c(1, 2), c(3, 3)),
  top = textGrob(gsub(".csv", "", names(scatter_plot)[i]), gp = gpar(fontsize = 15))
)


#####################FIT########################################################

# # Fit the linear regression model
# model <- lm(LFMC.MEASURED ~ LFMC.MODELED, data = data_list[[i]])
# summary(model)
# ggplotRegression(model,abline = T, caption = names(data_list[i]))