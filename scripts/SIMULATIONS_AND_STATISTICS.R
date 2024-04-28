library(medfate)
library(medfateutils)
library(meteoland)
library(medfateland)

library(tidyverse)

library(gridExtra)
library(grid)

#####################CAT_FR_SITES###################################
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
sites<-CAT_FR_SITES$site_name

#####################LFMC_DATA######################################

CAT_LFMC<-read.csv("data/CAT_LFMC.csv")
CAT_LFMC$date<-as.Date(CAT_LFMC$date)

FR_LFMC<-read.csv("data/FR_LFMC.csv")
FR_LFMC$date<-as.Date(FR_LFMC$date,format = "%d/%m/%Y")

#################################FUNCTIONS######################################

run_simulation <- function(SITE_NAME,YEARS,TYPE,SP=NULL,CONTROL,LAI=NULL,METEO) {
  
  CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
  SOURCE<-CAT_FR_SITES[CAT_FR_SITES$site_name == SITE_NAME,]$source
  
  
  if (SOURCE == "CAT") {
    SpParams<-SpParamsES
  }
  
  if (SOURCE == "FR")  {
    SpParams<-SpParamsFR
  }
  
  #Read the shrub data for the site
  shrubData<-read.csv(paste0("data/PLOTS/", SITE_NAME, "/shrubData_ALL.csv"))
  shrubData$Date<-as.Date(shrubData$Date)
  
  
  #LAI from MODIS
  if (!is.null(LAI) && LAI == "MODIS") {
    #Read the MODIS LAI data
    LAI_MODIS<-read.csv(paste0("data/PLOTS/", SITE_NAME, "/MODIS_LAI.csv"))
    LAI_YEAR<-unique(format(shrubData$Date,"%Y"))
    #New column LAI in shrubData
    shrubData$LAI<-LAI_MODIS$MCD15A2H_Lai_mean_top5[LAI_MODIS$YEAR %in% LAI_YEAR]
  }
  
  # Read the soil data for the site
  soil_table <- read.csv(paste0("data/PLOTS/", SITE_NAME, "/soil.csv"))
  soil <- soil(soil_table)
  
  # Read the meteorological data for the site
  
  if (METEO == "ERA5") {
    meteo <- read.csv(paste0("data/PLOTS/", SITE_NAME, "/meteo_ERA5.csv"))
    meteo$Dates <- as.Date(meteo$Dates)
    meteo <- meteo[meteo$YEAR %in% YEARS, ]
  }
  
  if (METEO == "INTERPOLATORS" && SOURCE == "CAT")  {
    meteo <- read.csv(paste0("data/PLOTS/", SITE_NAME, "/meteo_interpolator.csv"))
    meteo$dates <- as.Date(meteo$dates)
    meteo$YEAR <- format(meteo$dates, "%Y")
    meteo <- meteo[meteo$YEAR %in% YEARS, ]
  }
  
  if (METEO == "INTERPOLATORS" && SOURCE == "FR") {
    stop("No interpolators meteo data in FR plots, \nMETEO = INTERPOLATORS only in CAT plots. ")
  }
  
  # Read the topo data for the site
  elevation <- CAT_FR_SITES$elevation[CAT_FR_SITES$site_name == SITE_NAME]
  slope <- CAT_FR_SITES$slope[CAT_FR_SITES$site_name == SITE_NAME]
  aspect <- CAT_FR_SITES$aspect[CAT_FR_SITES$site_name == SITE_NAME]
  
  # Create an empty forest object
  forest <- emptyforest()
  
  if (TYPE == "ALL_FILTERED") {
    
    #keep only one of the repeated species
    df_filtered <- shrubData %>%
      group_by(Species) %>%
      slice(which.max(Cover)) #if there are repeated measurement of one sp. choose the one with higher coverage
    
    
    forest$shrubData <- df_filtered
    
    # Prepare the input for the SPWB model
    control <- defaultControl(CONTROL)
    x <- forest2spwbInput(forest, soil, SpParams, control)
    
    # Run the SPWB
    simulation <- spwb(x, 
                       meteo, 
                       latitude = CAT_FR_SITES$LAT[CAT_FR_SITES$site_name == SITE_NAME], 
                       elevation = elevation, 
                       slope = slope, 
                       aspect = aspect)
    
    return(simulation)
    
  }
  
  if (TYPE == "ALL_MEAN")  {
    
    #Calculate mean for repeated species
    df_mean <- shrubData %>%
      group_by(Species) %>%
      summarise(across(c(Height, Cover, Z50, Z95), \(x) mean(x)))
    
    forest$shrubData <- df_mean
    
    # Prepare the input for the SPWB model
    control <- defaultControl(CONTROL)
    x <- forest2spwbInput(forest, soil, SpParams, control)
    
    # Run the SPWB
    simulation <- spwb(x, 
                       meteo, 
                       latitude = CAT_FR_SITES$LAT[CAT_FR_SITES$site_name == SITE_NAME], 
                       elevation = elevation, 
                       slope = slope, 
                       aspect = aspect)
    
    return(simulation)
    
    }
  
  if (TYPE == "ALL_SINGLE")  {
      
    results <- list()  # Create an empty list to store the results
    for (i in 1:nrow(shrubData)) {
      
      df_species <- shrubData[i,]
      forest$shrubData <- df_species
      
      # Prepare the input for the SPWB model
      control <- defaultControl(CONTROL)
      x <- forest2spwbInput(forest, soil, SpParams, control)
      
      # Run the simulation for this species
      simulation <- spwb(x, 
                         meteo, 
                         latitude = CAT_FR_SITES$LAT[CAT_FR_SITES$site_name == SITE_NAME], 
                         elevation = elevation, 
                         slope = slope, 
                         aspect = aspect)
      
      # Create a unique identifier combining Species and Plot
      id <- paste(i,df_species$Species, sep="_")
      # Store the result in the list
      results[[id]] <- simulation
    }
    
    return(results)
    
    }
  
  if (TYPE == "SINGLE") {
    
    if (SP == "MEASURED") {
      
      if (SOURCE == "CAT") {
        CAT_LFMC<-read.csv("data/CAT_LFMC.csv")
        SP<-unique(CAT_LFMC$sp_correct_name[CAT_LFMC$LocalityName==SITE_NAME])
      }
      
      if (SOURCE == "FR")  {
        FR_LFMC<-read.csv("data/FR_LFMC.csv")
        SP<-unique(FR_LFMC$sp_correct_name[FR_LFMC$SiteCode==SITE_NAME])
      }
    }
    
    if (!is.null(SP)) {
      
      # Create a vector to hold  not found sp in shrubData
      not_found <- c()
      
      for (i in SP) {
        if (!i %in% shrubData$Species) {
          warning(paste0("Species ", i, " not found in ", SITE_NAME, " plot."))
          not_found <- c(not_found, i)  # Add not found species to the vector
        }
      }
      
      # Update SP to remove species not found
      SP <- SP[!SP %in% not_found]
      
      #choose the desired sp. from shrubdata
      df_sp <- shrubData %>%
        filter(Species %in% SP) %>%
        group_by(Species) %>%
        slice(which.max(Cover)) #if there are repeated measurement of one sp. choose the one with higher coverage
      
      results <- list()  # Create an empty list to store the results
      for (i in 1:nrow(df_sp)) {
        
        df_species <- df_sp[i,]
        forest$shrubData <- df_species
        
        # Prepare the input for the SPWB model
        control <- defaultControl(CONTROL)
        x <- forest2spwbInput(forest, soil, SpParams, control)
        
        # Run the simulation for this species
        simulation <- spwb(
          x,
          meteo,
          latitude = CAT_FR_SITES$LAT[CAT_FR_SITES$site_name == SITE_NAME],
          elevation = elevation,
          slope = slope,
          aspect = aspect
        )
        
        # Create a unique identifier
        id <- df_species$Species
        # Store the result in the list
        results[[id]] <- simulation
      }
      
      return(results)
      
    } else if (is.null(SP)) {
      stop("No specified sp")
    }
  }
}

extract_output<-function(SIMULATION,LEAFPSIMAX=FALSE,LEAFRWC=FALSE,LFMC=FALSE,LFMC_rodrigo=FALSE){
  
  extracted_data<-list()
  
  if (any(class(SIMULATION)=="spwb")) {
    
    if (LEAFPSIMAX==T) {
      extracted_data[["LEAFPSIMAX_data"]]<-medfate::extract(SIMULATION, level = "cohort", output = "Plants", vars = "LeafPsiMax")
    }
    
    if (LEAFRWC==T) {
      extracted_data[["LEAFRWC_data"]]<-medfate::extract(SIMULATION, level = "cohort", output = "Plants", vars = "LeafRWC")
    }
    
    if (LFMC==T) {
      extracted_data[["LFMC_data"]]<-medfate::extract(SIMULATION, level = "cohort", output = "Plants", vars = "LFMC")
      
      if (LFMC_rodrigo==T) {
        extracted_data[["LFMC_data"]]$LFMC_rodrigo <- 91.87 - (31.12 * log10(-(extracted_data[["LEAFPSIMAX_data"]]$LeafPsiMax)))
        
      }
    }
    
    for (i in 1:length(extracted_data)) {
      if (i==1){
        df<-extracted_data[[i]]
      }else {
        df<-merge(df,extracted_data[[i]], by = intersect(c("date","cohort","species"), c("date","cohort","species")))
      }
    }
    
    df <- df[order(df$species, df$date),]
    
    return(df)
    
  }
  
  if (class(SIMULATION)=="list"){
    
    
    if (LEAFPSIMAX==T) {
      LEAFPSIMAX_data<-list()
      for (i in names(SIMULATION)) {
        LEAFPSIMAX_data[[i]]<-medfate::extract(SIMULATION[[i]], level = "cohort", output = "Plants", vars = "LeafPsiMax")
      }
    }
    
    if (LEAFRWC==T) {
      LEAFRWC_data<-list()
      for (i in names(SIMULATION)) {
        LEAFRWC_data[[i]]<-medfate::extract(SIMULATION[[i]], level = "cohort", output = "Plants", vars = "LeafRWC")
      }
    }
    
    if (LFMC==T) {
      LFMC_data<-list()
      for (i in names(SIMULATION)) {
        LFMC_data[[i]]<-medfate::extract(SIMULATION[[i]], level = "cohort", output = "Plants", vars = "LFMC")
      }
      
      if (LFMC_rodrigo==T) {
        for (i in names(SIMULATION)) {
          LFMC_data[[i]]$LFMC_rodrigo <- 91.87 - (31.12 * log10(-(LEAFPSIMAX_data[[i]]$LeafPsiMax)))
        }
      }
    }
    
    extracted_data<-list(LEAFPSIMAX_data,LEAFRWC_data,LFMC_data)
    
    
    ls<-list()
    for (i in names(extracted_data[[1]])) {
      for (j in 1:length(extracted_data)) {
        if (j==1){
          df<-extracted_data[[j]][[i]]
        }else {
          df<-merge(df,extracted_data[[j]][[i]], by = intersect(c("date","cohort","species"), c("date","cohort","species")))
        }
        ls[[i]]<-df
      }
    }
    return(ls)
  }
}

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

#################################SIMULATIONS####################################

for (i in sites[4:length(sites)]) {
  
  #SIMULATION PARAMETERS
  
  site_name<- i 
  years<-c(2012:2022) #VECTOR OF YEARS
  type<-"SINGLE" #ALL_FILTERED, #ALL_MEAN, #ALL_SINGLE #SINGLE
  sp<-"MEASURED" # !!ONLY IF TYPE IS SINGLE!! vector of Species Names for specific species OR "MEASURED" for measured LFMC species
  default_control<-"Sureau" #“Granier”, “Sperry”, “Cochard”, “Sureau”
  lai<-"MEDFATE" #MODIS (MODIS LAI DATA, CONSTAT LAI FROM THE YEAR OF MEASURED DATA) #NULL = MEDFATE
  meteo<-"ERA5" #INTERPOLATORS, ERA5
  
  #RUN SIMULATION
  
  SIM <- run_simulation(
    SITE_NAME = site_name,
    YEARS = years,
    TYPE = type,
    SP = sp,
    CONTROL = default_control,
    LAI = lai,
    METEO = meteo
  )
  
  #EXTRACT SIMULATION DATA
  
  SIM_data <- extract_output(
    SIM,
    LEAFPSIMAX = T,
    LEAFRWC = T,
    LFMC = T,
    LFMC_rodrigo = T
  )
  
  #SAVE SIMULATION OBJECT AS .RDS
  
  if (type == "SINGLE"){
    name<-paste(site_name,paste0(years[1],"-",years[length(years)]),type,sp,default_control,lai,meteo, sep = "_")
    
  } else {
    name<-paste(site_name,paste0(years[1],"-",years[length(years)]),type,default_control,lai,meteo, sep = "_")
  }
  
  saveRDS(SIM, paste0("data/PLOTS/",site_name,"/",name,".RDS"))
  
  #SAVE SIMULATION DATA AS .CSV
  
  if (class(SIM_data)=="list") {
    for (j in 1:length(SIM_data)) {
      name<-paste(site_name,paste0(years[1],"-",years[length(years)]),type,names(SIM_data)[j],default_control,lai,meteo, sep = "_")
      write.csv(SIM_data[[j]],paste0("data/PLOTS/",site_name,"/",name,".csv"), row.names = F)
      print(paste0(site_name,"    ",names(SIM_data)[j],"    simulation data    SAVED ✓✓"))
    }
  }
  
  if (class(SIM_data)=="data.frame"){
    name<-paste(site_name,paste0(years[1],"-",years[length(years)]),type,default_control,lai,meteo, sep = "_")
    write.csv(SIM_data,paste0("data/PLOTS/",site_name,"/",name,".csv"), row.names = F)
    print(paste0(site_name,"    simulation data    SAVED ✓✓"))
  }
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