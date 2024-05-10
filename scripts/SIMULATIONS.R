library(medfate)
library(medfateutils)
library(meteoland)
library(medfateland)

library(tidyverse)

#####################CAT_FR_SITES###################################
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")

#################################FUNCTIONS######################################
# site_name<-"Badalona"
# 
# SITE_NAME <-site_name
# YEARS<-years
# TYPE<-type
# SP<-sp
# CONTROL<-control
# LAI<-lai
# METEO<-meteo


run_simulation <- function(SITE_NAME,YEARS,TYPE,SP=NULL,CONTROL,LAI=NULL,METEO) {
  
  CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
  SOURCE<-CAT_FR_SITES[CAT_FR_SITES$site_name == SITE_NAME,]$source
    
  SpParams<-SpParamsFR
  
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
    x <- forest2spwbInput(forest, soil, SpParams, CONTROL)
    
    # Run the SPWB
    simulation <- spwb(x, 
                       meteo, 
                       latitude = CAT_FR_SITES$LAT[CAT_FR_SITES$site_name == SITE_NAME], 
                       elevation = elevation, 
                       slope = slope, 
                       aspect = aspect)
    
    return(simulation)
    
  }
  
  # if (TYPE == "ALL_MEAN")  {
  #   
  #   #Calculate mean for repeated species
  #   df_mean <- shrubData %>%
  #     group_by(Species) %>%
  #     summarise(across(c(Height, Cover, Z50, Z95), \(x) mean(x)))
  #   
  #   forest$shrubData <- df_mean
  #   
  #   # Prepare the input for the SPWB model
  #   x <- forest2spwbInput(forest, soil, SpParams, CONTROL)
  #   
  #   # Run the SPWB
  #   simulation <- spwb(x, 
  #                      meteo, 
  #                      latitude = CAT_FR_SITES$LAT[CAT_FR_SITES$site_name == SITE_NAME], 
  #                      elevation = elevation, 
  #                      slope = slope, 
  #                      aspect = aspect)
  #   
  #   return(simulation)
  #   
  #   }
  
  if (TYPE == "ALL_SINGLE")  {
      
    results <- list()  # Create an empty list to store the results
    for (i in 1:nrow(shrubData)) {
      
      df_species <- shrubData[i,]
      forest$shrubData <- df_species
      
      # Prepare the input for the SPWB model
      x <- forest2spwbInput(forest, soil, SpParams, CONTROL)
      
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
      forest$shrubData<-shrubData
      lai_total_medfate <- stand_LAI(forest, SpParams)
      #choose the desired sp. from shrubdata
      df_sp <- shrubData %>%
        filter(Species %in% SP) %>%
        group_by(Species) %>%
        slice(which.max(Cover)) #if there are repeated measurement of one sp. choose the one with higher coverage
      
      results <- list()  # Create an empty list to store the results
      for (i in 1:nrow(df_sp)) {
        
        df_species <- df_sp[i,]
        df_species$LAI <- lai_total_medfate
        forest$shrubData <- df_species
        
        # Prepare the input for the SPWB model
        x <- forest2spwbInput(forest, soil, SpParams, CONTROL)
        
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

#################################SIMULATIONS####################################

sites<-CAT_FR_SITES$site_name
for (site_name in sites) {
  
  #SIMULATION PARAMETERS
  years<-c(2012:2022) #VECTOR OF YEARS
  type<-"SINGLE" #ALL_FILTERED, #ALL_MEAN, #ALL_SINGLE #SINGLE
  sp<-"MEASURED" # !!ONLY IF TYPE IS SINGLE!! vector of Species Names for specific species OR "MEASURED" for measured LFMC species
  transpirationMode <- "Sureau" #“Granier”, “Sperry”, “Cochard”, “Sureau”
  control<-defaultControl(transpirationMode)
  control$segmentedXylemVulnerability=F
  lai<-"MODIS" #MODIS (MODIS LAI DATA, CONSTAT LAI FROM THE YEAR OF MEASURED DATA) #NULL = MEDFATE
  meteo<-"ERA5" #INTERPOLATORS, ERA5
  spparams<-"SpParamsAlbert"
  
  #RUN SIMULATION
  
  SIM <- run_simulation(
    SITE_NAME = site_name,
    YEARS = years,
    TYPE = type,
    SP = sp,
    CONTROL = control,
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
    name<-paste(site_name,paste0(years[1],"-",years[length(years)]),type,sp,transpirationMode,lai,meteo,spparams, sep = "_")
    
  } else {
    name<-paste(site_name,paste0(years[1],"-",years[length(years)]),type,transpirationMode,lai,meteo, sep = "_")
  }
  
  path<-file.path("data","PLOTS",site_name,name)
  dir.create(path, showWarnings = FALSE)
  
  saveRDS(SIM, file.path(path, paste0(name, ".RDS")))
  
  #SAVE SIMULATION DATA AS .CSV
  
  if (class(SIM_data)=="list") {
    for (j in 1:length(SIM_data)) {
      name2<-paste(site_name,paste0(years[1],"-",years[length(years)]),type,names(SIM_data)[j],transpirationMode,lai,meteo, sep = "_")
      write.csv(SIM_data[[j]], file.path(path, paste0(name2, ".csv")), row.names = F)
      print(paste0(site_name,"    ",names(SIM_data)[j],"    simulation data    SAVED ✓✓"))
    }
  }
  
  if (class(SIM_data)=="data.frame"){
    name2<-paste(site_name,paste0(years[1],"-",years[length(years)]),type,transpirationMode,lai,meteo, sep = "_")
    write.csv(SIM_data,file.path(path, paste0(name2, ".csv")), row.names = F)
    print(paste0(site_name,"    simulation data    SAVED ✓✓"))
  }
}
