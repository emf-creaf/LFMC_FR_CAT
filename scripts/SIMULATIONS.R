library(medfate)
#library(medfateutils)
library(tidyverse)


#####################CAT_FR_SITES###################################
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")

#################################FUNCTIONS######################################

# #SIMULATION PARAMETERS
# site_name<- "Badalona"
# years<-c(2012:2022) #VECTOR OF YEARS
# type<-"SINGLE" #ALL_FILTERED, #ALL_SINGLE #SINGLE
# sp<-"Quercus coccifera" # !!ONLY IF TYPE IS SINGLE!! vector of Species Names for specific species OR "MEASURED" for measured LFMC species
# transpirationMode <- "Sureau" #“Granier”, “Sperry”, “Cochard”, “Sureau”
# lfmcomp<-"leaf" #"leaf" or "fine"
# 
# control<-defaultControl(transpirationMode)
# control$segmentedXylemVulnerability=F
# 
# control$lfmcComponent = lfmcomp
# 
# lai<-"MODIS" #MODIS (MODIS LAI DATA, CONSTAT LAI FROM THE YEAR OF MEASURED DATA) OR MEDFATE
# meteo<-"ERA5" #INTER, ERA5
# soil_mod<-T #T or F
# 
# 
# 
# SITE_NAME <- site_name
# YEARS<-years
# TYPE<-type
# SP<-sp
# CONTROL<-control
# LAI<-lai
# METEO<-meteo
# SOIL_MOD<-soil_mod

run_simulation <- function(SITE_NAME,YEARS,TYPE,SP=NULL,CONTROL,LAI=NULL,METEO,SOIL_MOD = F) {
  
  CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
  SOURCE<-CAT_FR_SITES[CAT_FR_SITES$site_name == SITE_NAME,]$source
    
  SpParams<-read.csv("data/SpParamsAlbert.csv")
  
  #Read the shrub data for the site
  shrubData<-read.csv(paste0("data/PLOTS/", SITE_NAME, "/shrubData_ALL.csv"))
  shrubData$Date<-as.Date(shrubData$Date)
  
  # if ("Quercus coccifera" %in% shrubData$Species) {
  #     shrubData$Z95[shrubData$Species == "Quercus coccifera"]<-1500
  #     shrubData$z50[shrubData$Species == "Quercus coccifera"]<-400
  # }
  # if ("Arbutus unedo" %in% shrubData$Species) {
  #   shrubData$Z95[shrubData$Species == "Arbutus unedo"]<-1500
  #   shrubData$Z95[shrubData$Species == "Arbutus unedo"]<-400
  # }
  
  #LAI from MODIS
  if (!is.null(LAI) && LAI == "MODIS") {
    #Read the MODIS LAI data
    LAI_MODIS<-read.csv(paste0("data/PLOTS/", SITE_NAME, "/MODIS_LAI.csv"))
    LAI_YEAR<-unique(format(shrubData$Date,"%Y"))
    #New column LAI in shrubData
    shrubData$LAI<-LAI_MODIS$MCD15A2H_Lai_mean_top5[LAI_MODIS$YEAR %in% LAI_YEAR]
  }
  
  # Read the soil data for the site
  
  
  if (SOIL_MOD == TRUE) {
    #LOAD THE MODIFIED ROCK SOIL
    soil_table <- read.csv(paste0("data/PLOTS/", SITE_NAME, "/soil_mod.csv"))
    soil <- medfate::soil_redefineLayers(soil_table, widths = c(200, 300, 600, 1000, 2000))
    #soil <- medfate::soil_redefineLayers(soil_table, widths = c(200, 300, 600, 3000))
  }else {
    soil_table <- read.csv(paste0("data/PLOTS/", SITE_NAME, "/soil.csv"))
    soil <- medfate::soil_redefineLayers(soil_table, widths = c(200, 300, 600, 1000, 2000))
    #soil <- medfate::soil_redefineLayers(soil_table, widths = c(200, 300, 600, 3000))
  }
  
  
  # Read the meteorological data for the site
  
  if (METEO == "ERA5") {
    met <- read.csv(paste0("data/PLOTS/", SITE_NAME, "/meteo_ERA5.csv"))
    met$Dates <- as.Date(met$Dates)
    met <- met[met$YEAR %in% YEARS, ]
  }
  
  if (METEO == "INTER" && SOURCE == "CAT")  {
    met <- read.csv(paste0("data/PLOTS/", SITE_NAME, "/meteo_interpolator.csv"))
    met$dates <- as.Date(met$dates)
    met$YEAR <- format(met$dates, "%Y")
    met <- met[met$YEAR %in% YEARS, ]
  }
  
  if (METEO == "INTER" && SOURCE == "FR") {
    stop("No interpolators meteo data in FR plots, \nMETEO = INTERPOLATORS only in CAT plots. ")
  }
  
  # Read the topo data for the site
  elevation <- CAT_FR_SITES$elevation[CAT_FR_SITES$site_name == SITE_NAME]
  slope <- CAT_FR_SITES$slope[CAT_FR_SITES$site_name == SITE_NAME]
  aspect <- CAT_FR_SITES$aspect[CAT_FR_SITES$site_name == SITE_NAME]
  
  # Create an empty forest object
  forest <- emptyforest()
  
  # if (TYPE == "ALL_FILTERED") {
  #   
  #   #keep only one of the repeated species
  #   df_filtered <- shrubData %>%
  #     group_by(Species) %>%
  #     slice(which.max(Cover)) #if there are repeated measurement of one sp. choose the one with higher coverage
  #   
  #   
  #   forest$shrubData <- df_filtered
  #   
  #   # Prepare the input for the SPWB model
  #   x <- forest2spwbInput(forest, soil, SpParams, CONTROL)
  #   
  #   # Run the SPWB
  #   simulation <- spwb(x, 
  #                      met, 
  #                      latitude = CAT_FR_SITES$LAT[CAT_FR_SITES$site_name == SITE_NAME], 
  #                      elevation = elevation, 
  #                      slope = slope, 
  #                      aspect = aspect)
  #   
  #   return(simulation)
  #   
  # }
  # 
  # if (TYPE == "ALL_SINGLE")  {
  #     
  #   results <- list()  # Create an empty list to store the results
  #   for (i in 1:nrow(shrubData)) {
  #     
  #     df_species <- shrubData[i,]
  #     forest$shrubData <- df_species
  #     
  #     # Prepare the input for the SPWB model
  #     x <- forest2spwbInput(forest, soil, SpParams, CONTROL)
  #     
  #     # Run the simulation for this species
  #     simulation <- spwb(x, 
  #                        met, 
  #                        latitude = CAT_FR_SITES$LAT[CAT_FR_SITES$site_name == SITE_NAME], 
  #                        elevation = elevation, 
  #                        slope = slope, 
  #                        aspect = aspect)
  #     
  #     # Create a unique identifier combining Species and Plot
  #     id <- paste(i,df_species$Species, sep="_")
  #     # Store the result in the list
  #     results[[id]] <- simulation
  #   }
  #   
  #   return(results)
  #   
  #   }
  
  if (TYPE == "SINGLE") {
    
    if (SP == "MEASURED") {
      
      if (SOURCE == "CAT") {
        CAT_LFMC<-read.csv("data/CAT_LFMC.csv")
        CAT_LFMC$date<-as.Date(CAT_LFMC$date)
        SP <- CAT_LFMC %>%
          filter(year(date) >= YEARS[1], LocalityName == SITE_NAME) %>%
          pull(sp_correct_name) %>%
          unique()
      }
      
      if (SOURCE == "FR")  {
        FR_LFMC<-read.csv("data/FR_LFMC.csv")
        FR_LFMC$date<-as.Date(FR_LFMC$date,format = "%d/%m/%Y")
        SP <- FR_LFMC %>%
          filter(year(date) >= YEARS[1], SiteCode == SITE_NAME) %>%
          pull(sp_correct_name) %>%
          unique()
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
      
      if (LAI == "MEDFATE"){
        lai_total_medfate <- stand_LAI(forest, SpParams)
      }
      
      #choose the desired sp. from shrubdata
      df_sp <- shrubData %>%
        filter(Species %in% SP) %>%
        group_by(Species) %>%
        slice(which.max(Cover)) #if there are repeated measurement of one sp. choose the one with higher coverage
      
      results <- list()  # Create an empty list to store the results
      for (i in 1:nrow(df_sp)) {
        
        df_species <- df_sp[i,]
        
        if (LAI == "MEDFATE"){
          df_species$LAI <- lai_total_medfate
        }
        
        forest$shrubData <- as.data.frame(df_species)
        
        # Prepare the input for the SPWB model
        x <- forest2spwbInput(forest, soil, SpParams, CONTROL)
        
        # Run the simulation for this species
        simulation <- spwb(
          x,
          met,
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

run_simulation_save_data <- function(sites, lai, meteo, soil_mod) {
  
  #SIMULATION PARAMETERS
  years <- c(2012:2022) # VECTOR OF YEARS
  type <- "SINGLE" # ALL_FILTERED, #ALL_SINGLE #SINGLE
  sp <- "MEASURED" # !!ONLY IF TYPE IS SINGLE!! vector of Species Names for specific species OR "MEASURED" for measured LFMC species
  transpirationMode <- "Sureau" # “Granier”, “Sperry”, “Cochard”, “Sureau”
  lfmcomp <- "leaf" #"leaf" or "fine"
  control <- defaultControl(transpirationMode)
  control$segmentedXylemVulnerability <- FALSE
  control$lfmcComponent <- lfmcomp
  
  
  for (site_name in sites) {
    
    #RUN SIMULATION
    SIM <- run_simulation(
      SITE_NAME = site_name,
      YEARS = years,
      TYPE = type,
      SP = sp,
      CONTROL = control,
      LAI = lai,
      METEO = meteo,
      SOIL_MOD = soil_mod
    )
    
    #EXTRACT SIMULATION DATA
    SIM_data <- extract_output(
      SIM,
      LEAFPSIMAX = TRUE,
      LEAFRWC = TRUE,
      LFMC = TRUE,
      LFMC_rodrigo = TRUE
    )
    
    #SAVE SIMULATION OBJECT AS .RDS
    if (type == "SINGLE") {
      name <- paste(site_name, paste0(years[1], "-", years[length(years)]), sp, lai, meteo, soil_mod, sep = "_")
    } else {
      name <- paste(site_name, paste0(years[1], "-", years[length(years)]), lai, meteo, soil_mod, sep = "_")
    }
    
    path <- file.path("results", "SIMULATION_RESULTS", site_name, name)
    dir.create(path, showWarnings = FALSE, recursive = TRUE)
    
    saveRDS(SIM, file.path(path, paste0(name, ".RDS")))
    
    #SAVE SIMULATION DATA AS .CSV
    if (class(SIM_data) == "list") {
      for (j in 1:length(SIM_data)) {
        name2 <- paste(site_name, paste0(years[1], "-", years[length(years)]), names(SIM_data)[j], lai, meteo, soil_mod, sep = "_")
        write.csv(SIM_data[[j]], file.path(path, paste0(name2, ".csv")), row.names = FALSE)
        cat(paste0(site_name, " ", names(SIM_data)[j], " SIMULATION DATA SAVED"), "\n\n")
      }
    }
    
    if (class(SIM_data) == "data.frame") {
      name2 <- paste(site_name, paste0(years[1], "-", years[length(years)]), lai, meteo, soil_mod, sep = "_")
      write.csv(SIM_data, file.path(path, paste0(name2, ".csv")), row.names = FALSE)
      cat(paste0(site_name, " SIMULATION DATA SAVED"), "\n\n")
    }
  }
}


# sites<-CAT_FR_SITES$site_name[CAT_FR_SITES$source=="CAT"]

# run_simulation_save_data(sites,"MODIS",  "INTER", TRUE)
# run_simulation_save_data(sites,"MEDFATE","INTER", TRUE)
# run_simulation_save_data(sites,"MODIS",  "INTER", FALSE)
# run_simulation_save_data(sites,"MEDFATE","INTER", FALSE)

# sites<-CAT_FR_SITES$site_name

# run_simulation_save_data(sites,"MODIS",  "ERA5", TRUE)
# run_simulation_save_data(sites,"MEDFATE","ERA5", TRUE)
# run_simulation_save_data(sites,"MODIS",  "ERA5", FALSE)
# run_simulation_save_data(sites,"MEDFATE","ERA5", FALSE)



