library(sf)
library(tidyverse)
library(medfate)
library(medfateutils)
library(meteoland)
library(medfateland)

############################################CREATE FOLDERS#######################
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
CAT_FR_SITES_NAMES<-CAT_FR_SITES$site_name

# Define the PLOTS directory
plots_dir <- file.path(getwd(), "data", "PLOTS")

# Check if the PLOTS directory exists, if not, create it
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
  print("PLOTS directory created.")
} else {
  print("PLOTS directory already exists.")
}

# Now, create the site-specific directories
for (i in CAT_FR_SITES_NAMES) {
  dir_name <- file.path(plots_dir, i)
  if (!dir.exists(dir_name)) {
    dir.create(dir_name)
    print(paste("Directory", i, "created."))
  } else {
    print(paste("Directory", i, "already exists."))
  }
}

###########################################SOIL####################################

#export soil_data to the correct plot directory, with medfate format

soil_data<-read.csv(file = "data/SOIL_DATA.csv")
soil_data_mod<-read.csv(file = "data/SOIL_DATA_MOD.csv")

soil_list <- split(soil_data, soil_data$site_name)
soil_list_mod <- split(soil_data_mod, soil_data_mod$site_name)

#remove site_name column
#soil_list<-lapply(soil_list, function(x) { x["site_name"] <- NULL; x })

#save the soil dataframe in PLOT folders.

for (i in CAT_FR_SITES_NAMES) {
  df <- soil_list[[i]]
  dir <- file.path(getwd(), "data", "PLOTS", i)
  if (dir.exists(dir)) {
    write.csv(df, file.path(dir, "soil.csv"),row.names = F)
    print(paste("soil", i, "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", i, "soil, not saved."))
  }
}

#save the modified soil dataframe in PLOT folders.

for (i in CAT_FR_SITES_NAMES) {
  df <- soil_list_mod[[i]]
  dir <- file.path(getwd(), "data", "PLOTS", i)
  if (dir.exists(dir)) {
    write.csv(df, file.path(dir, "soil_mod.csv"),row.names = F)
    print(paste("modified soil", i, "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", i, "soil, not saved."))
  }
}

########################CAT DATA######################################################

SpParamsAlbert<-read.csv("data/SpParamsAlbert.csv")

CAT_VEG_DATA<-read.csv("data/CAT_VEG_DATA.csv")

#SP#################################################################

cat_names<-unique(CAT_VEG_DATA$Site)


cat_forests<-list()
mapping_cat<-c("Species.name" = "sp_correct_name", "Height" = "Height", "Cover" = "Cover")
for (i in cat_names) {
  cat_forests[[i]] <- emptyforest()
  # Store the "plot" and "Date" info separately
  plot_data <- CAT_VEG_DATA[CAT_VEG_DATA$Site==i, "Plot"]
  Date_data <- CAT_VEG_DATA[CAT_VEG_DATA$Site==i, "Date"]
  # Apply the map function
  cat_forests[[i]]$shrubData <- forest_mapShrubTable(CAT_VEG_DATA[CAT_VEG_DATA$Site==i,], mapping_y = mapping_cat, SpParams = SpParamsAlbert)
  # Add the "plot" info to the shrubData
  cat_forests[[i]]$shrubData$Plot <- plot_data
  cat_forests[[i]]$shrubData$Date <- Date_data
}

#cat_forests[["Tivissa"]]$shrubData[20,1]<-"Asphodelus spp."
cat_forests[["Tivissa"]]$shrubData<-filter(cat_forests[["Tivissa"]]$shrubData, Species != "Cistus clusii")


#save the shrubData dataframe in plot directory.
for (i in cat_names) {
  df <- cat_forests[[i]]$shrubData
  dir <- file.path(getwd(), "data", "PLOTS", i)
  if (dir.exists(dir)) {
    write.csv(df, file.path(dir,"shrubData_ALL.csv"),row.names = F)
    print(paste("shrub", i, "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", i, "shrubData, not saved."))
  }
}

########################FRENCH DATA###################################################
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
FR_LFMC<-read.csv("data/FR_LFMC.csv")
FR_SHRUBS_VEG_DATA<-read.csv("data/FR_SHRUBS_VEG_DATA.csv")
FR_TREES_VEG_DATA<-read.csv("data/FR_TREES_VEG_DATA.csv")
FR_HERBS_VEG_DATA<-read.csv("data/FR_HERBS_VEG_DATA.csv")

FR_SHRUBS_VEG_DATA$coverage<-as.numeric(FR_SHRUBS_VEG_DATA$coverage)
FR_TREES_VEG_DATA$coverage<-as.numeric(FR_TREES_VEG_DATA$coverage)


FR_VEG_DATA<-bind_rows(FR_TREES_VEG_DATA, FR_SHRUBS_VEG_DATA) %>% 
  arrange(Code_Site.x)


#SP###################################################################################

fr_names<-unique(FR_VEG_DATA$Code_Site.x)

#Not all fr-sites have vegetation data!

CAT_FR_SITES$site_name %in% FR_VEG_DATA$Code_Site.x
intersect(CAT_FR_SITES$site_name,FR_VEG_DATA$Code_Site.x)
setdiff(CAT_FR_SITES$site_name,FR_VEG_DATA$Code_Site.x)

fr_forests <- list()
mapping_fr<-c("Species.name" = "sp_correct_name", "Height" = "height", "Cover" = "coverage")
for (i in fr_names) {
  fr_forests[[i]] <- emptyforest()
  # Store the "placette" info separately
  plot_data <- FR_VEG_DATA[FR_VEG_DATA$Code_Site.x==i, "Placette"]
  Date_data <- FR_VEG_DATA[FR_VEG_DATA$Code_Site.x==i, "Date"]
  # Apply the map function
  fr_forests[[i]]$shrubData <- forest_mapShrubTable(FR_VEG_DATA[FR_VEG_DATA$Code_Site.x==i,], mapping_y = mapping_fr, SpParams = SpParamsAlbert)
  # Add the "placette" info to the shrubData
  fr_forests[[i]]$shrubData$Plot <- plot_data
  fr_forests[[i]]$shrubData$Date <- Date_data
}


for (i in fr_names) {
  df <- fr_forests[[i]]$shrubData
  dir <- file.path(getwd(), "data", "PLOTS", i)
  if (dir.exists(dir)) {
    write.csv(df, file.path(dir,"shrubData_ALL.csv"),row.names = F)
    print(paste("shrub", i, "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", i, "shrubData, not saved."))
  }
}

###########################################METEO ERA_5##################################
#READ THE .CSV DATA
#METEO
METEO<-list.files("raw_data/ERA5_DATA/DATA ARSENE",pattern = "^ERA_land.*\\.csv$",full.names = TRUE)

METEO_list <- list()

# Read the CSV files
for(i in seq_along(METEO)) {
  METEO_list[[i]] <- read.csv(METEO[i], stringsAsFactors = FALSE, sep = ";")
}

str(METEO_list[[1]])

#CHANGE VARIABLE NAMES
mapping <- c(
  "Dates" = "DATE",
  "MinTemperature" = "Tair_min",
  "MaxTemperature" = "Tair_max",
  "MeanTemperature" = "Tair_mean",
  "Precipitation" = "PPT_sum",
  "MinRelativeHumidity" = "RHair_min",
  "MaxRelativeHumidity" = "RHair_max",
  "MeanRelativeHumidity" = "RHair_mean",
  "Radiation" = "RG_sum",
  "WindSpeed" = "WS_mean"
)

# METEO_list_copy<-METEO_list
# METEO_list<-METEO_list_copy

for(i in seq_along(METEO_list)) {
  METEO_list[[i]] <- rename(METEO_list[[i]], all_of(mapping))
  METEO_list[[i]]$Dates <- as.Date(METEO_list[[i]]$Dates,tryFormats = "%d/%m/%Y")
}

# # Save the data with a comma separator
# for(i in seq_along(METEO_list)) {
#   # Extract the original file name, remove the extension and add the new one
#   file_name <- gsub("^.*/", "", METEO[i])
#   file_name <- gsub("\\.csv$", "", file_name)
#   write.csv(METEO_list[[i]], file = paste0("raw_data/ERA5_DATA/DATA ARSENE/CORRECT_CSV/", file_name, ".csv"), row.names = FALSE)
# }

#PUT THE NAME OF THE ARCHIVE IN THE LIST
file_names <- gsub("\\.csv$", "", basename(METEO))
file_names <- gsub("ERA_land_", "", file_names)

names(METEO_list)<-file_names

#CHANGE THE NAME WITH THE CORRECT SITE NAME
for (i in 1:nrow(CAT_FR_SITES)) {
  row <- CAT_FR_SITES[i, ]
  
  if (row$ERA5_NAME_POINTS %in% names(METEO_list)) {
    names(METEO_list)[names(METEO_list) == row$ERA5_NAME_POINTS] <- row$site_name
  }
}

#REMOVE THE NON MACHING NAMES
METEO_list <- METEO_list[names(METEO_list) %in% CAT_FR_SITES$site_name]

#ADD SITE COLUMN TO THE DF OF EACH LIST:

for (i in 1:length(METEO_list)) {
  METEO_list[[i]]$Site<-names(METEO_list[i])
}

#MERGE THE LIST DF IN A SINGLE DF AND SAVE

METEO_df<-bind_rows(METEO_list)

write.csv(METEO_df, "data/ERA5_DATA.csv", row.names = F)

#SAVE THE METEO DATA TO CORRECT PLOT FOLDER

CAT_FR_SITES_NAMES<-CAT_FR_SITES$site_name

for (i in CAT_FR_SITES_NAMES) {
  df <- METEO_list[[i]]
  dir <- file.path(getwd(), "data", "PLOTS", i)
  if (dir.exists(dir)) {
    write.csv(df, file.path(dir,"meteo_ERA5.csv"),row.names = F)
    print(paste("meteo", i, "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", i, "meteo, not saved."))
  }
}

###################CAT METEO INTERPOLATORS#####################################

# CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
# 
# #CAT SITES
# CAT_SITES<-CAT_FR_SITES[1:9,]
# 
# #SF OBJECT
# SF_CAT_SITES<-st_as_sf(CAT_SITES, coords = c("LON", "LAT"), crs = 4326)
# 
# #LOAD INTERPOLATORS (SEPARATE YEARS)
# files<-list.files("raw_data/interpoladores_meteo_cat/Meteo/", pattern = ".nc", full.names = T)
# 
# #INTERPOLATE METEO DATA
# meteo<-list()
# for (i in 1:length(files)){
#   m<-read_interpolator(files[i])
#   meteo[[i]]<-interpolate_data(SF_CAT_SITES, m)
# }
# 
# #MERGE INTERPOLATED DATA
# meteo_interpolators<-list()
# for (i in 1:nrow(meteo[[1]])) {
#   temp <- list()
#   for(j in 1:length(meteo)) {
#     temp[[j]] <- meteo[[j]]$interpolated_data[[i]]
#   }
#   meteo_interpolators[[i]] <- do.call(rbind, temp)
# }
# 
# #CHANGE NAMES
# 
# new_names<-meteo[[1]]$site_name
# names(meteo_interpolators) <- new_names
meteo_interpolators<-readRDS("raw_data/interpoladores_meteo_cat/meteo_interpolators_list.RDS")

#ADD SITE COLUMN TO THE DF OF EACH LIST:

for (i in 1:length(meteo_interpolators)) {
  meteo_interpolators[[i]]$Site<-names(meteo_interpolators[i])
}

#MERGE THE LIST DF IN A SINGLE DF AND SAVE

meteo_df<-bind_rows(meteo_interpolators)

write.csv(meteo_df, "data/INTERPOLATED_CAT_DATA.csv", row.names = F)

#SAVE INTERPOLATED METEO TO CAT PLOTS
CAT_SITES_NAMES<-CAT_FR_SITES$site_name[1:9]

for (i in CAT_SITES_NAMES) {
  df <- meteo_interpolators[[i]]
  dir <- file.path(getwd(), "data", "PLOTS", i)
  if (dir.exists(dir)) {
    write.csv(df, file.path(dir,"meteo_interpolator.csv"),row.names = F)
    print(paste("meteo interpolator", i, "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", i, "meteo, not saved."))
  }
}

###########################################LAI##################################

#export LAI_data to the correct plot directory, with medfate format

LAI_data<-read.csv(file = "data/LAI_DATA_MODIS_MEANS.csv")

LAI_list <- split(LAI_data, LAI_data$site_name)

#remove site_name an ID column
LAI_list<-lapply(LAI_list, function(x) { x[c("site_name","ID")] <- NULL; x })

#save the LAI dataframe in PLOT folders.

for (i in CAT_FR_SITES_NAMES) {
  df <- LAI_list[[i]]
  dir <- file.path(getwd(), "data", "PLOTS", i)
  if (dir.exists(dir)) {
    write.csv(df, file.path(dir, "MODIS_LAI.csv"),row.names = F)
    print(paste("LAI", i, "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", i, "LAI, not saved."))
  }
}