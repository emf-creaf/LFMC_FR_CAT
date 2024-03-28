library(medfate)
library(medfateutils)
library(meteoland)
library(tidyverse)

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

#soil data, list with dataframes in each site
soil_list<-soil_data %>% 
  group_split(site_name) %>% 
  lapply(as.data.frame)

#site names
site_names<- lapply(soil_list, function(x) { x$site_name[1] })

#remove soil_data column
soil_list<-lapply(soil_list, function(x) { x["site_name"] <- NULL; x })


#save the soil dataframe in PLOT folders.

for (i in 1:length(soil_list)) {
  df <- soil_list[[i]]
  dir <- file.path(getwd(), "data", "PLOTS", site_names[[i]])
  if (dir.exists(dir)) {
    write.csv(df, file.path(dir, "soil.csv"), row.names = F)
    print(paste("Soil", site_names[[i]], "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", site_names[[i]], "soil, not saved."))
  }
}

###########################################CAT DATA######################################################

#data("SpParamsES")

CAT_VEG_DATA<-read.csv("data/CAT_VEG_DATA.csv")

#ALL SP#################################################################

cat_names<-unique(CAT_VEG_DATA$Site)

cat_forests <- list()
mapping_cat<-c("Species.name" = "sp_correct_name", "Height" = "Height", "Cover" = "Cover")
for (i in cat_names) {
  cat_forests[[i]] <- emptyforest()
  cat_forests[[i]]$shrubData <- forest_mapShrubTable(CAT_VEG_DATA[CAT_VEG_DATA$Site==i,], mapping_y = mapping_cat, SpParams = SpParamsES)
}

cat_forests[["Tivissa"]]$shrubData[20,1]<-"Asphodelus spp."

#save the shrubData dataframe in plot directory.

for (i in cat_names) {
  df <- cat_forests[[i]]$shrubData
  dir <- file.path(getwd(), "data", "PLOTS", i)
  if (dir.exists(dir)) {
    write.csv(df, paste0(dir,"/shrubData_ALL.csv"),row.names = F)
    print(paste("shrub", i, "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", i, "shrubData, not saved."))
  }
}

#MEASURED SP###################################################################################

CAT_LFMC<-read.csv("data/CAT_LFMC.csv")

#Create a dataframe with unique combinations of species and sites

CAT_list_sp_site <- split(CAT_LFMC$sp_correct_name, CAT_LFMC$LocalityName) %>%
  lapply(unique) %>%
  enframe(name = "Site", value = "Species") %>%
  unnest(cols = c(Species))

#DUPLICATED SP AND SITE MEASUREMENTS, BUT DIFERENT PLOTS (1-2)
DUPLICATED_LFMC_SITE_SP<-CAT_VEG_DATA %>%
  group_by(Site, Species) %>%
  filter(n() > 1) %>% 
  print(n=Inf)

#IF WEE AD THE PLOT THERE IS NO DUPLICATED ROWS

# CAT_VEG_DATA %>%
#   group_by(Site, Species, Plot) %>%
#   filter(n() > 1) %>% 
#   print(n=Inf)

#JOIN THE VEGETATION DATA WITH THE MEASURED SP
CAT_MEASURED_VEG_DATA<-left_join(CAT_list_sp_site,CAT_VEG_DATA, by = c("Site" = "Site","Species" = "Species")) %>% 
  filter(!is.na(Cover))

cat_measured_names<-CAT_MEASURED_VEG_DATA$Site

cat_measured_forests <- list()
mapping_cat<-c("Species.name" = "sp_correct_name", "Height" = "Height", "Cover" = "Cover")
for (i in 1:length(cat_measured_names)) {
  cat_measured_forests[[i]] <- emptyforest()
  cat_measured_forests[[i]]$shrubData <- forest_mapShrubTable(CAT_MEASURED_VEG_DATA[i,], mapping_y = mapping_cat, SpParams = SpParamsES)
  #GET THE SP NAME OF EACH SITE
  species_name <- cat_measured_forests[[i]]$shrubData$Species
  #RENAME THE LIST WITH THE SP
  names(cat_measured_forests)[i] <- paste0(cat_measured_names[i],"_",species_name,"_",i)
}

#save the "unique" shrubData dataframe in plot directory.

# for (i in 1:length(cat_measured_names)) {
#   df <- cat_measured_forests[[i]]$shrubData
#   dir <- file.path(getwd(), "data", "PLOTS", cat_measured_names[[i]])
#   if (dir.exists(dir)) {
#     write.csv(df, paste0(dir,"/shrubData_",cat_measured_forests[[i]]$shrubData$Species,i,".csv"),row.names = F)
#     print(paste("shrub", cat_measured_forests[[i]]$shrubData$Species,i, "saved."))
#   } else {
#     print(paste("directory", dir, "not found"))
#     warning(paste(" directory", dir, "not found","\n","shrubData_",cat_measured_forests[[i]]$shrubData$Species,i,"not saved."))
#   }
# }

rm(list=ls())
###########################################FRENCH DATA###################################################
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
FR_LFMC<-read.csv("data/FR_LFMC.csv")
FR_SHRUBS_VEG_DATA<-read.csv("data/FR_SHRUBS_VEG_DATA.csv")
FR_TREES_VEG_DATA<-read.csv("data/FR_TREES_VEG_DATA.csv")
FR_HERBS_VEG_DATA<-read.csv("data/FR_HERBS_VEG_DATA.csv")

FR_SHRUBS_VEG_DATA$coverage<-as.numeric(FR_SHRUBS_VEG_DATA$coverage)
FR_TREES_VEG_DATA$coverage<-as.numeric(FR_TREES_VEG_DATA$coverage)


FR_VEG_DATA<-bind_rows(FR_TREES_VEG_DATA, FR_SHRUBS_VEG_DATA) %>% 
  arrange(Code_Site.x)
  #filter(!is.na(coverage))

#ALL SP###################################################################################

fr_names<-unique(FR_VEG_DATA$Code_Site.x)

#Not all fr-sites have vegetation data!

CAT_FR_SITES$site_name %in% FR_VEG_DATA$Code_Site.x
intersect(CAT_FR_SITES$site_name,FR_VEG_DATA$Code_Site.x)
setdiff(CAT_FR_SITES$site_name,FR_VEG_DATA$Code_Site.x)

fr_forests <- list()
mapping_fr<-c("Species.name" = "sp_correct_name", "Height" = "height", "Cover" = "coverage")
for (i in fr_names) {
  fr_forests[[i]] <- emptyforest()
  fr_forests[[i]]$shrubData <- forest_mapShrubTable(FR_VEG_DATA[FR_VEG_DATA$Code_Site.x==i,], mapping_y = mapping_fr, SpParams = SpParamsFR)
}

for (i in fr_names) {
  df <- fr_forests[[i]]$shrubData
  dir <- file.path(getwd(), "data", "PLOTS", i)
  if (dir.exists(dir)) {
    write.csv(df, paste0(dir,"/shrubData_ALL.csv"),row.names = F)
    print(paste("shrub", i, "saved."))
  } else {
    warning(paste(" directory", dir, "not found","\n", i, "shrubData, not saved."))
  }
}

#MEASURED SP###################################################################################

FR_LFMC<-read.csv("data/FR_LFMC.csv")

#Create a dataframe with unique combinations of species and sites
FR_list_sp_site <- split(FR_LFMC$sp_correct_name, FR_LFMC$SiteCode) %>%
  lapply(unique) %>%
  enframe(name = "Site", value = "Species") %>%
  unnest(cols = c(Species))

#Not all fr-sites have vegetation data! D07S2, D2BS1, don't have vegetation data but have LFMC measurements
not_veg<-which((FR_list_sp_site$Site %in% FR_VEG_DATA$Code_Site.x)==F)
FR_list_sp_site[not_veg,]

#Not all LFMC sp measured have vegetation data  (Hippocrepis emerus, Cytisus scoparius, Calluna vulgaris, Cytisus scoparius).
measured_sp<-which((FR_list_sp_site$Species %in% FR_VEG_DATA$sp_correct_name)==F)
FR_list_sp_site[measured_sp,]


#JOIN THE VEGETATION DATA WITH THE MEASURED SP
FR_MEASURED_VEG_DATA<-left_join(FR_list_sp_site,FR_VEG_DATA, by = c("Site" = "Code_Site.x","Species" = "sp_correct_name")) %>% 
  filter(!is.na(coverage)) %>% 
  select(-c(sp))
  

fr_measured_names<-FR_MEASURED_VEG_DATA$Site

fr_measured_forests <- list()
mapping_fr<-c("Species.name" = "Species", "Height" = "height", "Cover" = "coverage")
for (i in 1:length(fr_measured_names)) {
  fr_measured_forests[[i]] <- emptyforest()
  fr_measured_forests[[i]]$shrubData <- forest_mapShrubTable(FR_MEASURED_VEG_DATA[i,], mapping_y = mapping_fr, SpParams = SpParamsFR)
  #GET THE SP NAME OF EACH SITE
  species_name <- fr_measured_forests[[i]]$shrubData$Species
  #RENAME THE LIST WITH THE SP
  names(fr_measured_forests)[i] <- paste0(fr_measured_names[i],"_",species_name,"_",i)
}

#save the "unique" shrubData dataframe in plot directory.

# for (i in 1:length(fr_measured_names)) {
#   df <- fr_measured_forests[[i]]$shrubData
#   dir <- file.path(getwd(), "data", "PLOTS", fr_measured_names[[i]])
#   if (dir.exists(dir)) {
#     write.csv(df, paste0(dir,"/shrubData_",fr_measured_forests[[i]]$shrubData$Species,i,".csv"),row.names = F)
#     print(paste("shrub", fr_measured_forests[[i]]$shrubData$Species,i, "saved."))
#   } else {
#     print(paste("directory", dir, "not found"))
#     warning(paste(" directory", dir, "not found","\n","shrubData_",fr_measured_forests[[i]]$shrubData$Species,i,"not saved."))
#   }
# }
