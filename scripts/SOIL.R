library(medfate)
library(medfateutils)
library(sf)
library(tidyverse)
library(terra)


CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")

sites<-st_as_sf(CAT_FR_SITES, coords = c("LON", "LAT"), crs = 4326)

#Soil from SoilGrid Hengl_2017####

all_soil<-data.frame()

for (i in 36:nrow(sites)) {
  site<-sites[i,]
  site_name<-site$site_name
  soil<- add_soilgrids(site, widths = c(50, 150, 300, 600, 1000, 2000))
  soil$soil[[1]]$site_name<-site_name
  soil$soil[[1]]<-relocate(soil$soil[[1]],site_name)
  all_soil<-rbind(all_soil,soil)
}

check_soils(all_soil)

soil_df<-data.frame()
for (i in 1:nrow(all_soil)){
  soil_df<-rbind(soil_df, all_soil$soil[[i]])
}

write.csv(soil_df, "data/SOIL_DATA.csv", row.names=FALSE)
saveRDS(all_soil, file = "data/SOIL_DATA.rds")

#####SOIL ROCK CONTENT MODIFICATION#####################

all_soil<-readRDS("data/SOIL_DATA.rds")

# Censored soil depth (cm)
bdricm <- terra::rast("raw_data/SOIL MOD/BDRICM_M_250m_ll.tif")
# Probability of bedrock within first 2m [0-100]
bdrlog <- terra::rast("raw_data/SOIL MOD/BDRLOG_M_250m_ll.tif")
# Absolute depth to bedrock (cm)

x_vect <- terra::vect(sf::st_transform(sf::st_geometry(sites), terra::crs(bdricm)))
x_ext <- terra::ext(x_vect)

bdricm <- terra::crop(bdricm, x_ext, snap = "out")
bdrlog <- terra::crop(bdrlog, x_ext, snap = "out")

soil_depth_mm <- (bdricm$BDRICM_M_250m_ll*10)*(1 - (bdrlog$BDRLOG_M_250m_ll/100))


all_soil_modify<- modify_soils(all_soil, 
                    soil_depth_map = soil_depth_mm)

soil_df_modify<-data.frame()
for (i in 1:nrow(all_soil)){
  soil_df_modify<-rbind(soil_df_modify, all_soil_modify$soil[[i]])
}

write.csv(soil_df_modify, "data/SOIL_DATA_MOD.csv", row.names=FALSE)
saveRDS(all_soil_modify, file = "data/SOIL_DATA_MOD.rds")


