library(DBI)
library(RSQLite)
library(tidyverse)
library(sf)
library(geodata)
library(terra)

#############PLOTS DATA###############

##CAT DATA####

#CONNECT DATABASE
CAT_LFMC <- dbConnect(SQLite(), "raw_data/CAT_DATA/lfmc.sqlite")
#dbDisconnect(CAT_LFMC)

#EXTRACT DATABASE TABLES
TABLES<-dbListTables(CAT_LFMC)

for(i in 1:length(TABLES)){
  assign(TABLES[i], dbReadTable(CAT_LFMC, TABLES[i]))
}
#REMOVE EMPTY TABLES
rm("phenology","soil_measurements","tdr_sensor")

names(lfmc)
names(sites)
names(sites_species)
names(species)

#CHOOSE CORRECT CAT PLOTS
CAT_SITES<-data.frame(sites[1:9,])
str(CAT_SITES)

#TRANSFORM DATA TO CORRECT FORMAT 

CAT_SITES$Longitude<-gsub(",",".",CAT_SITES$Longitude)  #COMMA TO DOTS
CAT_SITES$Longitude<-gsub("\ua0","",CAT_SITES$Longitude)#REMOVE EMPTY SPACE
CAT_SITES$Longitude<-as.numeric(CAT_SITES$Longitude)    #SET AS NUMERIC

CAT_SITES$Latitude<-gsub(",",".",CAT_SITES$Latitude)  #COMMA TO DOTS
CAT_SITES$Latitude<-gsub("\ua0","",CAT_SITES$Latitude)#REMOVE EMPTY SPACE
CAT_SITES$Latitude<-as.numeric(CAT_SITES$Latitude)    #SET AS NUMERIC

str(CAT_SITES)

CAT_SITES$LocalityName[CAT_SITES$LocalityName == "Torà"] <- "Tora"

##FRENCH DATA####

FR_SITES<-read.csv("raw_data/FR_DATA/RH_Sites_Coordinates.csv", sep = ";")
str(FR_SITES)

FR_SITES$WGS84_Longitude<-gsub(",",".",FR_SITES$WGS84_Longitude)#COMMA TO DOTS
FR_SITES$WGS84_Longitude<-as.numeric(FR_SITES$WGS84_Longitude)  #SET AS NUMERIC

FR_SITES$WGS84_Latitude<-gsub(",",".",FR_SITES$WGS84_Latitude)#COMMA TO DOTS
FR_SITES$WGS84_Latitude<-as.numeric(FR_SITES$WGS84_Latitude)  #SET AS NUMERIC

str(FR_SITES)

##MERGE CAT AND FR DATA####

CAT_FR_SITES<-data.frame(ID = c(1:(nrow(CAT_SITES)+nrow(FR_SITES))),
                         site_name = c(CAT_SITES$LocalityName,FR_SITES$SiteCode),
                         LON = c(CAT_SITES$Longitude,FR_SITES$WGS84_Longitude),
                         LAT = c(CAT_SITES$Latitude,FR_SITES$WGS84_Latitude),
                         start_date = c(CAT_SITES$StartYear,FR_SITES$Starting_Date),
                         end_date = c(CAT_SITES$EndYear,FR_SITES$Last_Year))

str(CAT_FR_SITES)

write.csv(CAT_FR_SITES,"data/CAT_FR_SITES.csv", row.names = F)

#####TOPO####

SF_CAT_FR_SITES<-st_as_sf(CAT_FR_SITES, coords = c("LON", "LAT"), crs = 4326)

vect_SF_CAT_FR_SITES <- terra::vect(SF_CAT_FR_SITES$geometry)


#TOPOGRAPHIC DATA
country_codes("Spain")
country_codes("France")

#SPAIN_elev_raster<-geodata::elevation_30s(country="ESP", path = "raw_data/TOPO_DATA/SPAIN/")
#FRANCE_elev_raster<-geodata::elevation_30s(country="FR", path = "raw_data/TOPO_DATA/FRANCE/")

SPAIN_elev_raster<-rast("raw_data/TOPO_DATA/SPAIN/ESP_elv_msk.tif")
FRANCE_elev_raster<-rast("raw_data/TOPO_DATA/FRANCE/FRA_elv_msk.tif")

names(SPAIN_elev_raster) <- "elevation"
SPAIN_elev_raster

names(FRANCE_elev_raster) <- "elevation"
FRANCE_elev_raster

SPAIN_slope_raster <- terra::terrain(SPAIN_elev_raster, v = "slope", unit="degrees")
SPAIN_aspect_raster <- terra::terrain(SPAIN_elev_raster, v = "aspect", unit="degrees")

FRANCE_slope_raster <- terra::terrain(FRANCE_elev_raster, v = "slope", unit="degrees")
FRANCE_aspect_raster <- terra::terrain(FRANCE_elev_raster, v = "aspect", unit="degrees")


#SPAIN_topo_raster <- c(SPAIN_elev_raster, SPAIN_slope_raster, SPAIN_aspect_raster)
#plot(SPAIN_topo_raster)

#EXTRACT TOPO
SPAIN_elev<-terra::extract(SPAIN_elev_raster,vect_SF_CAT_FR_SITES)
SPAIN_slope<-terra::extract(SPAIN_slope_raster,vect_SF_CAT_FR_SITES)
SPAIN_aspect<-terra::extract(SPAIN_aspect_raster,vect_SF_CAT_FR_SITES)

SPAIN_topo<-SPAIN_elev %>% 
  full_join(SPAIN_slope) %>% 
  full_join(SPAIN_aspect) %>% 
  filter(!is.na(elevation))

FRANCE_elev<-terra::extract(FRANCE_elev_raster,vect_SF_CAT_FR_SITES)
FRANCE_slope<-terra::extract(FRANCE_slope_raster,vect_SF_CAT_FR_SITES)
FRANCE_aspect<-terra::extract(FRANCE_aspect_raster,vect_SF_CAT_FR_SITES)

FRANCE_topo<-FRANCE_elev %>% 
  full_join(FRANCE_slope) %>% 
  full_join(FRANCE_aspect) %>% 
  filter(!is.na(elevation))

CAT_FR_SITES_topo<-bind_rows(SPAIN_topo,FRANCE_topo)

CAT_FR_SITES_2<-CAT_FR_SITES %>% 
  full_join(CAT_FR_SITES_topo, by = "ID")

####ARSENE METEO DATA IN CAT_FR_SITES######

CAT_FR_SITES_climName<-read.csv("raw_data/ERA5_DATA/DATA ARSENE/CAT_FR_SITES_climName.csv")

CAT_FR_SITES_2$ERA5_NAME_POINTS<-CAT_FR_SITES_climName$climPts

write.csv(CAT_FR_SITES_2,"data/CAT_FR_SITES.csv", row.names = F)