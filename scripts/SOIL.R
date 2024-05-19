library(medfate)
library(medfateutils)
library(sf)
library(tidyverse)


CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")

sites<-st_as_sf(CAT_FR_SITES, coords = c("LON", "LAT"), crs = 4326)

#Soil from SoilGrid Hengl_2017####

all_soil<-data.frame()
soil_df<-data.frame()
for (i in 36:nrow(sites)) {
  site<-sites[i,]
  site_name<-site$site_name
  soil<- add_soilgrids(site, widths = c(50, 150, 300, 600, 1000, 2000))
  soil$soil[[1]]$site_name<-site_name
  soil$soil[[1]]<-relocate(soil$soil[[1]],site_name)
  all_soil<-rbind(all_soil,soil)
  soil_df<-rbind(soil_df, soil$soil[[1]])
}

write.csv(soil_df, "data/SOIL_DATA.csv", row.names=FALSE)
save(all_soil, file = "data/SOIL_DATA.rds")
