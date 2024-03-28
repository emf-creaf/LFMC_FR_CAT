library(medfate)
library(medfateutils)
library(sf)
library(tidyverse)


sites<-read.csv("data/CAT_FR_SITES.csv")


#Soil from SoilGrid Hengl_2017

for (i in (1:nrow(sites))) {
  site<-sites[i,]
  LON<-site$LON
  LAT<-site$LAT
  site_name<-site$site_name
  coords_sf <- st_sfc(st_point(c(LON, LAT)), crs = 4326)
  soil<-soilgridsParams(coords_sf, widths = c(300,1000,2000))
  soil$site_name<-site_name
  soil<-relocate(soil,site_name)
    if(i==1){
    all_soil<-soil
  }else
  all_soil<-full_join(all_soil,soil)
}

write.csv(all_soil, "data/SOIL_DATA.csv", row.names=FALSE)

#all_sol data frame to list (medfate format)

soil_data<-read.csv(file = "data/SOIL_DATA.csv")

soil_list<-soil_data %>% 
  group_split(site_name) %>% 
  lapply(as.data.frame)

#remove soil_data column

soil_list<-lapply(soil_list, function(x) { x["site_name"] <- NULL; x })

summary(soil_list)