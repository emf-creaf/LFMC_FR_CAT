##########################LAI MODIS DATA##########################

library(MODIStsp)
library(terra)
library(sf)
library(tidyverse)

##extract data

#MODIStsp()

##UPLOAD DATA

#CAT_FR_SITES (ID, LONG, LAT)
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")

#LAI DATA .tif
LAIlistfiles<-list.files("raw_data/LAI_8Days_500m_v61/Lai",
                         pattern = ".tif",full.names = TRUE)
#RASTER .tif DATA
LAIrasters <- list()
for(i in 1:length(LAIlistfiles)) {
  LAIrasters[[i]] <- rast(LAIlistfiles[[i]])
}

#EXTRACT LAI DATA AT PLOTS COORDS

coords_sf<-st_as_sf(CAT_FR_SITES, coords = c("LON", "LAT"), crs = 4326)

LAI_all<-CAT_FR_SITES
for(i in 1:length(LAIrasters)) {
  LAI_data <- extract(LAIrasters[[i]], coords_sf)
  LAI_data$ID <- NULL
  LAI_all <- cbind(LAI_all, LAI_data)
}

#export LAI DATA

write.csv(LAI_all, "data/LAI_DATA_MODIS.csv", row.names=FALSE)


#AVERAGE 5 HIGHEST VALUES PER YEAR

LAI_DATA_MODIS<-read.csv("data/LAI_DATA_MODIS.csv")

years <- 2002:2024


LAI_DATA_MODIS_MEAN_TOP5<-LAI_DATA_MODIS
for (i in years) {
  year_str <- as.character(i)
  short_year<-substr(year_str, start = 3, stop = 4)
  LAI_DATA_MODIS_MEAN_TOP5 <- LAI_DATA_MODIS_MEAN_TOP5 %>%
    rowwise() %>%
    mutate(!!paste0("mean_top5", short_year) := mean(sort(c_across(contains(year_str)), decreasing = TRUE)[1:5], na.rm = TRUE)) %>%
    select(-c(contains(year_str)))
}






















df<-data.frame(site=c("site1","site2","site3"),
               value_2012=c(3,5,6),
               value_2013=c(6,7,9),
               value_2014=c(3,5,4),
               value_2015=c(3,5,6),
               value_2016=c(6,7,9),
               value_2017=c(3,5,4),
               value_2018=c(3,5,6),
               value_2019=c(6,7,9),
               value_2020=c(3,5,4),
               value_2021=c(3,5,6),
               value_2022=c(6,7,9),
               value_2023=c(3,5,4))


df <- df %>%
  rowwise() %>%
  mutate(mean = mean(sort(c_across(starts_with("value_")), decreasing = TRUE)[1:5], na.rm = TRUE)) %>%
  select(site, mean)

df2 <- df %>%
  rowwise() %>%
  mutate(sort(c_across(starts_with("value_")), decreasing = TRUE)[1:5]) %>% 
  mutate(mean = mean()) %>%
  select(site, mean)
