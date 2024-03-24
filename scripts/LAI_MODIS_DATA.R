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

class(LAIrasters[[1]])

#EXTRACT LAI DATA AT PLOTS COORDS

coords_sf<-st_as_sf(CAT_FR_SITES, coords = c("LON", "LAT"), crs = 4326)

LAI_all<-CAT_FR_SITES
for(i in 1:length(LAIrasters)) {
  LAI_data <- terra::extract(LAIrasters[[i]], coords_sf)
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
    mutate(!!paste0("mean_top5_", short_year) := mean(sort(c_across(contains(year_str)), decreasing = TRUE)[1:5], na.rm = TRUE)) %>%
    select(-c(contains(year_str)))
}

write.csv(LAI_DATA_MODIS_MEAN_TOP5, "data/LAI_DATA_MODIS_MEANS.csv", row.names=FALSE)
