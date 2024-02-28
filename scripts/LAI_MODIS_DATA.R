##########################LAI MODIS DATA##########################

library(MODIStsp)
library(terra)
library(sf)

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
