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
  LAI_data <- terra::extract(LAIrasters[[i]], coords_sf, ID=FALSE)
  LAI_all <- cbind(LAI_all, LAI_data)
}

#PLOT D66S2 have value 250 (urban) there is a road. extract the data from the pixel abvove

coords <- st_coordinates(coords_sf[25,])
LAI_25 <- CAT_FR_SITES[25,]

row_offset<--1 #THE ROW ABOVE
col_offset<- 0 #THE SAME COL

for(i in 1:length(LAIrasters)) {
  # ROW AND COLUM FROM THE RASTER IN THE COORDINATE
  index <- terra::cellFromXY(LAIrasters[[i]], coords)
  row_col <- terra::rowColFromCell(LAIrasters[[i]], index)
  #EXTRACT THE LAI VALUE ROW/COLUMN FROM THE PIXEL ABOVE
  LAI_data_25 <- LAIrasters[[i]][row_col[1]+row_offset, row_col[2]+col_offset]
  LAI_25 <- cbind(LAI_25, LAI_data_25)
}

#CHANGE THE ROW 25 (PLOT D66S2) with the new data

LAI_all_correct<-LAI_all %>%
  filter(!ID %in% LAI_25$ID) %>% #REMOVE THE OLD 25 ROW
  bind_rows(LAI_25) %>% #BIND THE NEW 25 ROW
  arrange(ID) #ORDER BY ID


# Apply scale factor of 0,1
LAI_all_correct[,-(1:11)] <- LAI_all_correct[,-(1:11)] * 0.1


#export LAI DATA
write.csv(LAI_all_correct, "data/LAI_DATA_MODIS.csv", row.names=FALSE)

#AVERAGE 5 HIGHEST VALUES PER YEAR#####

LAI_DATA_MODIS<-read.csv("data/LAI_DATA_MODIS.csv")


years <- 2002:2024
LAI_DATA_MODIS_MEAN_TOP5<-LAI_DATA_MODIS
for (i in years) {
  year_str <- as.character(i)
  LAI_DATA_MODIS_MEAN_TOP5 <- LAI_DATA_MODIS_MEAN_TOP5 %>%
    rowwise() %>%
    mutate(!!paste0("MCD15A2H_Lai_mean_top5_", year_str) := mean(sort(c_across(contains(year_str)), decreasing = TRUE)[1:5], na.rm = TRUE)) %>%
    select(-c(contains(year_str) & !contains("mean")))
}

write.csv(LAI_DATA_MODIS_MEAN_TOP5, "data/LAI_DATA_MODIS_MEANS.csv", row.names=FALSE)
