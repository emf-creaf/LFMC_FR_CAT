library(medfate)
library(medfateland)
library(sf)

sf_ALLOM <- readRDS("data/sf_inputs/sf_INTER_ALLOM_MOD.rds")
sf_MODIS <- readRDS("data/sf_inputs/sf_INTER_MODIS_MOD.rds")
cc <- sf::st_coordinates(sf_ALLOM)
TS1 <- sf_ALLOM |> 
  sf::st_drop_geometry()|>
  dplyr::select(id, site_name, source, elevation, slope, aspect, lai_stand) |>
  dplyr::rename("LAI_ALLOM" = lai_stand)
TS1$Latitude <- cc[,2]
TS1$Longitude <- cc[,1]
TS1$LAI_MODIS <- sf_MODIS$lai_stand
TS1$MAP_INTER <- NA
TS1$MAP_ERA5 <- NA

res_INTER <- readRDS(paste0("data/results/spwb_INTER_MODIS_MOD_40.rds"))
res_ERA5 <- readRDS(paste0("data/results/spwb_ERA5_ALLOM_MOD_40.rds"))
for(i in 1:38) {
  TS1$MAP_INTER[i] <- mean(summary(res_INTER$result[[i]])[,"Precipitation"])
  TS1$MAP_ERA5[i] <- mean(summary(res_ERA5$result[[i]])[,"Precipitation"])
}

write.csv2(TS1, "data/TableS1.csv", row.names = FALSE)

# Variation and covariation in LAI inputs
mean(TS1$LAI_ALLOM)
sd(TS1$LAI_ALLOM)
mean(TS1$LAI_MODIS)
sd(TS1$LAI_MODIS)
cor.test(TS1$LAI_ALLOM, TS1$LAI_MODIS)

# Variation and covariation in MAP inputs
mean(TS1$MAP_INTER)
sd(TS1$MAP_INTER)
mean(TS1$MAP_ERA5)
sd(TS1$MAP_ERA5)
cor.test(TS1$MAP_INTER, TS1$MAP_ERA5)
