library(medfateland)

SpParams<-read.csv("data/inputs/SpParamsAlbert.csv")
SpParams$LeafAngle[SpParams$Name=="Ulex parviflorus"] <- 50

control <- defaultControl(transpirationMode = "Sureau", 
                          soilDomains = "dual", 
                          rhizosphereOverlap = "partial")

control$temperatureResults  <- FALSE
control$snowResults <- FALSE


dates = seq(as.Date("2012-01-01"), as.Date("2022-01-31"), "day")

# INTER-ALLOM-MOD
sf_inter_allom_mod <- readRDS("data/results/sf_INTER_ALLOM_MOD.rds")
spwb_inter_allom_mod <- spwb_spatial(sf_inter_allom_mod, SpParams = SpParams, meteo = NULL, 
                                  local_control = control, 
                                  dates = dates, 
                                  parallelize = TRUE)
saveRDS(spwb_inter_allom_mod, "data/results/spwb_INTER_ALLOM_MOD.rds")

# ERA5-ALLOM-MOD
sf_era5_allom_mod <- readRDS("data/results/sf_ERA5_ALLOM_MOD.rds")
spwb_era5_allom_mod <- spwb_spatial(sf_era5_allom_mod, SpParams = SpParams, meteo = NULL, 
                                     local_control = control, 
                                     dates = dates, 
                                     parallelize = TRUE)
saveRDS(spwb_era5_allom_mod, "data/results/spwb_ERA5_ALLOM_MOD.rds")

# INTER-MODIS-MOD
sf_inter_modis_mod <- readRDS("data/results/sf_INTER_MODIS_MOD.rds")
spwb_inter_modis_mod <- spwb_spatial(sf_inter_modis_mod, SpParams = SpParams, meteo = NULL, 
                                     local_control = control, 
                                     dates = dates, 
                                     parallelize = TRUE)
saveRDS(spwb_inter_modis_mod, "data/results/spwb_INTER_MODIS_MOD.rds")

# ERA5-MODIS-MOD
sf_era5_modis_mod <- readRDS("data/results/sf_ERA5_MODIS_MOD.rds")
spwb_era5_modis_mod <- spwb_spatial(sf_era5_modis_mod, SpParams = SpParams, meteo = NULL, 
                                    local_control = control, 
                                    dates = dates, 
                                    parallelize = TRUE)
saveRDS(spwb_era5_modis_mod, "data/results/spwb_ERA5_MODIS_MOD.rds")