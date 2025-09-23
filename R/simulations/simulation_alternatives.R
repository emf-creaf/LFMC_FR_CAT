library(medfateland)

SpParams<-read.csv("data/inputs/SpParamsAlbert.csv")
SpParams$LeafAngle[SpParams$Name=="Ulex parviflorus"] <- 50

control <- defaultControl(transpirationMode = "Sureau", 
                          soilDomains = "dual", 
                          rhizosphereOverlap = "partial")

control$temperatureResults  <- FALSE
control$snowResults <- FALSE
## Root truncation should lead to more realistic results (update v. 4.8.4)
control$truncateRootDistribution <- FALSE
## This assumes stem growth occurs during winter
control$stemCavitationRecovery <- "annual"
## This assumes new leaves can be formed and they lack any PLC
control$leafCavitationRecovery <- "total"

modify_soil_taw <- function(s, target_taw, max_rocks = 97.5, soilFunctions = "VG") {
  f_sew_diff <- function(factor, sew_target, max_rocks, soil, soilFunctions) {
    soil_tmp <- soil
    soil_tmp$rfc <- pmax(pmin(soil_tmp$rfc*factor,max_rocks),0)
    sew <- sum(medfate::soil_waterExtractable(soil_tmp, model = soilFunctions))
    return(sew_target - sew)
  }
  r <- uniroot(f_sew_diff, c(0,10), target_taw, max_rocks, medfate::soil(s), soilFunctions)
  s$rfc <- pmax(pmin(s$rfc*r$root,max_rocks),0)
  return(s)
}

dates = seq(as.Date("2012-01-01"), as.Date("2022-12-31"), "day")

for(target_taw in c(40, 60, 80, 100, 120, 140)) {

  # INTER-ALLOM-MOD
  sf_inter_allom_mod <- readRDS("data/results/sf_INTER_ALLOM_MOD.rds")
  sf_inter_allom_mod$soil <- lapply(sf_inter_allom_mod$soil, modify_soil_taw, target_taw = target_taw)
  spwb_inter_allom_mod <- spwb_spatial(sf_inter_allom_mod, SpParams = SpParams, meteo = NULL,
                                       local_control = control,
                                       dates = dates,
                                       parallelize = TRUE, chunk_size = 1)
  saveRDS(spwb_inter_allom_mod, paste0("data/results/spwb_INTER_ALLOM_MOD_", target_taw, ".rds"))

  # ERA5-ALLOM-MOD
  sf_era5_allom_mod <- readRDS("data/results/sf_ERA5_ALLOM_MOD.rds")
  sf_era5_allom_mod$soil <- lapply(sf_era5_allom_mod$soil, modify_soil_taw, target_taw = target_taw)
  spwb_era5_allom_mod <- spwb_spatial(sf_era5_allom_mod, SpParams = SpParams, meteo = NULL,
                                      local_control = control,
                                      dates = dates,
                                      parallelize = TRUE, chunk_size = 1)
  saveRDS(spwb_era5_allom_mod, paste0("data/results/spwb_ERA5_ALLOM_MOD_", target_taw, ".rds"))

  # INTER-MODIS-MOD
  sf_inter_modis_mod <- readRDS("data/results/sf_INTER_MODIS_MOD.rds")
  sf_inter_modis_mod$soil <- lapply(sf_inter_modis_mod$soil, modify_soil_taw, target_taw = target_taw)
  spwb_inter_modis_mod <- spwb_spatial(sf_inter_modis_mod, SpParams = SpParams, meteo = NULL,
                                       local_control = control,
                                       dates = dates,
                                       parallelize = TRUE, chunk_size = 1)
  saveRDS(spwb_inter_modis_mod, paste0("data/results/spwb_INTER_MODIS_MOD_", target_taw, ".rds"))

  # ERA5-MODIS-MOD
  sf_era5_modis_mod <- readRDS("data/results/sf_ERA5_MODIS_MOD.rds")
  sf_era5_modis_mod$soil <- lapply(sf_era5_modis_mod$soil, modify_soil_taw, target_taw = target_taw)
  spwb_era5_modis_mod <- spwb_spatial(sf_era5_modis_mod, SpParams = SpParams, meteo = NULL, 
                                      local_control = control, 
                                      dates = dates, 
                                      parallelize = TRUE, chunk_size = 1)
  saveRDS(spwb_era5_modis_mod, paste0("data/results/spwb_ERA5_MODIS_MOD_", target_taw, ".rds"))  
}

