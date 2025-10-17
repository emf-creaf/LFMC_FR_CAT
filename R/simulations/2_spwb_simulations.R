library(medfateland)


# Species parameters and simulation control -------------------------------
SpParams<-read.csv("data/inputs/SpParamsAlbert.csv")
SpParams$LeafAngle[SpParams$Name=="Ulex parviflorus"] <- 50

control <- defaultControl(transpirationMode = "Sureau", 
                          soilDomains = "dual", 
                          rhizosphereOverlap = "partial")

control$temperatureResults  <- FALSE
control$snowResults <- FALSE
## Root truncation should lead to more realistic results (update v. 4.8.4)
## but presently leads to model crashes
control$truncateRootDistribution <- TRUE
## This assumes stem growth occurs during winter
control$stemCavitationRecovery <- "annual"
## This assumes leaf growth occurs during winter
control$leafCavitationRecovery <- "annual"


# Function to estimate rfc for a target taw -------------------------------
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



# Performs all simulations (two days in server) ---------------------------

dates <- seq(as.Date("2012-01-01"), as.Date("2022-12-31"), "day")
for(target_taw in seq(30,160, by = 10)) {

  for(meteo in c("INTER", "ERA5")) {
    for(lai in c("ALLOM", "MODIS")) {
      cli::cli_h1(paste0(meteo, "-", lai, "-", target_taw))
      sf <- readRDS(paste0("data/sf_inputs/sf_", meteo, "_", lai, "_MOD.rds"))
      sf$soil <- lapply(sf$soil, modify_soil_taw, target_taw = target_taw)
      res <- spwb_spatial(sf, SpParams = SpParams, meteo = NULL,
                          local_control = control,
                          dates = dates,
                          parallelize = TRUE, chunk_size = 1)
      saveRDS(res, paste0("data/results/spwb_", meteo, "_", lai,"_MOD_", target_taw, ".rds"))
    }
  }
}

