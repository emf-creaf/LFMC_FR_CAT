library(medfateland)

# Builds sf input objects for medfateland ---------------------------------
build_sf<-function(SpParams,
                   plot_source = c("CAT", "FR"), 
                   meteo_source = "INTER", 
                   lai_source = "ALLOM",
                   soil_mod = TRUE,
                   species = "ALL",
                   years = c(2012:2022)) {
  plot_source <- match.arg(plot_source, c("CAT", "FR"), several.ok = TRUE)
  meteo_source <- match.arg(meteo_source, c("ERA5", "INTER"), several.ok = FALSE)
  lai_source <- match.arg(lai_source, c("MODIS", "ALLOM"), several.ok = FALSE)
  
  CAT_FR_SITES<-read.csv("data/inputs/CAT_FR_SITES.csv") |>
    dplyr::rename(id = ID)
  sf <- sf::st_as_sf(CAT_FR_SITES, coords = c("LON", "LAT"), crs = 4326)
  sf[["meteo_source"]] <- meteo_source
  sf[["lai_source"]] <- lai_source
  sf[["soil_mod"]] <- soil_mod
  sf[["species"]] <- species
  sf[["lai_stand"]] <- NA
  sf[["soil_taw"]] <- NA
  sf[["forest"]] <- vector("list", nrow(sf))
  sf[["soil"]] <- vector("list", nrow(sf))
  sf[["meteo"]] <- vector("list", nrow(sf))
  sf <- sf |> 
    dplyr::filter(source %in% plot_source)
  
  for(i in 1:nrow(sf)) {
    SITE_NAME <- sf$site_name[i]
    f <- medfate::emptyforest()
    f$shrubData <- read.csv(paste0("data/inputs/PLOTS/", SITE_NAME, "/shrubData_ALL.csv")) |>
      dplyr::filter(!is.na(Height)) 
    f$shrubData$Cover[is.na(f$shrubData$Cover)] <- 5 ## This happens for french trees often
    
    nplots <- length(unique(f$shrubData$Plot))
    if(nplots>1) {
      f$shrubData$Cover <- f$shrubData$Cover/nplots
    }
    ## This merges cohorts from different subplots or being sampled using different heights
    date <- f$shrubData$Date[1]
    f <- medfate::forest_mergeShrubs(f, byHeightclass = FALSE)
    f$shrubData$Date <- date
    
    ## Estimate plant LAI (for single species simulations or correction using MODIS)
    f$shrubData$LAI <- pmax(0.01, medfate::plant_LAI(f, SpParams))
    
    lai_tot <- max(1.0, min(3.0, sum(f$shrubData$LAI)))
    if(species !="ALL") {
      if (sf$source[i] == "CAT") {
        CAT_LFMC<-read.csv("data/inputs/CAT_LFMC.csv")
        CAT_LFMC$date<-as.Date(CAT_LFMC$date)
        SP_MEASURED <- CAT_LFMC |>
          dplyr::filter(lubridate::year(date) >= years[1], LocalityName == sf$site_name[i]) %>%
          dplyr::pull(sp_correct_name) |>
          unique()
      } else if (sf$source[i] == "FR")  {
        FR_LFMC<-read.csv("data/inputs/FR_LFMC.csv")
        FR_LFMC$date<-as.Date(FR_LFMC$date,format = "%d/%m/%Y")
        SP_MEASURED <- FR_LFMC |>
          dplyr::filter(lubridate::year(date) >= years[1], SiteCode == sf$site_name[i]) |>
          dplyr::pull(sp_correct_name) |>
          unique()
      }
      if(species %in% SP_MEASURED) {
        f$shrubData <- f$shrubData |>
          dplyr::filter(Species %in% species) |>
          dplyr::slice(which.max(Cover))
        f$shrubData$LAI <- f$shrubData$LAI/sum(f$shrubData$LAI)*lai_tot
      } else {
        f$shrubData <- f$shrubData[numeric(0),,drop=FALSE]
      }
    } else {
      f$shrubData$LAI <- f$shrubData$LAI/sum(f$shrubData$LAI)*lai_tot
    }
    
    #LAI from MODIS
    if (lai_source == "MODIS") {
      #Read the MODIS LAI data
      LAI_MODIS<-read.csv(paste0("data/inputs/PLOTS/", SITE_NAME, "/MODIS_LAI.csv"))
      LAI_YEAR<-unique(format(as.Date(f$shrubData$Date),"%Y"))
      ## Truncate to LAI between 1.0 and 3.0
      lai_modis <- max(1.0, min(3.0, LAI_MODIS$MCD15A2H_Lai_mean_top5[LAI_MODIS$YEAR %in% LAI_YEAR]))
      #New column LAI in shrubData
      if(nrow(f$shrubData)>0) f$shrubData$LAI<-f$shrubData$LAI/sum(f$shrubData$LAI)*lai_modis
    }
    sf$forest[[i]] <- f
    sf$lai_stand[i] <- sum(f$shrubData$LAI)
    
    if (soil_mod == TRUE) {
      #LOAD THE MODIFIED ROCK SOIL
      soil_table <- read.csv(paste0("data/inputs/PLOTS/", SITE_NAME, "/soil_mod.csv"))
      soil <- medfate::soil_redefineLayers(soil_table, widths = c(200, 300, 600, 1000, 2000))
      #soil <- medfate::soil_redefineLayers(soil_table, widths = c(200, 300, 600, 3000))
    } else {
      soil_table <- read.csv(paste0("data/inputs/PLOTS/", SITE_NAME, "/soil.csv"))
      soil <- medfate::soil_redefineLayers(soil_table, widths = c(200, 300, 600, 1000, 2000))
      #soil <- medfate::soil_redefineLayers(soil_table, widths = c(200, 300, 600, 3000))
    }
    sf$soil[[i]] <- soil
    sf$soil_taw[i] <- sum(medfate::soil_waterExtractable(medfate::soil(soil), "VG"))
    
    if(meteo_source == "ERA5") {
      met <- read.csv(paste0("data/inputs/PLOTS/", SITE_NAME, "/meteo_ERA5.csv"))
      met$Dates <- as.Date(met$Dates)
      met <- met[met$YEAR %in% years, ]
      row.names(met) <- as.character(met$Dates)
    } else if (meteo_source == "INTER")  {
      met <- read.csv(paste0("data/inputs/PLOTS/", SITE_NAME, "/meteo_interpolator.csv"))
      met$dates <- as.Date(met$dates)
      met$YEAR <- as.numeric(format(met$dates, "%Y"))
      met <- met[met$YEAR %in% years, ]
    }

    sf$meteo[[i]] <- met
    
  }
  keep <- rep(TRUE, nrow(sf))
  for(i in 1:nrow(sf)) {
    if(nrow(sf$forest[[i]]$shrubData)==0) keep[i] <- FALSE
  }
  sf<- sf::st_as_sf(tibble::as_tibble(sf[keep,,drop = FALSE]))
}


SpParams<-read.csv("data/inputs/SpParamsAlbert.csv")

sf_inter_allom_mod <- build_sf(SpParams, meteo_source = "INTER", lai_source = "ALLOM", soil_mod = TRUE)
saveRDS(sf_inter_allom_mod, "data/sf_inputs/sf_INTER_ALLOM_MOD.rds")

sf_era5_allom_mod <- build_sf(SpParams, meteo_source = "ERA5", lai_source = "ALLOM", soil_mod = TRUE)
saveRDS(sf_era5_allom_mod, "data/sf_inputs/sf_ERA5_ALLOM_MOD.rds")

sf_inter_modis_mod <- build_sf(SpParams, meteo_source = "INTER", lai_source = "MODIS", soil_mod = TRUE)
saveRDS(sf_inter_modis_mod, "data/sf_inputs/sf_INTER_MODIS_MOD.rds")

sf_era5_modis_mod <- build_sf(SpParams, meteo_source = "ERA5", lai_source = "MODIS", soil_mod = TRUE)
saveRDS(sf_era5_modis_mod, "data/sf_inputs/sf_ERA5_MODIS_MOD.rds")

