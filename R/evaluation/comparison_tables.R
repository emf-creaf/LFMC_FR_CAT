library(medfateland)
library(medfate)

CAT_LFMC<-read.csv("data/inputs/CAT_LFMC.csv")
CAT_LFMC$date<-as.Date(CAT_LFMC$date)

FR_LFMC<-read.csv("data/inputs/FR_LFMC.csv")
FR_LFMC$date<-as.Date(FR_LFMC$date,format = "%d/%m/%Y")


extract_output<-function(x,
                         LFMC_full1=TRUE,
                         LFMC_full2=TRUE,
                         LFMC_full3=TRUE,
                         LFMC_semi=TRUE){
  
  extracted_data<-list()
  
  extracted_data[["LAI"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "LAI")
  extracted_data[["LAIlive"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "LAIlive")
  extracted_data[["LeafPsiMax"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "LeafPsiMax")
  extracted_data[["LeafRWC"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "LeafRWC")
  extracted_data[["StemRWC"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "StemRWC")
  extracted_data[["LeafPLC"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "LeafPLC")
  extracted_data[["StemPLC"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "StemPLC")
  extracted_data[["LFMC"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "LFMC")
  
  for (i in 1:length(extracted_data)) {
    if (i==1){
      df<-extracted_data[[i]]
    }else {
      df<-merge(df,extracted_data[[i]], by = intersect(c("date","cohort","species"), c("date","cohort","species")))
    }
  }
  
  params <- data.frame(species = x$spwbInput$cohorts$Name,
                       r635 = x$spwbInput$paramsAnatomy$r635,
                       FMCmax = x$spwbInput$paramsWaterStorage$maxFMC,
                       maxMCleaf = x$spwbInput$paramsWaterStorage$maxMCleaf,
                       maxMCstem = x$spwbInput$paramsWaterStorage$maxMCstem)
  
  df <- df |>
    dplyr::left_join(params, by="species") |>
    dplyr::mutate(fleaf = 1/r635,
                  fleafmod = fleaf*(LAI/LAIlive))|>
    dplyr::mutate(LFMC_full1 = FMCmax*LeafRWC,
                  LFMC_full2 = maxMCleaf*LeafRWC*fleaf + maxMCstem*StemRWC*(1.0 - fleaf),
                  LFMC_full3 = maxMCleaf*LeafRWC*fleafmod + maxMCstem*StemRWC*(1.0 - fleafmod),
                  LFMC_semi = 91.87 - (31.12 * log10(-(LeafPsiMax))))
  
  df <- df[order(df$species, df$date),]
  
  return(df)
  
}

comparison_tables <- function(sf, res, taw, DF_TYPE = c("outlier_top"), TH = 2.5,
                              years = c(2012:2022)) {
  output <- vector("list", nrow(sf))
  simulation_data <- lapply(res$result, extract_output)
  for(i in 1:nrow(sf)) {
    source <- sf$source[i]
    site_name <- sf$site_name[i]
    lai_source <- sf$lai_source[i]
    meteo_source <- sf$meteo_source[i]
    soil_mod <- sf$soil_mod[i]
    if (source == "CAT") {
      FILTERED_LFMC <- CAT_LFMC |>
        dplyr::rename(date = date, site = LocalityName, specie = sp_correct_name, LFMC_observed = LFMC) |>
        dplyr::filter(site == site_name, lubridate::year(date) >= years[1])|>
        dplyr::select(date, LFMC_observed, specie)
    } else if (source == "FR") {
      FILTERED_LFMC <- FR_LFMC |>
        dplyr::rename(date = date, site = SiteCode, specie = sp_correct_name, LFMC_observed = RobustLFMC) |>
        dplyr::filter(site == site_name, lubridate::year(date) >= years[1]) |> 
        dplyr::select(date, LFMC_observed, specie) |> 
        dplyr::mutate(specie = ifelse(specie == "Calicotome", "Calicotome spinosa", specie))|> 
        dplyr::mutate(specie = ifelse(specie == "Calluna vulgaris", "Erica cinerea", specie)) # Erica cinerea in plot data and Calluna vulgaris in LFMC data
    }
    measured_species <- unique(FILTERED_LFMC$specie)
    MERGED_LFMC_ALL <- simulation_data[[i]] |>
      dplyr::filter(species %in% measured_species) |>
      dplyr::mutate(date = as.Date(date),
                    site = site_name,
                    source = source,
                    lai_source = lai_source,
                    meteo_source = meteo_source,
                    soil_mod = soil_mod) |>
      dplyr::full_join(FILTERED_LFMC, by = c("date" = "date", "species" = "specie"))
    
    MERGED_LFMC_ALL <- MERGED_LFMC_ALL |>
      dplyr::mutate(is_summer = (lubridate::month(date) >= 5 & lubridate::month(date) <= 9),
                    is_outrange = LFMC_observed > FMCmax,
                    is_outlier = FALSE)
    
    if ("outlier_top" %in% DF_TYPE) {
      for(j in 1:length(measured_species)) {
        sel_sp <- MERGED_LFMC_ALL$species==measured_species[j]
        obs_sp <- MERGED_LFMC_ALL$LFMC_observed[sel_sp]
        med_obs <- median(obs_sp, na.rm = TRUE) #median 
        mad_obs <- mad(obs_sp, na.rm = TRUE) #Median absolute deviation
        threshold_obs <- TH * mad_obs
        MERGED_LFMC_ALL$is_outlier[sel_sp] <- (obs_sp > (med_obs + threshold_obs))
      }
    }
    
    # Add taw info
    MERGED_LFMC_ALL <-  MERGED_LFMC_ALL |>
      dplyr::mutate(taw = taw) |>
      dplyr::relocate("site", "source","lai_source", "meteo_source", "soil_mod", "taw", .before = "LAI")
    
    output[[i]] <- MERGED_LFMC_ALL
    
  }
  return(output)
}


for(meteo in c("INTER", "ERA5")) {
  for(lai in c("ALLOM", "MODIS")) {
    sf <- readRDS(paste0("data/sf_inputs/sf_", meteo, "_", lai, "_MOD.rds"))
    # for(taw in c(30, 40, 50, 60,70,80,90,100,120,140,160)) {
    for(taw in c(140,150)) {
      cat(paste0(meteo, "-", lai, "-",taw,"\n"))
      res <- readRDS(paste0("data/results/spwb_", meteo, "_", lai, "_MOD_",taw,".rds"))
      ct <- comparison_tables(sf, res, taw, DF_TYPE = c("outlier_top"))
      saveRDS(ct, paste0("data/comparison_tables/ct_", meteo, "_", lai, "_MOD_",taw,".rds"))
    }
  }
}

