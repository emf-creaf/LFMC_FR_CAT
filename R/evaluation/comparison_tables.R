library(medfateland)
library(medfate)

CAT_LFMC<-read.csv("data/inputs/CAT_LFMC.csv")
CAT_LFMC$date<-as.Date(CAT_LFMC$date)

FR_LFMC<-read.csv("data/inputs/FR_LFMC.csv")
FR_LFMC$date<-as.Date(FR_LFMC$date,format = "%d/%m/%Y")


extract_output<-function(x,
                         LEAFPSIMAX=TRUE,
                         LEAFRWC=TRUE,
                         LFMC=TRUE,
                         LFMC_semi_mechanistic=TRUE){
  
  extracted_data<-list()
  
  if (LEAFPSIMAX==T) {
    extracted_data[["LEAFPSIMAX_data"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "LeafPsiMax")
  }
  
  if (LEAFRWC==T) {
    extracted_data[["LEAFRWC_data"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "LeafRWC")
  }
  
  if (LFMC==T) {
    extracted_data[["LFMC_data"]]<-medfate::extract(x, level = "cohort", output = "Plants", vars = "LFMC") |>
      dplyr::rename(LFMC_full_mechanistic = LFMC)
    
    if (LFMC_semi_mechanistic==T) {
      extracted_data[["LFMC_data"]]$LFMC_semi_mechanistic <- 91.87 - (31.12 * log10(-(extracted_data[["LEAFPSIMAX_data"]]$LeafPsiMax)))
      
    }
  }
  
  for (i in 1:length(extracted_data)) {
    if (i==1){
      df<-extracted_data[[i]]
    }else {
      df<-merge(df,extracted_data[[i]], by = intersect(c("date","cohort","species"), c("date","cohort","species")))
    }
  }
  
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
        dplyr::select(date, LFMC_observed, specie)
      
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
      dplyr::mutate(is_summer = (lubridate::month(date) >= 6 & lubridate::month(date) <= 9),
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
      dplyr::relocate("site", "source","lai_source", "meteo_source", "soil_mod", "taw", .before = "LeafPsiMax")
    
    output[[i]] <- MERGED_LFMC_ALL
    
  }
  return(output)
}


for(meteo in c("ERA5", "INTER")) {
  for(lai in c("ALLOM", "MODIS")) {
    sf <- readRDS(paste0("data/sf_inputs/sf_", meteo, "_", lai, "_MOD.rds"))
    # for(taw in c(40,60,80,100,120,140,160)) {
    for(taw in c(100)) {
      cat(paste0(meteo, "-", lai, "-",taw,"\n"))
      res <- readRDS(paste0("data/results/spwb_", meteo, "_", lai, "_MOD_",taw,".rds"))
      ct <- comparison_tables(sf, res, taw, DF_TYPE = c("outlier_top"))
      saveRDS(ct, paste0("data/comparison_tables/ct_", meteo, "_", lai, "_MOD_",taw,".rds"))
    }
  }
}

