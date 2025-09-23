library(medfateland)
library(medfate)
source("R/evaluation/evalstats.R")

evaluation_table <-function(ct_i, remove_outlier = TRUE) {
  species_measured <- unique(ct_i$species)
  ct_filtered <- ct_i |>
    dplyr::filter(lubridate::month(date) >= 6 & lubridate::month(date) <= 9)
  df_full <- data.frame(site = rep(ct_filtered$site[1], length(species_measured)),
                        species = species_measured, 
                        lai_source  = rep(ct_filtered$lai_source[1], length(species_measured)),
                        meteo_source   = rep(ct_filtered$meteo_source[1], length(species_measured)),
                        soil_mod   = rep(ct_filtered$soil_mod[1], length(species_measured)),
                        taw     = rep(ct_filtered$taw[1], length(species_measured)),
                        lfmc = "full", 
                        n = NA, Bias = NA, MAE = NA, RMSE = NA, r2 = NA)
  df_semi <- df_full
  df_semi$lfmc <- "semi"
  for(j in 1:length(species_measured)) {
    ct_sp <- ct_filtered |>
      dplyr::filter(species == species_measured[j])
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_full_mechanistic, ct_sp$is_outlier, remove_outlier)
    df_full$n[j] <- s$n
    df_full$Bias[j] <- s$Bias
    df_full$MAE[j] <- s$MAE
    df_full$RMSE[j] <- s$RMSE
    df_full$r2[j] <- s$r2
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_semi_mechanistic, ct_sp$is_outlier, remove_outlier)
    df_semi$n[j] <- s$n
    df_semi$Bias[j] <- s$Bias
    df_semi$MAE[j] <- s$MAE
    df_semi$RMSE[j] <- s$RMSE
    df_semi$r2[j] <- s$r2
  }
  df <- dplyr::bind_rows(df_full, df_semi)
  return(df)
}

et_table_all <- data.frame() 
for(meteo in c("ERA5", "INTER")) {
  for(lai in c("ALLOM", "MODIS")) {
    sf <- readRDS(paste0("data/results/sf_", meteo, "_", lai, "_MOD.rds"))
    et_table_comb <- data.frame() 
    cat(paste0(meteo, "-", lai, "\n"))
    # for(taw in c(40,60,80,100,120,140,160)) {
    for(taw in c(40, 60, 80, 100)) {
      ct <- readRDS(paste0("data/comparison_tables/ct_", meteo, "_", lai, "_MOD_",taw,".rds"))
      et <- dplyr::bind_rows(lapply(ct, evaluation_table))
      et_table_comb <- dplyr::bind_rows(et_table_comb, et)
    }
    et_table_comb$MinMAE <- FALSE
    sites_unique <- unique(et_table_comb$site)
    for(site in sites_unique) {
      species_unique <- unique(et_table_comb$species[et_table_comb$site==site])
      for(sp in species_unique) {
        sel_full <- (et_table_comb$site==site & et_table_comb$species== sp & et_table_comb$lfmc == "full")
        sel_semi <- (et_table_comb$site==site & et_table_comb$species== sp & et_table_comb$lfmc == "semi")
        mae_full <- et_table_comb$MAE[sel_full]
        et_table_comb$MinMAE[sel_full] <- (et_table_comb$MAE[sel_full] == min(mae_full, na.rm = TRUE))
        mae_semi <- et_table_comb$MAE[sel_semi]
        et_table_comb$MinMAE[sel_semi] <- (et_table_comb$MAE[sel_semi] == min(mae_semi, na.rm = TRUE))
      }
    }
    cat("FULL\n")
    et_table_comb_full <- et_table_comb |>
      dplyr::filter(lfmc=="full")
    print(table(et_table_comb_full$taw[et_table_comb_full$MinMAE]))
    cat("SEMI\n")
    et_table_comb_semi <- et_table_comb |>
      dplyr::filter(lfmc=="semi")
    print(table(et_table_comb_semi$taw[et_table_comb_semi$MinMAE]))
    et_table_all <- dplyr::bind_rows(et_table_all, et_table_comb)
  }
}

et_table_all$MinMAEsemi <- FALSE
et_table_all$MinMAEfull <- FALSE
et_table_all$MinMAEall <- FALSE
sites_unique <- unique(et_table_all$site)
for(site in sites_unique) {
  species_unique <- unique(et_table_all$species[et_table_all$site==site])
  for(sp in species_unique) {
    sel_full <- (et_table_all$site==site & et_table_all$species== sp & et_table_all$lfmc == "full")
    sel_semi <- (et_table_all$site==site & et_table_all$species== sp & et_table_all$lfmc == "semi")
    sel_all <- (et_table_all$site==site & et_table_all$species== sp)
    mae_full <- et_table_all$MAE[sel_full]
    mae_full <- mae_full[!is.na(mae_full)]
    if(length(mae_full)>0) {
      et_table_all$MinMAEfull[sel_full] <- (et_table_all$MAE[sel_full] == min(mae_full))
    }
    mae_semi <- et_table_all$MAE[sel_semi]
    mae_semi <- mae_semi[!is.na(mae_semi)]
    if(length(mae_semi)>0) {
      et_table_all$MinMAEsemi[sel_semi] <- (et_table_all$MAE[sel_semi] == min(mae_semi))
    }
    mae_all <- et_table_all$MAE[sel_all]
    mae_all <- mae_all[!is.na(mae_all)]
    if(length(mae_all)>0) {
      et_table_all$MinMAEall[sel_all] <- (et_table_all$MAE[sel_all] == min(mae_all))
    }
  }
}
et_table_all$MinMAE[is.na(et_table_all$MinMAE)] <- FALSE
et_table_all$MinMAEfull[is.na(et_table_all$MinMAEfull)] <- FALSE
et_table_all$MinMAEsemi[is.na(et_table_all$MinMAEsemi)] <- FALSE
et_table_all$MinMAEall[is.na(et_table_all$MinMAEall)] <- FALSE
saveRDS(et_table_all, "data/evaluation_table_all.rds")


cat("FULL source: \n")
print(table(et_table_all$lai_source[et_table_all$MinMAEfull], et_table_all$meteo_source[et_table_all$MinMAEfull]))
cat("SEMI source: \n")
print(table(et_table_all$lai_source[et_table_all$MinMAEsemi], et_table_all$meteo_source[et_table_all$MinMAEsemi]))
cat("FULL vs SEMI: \n")
print(table(et_table_all$lfmc[et_table_all$MinMAEall]))

et_table_best_all <- et_table_all[et_table_all$MinMAEall, ]
saveRDS(et_table_best_all, "data/evaluation_table_best.rds")
et_table_best_full <- et_table_all[et_table_all$MinMAEfull, ]
saveRDS(et_table_best_full, "data/evaluation_table_best_full.rds")
et_table_best_semi <- et_table_all[et_table_all$MinMAEsemi, ]
saveRDS(et_table_best_semi, "data/evaluation_table_best_semi.rds")

et_table_best_full |>
  dplyr::group_by(species) |>
  dplyr::summarise(n = dplyr::n(),
                   MAE = mean(MAE),
                   RMSE = mean(RMSE),
                   r2 = mean(r2)) |>
  dplyr::arrange(r2)

et_table_best_semi |>
  dplyr::group_by(species) |>
  dplyr::summarise(n = dplyr::n(),
                   MAE = mean(MAE),
                   RMSE = mean(RMSE),
                   r2 = mean(r2)) |>
  dplyr::arrange(r2)
