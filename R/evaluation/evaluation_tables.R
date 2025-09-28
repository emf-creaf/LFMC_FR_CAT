library(medfateland)
library(medfate)
source("R/evaluation/evalstats.R")

evaluation_table <-function(ct_i, remove_outlier = TRUE) {
  species_measured <- unique(ct_i$species)
  ct_filtered <- ct_i |>
    dplyr::filter(lubridate::month(date) >= 6 & lubridate::month(date) <= 9)
  df_full1 <- data.frame(site = rep(ct_filtered$site[1], length(species_measured)),
                        species = species_measured, 
                        lai_source  = rep(ct_filtered$lai_source[1], length(species_measured)),
                        meteo_source   = rep(ct_filtered$meteo_source[1], length(species_measured)),
                        soil_mod   = rep(ct_filtered$soil_mod[1], length(species_measured)),
                        taw     = rep(ct_filtered$taw[1], length(species_measured)),
                        lfmc = "full1", 
                        n = NA, Bias = NA, Bias.rel = NA, MAE = NA, MAE.rel = NA, RMSE = NA, r2 = NA, NSE = NA)
  df_full2 <- df_full1
  df_full2$lfmc <- "full2"
  df_full3 <- df_full1
  df_full3$lfmc <- "full3"
  df_semi <- df_full1
  df_semi$lfmc <- "semi"
  for(j in 1:length(species_measured)) {
    ct_sp <- ct_filtered |>
      dplyr::filter(species == species_measured[j])
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_full1, ct_sp$is_outlier, remove_outlier)
    df_full1$n[j] <- s$n
    df_full1$Bias[j] <- s$Bias
    df_full1$Bias.rel[j] <- s$Bias.rel
    df_full1$MAE[j] <- s$MAE
    df_full1$MAE.rel[j] <- s$MAE.rel
    df_full1$RMSE[j] <- s$RMSE
    df_full1$r2[j] <- s$r2
    df_full1$NSE[j] <- s$NSE
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_full2, ct_sp$is_outlier, remove_outlier)
    df_full2$n[j] <- s$n
    df_full2$Bias[j] <- s$Bias
    df_full2$Bias.rel[j] <- s$Bias.rel
    df_full2$MAE[j] <- s$MAE
    df_full2$MAE.rel[j] <- s$MAE.rel
    df_full2$RMSE[j] <- s$RMSE
    df_full2$r2[j] <- s$r2
    df_full2$NSE[j] <- s$NSE
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_full3, ct_sp$is_outlier, remove_outlier)
    df_full3$n[j] <- s$n
    df_full3$Bias[j] <- s$Bias
    df_full3$Bias.rel[j] <- s$Bias.rel
    df_full3$MAE[j] <- s$MAE
    df_full3$MAE.rel[j] <- s$MAE.rel
    df_full3$RMSE[j] <- s$RMSE
    df_full3$r2[j] <- s$r2
    df_full3$NSE[j] <- s$NSE
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_semi, ct_sp$is_outlier, remove_outlier)
    df_semi$n[j] <- s$n
    df_semi$Bias[j] <- s$Bias
    df_semi$Bias.rel[j] <- s$Bias.rel
    df_semi$MAE.rel[j] <- s$MAE.rel
    df_semi$MAE[j] <- s$MAE
    df_semi$RMSE[j] <- s$RMSE
    df_semi$r2[j] <- s$r2
    df_semi$NSE[j] <- s$NSE
  }
  df <- dplyr::bind_rows(df_full1, df_full2, df_full3, df_semi)
  return(df)
}

et_table_all <- data.frame() 
for(meteo in c("ERA5", "INTER")) {
  for(lai in c("ALLOM", "MODIS")) {
    sf <- readRDS(paste0("data/sf_inputs/sf_", meteo, "_", lai, "_MOD.rds"))
    et_table_comb <- data.frame() 
    cat(paste0(meteo, "-", lai, "\n"))
    for(taw in c(30, 40, 50, 60, 70, 80, 90, 100, 120)) {
      file_ct <- paste0("data/comparison_tables/ct_", meteo, "_", lai, "_MOD_",taw,".rds")
      if(file.exists(file_ct)) {
        ct <- readRDS(file_ct)
        et <- dplyr::bind_rows(lapply(ct, evaluation_table))
        et_table_comb <- dplyr::bind_rows(et_table_comb, et)
      }
    }
    et_table_all <- dplyr::bind_rows(et_table_all, et_table_comb)
  }
}

et_table_all <-et_table_all |>
  dplyr::group_by(site, species,meteo_source, lai_source, lfmc) |>
  dplyr::mutate(minMAETAW = (MAE == min(MAE)))
saveRDS(et_table_all, "data/evaluation_table_all.rds")


et_table_best_full1 <- et_table_all |>
  dplyr::filter(lfmc == "full1") |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) 
saveRDS(et_table_best_full1, "data/evaluation_table_best_full1.rds")

et_table_best_full2 <- et_table_all |>
  dplyr::filter(lfmc == "full2") |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) 
saveRDS(et_table_best_full2, "data/evaluation_table_best_full2.rds")

et_table_best_full3 <- et_table_all |>
  dplyr::filter(lfmc == "full3") |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) 
saveRDS(et_table_best_full3, "data/evaluation_table_best_full3.rds")

et_table_best_semi <- et_table_all |>
  dplyr::filter(lfmc == "semi") |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) 
saveRDS(et_table_best_semi, "data/evaluation_table_best_semi.rds")


et_table_best_all <- et_table_all |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) 
saveRDS(et_table_best_all, "data/evaluation_table_best_all.rds")

table(et_table_best_all$lfmc)
table(et_table_best_all$lai_source)
table(et_table_best_all$meteo_source)

# Comparison of lfmc approach by species
# Pooling all sources
species_means <- et_table_all |>
  dplyr::filter(minMAETAW) |>
  dplyr::group_by(species, lfmc) |>
  dplyr::summarise(n = dplyr::n(),
                   taw = mean(taw, na.rm = TRUE),
                   Bias = mean(Bias, na.rm = TRUE),
                   Bias.rel = mean(Bias.rel, na.rm=TRUE),
                   MAE = mean(MAE, na.rm=TRUE),
                   MAE.rel = mean(MAE.rel, na.rm = TRUE),
                   r2 = mean(r2, na.rm = TRUE), .groups = "drop") 

# Comparing best sources only
species_means_optTAW <- et_table_best_semi |>
  dplyr::bind_rows(et_table_best_full1)|>
  dplyr::bind_rows(et_table_best_full2)|>
  dplyr::bind_rows(et_table_best_full3)|>
  dplyr::group_by(species, lfmc) |>
  dplyr::summarise(n = dplyr::n(),
                   taw = mean(taw, na.rm = TRUE),
                   Bias = mean(Bias, na.rm = TRUE),
                   Bias.rel = mean(Bias.rel, na.rm=TRUE),
                   MAE = mean(MAE, na.rm=TRUE),
                   MAE.rel = mean(MAE.rel, na.rm = TRUE),
                   r2 = mean(r2, na.rm = TRUE), 
                   NSE = mean(NSE, na.rm = TRUE),.groups = "drop") 

## Which TAW should we use by default?
taw_means <- et_table_all |>
  dplyr::group_by(meteo_source, lai_source, lfmc, taw) |>
  dplyr::summarise(Bias = mean(Bias, na.rm = TRUE),
                   Bias.rel = mean(Bias.rel, na.rm=TRUE),
                   MAE = mean(MAE, na.rm=TRUE),
                   MAE.rel = mean(MAE.rel, na.rm = TRUE),
                   r2 = mean(r2, na.rm = TRUE), .groups = "drop") 
taw_means |>
  dplyr::group_by(meteo_source, lai_source, lfmc) |>
  dplyr::mutate(minMAErel = min(MAE.rel)) |>
  dplyr::filter(MAE.rel ==minMAErel)

et_table_all |>
  dplyr::group_by(taw, lfmc) |>
  dplyr::summarise(Bias = mean(Bias, na.rm = TRUE),
                   Bias.rel = mean(Bias.rel, na.rm=TRUE),
                   MAE = mean(MAE, na.rm=TRUE),
                   MAE.rel = mean(MAE.rel, na.rm = TRUE),
                   r2 = mean(r2, na.rm = TRUE),
                   NSE = mean(NSE, na.rm = TRUE), .groups = "drop") |>
  dplyr::arrange(lfmc)



## If TAW was known, which combination should we use?
comb_means <- et_table_all |>
  dplyr::filter(minMAETAW) |>
  dplyr::group_by(meteo_source, lai_source, lfmc) |>
  dplyr::summarise(Bias = mean(Bias, na.rm = TRUE),
                   Bias.rel = mean(Bias.rel, na.rm=TRUE),
                   MAE = mean(MAE, na.rm=TRUE),
                   MAE.rel = mean(MAE.rel, na.rm = TRUE),
                   r2 = mean(r2, na.rm = TRUE), .groups = "drop") 





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
print(table(et_table_all$taw[et_table_all$MinMAEfull]))
print(table(et_table_all$lai_source[et_table_all$MinMAEfull], et_table_all$meteo_source[et_table_all$MinMAEfull]))
cat("SEMI source: \n")
print(table(et_table_all$taw[et_table_all$MinMAEsemi]))
print(table(et_table_all$lai_source[et_table_all$MinMAEsemi], et_table_all$meteo_source[et_table_all$MinMAEsemi]))
cat("FULL vs SEMI: \n")
print(table(et_table_all$lfmc[et_table_all$MinMAEall]))

et_table_best <- et_table_all[et_table_all$MinMAE, ]
saveRDS(et_table_best, "data/evaluation_table_best.rds")
et_table_best_all <- et_table_all[et_table_all$MinMAEall, ]
saveRDS(et_table_best_all, "data/evaluation_table_best_all.rds")
et_table_best_full <- et_table_all[et_table_all$MinMAEfull, ]
saveRDS(et_table_best_full, "data/evaluation_table_best_full.rds")
et_table_best_semi <- et_table_all[et_table_all$MinMAEsemi, ]
saveRDS(et_table_best_semi, "data/evaluation_table_best_semi.rds")

sum_full <- et_table_best_full |>
  dplyr::group_by(species) |>
  dplyr::summarise(n = dplyr::n(),
                   Bias = mean(Bias),
                   MAE = mean(MAE),
                   RMSE = mean(RMSE),
                   r2 = mean(r2)) 

sum_semi <- et_table_best_semi |>
  dplyr::group_by(species) |>
  dplyr::summarise(n = dplyr::n(),
                   Bias = mean(Bias),
                   MAE = mean(MAE),
                   RMSE = mean(RMSE),
                   r2 = mean(r2)) 

# Range of results
evaluation_table_all <- readRDS("data/evaluation_table_all.rds")
evaluation_table_best <- readRDS("data/evaluation_table_best.rds")
summary(evaluation_table_all$r2)
summary(evaluation_table_all$MAE)
summary(evaluation_table_all$Bias)

# Which is the best combination of lai/meteo source, soil taw and lfmc estimation method
# Highest r2 
# ERA5/MODIS with 120 mm and semi
# ERA5/MODIS with 120 mm and full
# Lowest bias
# ERA5/MODIS with 80 mm and full
# ERA5/ALLOM with 60 mm and full
et_table_all |>
  dplyr::group_by(meteo_source, lai_source, taw, lfmc) |>
  dplyr::summarise(n = dplyr::n(),
                   Bias = mean(Bias, na.rm = TRUE),
                   MAE = mean(MAE, na.rm = TRUE),
                   RMSE = mean(RMSE, na.rm = TRUE),
                   r2 = mean(r2, na.rm = TRUE)) |>
  dplyr::arrange(MAE)

# What is the best combination of lai/meteo source and lfmc assuming optimum soil estimation?
# Highest r2 
# ERA5/ALLOM/full
# ERA5/MODIS/full
# Lowest bias
# INTER/MODIS/semi
# INTER/MODIS/full
et_table_all |>
  dplyr::filter(minMAETAW) |>
  dplyr::group_by(meteo_source, lai_source, lfmc) |>
  dplyr::summarise(n = dplyr::n(),
                   Bias = mean(Bias, na.rm = TRUE),
                   MAE = mean(MAE, na.rm = TRUE),
                   RMSE = mean(RMSE, na.rm = TRUE),
                   r2 = mean(r2, na.rm = TRUE)) |>
  dplyr::arrange(-abs(r2))
  

# What is the best combination for full LFMC? (assuming optimum soil)
# ERA5/ALLOM
et_table_best_full |>
  dplyr::group_by(meteo_source, lai_source) |>
  dplyr::summarise(n = dplyr::n(),
                   Bias = mean(Bias, na.rm = TRUE),
                   MAE = mean(MAE, na.rm = TRUE),
                   RMSE = mean(RMSE, na.rm = TRUE),
                   r2 = mean(r2, na.rm = TRUE)) 

# What is the best combination for semi LFMC? (assuming optimum soil)
# ERA5/MODIS
et_table_best_semi |>
  dplyr::group_by(meteo_source, lai_source) |>
  dplyr::summarise(n = dplyr::n(),
                   Bias = mean(Bias, na.rm = TRUE),
                   MAE = mean(MAE, na.rm = TRUE),
                   RMSE = mean(RMSE, na.rm = TRUE),
                   r2 = mean(r2, na.rm = TRUE)) 


