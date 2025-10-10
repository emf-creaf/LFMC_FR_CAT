evaluation_table <-function(ct_i, remove_outlier = FALSE, remove_outrange = TRUE) {
  species_measured <- unique(ct_i$species)
  ct_filtered <- ct_i |>
    dplyr::filter(is_summer)
  df_full1 <- data.frame(site = rep(ct_filtered$site[1], length(species_measured)),
                         species = species_measured, 
                         lai_source  = rep(ct_filtered$lai_source[1], length(species_measured)),
                         meteo_source   = rep(ct_filtered$meteo_source[1], length(species_measured)),
                         soil_mod   = rep(ct_filtered$soil_mod[1], length(species_measured)),
                         taw     = rep(ct_filtered$taw[1], length(species_measured)),
                         lfmc = "full1", 
                         n = NA, Bias = NA, Bias.rel = NA, MAE = NA, MAE.rel = NA, 
                         b = NA, r2 = NA, NSE = NA)
  df_full2 <- df_full1
  df_full2$lfmc <- "full2"
  df_full3 <- df_full1
  df_full3$lfmc <- "full3"
  df_semi <- df_full1
  df_semi$lfmc <- "semi"
  for(j in 1:length(species_measured)) {
    ct_sp <- ct_filtered |>
      dplyr::filter(species == species_measured[j])
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_full1, ct_sp$is_outlier, ct_sp$is_outrange, remove_outlier, remove_outrange)
    df_full1$n[j] <- s$n
    df_full1$Bias[j] <- s$Bias
    df_full1$Bias.rel[j] <- s$Bias.rel
    df_full1$MAE[j] <- s$MAE
    df_full1$MAE.rel[j] <- s$MAE.rel
    df_full1$b[j] <- s$b
    df_full1$r2[j] <- s$r2
    df_full1$NSE[j] <- s$NSE
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_full2, ct_sp$is_outlier, ct_sp$is_outrange, remove_outlier, remove_outrange)
    df_full2$n[j] <- s$n
    df_full2$Bias[j] <- s$Bias
    df_full2$Bias.rel[j] <- s$Bias.rel
    df_full2$MAE[j] <- s$MAE
    df_full2$MAE.rel[j] <- s$MAE.rel
    df_full2$b[j] <- s$b
    df_full2$r2[j] <- s$r2
    df_full2$NSE[j] <- s$NSE
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_full3, ct_sp$is_outlier, ct_sp$is_outrange, remove_outlier, remove_outrange)
    df_full3$n[j] <- s$n
    df_full3$Bias[j] <- s$Bias
    df_full3$Bias.rel[j] <- s$Bias.rel
    df_full3$MAE[j] <- s$MAE
    df_full3$MAE.rel[j] <- s$MAE.rel
    df_full3$b[j] <- s$b
    df_full3$r2[j] <- s$r2
    df_full3$NSE[j] <- s$NSE
    s <- evalstats(ct_sp$LFMC_observed, ct_sp$LFMC_semi, ct_sp$is_outlier, ct_sp$is_outrange, remove_outlier, remove_outrange)
    df_semi$n[j] <- s$n
    df_semi$Bias[j] <- s$Bias
    df_semi$Bias.rel[j] <- s$Bias.rel
    df_semi$MAE.rel[j] <- s$MAE.rel
    df_semi$MAE[j] <- s$MAE
    df_semi$b[j] <- s$b
    df_semi$r2[j] <- s$r2
    df_semi$NSE[j] <- s$NSE
  }
  df <- dplyr::bind_rows(df_full1, df_full2, df_full3, df_semi)
  return(df)
}
