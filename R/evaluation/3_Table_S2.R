library(medfateland)


# Creates supplementary table with LFMC statistics ------------------------
ct <- readRDS(paste0("data/comparison_tables/ct_INTER_ALLOM_MOD_30.rds"))
ct_bind <- dplyr::bind_rows(ct)

TS_full <- ct_bind |>
  dplyr::group_by(species, site) |>
  dplyr::summarize(maxFMC = ceiling(max(LFMC_full3)), .groups = "drop")

TS2 <- ct_bind |>
  dplyr::filter(!is.na(LFMC_observed))|>
  dplyr::filter(is_summer)|>
  dplyr::group_by(species, site) |>
  dplyr::summarize(Date_ini = as.character(min(date)), 
                   Date_fin = as.character(max(date)), 
                   n = dplyr::n(),
                   LFMC_min = min(LFMC_observed), 
                   LFMC_median = median(LFMC_observed), 
                   LFMC_max = max(LFMC_observed), .groups = "drop")

TS3 <- ct_bind |>
  dplyr::filter(!is.na(LFMC_observed))|>
  dplyr::filter(is_summer)|>
  dplyr::filter(!is_outrange)|>
  dplyr::group_by(species, site) |>
  dplyr::summarize(n_inrange = dplyr::n(), .groups = "drop")
  
TS <- TS2 |>
  dplyr::left_join(TS_full, by=c( "species", "site")) |>
  dplyr::left_join(TS3, by=c( "species", "site"))

write.csv2(TS, "data/report_tables/TableS2.csv", row.names = FALSE)
