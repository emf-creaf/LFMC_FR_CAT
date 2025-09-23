library(medfateland)

ct <- readRDS(paste0("data/comparison_tables/ct_INTER_ALLOM_MOD_40.rds"))
ct_bind <- dplyr::bind_rows(ct)

TS2 <- ct_bind |>
  dplyr::filter(!is.na(LFMC_observed))|>
  dplyr::filter(is_summer)|>
  dplyr::filter(!is_outlier)|>
  dplyr::group_by(species, site) |>
  dplyr::summarize(Date_ini = as.character(min(date)), 
                   Date_fin = as.character(max(date)), 
                   LFMC_min = min(LFMC_observed), 
                   LFMC_median = median(LFMC_observed), 
                   LFMC_max = max(LFMC_observed), .groups = "drop")

write.csv2(TS2, "data/results/TableS2.csv", row.names = FALSE)
