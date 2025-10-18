source("R/evaluation/evalstats.R")
source("R/evaluation/evalplots.R")
source("R/evaluation/evaltable.R")


# Build evaluation table of all combinations -----------------------------

et_all <- data.frame() 
for(meteo in c("ERA5", "INTER")) {
  for(lai in c("ALLOM", "MODIS")) {
    sf <- readRDS(paste0("data/sf_inputs/sf_", meteo, "_", lai, "_MOD.rds"))
    et_comb <- data.frame() 
    cat(paste0(meteo, "-", lai, "\n"))
    for(taw in seq(30,200, by = 10)) {
      file_ct <- paste0("data/comparison_tables/ct_", meteo, "_", lai, "_MOD_",taw,".rds")
      if(file.exists(file_ct)) {
        ct <- readRDS(file_ct)
        et <- dplyr::bind_rows(lapply(ct, evaluation_table, remove_outlier = FALSE, remove_outrange = TRUE))
        et_comb <- dplyr::bind_rows(et_comb, et)
      }
    }
    et_all <- dplyr::bind_rows(et_all, et_comb)
  }
}
# Add indicator of minimum MAE or maximum NSE
et_all <-et_all |>
  dplyr::group_by(site, species,meteo_source, lai_source, lfmc) |>
  dplyr::mutate(minMAETAW = (MAE == min(MAE)),
                maxNSETAW = (NSE == max(NSE)))
saveRDS(et_all, "data/evaluation_tables/evaluation_table_all.rds")



# Select best combinations under different subgroups ------------------------------------------------
et_best <- et_all |>
  dplyr::group_by(species, site, lfmc) |>
  dplyr::filter(MAE == min(MAE))
saveRDS(et_best, "data/evaluation_tables/evaluation_table_best.rds")

# Best TAW simulation for all combinations of lfmc, meteo source and lai source
et_best_comb <- et_all |>
  dplyr::group_by(species, site, lfmc, meteo_source, lai_source) |>
  dplyr::filter(MAE == min(MAE)) |>
  dplyr::arrange(species, site, meteo_source, lai_source, lfmc)
saveRDS(et_best_comb, "data/evaluation_tables/evaluation_table_best_per_comb.rds")

et_best_full1 <- et_all |>
  dplyr::filter(lfmc == "full1") |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) |>
  dplyr::arrange(species)
saveRDS(et_best_full1, "data/evaluation_tables/evaluation_table_best_full1.rds")

et_best_full2 <- et_all |>
  dplyr::filter(lfmc == "full2") |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) |>
  dplyr::arrange(species)
saveRDS(et_best_full2, "data/evaluation_tables/evaluation_table_best_full2.rds")

et_best_full3 <- et_all |>
  dplyr::filter(lfmc == "full3") |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) |>
  dplyr::arrange(species)
saveRDS(et_best_full3, "data/evaluation_tables/evaluation_table_best_full3.rds")

et_best_semi <- et_all |>
  dplyr::filter(lfmc == "semi") |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) |>
  dplyr::arrange(species)
saveRDS(et_best_semi, "data/evaluation_tables/evaluation_table_best_semi.rds")

et_best_full_all <- et_all |>
  dplyr::filter(lfmc %in% c("full1", "full2", "full3")) |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) |>
  dplyr::arrange(species)
saveRDS(et_best_full_all, "data/evaluation_tables/evaluation_table_best_full_all.rds")

et_best_all <- et_all |>
  dplyr::group_by(species, site) |>
  dplyr::filter(MAE == min(MAE)) |>
  dplyr::arrange(species)
saveRDS(et_best_all, "data/evaluation_tables/evaluation_table_best_all.rds")

print(table(et_best_comb$taw, et_best_comb$lfmc))

print(table(et_best_all$taw))
print(table(et_best_all$lfmc))
print(table(et_best_full_all$lfmc))
print(table(et_best_all$lfmc, et_best_all$taw))
print(table(et_best_all$lai_source))
print(table(et_best_all$lai_source, et_best_all$taw))
print(table(et_best_all$meteo_source))
print(table(et_best_all$meteo_source, et_best_all$taw))

print(table(et_best_semi$taw))
print(table(et_best_full1$taw))
print(table(et_best_full2$taw))
print(table(et_best_full3$taw))


# Select comparison tables of the best combinations -----------------------
sf <- readRDS(paste0("data/sf_inputs/sf_ERA5_MODIS_MOD.rds"))
site_names <- sf$site_name
ct_best_comb <- vector("list", nrow(et_best_comb))
for(meteo in c("ERA5", "INTER")) {
  for(lai in c("ALLOM", "MODIS")) {
    for(taw in seq(30,200, by=10)) {
      cat(".")
      sel <- (et_best_comb$lai_source==lai) & (et_best_comb$meteo_source==meteo) & (et_best_comb$taw == taw)
      if(sum(sel)>0)  {
        ct <- readRDS(paste0("data/comparison_tables/ct_", meteo, "_", lai, "_MOD_", taw, ".rds"))
        for(i in which(sel)) {
          site <- et_best_comb$site[i]
          lfmc_i <- et_best_comb$lfmc[i]
          isite <- which(site_names==site)
          ct_i <- ct[[isite]]
          ct_i <- ct_i |> 
            dplyr::select(c(date, species, site, lai_source, meteo_source, taw, 
                            LFMC_full1, LFMC_full2, LFMC_full3, LFMC_semi, LFMC_observed)) |>
            dplyr::mutate(lfmc = lfmc_i)
          ct_best_comb[[i]] <- ct_i
        }
      }
    }
  }
}
cat("\n")
ct_best_comb <- dplyr::bind_rows(ct_best_comb)
saveRDS(ct_best_comb, paste0("data/comparison_tables/ct_best_comb.rds"))

# Draw evaluation plots for the best combinations -----------------------------------
sf <- readRDS(paste0("data/sf_inputs/sf_ERA5_MODIS_MOD.rds"))
site_names <- sf$site_name

for(i in 1:nrow(et_best_all)) {
  meteo <- et_best_all$meteo_source[i]
  lai <- et_best_all$lai_source[i]
  site <- et_best_all$site[i]
  isite <- which(site_names==site)
  species <- et_best_all$species[i]
  taw <- et_best_all$taw[i]
  lfmc <- et_best_all$lfmc[i]
  ct <- readRDS(paste0("data/comparison_tables/ct_", meteo, "_", lai, "_MOD_", taw, ".rds"))
  
  scenario <- paste0(meteo,"/", lai, "/", taw, "mm")
  cat(paste0(site, " / ", species, "  [", scenario, " : ", lfmc, "] \n"))
  best_dir <- paste0("plots/evaluation_plots/0_BEST")
  if(!dir.exists(best_dir)) dir.create(best_dir)
  p <-combined_evaluation_plot(ct[[isite]], species, site, scenario)
  file <- paste0(best_dir, "/0_BEST_", species, "_", site, ".png")
  file <- stringr::str_replace_all(file, " ", "_")
  ggsave(file, p, width = 14, height = 8, units = "in")
}

# Draw evaluation plots for all combinations -----------------------------------
# for(taw in seq(30,160, by = 10)) {
#   for(meteo in c("INTER", "ERA5")) {
#     for(lai in c("ALLOM", "MODIS")) {
#       cat(paste0("METEO: " , meteo, " / LAI: ", lai,  " / TAW: ", taw, "\n\n"))
#       ct <- readRDS(paste0("data/comparison_tables/ct_", meteo, "_", lai, "_MOD_", taw, ".rds"))
#       scenario <- paste0(meteo,"/", lai, "/", taw, "mm")
#       for(i in 1:length(ct)) {
#         species <- unique(ct[[i]]$species)
#         site <- ct[[i]]$site[1]
#         for(j in 1:length(species)) {
#           species_dir <- paste0("plots/",species[j])
#           species_dir <- stringr::str_replace_all(species_dir, " ", "_")
#           if(!dir.exists(species_dir)) dir.create(species_dir)
#           species_site_dir <- paste0("plots/evaluation_plots/",species[j],"/", site)
#           species_site_dir <- stringr::str_replace_all(species_site_dir, " ", "_")
#           if(!dir.exists(species_site_dir)) dir.create(species_site_dir)
#           cat(paste0(site, " / ", species[j], "\n"))
#           p <-combined_evaluation_plot(ct[[i]], species[j], site, scenario)
#           file <- paste0(species_site_dir, "/", species[j], "_", site, "_", meteo, "_", lai,"_",taw,"mm.png")
#           file <- stringr::str_replace_all(file, " ", "_")
#           ggsave(file, p, width = 14, height = 8, units = "in")
#         }
#       }
#     }
#   }
# }



