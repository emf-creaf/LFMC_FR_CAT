library(medfateland)
library(medfate)
library(ggplot2)
source("R/evaluation/evalstats.R")


time_plot_summer <- function(ct, sp_name, site_name, scenario, 
                             detect_outliers = TRUE, 
                             remove_outlier = FALSE,
                             trim_observed = TRUE) {

  ct <- ct |> 
    dplyr::filter(species == sp_name) |>
    dplyr::mutate(is.summer = lubridate::month(date) >= 6 & lubridate::month(date) <= 9) |>
    dplyr::mutate(summer_group = cumsum(lag(is.summer, default = FALSE) != is.summer)) |>
    dplyr::mutate(summer_group = ifelse(is.summer, summer_group, NA))

  if(trim_observed) {
    min_year <- min(lubridate::year(ct$date[!is.na(ct$LFMC_observed)]))
    max_year <- max(lubridate::year(ct$date[!is.na(ct$LFMC_observed)]))
    ct <- ct |> 
      dplyr::filter(lubridate::year(date) >= min_year & lubridate::year(date) <= max_year)
  }
  time_p <- ggplot(data = ct, aes(x = date)) +
    geom_line(aes(y = LFMC_full1, colour = "Full1")) +
    geom_line(aes(y = LFMC_full2, colour = "Full2")) +
    geom_line(aes(y = LFMC_full3, colour = "Full3")) +
    geom_line(aes(y = LFMC_semi, colour = "Semi"))
  
  if (detect_outliers) {
    if (!remove_outlier) {
      time_p <- time_p + 
        geom_point(data = ct |> dplyr::filter(is_outlier & is.summer), aes(y = LFMC_observed, colour = "outlier"))
    }
    time_p <- time_p +
      geom_point(data = ct |> dplyr::filter(!is_outlier & is.summer), aes(y = LFMC_observed, colour = "Measured")) 
    # +
    #   geom_line(data = ct |> dplyr::filter(!is_outlier & is.summer) |> na.omit(), aes(y = LFMC_observed, group = summer_group, colour = "Measured LFMC"))
  } else {
    time_p <- time_p +
      geom_point(data = ct |> dplyr::filter(is.summer), aes(y = LFMC_observed, colour = "Measured")) 
    # +
    #   geom_line(data = ct |> dplyr::filter(is.summer) |> na.omit(), aes(y = LFMC_observed, group = summer_group, colour = "Measured LFMC"))
  }
  
  time_p <- time_p +
    scale_color_manual(values = c("Full1" = "brown",
                                  "Full2" = "gray",
                                  "Full3" = "black", 
                                  "Semi" = "blue", 
                                  "Measured" = "red", "outlier" = "orange")) +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    xlab(NULL) +
    ylab("LFMC (%)") +
    labs(title = paste0(sp_name, " - ", site_name), 
         subtitle = scenario) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(limits = c(0, 220), expand = c(0,0))
  
  return(time_p)
}
scatter_plot <- function(ct, sp_name, focus_summer = TRUE, detect_outliers = FALSE, remove_outlier = TRUE, LFMC_TYPE = "FULL3") {

  ct <- ct |> 
    dplyr::filter(species == sp_name)

  
  if(focus_summer) ct <- ct |>
      dplyr::filter(lubridate::month(date) >= 6 & lubridate::month(date) <= 9)
  
  if(remove_outlier) ct <- ct |> 
      dplyr::filter(!is_outlier)
  
  if (LFMC_TYPE == "FULL1") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_full1, y = LFMC_observed))
    stats <- evalstats(ct$LFMC_observed, ct$LFMC_full1, ct$is_outlier, remove_outlier)
    x_label = "LFMC full1 (%)"
  } else if (LFMC_TYPE == "FULL2") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_full2, y = LFMC_observed))
    stats <- evalstats(ct$LFMC_observed, ct$LFMC_full2, ct$is_outlier, remove_outlier)
    x_label = "LFMC full2 (%)"
  } else if (LFMC_TYPE == "FULL3") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_full3, y = LFMC_observed))
    stats <- evalstats(ct$LFMC_observed, ct$LFMC_full3, ct$is_outlier, remove_outlier)
    x_label = "LFMC full3 (%)"
  } else if (LFMC_TYPE == "SEMI") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_semi, y = LFMC_observed))
    stats <- evalstats(ct$LFMC_observed, ct$LFMC_semi, ct$is_outlier, remove_outlier)
    x_label = "LFMC semi (%)"
  }
  
   base_plot <- base_plot +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_x_continuous(limits = c(0, 220), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 220), expand = c(0,0))
  
  if (detect_outliers) {
    scatter_p <- base_plot +
      geom_point(aes(colour = is_outlier)) +
      scale_color_manual(values = c("FALSE" = "black", "TRUE" = "orange"))
  } else {
    scatter_p <- base_plot +
      geom_point(colour = "black", alpha = 0.8)
  }
  
  scatter_p <- scatter_p +
    stat_smooth(method = "lm", se = FALSE, formula = "y ~ x")
  
  scatter_p <- scatter_p +
    annotate(geom = "text", x = 130, y = 20, size= 3, 
             label = paste0("Bias = ", round(stats$Bias,1),
                            " MAE = ", round(stats$MAE,1), "\n",
                            " R2 = ", round(100*stats$r2,1),"%",
                            " NSE = ", round(stats$NSE,2)))

  scatter_p <- scatter_p +
    xlab(x_label)+ ylab("LFMC observed (%)")
  return(scatter_p)
}

scatter_plot_panel<-function(ct, sp_name, focus_summer = TRUE, detect_outliers = FALSE, remove_outlier = TRUE, 
                             include_RWC = FALSE, title = "") {
  if(title=="") title = sp_name
  p1 <- scatter_plot(ct, sp_name, focus_summer, detect_outliers, remove_outlier, LFMC_TYPE = "FULL1") +
    labs(subtitle = "Fully mechanistic (1)", title = title)
  p2 <- scatter_plot(ct, sp_name, focus_summer, detect_outliers, remove_outlier, LFMC_TYPE = "FULL2") +
    labs(subtitle = "Fully mechanistic (2)", title = title)
  p3 <- scatter_plot(ct, sp_name, focus_summer, detect_outliers, remove_outlier, LFMC_TYPE = "FULL3") +
    labs(subtitle = "Fully mechanistic (3)", title = title)
  ps <- scatter_plot(ct, sp_name, focus_summer, detect_outliers, remove_outlier, LFMC_TYPE = "SEMI") +
    labs(subtitle = "Semi-mechanistic", title = "")
  return(cowplot::plot_grid(p1, p2, p3, ps, ncol = 4))
}

combined_plot<-function(ct, sp_name, site_name, scenario, ...) {
  p1 <-time_plot_summer(ct, sp_name, site_name, scenario,  ...)
  p2 <- scatter_plot_panel(ct, sp_name, title=" ", ...)
  return(cowplot::plot_grid(p1, p2, nrow=2))
}

for(taw in c(140)) {
  for(meteo in c("INTER", "ERA5")) {
    for(lai in c("ALLOM", "MODIS")) {
      cat(paste0("METEO: " , meteo, " / LAI: ", lai,  " / TAW: ", taw, "\n\n"))
      ct <- readRDS(paste0("data/comparison_tables/ct_", meteo, "_", lai, "_MOD_", taw, ".rds"))
      scenario <- paste0(meteo,"/", lai, "/", taw, "mm")
      for(i in 1:length(ct)) {
        species <- unique(ct[[i]]$species)
        site <- ct[[i]]$site[1]
        for(j in 1:length(species)) {
          species_dir <- paste0("plots/",species[j])
          species_dir <- stringr::str_replace_all(species_dir, " ", "_")
          if(!dir.exists(species_dir)) dir.create(species_dir)
          species_site_dir <- paste0("plots/",species[j],"/", site)
          species_site_dir <- stringr::str_replace_all(species_site_dir, " ", "_")
          if(!dir.exists(species_site_dir)) dir.create(species_site_dir)
          cat(paste0(site, " / ", species[j], "\n"))
          p <-combined_plot(ct[[i]], species[j], site, scenario)
          file <- paste0(species_site_dir, "/", species[j], "_", site, "_", meteo, "_", lai,"_",taw,"mm.png")
          file <- stringr::str_replace_all(file, " ", "_")
          ggsave(file, p, width = 14, height = 8, units = "in")
        }
      }
    }
  }
}

# 
# 
# res_nomod[["comparison_tables"]] <- comparison_tables(sf, res_nomod, DF_TYPE = c("summer", "outlier"))
# res_mod <- readRDS("results/res_sureau_FR_MODIS_ERA5_SOIL_MOD.rds")
# res_mod[["comparison_tables"]] <- comparison_tables(sf, res_mod, DF_TYPE = c("summer", "outlier"))
# res_opt <- readRDS("results/res_opt_sureau_FR_MODIS_ERA5_SOIL_MOD.rds")
# res_opt[["comparison_tables"]] <- comparison_tables(sf, res_opt, DF_TYPE = c("summer", "outlier"))
# 
# res_nomod_comparison_all <- dplyr::bind_rows(res_nomod$comparison_tables)
# res_mod_comparison_all <- dplyr::bind_rows(res_mod$comparison_tables)
# res_opt_comparison_all <- dplyr::bind_rows(res_opt$comparison_tables)
# 
# 
# 
# spp <- c("Acacia dealbata", # (1) Simulation fails
#         "Arbutus unedo", # (2) OK
#         "Buxus sempervirens", # (1) OK
#         "Cistus albidus", # (5) OK
#         "Cistus monspeliensis", # (9) OK
#         "Cytisophyllum sessilifolium", # (1) OK
#         "Cytisus oromediterraneus", # (1) OK
#         "Erica arborea", # (7) OK
#         "Erica cinerea", # (1) OK
#         "Erica scoparia subsp. scoparia", # (2) OK
#         "Genista cinerea", # (3) OK
#         "Genista scorpius", # (1) OK
#         "Juniperus oxycedrus subsp. oxycedrus", # (2) OK
#         "Pinus halepensis", # (0)
#         "Quercus coccifera", # (4) OK
#         "Quercus ilex", # (4) OK
#         "Rosmarinus officinalis")
# 
# for(sp in spp) {
#   print(sp)
#   p<- scatter_plot_three_panels(res_nomod_comparison_all |> dplyr::filter(species==sp),
#                                 res_mod_comparison_all |> dplyr::filter(species==sp),
#                                 res_opt_comparison_all |> dplyr::filter(species==sp))
#   ggsave(paste0("plots/scatter_plots_", sp, ".png"), p, width = 9, height=10)
# }
# 
# p<- scatter_plot_three_panels(res_nomod_comparison_all,
#                               res_mod_comparison_all,
#                               res_opt_comparison_all)
# ggsave(paste0("plots/scatter_plots_all.png"), p, width = 9, height=10)
