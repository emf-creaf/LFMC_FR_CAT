library(medfateland)
library(medfate)
library(ggplot2)


time_plot_summer <- function(ct, sp_name, site_name, scenario, 
                             trim_observed = TRUE, exclude_outliers = FALSE, exclude_outrange = TRUE) {

  ct <- ct |> 
    dplyr::filter(species == sp_name) |>
    dplyr::mutate(excluded = FALSE)
  
  if(exclude_outliers) {
    ct <- ct |>
      dplyr::mutate(excluded = excluded | is_outlier)
  }
  if(exclude_outrange) {
    ct <- ct |>
      dplyr::mutate(excluded = excluded | is_outrange)
  }
  

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

  time_p <- time_p +
    geom_point(data = ct |> dplyr::filter(!excluded & is_summer), aes(y = LFMC_observed, colour = "Measured")) 
  time_p <- time_p +
    geom_point(data = ct |> dplyr::filter(excluded & is_summer), aes(y = LFMC_observed, colour = "Excluded"),
               size = 0.7) 
  
  time_p <- time_p +
    scale_color_manual(values = c("Full1" = "darkgreen",
                                  "Full2" = "gray",
                                  "Full3" = "black", 
                                  "Semi" = "blue", 
                                  "Measured" = "red", "Excluded" = "orange"),
                       breaks = c("Measured", "Excluded", "Semi", "Full1", "Full2", "Full3")) +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    xlab(NULL) +
    ylab("LFMC (%)") +
    labs(title = paste0(sp_name, " - ", site_name), 
         subtitle = scenario) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(limits = c(20, 170), expand = c(0,0))
  
  return(time_p)
}
scatter_plot <- function(ct, sp_name, focus_summer = TRUE, remove_outlier = FALSE, remove_outrange = TRUE, LFMC_TYPE = "FULL3") {

  ct <- ct |> 
    dplyr::filter(species == sp_name)
  
  if(focus_summer) ct <- ct |>
      dplyr::filter(is_summer)
  
  ct$excluded <- FALSE
  
  if(remove_outlier) ct <- ct |> 
      dplyr::mutate(excluded = excluded | is_outlier)
  
  if(remove_outrange) ct <- ct |> 
    dplyr::mutate(excluded = excluded | is_outrange)
  
  ct_excl <- ct |>
    dplyr::filter(excluded)

  ct <- ct |>
    dplyr::filter(!excluded)
  
  if (LFMC_TYPE == "FULL1") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_full1, y = LFMC_observed))
    stats <- evalstats(ct$LFMC_observed, ct$LFMC_full1, ct$is_outlier, ct$is_outrange, remove_outlier, remove_outrange)
    col <- "darkgreen"
    x_label = "LFMC full1 (%)"
  } else if (LFMC_TYPE == "FULL2") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_full2, y = LFMC_observed))
    stats <- evalstats(ct$LFMC_observed, ct$LFMC_full2, ct$is_outlier, ct$is_outrange, remove_outlier, remove_outrange)
    col <- "gray"
    x_label = "LFMC full2 (%)"
  } else if (LFMC_TYPE == "FULL3") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_full3, y = LFMC_observed))
    stats <- evalstats(ct$LFMC_observed, ct$LFMC_full3, ct$is_outlier, ct$is_outrange, remove_outlier, remove_outrange)
    x_label = "LFMC full3 (%)"
    col <- "black"
  } else if (LFMC_TYPE == "SEMI") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_semi, y = LFMC_observed))
    stats <- evalstats(ct$LFMC_observed, ct$LFMC_semi, ct$is_outlier, ct$is_outrange, remove_outlier, remove_outrange)
    x_label = "LFMC semi (%)"
    col <- "blue"
  }
  
   base_plot <- base_plot +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_x_continuous(limits = c(20, 170), expand = c(0,0)) +
    scale_y_continuous(limits = c(20, 170), expand = c(0,0))

   
  scatter_p <- base_plot +
     geom_point(colour = col, alpha = 0.8)
   
  scatter_p <- scatter_p +
    stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")
  
  scatter_p <- scatter_p +
    annotate(geom = "text", x = 105, y = 38, size= 3, 
             label = paste0("n = ", stats$n,
                            " Bias = ", round(stats$Bias,1),
                            " MAE = ", round(stats$MAE,1), "\n",
                            " slope = ", round(stats$b, 2),
                            " R2 = ", round(100*stats$r2,1),"%",
                            " NSE = ", round(stats$NSE,2)))

  
  if (LFMC_TYPE == "FULL1") {
    scatter_p <- scatter_p +
      geom_point(data = ct_excl, aes(x = LFMC_full1, y = LFMC_observed), colour = "orange", alpha = 0.8, size = 0.7)
  } else if (LFMC_TYPE == "FULL2") {
    scatter_p <- scatter_p +
      geom_point(data = ct_excl, aes(x = LFMC_full2, y = LFMC_observed), colour = "orange", alpha = 0.8, size = 0.7)
  } else if (LFMC_TYPE == "FULL3") {
    scatter_p <- scatter_p +
      geom_point(data = ct_excl, aes(x = LFMC_full3, y = LFMC_observed), colour = "orange", alpha = 0.8, size = 0.7)
  } else if (LFMC_TYPE == "SEMI") {
    scatter_p <- scatter_p +
      geom_point(data = ct_excl, aes(x = LFMC_semi, y = LFMC_observed), colour = "orange", alpha = 0.8, size = 0.7)
  }
  
  scatter_p <- scatter_p +
    xlab(x_label)+ ylab("LFMC observed (%)")
  return(scatter_p)
}

scatter_plot_panel<-function(ct, sp_name, focus_summer = TRUE, remove_outlier = FALSE, remove_outrange = TRUE,
                             title = "") {
  if(title=="") title = sp_name
  p1 <- scatter_plot(ct, sp_name, focus_summer, remove_outlier, remove_outrange, LFMC_TYPE = "FULL1") +
    labs(subtitle = "Fully mechanistic (1)", title = title)
  p2 <- scatter_plot(ct, sp_name, focus_summer, remove_outlier, remove_outrange, LFMC_TYPE = "FULL2") +
    labs(subtitle = "Fully mechanistic (2)", title = title)
  p3 <- scatter_plot(ct, sp_name, focus_summer, remove_outlier, remove_outrange, LFMC_TYPE = "FULL3") +
    labs(subtitle = "Fully mechanistic (3)", title = title)
  ps <- scatter_plot(ct, sp_name, focus_summer, remove_outlier, remove_outrange, LFMC_TYPE = "SEMI") +
    labs(subtitle = "Semi-mechanistic", title = "")
  return(cowplot::plot_grid(ps, p1, p2, p3, ncol = 4))
}

combined_evaluation_plot<-function(ct, sp_name, site_name, scenario, ...) {
  p1 <-time_plot_summer(ct, sp_name, site_name, scenario,  ...)
  p2 <- scatter_plot_panel(ct, sp_name, title=" ", ...)
  return(cowplot::plot_grid(p1, p2, nrow=2))
}
