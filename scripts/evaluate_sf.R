library(medfateland)
library(medfate)
library(ggplot2)
# CAT_LFMC<-read.csv("data/CAT_LFMC.csv")
# CAT_LFMC$date<-as.Date(CAT_LFMC$date)

FR_LFMC<-read.csv("data/FR_LFMC.csv")
FR_LFMC$date<-as.Date(FR_LFMC$date,format = "%d/%m/%Y")


evalstats <- function(obs, pred) {
  sel_complete = !(is.na(obs) | is.na(pred))
  n<- sum(sel_complete)
  
  n_obs<- sum(!is.na(obs))
  n_pred<- sum(!is.na(pred))
  
  obs <- obs[sel_complete]
  pred <- pred[sel_complete]
  E <- pred - obs
  Bias <- mean(E)
  #Bias.rel <- 100 * Bias / abs(mean(obs))
  MAE <- mean(abs(E)) #Mean absolute error
  #MAE.rel <- 100 * MAE / abs(mean(obs))
  r <- cor(obs, pred)
  r2<- r^2
  #NSE <- 1 - (sum((obs - pred) ^ 2) / sum((obs - mean(obs)) ^ 2)) #Nash–Sutcliffe model efficiency coefficient (NSE)
  #NSE.abs <- 1 - (sum(abs(obs - pred)) / sum(abs(obs - mean(obs))))
  return(
    list(
      n_obs = n_obs,
      n_pred = n_pred,
      n = n,
      Bias = Bias,
      #Bias.rel = Bias.rel,
      MAE = MAE,
      #MAE.rel = MAE.rel,
      #r = r,
      r2 = r2#,
      #NSE = NSE,
      #NSE.abs = NSE.abs
    )
  )
}


extract_output<-function(x,
                         LEAFPSIMAX=TRUE,
                         LEAFRWC=TRUE,
                         LFMC=TRUE,
                         LFMC_rodrigo=TRUE){
  
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
    
    if (LFMC_rodrigo==T) {
      extracted_data[["LFMC_data"]]$LFMC_rodrigo <- 91.87 - (31.12 * log10(-(extracted_data[["LEAFPSIMAX_data"]]$LeafPsiMax)))
      
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

comparison_tables <- function(sf, res, DF_TYPE = c("ALL"), TH = 3,
                              years = c(2012:2022)) {
  output <- vector("list", nrow(sf))
  simulation_data <- lapply(res$result, extract_output)
  for(i in 1:nrow(sf)) {
    source <- sf$source[i]
    sp <- sf$species[i]
    site_name <- sf$site_name[i]
    if (source == "CAT") {
      FILTERED_LFMC <- CAT_LFMC |>
        dplyr::rename(date = date, site = LocalityName, specie = sp_correct_name, LFMC_observed = LFMC) |>
        dplyr::filter(specie == sp) |>
        dplyr::filter(site == site_name, lubridate::year(date) >= years[1])|>
        dplyr::select(date, LFMC_observed, specie)
    } else if (source == "FR") {
      FILTERED_LFMC <- FR_LFMC |>
        dplyr::rename(date = date, site = SiteCode, specie = sp_correct_name, LFMC_observed = RobustLFMC) |>
        dplyr::filter(specie == sp) |>
        dplyr::filter(site == site_name, lubridate::year(date) >= years[1]) |> 
        dplyr::select(date, LFMC_observed, specie)
        
    }
    MERGED_LFMC_ALL <- simulation_data[[i]] |>
      dplyr::mutate(date = as.Date(date)) |>
      dplyr::full_join(FILTERED_LFMC, by = c("date" = "date", "species" = "specie"))
    
    if ("summer" %in% DF_TYPE) {
      MERGED_LFMC_ALL <- MERGED_LFMC_ALL |>
        dplyr::filter(lubridate::month(date) >= 6 & lubridate::month(date) <= 9)
    }
    
    if ("outlier" %in% DF_TYPE) {
      med <- median(MERGED_LFMC_ALL$LFMC_observed, na.rm = TRUE) #median 
      mad <- mad(MERGED_LFMC_ALL$LFMC_observed, na.rm = TRUE) #Median absolute deviation
      threshold <- TH * mad
      
      MERGED_LFMC_ALL_OUTLIER <- MERGED_LFMC_ALL |>
        dplyr::mutate(is_outlier = (LFMC_observed > (med + threshold)) | (LFMC_observed < (med - threshold)))
      
      MERGED_LFMC_ALL <- MERGED_LFMC_ALL |>
        dplyr::mutate(is_outlier = (LFMC_observed > (med + threshold)) | (LFMC_observed < (med - threshold))) |>
        dplyr::mutate(LFMC_observed = ifelse(is_outlier, NA, LFMC_observed)) 
    }
    
    if ("outlier_top" %in% DF_TYPE) {
      med <- median(MERGED_LFMC_ALL$LFMC_observed, na.rm = TRUE) #median 
      mad <- mad(MERGED_LFMC_ALL$LFMC_observed, na.rm = TRUE) #Median absolute deviation
      threshold <- TH * mad
      
      MERGED_LFMC_ALL_OUTLIER <- MERGED_LFMC_ALL |>
        dplyr::mutate(is_outlier = (LFMC_observed > (med + threshold)))
      
      MERGED_LFMC_ALL <- MERGED_LFMC_ALL |> 
        dplyr::mutate(is_outlier = (LFMC_observed > (med + threshold))) |>
        dplyr::mutate(LFMC_observed = ifelse(is_outlier, NA, LFMC_observed))
    }
    output[[i]] <- MERGED_LFMC_ALL
    
  }
  return(output)
}

time_plot_summer <- function(ct, detect_outliers = TRUE, remove_outlier = TRUE) {

  ct <- ct |> 
    dplyr::mutate(is.summer = lubridate::month(date) >= 6 & lubridate::month(date) <= 9) |>
    dplyr::mutate(summer_group = cumsum(lag(is.summer, default = FALSE) != is.summer)) |>
    dplyr::mutate(summer_group = ifelse(is.summer, summer_group, NA))
  
  time_p <- ggplot(data = ct, aes(x = date)) +
    geom_line(aes(y = LFMC_full_mechanistic, colour = "Fully-mechanistic LFMC")) +
    geom_line(aes(y = LFMC_rodrigo, colour = "Semi-mechanistic LFMC"))
  
  if (detect_outliers) {
    if (!remove_outlier) {
      time_p <- time_p + 
        geom_point(data = ct |> dplyr::filter(is_outlier & is.summer), aes(y = LFMC_observed, colour = "outlier"))
    }
    time_p <- time_p +
      geom_point(data = ct |> dplyr::filter(!is_outlier & is.summer), aes(y = LFMC_observed, colour = "Measured LFMC")) +
      geom_line(data = ct |> dplyr::filter(!is_outlier & is.summer) |> na.omit(), aes(y = LFMC_observed, group = summer_group, colour = "Measured LFMC"))
  } else {
    time_p <- time_p +
      geom_point(data = ct |> dplyr::filter(is.summer), aes(y = LFMC_observed, colour = "Measured LFMC")) +
      geom_line(data = ct |> dplyr::filter(is.summer) |> na.omit(), aes(y = LFMC_observed, group = summer_group, colour = "Measured LFMC"))
  }
  
  time_p <- time_p +
    scale_color_manual(values = c("Fully-mechanistic LFMC" = "black", "Semi-mechanistic LFMC" = "blue", "Measured LFMC" = "red", "outlier" = "orange")) +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    xlab("DATE") +
    ylab("LFMC") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_y_continuous(limits = c(0, 220), expand = c(0,0))
  
  return(time_p)
}
scatter_plot <- function(ct, detect_outliers = TRUE, remove_outlier = TRUE, LFMC_TYPE = "MODELED") {

  x_variable <- if (LFMC_TYPE == "MODELED") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_full_mechanistic, y = LFMC_observed))
  } else if (LFMC_TYPE == "RODRIGO") {
    base_plot <- ggplot(data = ct, aes(x = LFMC_rodrigo, y = LFMC_observed))
  }
  
   base_plot <- base_plot +
    theme_classic() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    scale_x_continuous(limits = c(0, 220), expand = c(0,0)) +
    scale_y_continuous(limits = c(0, 220), expand = c(0,0))
  
  if (detect_outliers) {
    if (remove_outlier) {
      scatter_p <- base_plot +
        geom_point(data = ct |> dplyr::filter(!is_outlier), colour = "black") #alpha = .3, stroke = 1, size = 2
    } else {
      scatter_p <- base_plot +
        geom_point(aes(colour = is_outlier)) +
        scale_color_manual(values = c("FALSE" = "black", "TRUE" = "orange"))
    }
  } else {
    scatter_p <- base_plot +
      geom_point()
  }
  
  scatter_p <- scatter_p +
    stat_smooth(method = "lm", se = FALSE)
  
  
  return(scatter_p)
}


sf <- readRDS("results/sf_FR_MODIS_ERA5_SOIL_MOD.rds")
res <- readRDS("results/res_sureau_FR_MODIS_ERA5_SOIL_MOD.rds")
res[["comparison_tables"]] <- comparison_tables(sf, res, DF_TYPE = c("summer", "outlier"))
res_opt <- readRDS("results/res_opt_sureau_FR_MODIS_ERA5_SOIL_MOD.rds")
res_opt[["comparison_tables"]] <- comparison_tables(sf, res_opt, DF_TYPE = c("summer", "outlier"))

res_comparison_all <- dplyr::bind_rows(res$comparison_tables)
res_opt_comparison_all <- dplyr::bind_rows(res_opt$comparison_tables)

i = 47
time_plot_summer(res$comparison_tables[[i]])
time_plot_summer(res_opt$comparison_tables[[i]])
scatter_plot(res$comparison_tables[[i]], LFMC_TYPE = "MODELED")
scatter_plot(res$comparison_tables[[i]], LFMC_TYPE = "RODRIGO")
scatter_plot(res_opt$comparison_tables[[i]], LFMC_TYPE = "MODELED")
scatter_plot(res_opt$comparison_tables[[i]], LFMC_TYPE = "RODRIGO")


scatter_plot(res_comparison_all |> dplyr::filter(species=="Arbutus unedo"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Arbutus unedo"), LFMC_TYPE = "MODELED")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Buxus sempervirens"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Buxus sempervirens"), LFMC_TYPE = "MODELED")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Cistus albidus"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Cistus albidus"), LFMC_TYPE = "MODELED")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Cistus monspeliensis"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Cistus monspeliensis"), LFMC_TYPE = "MODELED")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Cytisophyllum sessilifolium"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Cytisophyllum sessilifolium"), LFMC_TYPE = "MODELED")


scatter_plot(res_comparison_all |> dplyr::filter(species=="Erica arborea"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Erica arborea"), LFMC_TYPE = "MODELED")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Genista cinerea"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Genista cinerea"), LFMC_TYPE = "MODELED")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Juniperus oxycedrus subsp. oxycedrus"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Juniperus oxycedrus subsp. oxycedrus"), LFMC_TYPE = "MODELED")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Juniperus oxycedrus subsp. oxycedrus"), LFMC_TYPE = "RODRIGO")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Juniperus oxycedrus subsp. oxycedrus"), LFMC_TYPE = "RODRIGO")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Quercus coccifera"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Quercus coccifera"), LFMC_TYPE = "MODELED")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Quercus ilex"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Quercus ilex"), LFMC_TYPE = "MODELED")

scatter_plot(res_comparison_all |> dplyr::filter(species=="Rosmarinus officinalis"), LFMC_TYPE = "MODELED")
scatter_plot(res_opt_comparison_all|> dplyr::filter(species=="Rosmarinus officinalis"), LFMC_TYPE = "MODELED")
scatter_plot(res_comparison_all |> dplyr::filter(species=="Rosmarinus officinalis"), LFMC_TYPE = "RODRIGO")

scatter_plot(res_comparison_all, LFMC_TYPE = "RODRIGO")
scatter_plot(res_opt_comparison_all, LFMC_TYPE = "RODRIGO")

evalstats(res_comparison_all$LFMC_observed, res_comparison_all$LFMC_full_mechanistic)
evalstats(res_opt_comparison_all$LFMC_observed, res_opt_comparison_all$LFMC_full_mechanistic)

evalstats(res_comparison_all$LFMC_observed, res_comparison_all$LFMC_rodrigo)
evalstats(res_opt_comparison_all$LFMC_observed, res_opt_comparison_all$LFMC_rodrigo)
