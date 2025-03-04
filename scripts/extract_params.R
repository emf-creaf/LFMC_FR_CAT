library(medfateland)
sf <- readRDS("results/sf_FR_MODIS_ERA5_SOIL_MOD.rds")
res <- readRDS("results/res_sureau_FR_MODIS_ERA5_SOIL_MOD.rds")
res_opt <- readRDS("results/res_opt_sureau_FR_MODIS_ERA5_SOIL_MOD.rds")

extract_spparams<-function(x, param_selection = c( "Name","Z50", "Z95", "SLA",
                                                   "Al2As", "r635",  
                                                   "Vmax298","Jmax298", 
                                                   "maxFMC","LeafPI0", "LeafEPS", "LeafAF", "StemAF",
                                                   "Gs_P50", "Gswmin", 
                                                   "VCstem_P50", "VCstem_slope","VCleaf_P50", "VCleaf_slope",
                                                   "Kmax_stemxylem")) {
  df <-cbind(as.data.frame(x$cohorts),
               as.data.frame(x$below),
               as.data.frame(x$paramsAnatomy),
               as.data.frame(x$paramsTranspiration),
               as.data.frame(x$paramsWaterStorage))
  df <- df |>
    dplyr::select(any_of(param_selection))
  row.names(df) <- NULL
  return(df)
}

spparams_all <-dplyr::bind_rows(lapply(res$state, extract_spparams)) |>
  dplyr::distinct()

extract_sew<-function(x) {
  sum(soil_waterExtractable(x$soil, x$control$soilFunctions), na.rm = TRUE)
}

df <- sf::st_drop_geometry(sf[,c("id", "site_name", "species")])
df$SEW_MOD <- unlist(lapply(res$state, extract_sew))
df$SEW_OPT <- unlist(lapply(res_opt$state, extract_sew))
