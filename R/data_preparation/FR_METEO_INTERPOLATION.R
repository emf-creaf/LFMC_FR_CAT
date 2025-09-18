
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv") |>
  dplyr::rename(id = ID)
sf_fr <- CAT_FR_SITES |>
  dplyr::filter(source=="FR") |> 
  sf::st_as_sf(coords = c("LON", "LAT"), crs = 4326)

dates <- seq(as.Date("1980-01-01"), as.Date("2022-12-31"), by="day")

interpolator <- meteoland::read_interpolator("data/MeteoRH_1950_2022.nc")

sf_fr_int <- meteoland::interpolate_data(sf_fr, interpolator, dates = dates)
site_names <- sf_fr_int$site_name

for(i in 1:length(site_names)) {
  df <- sf_fr_int$interpolated_data[[i]]
  write.csv(x = df, file=paste0("data/PLOTS/", site_names[i], "/meteo_interpolator.csv"))
}
