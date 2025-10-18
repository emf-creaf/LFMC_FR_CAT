library(sf)
library(tidyverse)
library(rnaturalearth) #natural earth map data
library(rnaturalearthhires) #natural earth map data
library(ggspatial) # annotation_scale()
library(ggrepel) #geom_text_repel()
library(patchwork)

CAT_FR_SITES <- read.csv("data/inputs/CAT_FR_SITES.csv")


sites <- st_as_sf(CAT_FR_SITES, coords = c("LON", "LAT"), crs = 4326, remove = F)


world <- ne_countries(scale = "large", returnclass = "sf")

spain <- ne_countries(scale = "large", returnclass = "sf", country = "Spain")
france <- ne_countries(scale = "large", returnclass = "sf", country = "france")

spain_regions<- ne_states(country = "Spain", returnclass = "sf") %>% 
  group_by(region) %>% 
  summarise()
france_regions<- ne_states(country = "France", returnclass = "sf") %>% 
  group_by(region) %>% 
  summarise()

#SITES MAP
site_p <- ggplot() +
  geom_sf(data = world %>% filter(!sovereignt %in% c("Spain","France")), fill = "grey90", color = "white", linewidth = .5) + #all world data
  geom_sf(data = spain_regions %>% group_by(region), fill = "grey80", color = "white", linewidth = .2) + #spain_regions
  geom_sf(data = france_regions, fill = "grey80", color = "white", linewidth = .2) + #france_regions
  geom_sf(data = spain, fill = "NA", color = "grey30", linewidth = .5) + #spain contour
  geom_sf(data = france, fill = "NA", color = "grey30", linewidth = .5) + #france contour
  geom_sf(data = sites, aes(fill = source), size = 5, shape = 21, color = "grey30", alpha = .8, stroke = .5) +
  scale_fill_manual(values = c("#96ceb4", "#ffcc5c")) +
  labs(title = element_blank(),#"STUDY AREA",
       fill = "LFMC sampling networks",
       x = element_blank(),
       y = element_blank()) +
  #geom_text_repel(data = sites, aes(x = LON, y = LAT, label = site_name ), nudge_y = 0.06, nudge_x = 0.06, size = 2)+
  #annotation_scale(location = "br", )+
  coord_sf(xlim = c(-2, 10), ylim = c(38, 46), expand = TRUE)+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = "none",
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
        text = element_text(size = 19),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15)
        ) 


#AREA MAP
area_p <- ggplot() +
  geom_sf(data = world, fill = "grey90", color = "grey60", linewidth = .2) +
  geom_rect(aes(xmin = -2, xmax = 10, ymin = 38, ymax = 45.5), color = "red", fill = NA, alpha = .8, linewidth = 1) +
  coord_sf(xlim = c(-13, 40), ylim = c(32, 74), expand = F)+
  theme_test() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "pt"),
        axis.title=element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", linewidth = 1.4)
        )


#INSET MAP
plot_with_inset <- function(main, inset, zoom){
  main_is_sf <- inherits(ggplot_build(site_p)$layout$coord, "CoordSf")
  left <- ifelse(main_is_sf , -zoom/3.115, 0)
  bottom <- ifelse(main_is_sf, 1 - zoom, 1 - zoom/2)
  main + inset_element(inset, left, bottom, zoom, 1)
}

p <- plot_with_inset(site_p,area_p, zoom = .4)

#SAVE MAPS

map_path <- "plots/figures/"

ggsave(filename = "site_plot.png",plot = site_p,path = map_path ,width = 190,height = 190,units = "mm",dpi = 900)
ggsave(filename = "area_plot.png",plot = area_p,path = map_path ,width = 190,height = 190,units = "mm",dpi = 900)
ggsave(filename = "final_plot.png",plot = p,path = map_path ,width = 190,height = 190,units = "mm",dpi = 900)

ggsave(filename = "site_plot.pdf",plot = site_p,path = map_path ,width = 190,height = 190,units = "mm",dpi = 900)
ggsave(filename = "area_plot.pdf",plot = area_p,path = map_path ,width = 190,height = 190,units = "mm",dpi = 900)
ggsave(filename = "final_plot.pdf",plot = p,path = map_path ,width = 190,height = 190,units = "mm",dpi = 900)

