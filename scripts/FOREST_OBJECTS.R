library(medfate)
library(medfateutils)
library(meteoland)



data("SpParamsES")
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")

#CAT FOREST OBJECTS####################################################################################

CAT_VEG_DATA<-read.csv("data/CAT_VEG_DATA.csv")
CAT_SITES<-CAT_FR_SITES[1:9,]

#ALL SP

mapping_cat<-c("Species.name" = "sp_correct_name", "Height" = "Height", "Cover" = "Cover")

cat_names<-unique(CAT_VEG_DATA$Site)

cat_forests <- list()

for (i in cat_names) {
  cat_forests[[paste0("CAT_forest_", i)]] <- emptyforest()
  cat_forests[[paste0("CAT_forest_", i)]]$shrubData <- forest_mapShrubTable(CAT_VEG_DATA[CAT_VEG_DATA$Site==i,], mapping_y = mapping_cat, SpParams = SpParamsES)
}

#MEASURED SP:

#UNIQUE SP IN SITES:

CAT_LFMC<-read.csv("data/CAT_LFMC.csv")

CAT_list_sp_site <- split(CAT_LFMC$SpeciesName, CAT_LFMC$LocalityName) %>%
  lapply(unique) %>%
  enframe(name = "Site", value = "Species") %>%
  unnest(cols = c(Species))

CAT_MEASURED_VEG_DATA<-left_join(CAT_list_sp_site,CAT_VEG_DATA, by = c("Site" = "Site","Species" = "Species")) %>% 
  filter(!is.na(Cover))

cat_measured_names<-CAT_MEASURED_VEG_DATA$Site

cat_measured_forests <- list()

for (i in 1:length(cat_measured_names)) {
  cat_measured_forests[[paste0("CAT_measured_forest_", cat_measured_names[i],"_", i)]] <- emptyforest()
  cat_measured_forests[[paste0("CAT_measured_forest_", cat_measured_names[i],"_", i)]]$shrubData <- forest_mapShrubTable(CAT_MEASURED_VEG_DATA[i,], mapping_y = mapping_cat, SpParams = SpParamsES)
}

#THE TWO FOREST LIST ARE:

rm(list=setdiff(ls(), c("cat_forests","cat_measured_forests")))

summary(cat_forests[[1]], SpParamsES)

summary(cat_measured_forests[[1]], SpParamsES)




#FR FOREST OBJECTS####################################################################################

