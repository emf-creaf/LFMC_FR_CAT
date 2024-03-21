library(DBI)
library(RSQLite)
library(tidyverse)


##CAT_FR_SITES####

CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")

##CAT LFMC DATA####

#CONNECT DATABASE
CAT_DATA_LFMC <- dbConnect(SQLite(), "raw_data/CAT_DATA/lfmc.sqlite")

#EXTRACT DATABASE TABLES
TABLES<-dbListTables(CAT_DATA_LFMC)

for(i in 1:length(TABLES)){
  assign(TABLES[i], dbReadTable(CAT_DATA_LFMC, TABLES[i]))
}

#REMOVE EMPTY TABLES
dbDisconnect(CAT_DATA_LFMC)
#CAT_LFMC<-lfmc


rm("phenology","soil_measurements","tdr_sensor","CAT_DATA_LFMC")

#rm("phenology","soil_measurements","tdr_sensor","sites","sites_species","species","CAT_DATA_LFMC","lfmc","i","TABLES")

sites2<-sites[1:9,]

merge_sites_species<-sites_species %>% 
  left_join(sites, by=c("SamplingSiteCode")) %>%
  left_join(species, by=c("SpeciesCode")) %>%
  select(SiteSpCode,LocalityName,SpeciesName)

CAT_LFMC<-lfmc %>% 
  left_join(merge_sites_species, by=c("SiteSpCode")) %>% 
  select(c(Date,LFMC,LocalityName,SpeciesName)) %>% 
  mutate(Date=as.Date(Date)) %>% 
  relocate(LFMC,.after = last_col()) %>% 
  arrange(LocalityName) %>% 
  filter(!is.na(LFMC)) %>% 
  mutate(LocalityName = if_else(LocalityName == "Torà", "Tora", LocalityName))

write.csv(CAT_LFMC,"data/CAT_LFMC.csv", row.names = F)

rm(list=setdiff(ls(), c("CAT_LFMC","CAT_FR_SITES")))

##FR LFMC DATA####


FR_LFMC<-read.csv("raw_data/FR_DATA/LFMC/LFMC_final_Table.csv")

FR_SITES<-CAT_FR_SITES$site_name[10:nrow(CAT_FR_SITES)]

FR_LFMC<-filter(FR_LFMC, SiteCode %in% FR_SITES)

write.csv(FR_LFMC,"data/FR_LFMC.csv", row.names = F)

#UNIQUE SP IN SITES:

FR_list_sp_site <- split(FR_LFMC$Species, FR_LFMC$SiteCode)
FR_list_sp_site <- lapply(FR_list_sp_site, unique)





