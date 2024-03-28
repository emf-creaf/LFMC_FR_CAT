library(DBI)
library(RSQLite)
library(tidyverse)
library(medfate)
library(medfateutils)

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


data("SpParamsES")
not_correct<-which((CAT_LFMC$SpeciesName %in% SpParamsES$Name)==F)
not_correct_name<-unique(CAT_LFMC[not_correct,3])
sp_correct_name<-c("Rosmarinus officinalis")

CAT_LFMC$IS_CORRECT<-CAT_LFMC$SpeciesName %in% SpParamsES$Name

index<-NA
CAT_LFMC$sp_correct_name<-NA

for (i in 1:nrow(CAT_LFMC)) {
  if (CAT_LFMC$IS_CORRECT[i]==TRUE){
    CAT_LFMC$sp_correct_name[i]<-CAT_LFMC$SpeciesName[i]
  }else {
    index <- match(CAT_LFMC$SpeciesName[i], not_correct_name)
    if (!is.na(index)) {
      CAT_LFMC$sp_correct_name[i] <- sp_correct_name[index]
    }
  }
}

#REORGANIZE DATA AND EXPORT DATA

CAT_LFMC<-CAT_LFMC %>% 
  relocate(sp_correct_name, .after = "SpeciesName") %>% 
  select(-IS_CORRECT)

write.csv(CAT_LFMC,"data/CAT_LFMC.csv", row.names = F)

rm(list=setdiff(ls(), c("CAT_LFMC","CAT_FR_SITES")))

##FR LFMC DATA####

FR_LFMC<-read.csv("raw_data/FR_DATA/LFMC/LFMC_final_Table.csv")
FR_SITES<-CAT_FR_SITES$site_name[10:nrow(CAT_FR_SITES)]

#FILTER BY OUR PLOTS
FR_LFMC<-filter(FR_LFMC, SiteCode %in% FR_SITES)


#CORRECT NAMES
data("SpParamsFR")

not_correct<-which((FR_LFMC$Species %in% SpParamsFR$Name)==F)
not_correct_name<-unique(FR_LFMC[not_correct,3])
sp_correct_name<-c("Cytisophyllum sessilifolium", "Juniperus oxycedrus subsp. oxycedrus",
                   "Cistus monspeliensis", "Calicotome", "Hippocrepis emerus", 
                   "Erica scoparia subsp. scoparia", "Cytisus oromediterraneus")


FR_LFMC$IS_CORRECT<-FR_LFMC$Species %in% SpParamsFR$Name

index<-NA
FR_LFMC$sp_correct_name<-NA

for (i in 1:nrow(FR_LFMC)) {
  if (FR_LFMC$IS_CORRECT[i]==TRUE){
    FR_LFMC$sp_correct_name[i]<-FR_LFMC$Species[i]
  }else {
    index <- match(FR_LFMC$Species[i], not_correct_name)
    if (!is.na(index)) {
      FR_LFMC$sp_correct_name[i] <- sp_correct_name[index]
    }
  }
}

#REORGANIZE DATA AND EXPORT DATA

FR_LFMC<-FR_LFMC %>% 
  relocate(sp_correct_name, .after = "Species") %>% 
  select(-IS_CORRECT)

write.csv(FR_LFMC,"data/FR_LFMC.csv", row.names = F)

#UNIQUE SP IN SITES:

FR_list_sp_site <- split(FR_LFMC$Species, FR_LFMC$SiteCode)
FR_list_sp_site <- lapply(FR_list_sp_site, unique)





