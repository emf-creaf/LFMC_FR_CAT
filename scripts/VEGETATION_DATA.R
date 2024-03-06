library(readxl)
library(tidyverse)

##CAT VEGETATION DATA####
CAT_VEG_DATA <- read_excel("raw_data/CAT_DATA/SPIF_plotvegdata.xlsx", sheet = "vegetation_data")
str(CAT_VEG_DATA)

write.csv(CAT_VEG_DATA, "data/CAT_VEG_DATA.csv", row.names=FALSE)

#FRENCH VEGETATION DATA####
FR_VEG_DATA_RAW<- read.csv("raw_data/FR_DATA/RH_Sites_coverage.csv", sep = ";")
str(FR_VEG_DATA_RAW)

#TRANSFORM np/na to NA values
FR_VEG_DATA_RAW[FR_VEG_DATA_RAW == "np"] <- NA
FR_VEG_DATA_RAW[FR_VEG_DATA_RAW == "na"] <- NA

#TANSFORM DATE TO CORRECT FORMAT
D11S2_DATE<-FR_VEG_DATA_RAW[16,1] #PLOT D11S2 TWO DATES, SAVE THE DATES FOR LATER
FR_VEG_DATA_RAW$Date<- FR_VEG_DATA_RAW$Date %>% as.numeric() %>% as.Date(origin = "1899-12-30")
names(FR_VEG_DATA_RAW)
summary(FR_VEG_DATA_RAW)


####TREES#############################################
ARBORE_COLUMNS<-FR_VEG_DATA_RAW %>%
  select(contains("arbore"))
names(ARBORE_COLUMNS)

##TREES SP
TREES_SP_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x", "Placette", "esp1_arbore","esp2_arbore","esp3_arbore" ))

tree_new_df_sp<-TREES_SP_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "sp_category",
               values_to= "sp") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

##TREES COVERAGE
TREES_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x", "Placette", "taux_esp1_arbore","taux_esp2_arbore","taux_esp3_arbore" ))

tree_new_df_cov<-TREES_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "cov_category",
               values_to= "coverage") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")


##TREES MEAN HEIGHT
TREES_HEIGHT_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x", "Placette", "ht_toit_esp1_arbore", "ht_toit_esp2_arbore", "ht_toit_esp3_arbore" ))

tree_new_df_height<-TREES_HEIGHT_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "height_category",
               values_to= "height") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

#MERGE THE 3 TREE DATAFRAMES

MERGE_TREES_FR_VEG_DATA <- tree_new_df_sp %>%
  inner_join(tree_new_df_cov[,c(1,4,5)], by = "ID") %>%
  inner_join(tree_new_df_height[,c(1,4,5)], by = "ID") %>% 
  select(-ID)

write.csv(MERGE_TREES_FR_VEG_DATA, "data/TREES_FR_VEG_DATA.csv", row.names=FALSE)

####SHRUBS################################
ARBUSIF_COLUMNS<-FR_VEG_DATA_RAW %>%
  select(contains("arbustif"))
names(ARBUSIF_COLUMNS)

##SHRUBS SP
SHRUBS_SP_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x", "Placette", "esp1_arbustif","esp2_arbustif","esp3_arbustif","esp_hyd_arbustif"))

shrub_new_df_sp<-SHRUBS_SP_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "sp_category",
               values_to= "sp") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

##SHRUBS COVERAGE
SHRUBS_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x", "Placette", "taux_esp1_arbustif","taux_esp2_arbustif","taux_esp3_arbustif","taux_esp_hyd_arbustif" ))

SHRUBS_COV_FR_VEG_DATA$taux_esp_hyd_arbustif <- as.integer(SHRUBS_COV_FR_VEG_DATA$taux_esp_hyd_arbustif)

shrub_new_df_cov<-SHRUBS_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "cov_category",
               values_to= "coverage") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

##SHRUBS COVERAGE 0_50

SHRUBS_0_50_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x","Placette","taux_0_50_esp1_arbustif","taux_0_50_esp2_arbustif","taux_0_50_esp3_arbustif","taux_0_50_esp_hyd_arbustif"))
str(SHRUBS_0_50_COV_FR_VEG_DATA)
SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp2_arbustif <- as.integer(SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp2_arbustif)
SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp3_arbustif <- as.integer(SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp3_arbustif)
SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp_hyd_arbustif <- as.integer(SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp_hyd_arbustif)


shrub_new_df_0_50_cov<-SHRUBS_0_50_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "cov_0_50_category",
               values_to= "coverage_0_50") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

##SHRUBS COVERAGE 50_100

SHRUBS_50_100_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x","Placette","taux_50_100_esp1_arbustif","taux_50_100_esp2_arbustif","taux_50_100_esp3_arbustif","taux_50_100_esp_hyd_arbustif"))
str(SHRUBS_50_100_COV_FR_VEG_DATA)
SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp2_arbustif <- as.integer(SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp2_arbustif)
SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp3_arbustif <- as.integer(SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp3_arbustif)
SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp_hyd_arbustif <- as.integer(SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp_hyd_arbustif)


shrub_new_df_50_100_cov<-SHRUBS_50_100_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "cov_50_100_category",
               values_to= "coverage_50_100") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

##SHRUBS COVERAGE 100_200

SHRUBS_100_200_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x","Placette","taux_100_200_esp1_arbustif","taux_100_200_esp2_arbustif","taux_100_200_esp3_arbustif","taux_100_200_esp_hyd_arbustif"))
str(SHRUBS_100_200_COV_FR_VEG_DATA)
SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp2_arbustif <- as.integer(SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp2_arbustif)
SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp3_arbustif <- as.integer(SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp3_arbustif)
SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp_hyd_arbustif <- as.integer(SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp_hyd_arbustif)


shrub_new_df_100_200_cov<-SHRUBS_100_200_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "cov_100_200_category",
               values_to= "coverage_100_200") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

##SHRUBS HEIGHT 0_50

SHRUBS_0_50_HEIGHT_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x","Placette","ht_toit_0_50_esp1_arbustif","ht_toit_0_50_esp2_arbustif","ht_toit_0_50_esp3_arbustif","ht_toit_0_50_esp_hyd_arbustif"))


shrub_new_df_0_50_height<-SHRUBS_0_50_HEIGHT_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "height_0_50_category",
               values_to= "height_0_50") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

##SHRUBS HEIGHT 50_100

SHRUBS_50_100_HEIGHT_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x","Placette","ht_toit_50_100_esp1_arbustif","ht_toit_50_100_esp2_arbustif","ht_toit_50_100_esp3_arbustif","ht_toit_50_100_esp_hyd_arbustif"))


shrub_new_df_50_100_height<-SHRUBS_50_100_HEIGHT_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "height_50_100_category",
               values_to= "height_50_100") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

##SHRUBS HEIGHT 100_200

SHRUBS_100_200_HEIGHT_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Code_Site.x","Placette","ht_toit_100_200_esp1_arbustif","ht_toit_100_200_esp2_arbustif","ht_toit_100_200_esp3_arbustif","ht_toit_100_200_esp_hyd_arbustif"))


shrub_new_df_100_200_height<-SHRUBS_100_200_HEIGHT_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Code_Site.x,Placette),
               names_to = "height_100_200_category",
               values_to= "height_100_200") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

#MERGE THE 8 TREE DATAFRAMES

MERGE_SHRUBS_FR_VEG_DATA <- shrub_new_df_sp %>%
  inner_join(shrub_new_df_cov[,c(1,4,5)], by = "ID") %>%
  inner_join(shrub_new_df_0_50_cov[,c(1,4,5)], by = "ID") %>%
  inner_join(shrub_new_df_50_100_cov[,c(1,4,5)], by = "ID") %>%
  inner_join(shrub_new_df_100_200_cov[,c(1,4,5)], by = "ID") %>%
  inner_join(shrub_new_df_0_50_height[,c(1,4,5)], by = "ID") %>%
  inner_join(shrub_new_df_50_100_height[,c(1,4,5)], by = "ID") %>%
  inner_join(shrub_new_df_100_200_height[,c(1,4,5)], by = "ID") %>% 
  select(-ID)

write.csv(MERGE_SHRUBS_FR_VEG_DATA, "data/SHRUBS_FR_VEG_DATA.csv", row.names=FALSE)

################################################################################
str(MERGE_TREES_FR_VEG_DATA)
str(MERGE_SHRUBS_FR_VEG_DATA)

MERGE_SHRUBS_FR_VEG_DATA$coverage<-as.character(MERGE_SHRUBS_FR_VEG_DATA$coverage)

FR_VEG_DATA_ALL<-full_join(MERGE_TREES_FR_VEG_DATA,MERGE_SHRUBS_FR_VEG_DATA, by = c("Code_Site.x","sp","Placette","sp_category","cov_category","coverage")) %>% 
  arrange(Code_Site.x)

#If "sp" colum is "NA" all the other values are "NA"

FR_VEG_DATA_ALL <- FR_VEG_DATA_ALL %>%
  mutate(across(c(coverage, height, coverage_0_50, coverage_50_100,coverage_100_200,height_0_50,height_50_100,height_100_200), ~ifelse(is.na(sp), NA_character_, .)))


write.csv(FR_VEG_DATA_ALL, "data/FR_VEG_DATA_ALL.csv", row.names=FALSE)



