library(readxl)
library(tidyverse)
library(medfate)

#FRENCH VEGETATION DATA################################################################################################
FR_VEG_DATA_RAW<- read.csv("raw_data/FR_DATA/RH_Sites_coverage.csv", sep = ";")
str(FR_VEG_DATA_RAW)

#TRANSFORM np/na to NA values
FR_VEG_DATA_RAW[FR_VEG_DATA_RAW == "np"] <- NA
FR_VEG_DATA_RAW[FR_VEG_DATA_RAW == "na"] <- NA

#TANSFORM DATE TO CORRECT FORMAT

FR_VEG_DATA_RAW$Date[16]<-"42529" #PLOT D11S2 DATE INCORRECT, CHANGE TO CORRECT NUMBER (in excel format)
FR_VEG_DATA_RAW$Date<- FR_VEG_DATA_RAW$Date %>% as.numeric() %>% as.Date(origin = "1899-12-30")

#D13S1, change row 10 to plot 2
FR_VEG_DATA_RAW$Placette[10]<-2
#View(FR_VEG_DATA_RAW[FR_VEG_DATA_RAW$Code_Site.x=="D13S1",])

#DELETE THE LAST COLUMN, WHICH IS REPEATED
FR_VEG_DATA_RAW<-FR_VEG_DATA_RAW %>% select(-Code_Site.y)

##TREES################################################################################################
ARBORE_COLUMNS<-FR_VEG_DATA_RAW %>%
  select(contains("arbore"))
names(ARBORE_COLUMNS)

###TREES SP
TREES_SP_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                           "Placette", "esp1_arbore",
                                                           "esp2_arbore","esp3_arbore" ))

tree_new_df_sp<-TREES_SP_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "sp_category",
               values_to= "sp") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

###TREES COVERAGE
TREES_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                            "Placette","taux_esp1_arbore",
                                                            "taux_esp2_arbore","taux_esp3_arbore" ))

tree_new_df_cov<-TREES_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "cov_category",
               values_to= "coverage") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")


###TREES MEAN HEIGHT
TREES_HEIGHT_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                               "Placette","ht_toit_esp1_arbore",
                                                               "ht_toit_esp2_arbore", "ht_toit_esp3_arbore" ))

tree_new_df_height<-TREES_HEIGHT_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "height_category",
               values_to= "height") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

####MERGE THE 3 TREE DATAFRAMES

MERGE_TREES_FR_VEG_DATA <- tree_new_df_sp %>%
  inner_join(tree_new_df_cov[,c(1,5,6)], by = "ID") %>%
  inner_join(tree_new_df_height[,c(1,5,6)], by = "ID") %>% 
  select(-ID)



##SHRUBS################################################################################################
ARBUSIF_COLUMNS<-FR_VEG_DATA_RAW %>%
  select(contains("arbustif"))
names(ARBUSIF_COLUMNS)

###SHRUBS SP
SHRUBS_SP_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                            "Placette","esp1_arbustif","esp2_arbustif",
                                                            "esp3_arbustif","esp_hyd_arbustif"))

shrub_new_df_sp<-SHRUBS_SP_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "sp_category",
               values_to= "sp") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

###SHRUBS COVERAGE
SHRUBS_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                             "Placette","taux_esp1_arbustif",
                                                             "taux_esp2_arbustif",
                                                             "taux_esp3_arbustif",
                                                             "taux_esp_hyd_arbustif" ))

SHRUBS_COV_FR_VEG_DATA$taux_esp_hyd_arbustif <- as.integer(SHRUBS_COV_FR_VEG_DATA$taux_esp_hyd_arbustif)

shrub_new_df_cov<-SHRUBS_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "cov_category",
               values_to= "coverage") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

###SHRUBS COVERAGE 0_50

SHRUBS_0_50_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                                  "Placette","taux_0_50_esp1_arbustif",
                                                                  "taux_0_50_esp2_arbustif",
                                                                  "taux_0_50_esp3_arbustif",
                                                                  "taux_0_50_esp_hyd_arbustif"))
str(SHRUBS_0_50_COV_FR_VEG_DATA)
SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp2_arbustif <- as.integer(SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp2_arbustif)
SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp3_arbustif <- as.integer(SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp3_arbustif)
SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp_hyd_arbustif <- as.integer(SHRUBS_0_50_COV_FR_VEG_DATA$taux_0_50_esp_hyd_arbustif)


shrub_new_df_0_50_cov<-SHRUBS_0_50_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "cov_0_50_category",
               values_to= "coverage_0_50") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

###SHRUBS COVERAGE 50_100

SHRUBS_50_100_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                                    "Placette","taux_50_100_esp1_arbustif",
                                                                    "taux_50_100_esp2_arbustif",
                                                                    "taux_50_100_esp3_arbustif",
                                                                    "taux_50_100_esp_hyd_arbustif"))
str(SHRUBS_50_100_COV_FR_VEG_DATA)
SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp2_arbustif <- as.integer(SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp2_arbustif)
SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp3_arbustif <- as.integer(SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp3_arbustif)
SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp_hyd_arbustif <- as.integer(SHRUBS_50_100_COV_FR_VEG_DATA$taux_50_100_esp_hyd_arbustif)


shrub_new_df_50_100_cov<-SHRUBS_50_100_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "cov_50_100_category",
               values_to= "coverage_50_100") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

###SHRUBS COVERAGE 100_200

SHRUBS_100_200_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                                     "Placette","taux_100_200_esp1_arbustif",
                                                                     "taux_100_200_esp2_arbustif",
                                                                     "taux_100_200_esp3_arbustif",
                                                                     "taux_100_200_esp_hyd_arbustif"))
str(SHRUBS_100_200_COV_FR_VEG_DATA)
SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp2_arbustif <- as.integer(SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp2_arbustif)
SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp3_arbustif <- as.integer(SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp3_arbustif)
SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp_hyd_arbustif <- as.integer(SHRUBS_100_200_COV_FR_VEG_DATA$taux_100_200_esp_hyd_arbustif)


shrub_new_df_100_200_cov<-SHRUBS_100_200_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "cov_100_200_category",
               values_to= "coverage_100_200") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

###SHRUBS HEIGHT 0_50

SHRUBS_0_50_HEIGHT_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                                     "Placette","ht_toit_0_50_esp1_arbustif",
                                                                     "ht_toit_0_50_esp2_arbustif",
                                                                     "ht_toit_0_50_esp3_arbustif",
                                                                     "ht_toit_0_50_esp_hyd_arbustif"))


shrub_new_df_0_50_height<-SHRUBS_0_50_HEIGHT_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "height_0_50_category",
               values_to= "height_0_50") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

###SHRUBS HEIGHT 50_100

SHRUBS_50_100_HEIGHT_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                                       "Placette","ht_toit_50_100_esp1_arbustif",
                                                                       "ht_toit_50_100_esp2_arbustif",
                                                                       "ht_toit_50_100_esp3_arbustif",
                                                                       "ht_toit_50_100_esp_hyd_arbustif"))


shrub_new_df_50_100_height<-SHRUBS_50_100_HEIGHT_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "height_50_100_category",
               values_to= "height_50_100") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

###SHRUBS HEIGHT 100_200

SHRUBS_100_200_HEIGHT_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date","Code_Site.x",
                                                                        "Placette","ht_toit_100_200_esp1_arbustif",
                                                                        "ht_toit_100_200_esp2_arbustif",
                                                                        "ht_toit_100_200_esp3_arbustif",
                                                                        "ht_toit_100_200_esp_hyd_arbustif"))


shrub_new_df_100_200_height<-SHRUBS_100_200_HEIGHT_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette),
               names_to = "height_100_200_category",
               values_to= "height_100_200") %>%
  mutate(ID = c(1:n())) %>% 
  relocate("ID")

####MERGE THE 8 TREE DATAFRAMES

MERGE_SHRUBS_FR_VEG_DATA <- shrub_new_df_sp %>%
  inner_join(shrub_new_df_cov[,c(1,5,6)], by = "ID") %>%
  inner_join(shrub_new_df_0_50_cov[,c(1,5,6)], by = "ID") %>%
  inner_join(shrub_new_df_50_100_cov[,c(1,5,6)], by = "ID") %>%
  inner_join(shrub_new_df_100_200_cov[,c(1,5,6)], by = "ID") %>%
  inner_join(shrub_new_df_0_50_height[,c(1,5,6)], by = "ID") %>%
  inner_join(shrub_new_df_50_100_height[,c(1,5,6)], by = "ID") %>%
  inner_join(shrub_new_df_100_200_height[,c(1,5,6)], by = "ID") %>% 
  select(-ID)

##HERBS################################################################################################

HERBACEE_COLUMNS<-FR_VEG_DATA_RAW %>%
  select(contains("herbacee"))
names(HERBACEE_COLUMNS)

###HERBS COVERAGE
HERBS_COV_FR_VEG_DATA <- subset(FR_VEG_DATA_RAW, select = c("Date", "Code_Site.x", 
                                                            "Placette","esp1_herbacee", 
                                                            "esp2_herbacee", "esp3_herbacee", 
                                                            "esp4_herbacee", "taux_total_herbacee"))

herb_new_df_cov<-HERBS_COV_FR_VEG_DATA %>% 
  pivot_longer(cols = -c(Date,Code_Site.x,Placette,taux_total_herbacee),
               names_to = "sp_category",
               values_to= "sp") %>%
  relocate("taux_total_herbacee",.after = last_col())

MERGE_HERBS_FR_VEG_DATA<-herb_new_df_cov
MERGE_HERBS_FR_VEG_DATA <- rename(MERGE_HERBS_FR_VEG_DATA, coverage = taux_total_herbacee)

##PROCESSING THE DATA################################################################################################

# 1.REMOVE NA SPECIES ROWS, If "sp" colum is "NA" remove the row
# 2.REMOVE THE "CATEGORY COLUMNS"

MERGE_TREES_FR_VEG_DATA <- MERGE_TREES_FR_VEG_DATA %>%
  filter(!is.na(sp)) %>%
  select(-contains("category"))

MERGE_SHRUBS_FR_VEG_DATA <- MERGE_SHRUBS_FR_VEG_DATA %>%
  filter(!is.na(sp)) %>%
  select(-contains("category"))

MERGE_HERBS_FR_VEG_DATA <- MERGE_HERBS_FR_VEG_DATA %>%
  filter(!is.na(sp)) %>%
  select(-contains("category"))


#REMOVE ALL THE NOT USEFUL DATAFRAMES
#rm(list=setdiff(ls(), c("MERGE_HERBS_FR_VEG_DATA","MERGE_SHRUBS_FR_VEG_DATA","MERGE_TREES_FR_VEG_DATA")))

##CHANGE TO THE CORRECT SP NAME################################################################################################
data("SpParamsFR")

#MEDFATE SP FRENCH ACCEPTED NAMES
correct_names<-data.frame(CorrectName=SpParamsFR$Name)

#CONVERSION TABLE

CONVERSION <- read_excel("raw_data/FR_DATA/ORIGINAL_species_list_rh.xlsx", col_names = FALSE)

#TRANSFORM THE DATAFRAME
CONVERSION_SP <- CONVERSION %>%
  mutate(CONVERSION_NAME = ifelse(is.na(...3), ...2, paste(...2, ...3, sep = " "))) %>% #MERGE 2 AND 3 COLUMS
  select(-c(...2, ...3)) %>% #REMOVE ...2 AND ...3 COLUMNS
  rename(FR_NAME=...1) #%>% #RENAME FIRST COLUM


#CHECK IF THE VALUES OF CONVERSION_SP$CONVERSION_NAME ARE CORRECTLY SPELLED, COMPARING WITH THE "SpParamsFR" DATA OF MEDFATE.

CONVERSION_SP$IS_CORRECT <- CONVERSION_SP$CONVERSION_NAME %in% correct_names$CorrectName

WRONG_FR_VEG_DATA<-CONVERSION_SP %>% filter(IS_CORRECT==FALSE)
print(count(WRONG_FR_VEG_DATA,FR_NAME,CONVERSION_NAME), n=nrow(WRONG_FR_VEG_DATA))

old_names <- pull(distinct(WRONG_FR_VEG_DATA,CONVERSION_NAME))
new_names <- c("Pinus sylvestris","Acer opalus subsp. opalus","Juniperus oxycedrus subsp. oxycedrus",
               "Cytisophyllum sessilifolium","Cytisus oromediterraneus","Erica scoparia subsp. scoparia",
               "Calicotome spinosa","Cistus salviifolius","Poaceae","Aphyllanthes monspeliensis",NA,"Convolvulaceae",
               "Apiaceae","Caryophyllaceae","Lotus dorycnium","Sorbus aria","Pinus nigra subsp. salzmanni",
               "Rhamnus alaternus","Lavandula angustifolia","Cistus umbellatus subsp. umbellatus")


CONVERSION_SP$sp_correct_name<-NA
index<-c()
for (i in 1:nrow(CONVERSION_SP)) {
  if (CONVERSION_SP$IS_CORRECT[i]==TRUE){
    CONVERSION_SP$sp_correct_name[i]<-CONVERSION_SP$CONVERSION_NAME[i]
  }else {
    index <- match(CONVERSION_SP$CONVERSION_NAME[i], old_names)
    if (!is.na(index)) {
      CONVERSION_SP$sp_correct_name[i] <- new_names[index]
    }
  }
}

#REORGANIZE DATA AND EXPORT THE CONVERSION TABLE

CONVERSION_SP<-CONVERSION_SP %>% 
  relocate(sp_correct_name, .after = "CONVERSION_NAME") %>% 
  select(-c(IS_CORRECT,CONVERSION_NAME))

write.csv(CONVERSION_SP, "raw_data/FR_DATA/SPECIES_FR_CONVERSION.csv", row.names=FALSE)

#READ CONVERSION TABLE (IF NEEDED)

#CONVERSION_SP<-read.csv("raw_data/FR_DATA/SPECIES_FR_CONVERSION.csv")

#ADD CORRECT NAME TO "MERGE" DATA FRAMES

MERGE_TREES_FR_VEG_DATA <- MERGE_TREES_FR_VEG_DATA %>% 
  inner_join(CONVERSION_SP, by = c("sp" = "FR_NAME")) %>%  
  relocate(sp_correct_name, .after = "sp")

MERGE_SHRUBS_FR_VEG_DATA <- MERGE_SHRUBS_FR_VEG_DATA %>% 
  inner_join(CONVERSION_SP, by = c("sp" = "FR_NAME")) %>% 
  relocate(sp_correct_name, .after = "sp")

MERGE_HERBS_FR_VEG_DATA <- MERGE_HERBS_FR_VEG_DATA %>% 
  inner_join(CONVERSION_SP, by = c("sp" = "FR_NAME")) %>% 
  relocate(sp_correct_name, .after = "sp")

#CHECK THE COLUMN

MERGE_TREES_FR_VEG_DATA$sp_correct_name %in% SpParamsFR$Name
MERGE_SHRUBS_FR_VEG_DATA$sp_correct_name %in% SpParamsFR$Name
MERGE_HERBS_FR_VEG_DATA$sp_correct_name %in% SpParamsFR$Name #not all sp are in SpParamsFR, not important in herbs?



##SHRUBS NEED MORE STEPS! TRNSFORM COVERAGE % TO "TOTAL COVERAGE", AND SEPARATE THE DIFERENT LEVELS (0,50)(50_100)(100_200)################################################################

MERGE_SHRUBS_FR_VEG_DATA <- MERGE_SHRUBS_FR_VEG_DATA %>%
  mutate(across(starts_with("coverage_"), ~ . * coverage / 100)) %>% 
  select(-coverage)

#rm(list=setdiff(ls(), c("MERGE_HERBS_FR_VEG_DATA","MERGE_SHRUBS_FR_VEG_DATA","MERGE_TREES_FR_VEG_DATA")))


# SEPARATE THE COVER AND HEIGHT
coverage_df <- MERGE_SHRUBS_FR_VEG_DATA %>%
  pivot_longer(cols = starts_with("coverage"),
               names_to = "level",
               names_prefix = "coverage_",
               values_to = "coverage") %>% 
  mutate(ID = c(1:n())) %>%
  select(-c(contains("height"))) %>% 
  relocate("ID")

height_df <- MERGE_SHRUBS_FR_VEG_DATA %>%
  pivot_longer(cols = starts_with("height"),
               names_to = "level",
               names_prefix = "height_",
               values_to = "height") %>% 
  mutate(ID = c(1:n())) %>% 
  select(-c(contains("coverage"))) %>%
  relocate("ID")

#FINAL SHRUB DATAFRAME

MERGE_SHRUBS_FR_VEG_DATA <- coverage_df %>%
  inner_join(height_df[,c(1,8)], by = "ID") %>%
  select(-c(ID,level)) %>% 
  filter(!is.na(height))         

##EXPORT FINAL DATA (.CSV)################################################################################################

#TREES
write.csv(MERGE_TREES_FR_VEG_DATA, "data/FR_TREES_VEG_DATA.csv", row.names=FALSE)
#SHRUBS
write.csv(MERGE_SHRUBS_FR_VEG_DATA, "data/FR_SHRUBS_VEG_DATA.csv", row.names=FALSE)
#HERBS
write.csv(MERGE_HERBS_FR_VEG_DATA, "data/FR_HERBS_VEG_DATA.csv", row.names=FALSE)

