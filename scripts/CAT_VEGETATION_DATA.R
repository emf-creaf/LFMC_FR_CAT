library(readxl)
library(medfate)
library(tidyverse)

data(SpParamsES)

#CAT VEGETATION DATA####
CAT_VEG_DATA <- read_excel("raw_data/CAT_DATA/SPIF_plotvegdata.xlsx", sheet = "vegetation_data")

#CHECK IF THE SP NAME ARE IN SpParamsES

CAT_VEG_DATA$IS_CORRECT_ES<-CAT_VEG_DATA$Species %in% SpParamsES$Name

#CREATE A WRONG SP DATAFRAME

WRONG_CAT_VEG_DATA<-CAT_VEG_DATA %>% filter(IS_CORRECT_ES==FALSE)
count(WRONG_CAT_VEG_DATA,Species)

#REPLACE THE OLD NAMES WITH THE CORRECT NAMES FROM SpParamsES

old_names <- pull(distinct(WRONG_CAT_VEG_DATA,Species))
new_names <- c("Rosmarinus officinalis","Thymelaea spp.",
               "Helianthemum spp.","Thymus spp.","Coronilla spp.",
               "Euphorbia spp.","Lavandula spp.","Rubus spp.",
               "Asparagus spp.","Helianthemum spp.","Asphodelus spp.")
  

index<-NA
CAT_VEG_DATA$sp_correct_name<-NA

for (i in 1:nrow(CAT_VEG_DATA)) {
  if (CAT_VEG_DATA$IS_CORRECT_ES[i]==TRUE){
    CAT_VEG_DATA$sp_correct_name[i]<-CAT_VEG_DATA$Species[i]
  }else {
    index <- match(CAT_VEG_DATA$Species[i], old_names)
    if (!is.na(index)) {
      CAT_VEG_DATA$sp_correct_name[i] <- new_names[index]
    }
  }
}

#REORGANIZE DATA AND EXPORT DATA

CAT_VEG_DATA<-CAT_VEG_DATA %>% 
  relocate(sp_correct_name, .after = "Species") %>% 
  select(-IS_CORRECT_ES)

write.csv(CAT_VEG_DATA, "data/CAT_VEG_DATA.csv", row.names=FALSE)

#Asphodelus spp. not correct

which(!CAT_VEG_DATA$sp_correct_name %in% SpParamsES$Name)
