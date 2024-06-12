library(medfate)
library(medfateutils)
library(tidyverse)
library(readxl)

# CAT_LFMC<-read.csv("data/CAT_LFMC.csv")
# FR_LFMC<-read.csv("data/FR_LFMC.csv")
# 
# 
# MEASURED_SP<- unique(c(unique(CAT_LFMC$sp_correct_name),unique(FR_LFMC$sp_correct_name)))
# 
# Params<-c("Name",
#           "Z50",
#           "Z95",
#           "SLA",
#           "Nleaf",
#           "Vmax298",
#           "Jmax298",
#           "r635",
#           "maxFMC",
#           "LeafPI0",
#           "LeafEPS",
#           "LeafAF",
#           "StemPI0",
#           "StemEPS",
#           "StemAF",
#           "Al2As",
#           "Kmax_stemxylem",
#           "VCstem_P12",
#           "VCstem_P50",
#           "VCstem_P88",
#           "VCstem_slope",
#           "Gs_P50",
#           "Gs_slope",
#           "Gswmin",
#           "Gswmax")
# 
# SpParamsAlbert<-SpParamsFR %>% 
#   filter(Name %in% MEASURED_SP) %>% 
#   select(all_of(Params))
# 
# ###95 % quatile LFMC by SP
# 
# CAT_DATA<-CAT_LFMC[,c(4,5)]
# FR_DATA<-FR_LFMC[,c(4,10)]
# names(FR_DATA)[names(FR_DATA) == "RobustLFMC"] <- "LFMC"
# 
# LFMC_DATA<-rbind(CAT_DATA,FR_DATA)
# 
# LFMC_95Q<-LFMC_DATA %>%
#   group_by(sp_correct_name) %>%
#   summarise(maxLFMC_95Q = quantile(LFMC, probs = 0.95))
# 
# 
# SpParamsAlbert2 <- SpParamsAlbert %>% 
#   inner_join(LFMC_95Q, by = c("Name" = "sp_correct_name")) %>% 
#   relocate(maxLFMC_95Q, .after = maxFMC)
# 
# write.csv(SpParamsAlbert2, "data/SpParamsAlbert.csv")



#SpParamsAlbert FINAL CORRECT PARAMS

SpParamsAlbert <- read_excel("data/SpParamsAlbert.xlsx", sheet = "SpParams_final") %>% 
  #select(-c("maxLFMC_95Q","LDMC" ,"TLP")) %>% 
  mutate(Name = ifelse(Name == "Calicotome spinosa", "Calicotome", Name))

SpParams<-modifySpParams(SpParamsFR,SpParamsAlbert) %>% 
  mutate(Name = ifelse(Name == "Calicotome", "Calicotome spinosa", Name))

write.csv(SpParams, "data/SpParamsAlbert.csv", row.names = F)