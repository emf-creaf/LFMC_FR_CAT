library(medfate)
library(traits4models)
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

SpParamsAlbert <- read_excel("data/inputs/SpParamsAlbert.xlsx", sheet = "SpParams_final") %>% 
  #select(-c("maxLFMC_95Q","LDMC" ,"TLP")) %>% 
  mutate(Name = ifelse(Name == "Calicotome spinosa", "Calicotome", Name))

SpParams<-modifySpParams(SpParamsFR,SpParamsAlbert) %>% 
  mutate(Name = ifelse(Name == "Calicotome", "Calicotome spinosa", Name))

write.csv(SpParams, "data/inputs/SpParamsAlbert.csv", row.names = F)


sp_names <- SpParamsAlbert$Name
get_param <- function(parName) {
  vals <- rep(NA, length(sp_names))
  for(i in 1:length(sp_names)) {
    vals[i] <- medfate::species_parameter(sp_names[i], SpParams, parName)
  }
  names(vals) <- sp_names
  return(vals)
}

## PARAMETER VALUES AFTER IMPUTATION
data.frame(SLA = get_param("SLA"),
           Nleaf = get_param("Nleaf"),
           Vmax298 = get_param("Vmax298"),
           Jmax298 = get_param("Jmax298"))

data.frame(LeafPI0 = get_param("LeafPI0"),
           LeafEPS = get_param("LeafEPS"),
           LeafAF = get_param("LeafAF"),
           StemPI0 = get_param("StemPI0"),
           StemEPS = get_param("StemEPS"),
           StemAF = get_param("StemAF"))


data.frame(Al2As = get_param("Al2As"),
           Kmax_stemxylem = get_param("Kmax_stemxylem"),
           VCstem_P12 = get_param("VCstem_P12"),
           VCstem_P50 = get_param("VCstem_P50"),
           VCstem_P88 = get_param("VCstem_P88"),
           VCstem_slope = get_param("VCstem_slope"))

data.frame(TLP = get_param("TLP"),
           Gs_P50 = get_param("Gs_P50"),
           Gs_slope = get_param("Gs_slope"),
           Gswmin = get_param("Gswmin"),
           Gswmax = get_param("Gswmax"))
