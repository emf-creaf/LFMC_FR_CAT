library(medfate)
library(traits4models)
library(tidyverse)
library(readxl)


# Loads custom params and merges with SpParamsFR --------------------------

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
