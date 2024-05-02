library(medfate)
library(medfateutils)
library(tidyverse)

CAT_LFMC<-read.csv("data/CAT_LFMC.csv")
FR_LFMC<-read.csv("data/FR_LFMC.csv")


MEASURED_SP<- unique(c(unique(CAT_LFMC$sp_correct_name),unique(FR_LFMC$sp_correct_name)))

Params<-c("Name",
          "SLA",
          "Nleaf",
          "Vmax298",
          "Jmax298",
          "r635",
          "maxFMC",
          "LeafPI0",
          "LeafEPS",
          "LeafAF",
          "StemPI0",
          "StemEPS",
          "StemAF",
          "Al2As",
          "Kmax_stemxylem",
          "VCstem_P12",
          "VCstem_P50",
          "VCstem_P88",
          "VCstem_slope",
          "Gs_P50",
          "Gs_slope",
          "Gswmin",
          "Gswmax")

SpParamsAlbert<-SpParamsFR %>% 
  filter(Name %in% MEASURED_SP) %>% 
  select(all_of(Params))

# SpParamsAlbert2<-SpParamsES %>% 
#   filter(Name %in% MEASURED_SP) %>% 
#   select(all_of(Params))
# 
# SpParamsAlbert3<-SpParamsMED %>% 
#   filter(Name %in% MEASURED_SP) %>% 
#   select(all_of(Params))


write.csv(SpParamsAlbert, "data/SpParamsAlbert.csv")
