#################################CAT_FR_SITES###################################
CAT_FR_SITES<-read.csv("data/CAT_FR_SITES.csv")
sites<-CAT_FR_SITES$site_name

PATTERN<-"SINGLE.*MEDFATE.*"

files_path1<-list.files(paste0("data/PLOTS/", sites), pattern = paste0(PATTERN,"\\.RDS$"), recursive = TRUE, full.names = TRUE)
files_name1<-basename(files_path1)

sim_list<-list()
for (i in 1:length(files_path1)) {
  sim_list[[files_name1[i]]]<-readRDS(files_path1[i])
}
MEDFATE_LAI <- data.frame()


for (i in 1:length(sim_list)) {
  lai <- sim_list[[i]][[1]]$spwbOutput$above$LAI_live
  
  name<-names(sim_list[i])
  split<-strsplit(name, "_")[[1]][1]
  
  temp_df <- data.frame("Site" = split,
                        "LAI_medfate" = lai)
  MEDFATE_LAI <- rbind(MEDFATE_LAI, temp_df)
}

write.csv(MEDFATE_LAI,"data/LAI_MEDFATE.csv", row.names = F)


