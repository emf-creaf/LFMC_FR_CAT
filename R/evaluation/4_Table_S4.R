#################################### Table S4 #####################################
data <- readRDS("data/evaluation_table_best_all.rds")
data <- data[-c(3,7,48,29,40),] # delete repeated species-site combinations
#
table <- data[,c(1:4,6,7,8,11,14)]
table$r2 <- round(table$r2, 2)
table$MAE <- round(table$MAE, 1)
sp <- levels(as.factor(table$species))
new <- data.frame(
  species = as.factor(sp),
  LeafP10_imp = NA,
  VcStemP50_imp = NA
)
new$LeafP10_imp <- c("YES","NO","NO","YES","NO","NO","YES","YES","NO","NO","YES","NO","NO","NO","YES","YES","NO","NO","NO","NO")
new$VcStemP50_imp <- c("YES","NO","NO","YES","NO","NO","YES","YES","NO","NO","YES","NO","NO","NO","YES","YES","NO","NO","NO","NO")
#
table$Trait_imp <- NA
for (i in 1:nrow(table)) {
  sp.name <- table$species[i]
  val <- subset(new, new$species==sp.name)
  table$Trait_imp[i] <- val$LeafP10_imp
}
write.csv(table, "Data/TableS4.csv", quote=F, row.names = F)
