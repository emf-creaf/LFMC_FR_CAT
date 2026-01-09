#Librerias
library(ggplot2)
library(grid)
library(ggpubr)

# Data
data <- readRDS("data/comparison_tables/ct_best_comb.rds")
data <- subset(data, is.na(data$LFMC_observed) == F)
data <- subset(data, data$is_outlier == F)

## Data Substes
# # Semi
semi <- subset(data, lfmc=="semi")
semi_eraModis <- subset(semi, meteo_source == "ERA5" & lai_source == "MODIS")
semi_interModis <- subset(semi, meteo_source == "INTER" & lai_source == "MODIS")
semi_eraAllom <- subset(semi, meteo_source == "ERA5" & lai_source == "ALLOM")
semi_interAllom <- subset(semi, meteo_source == "INTER" & lai_source == "ALLOM")
# Full-1
ful1 <- subset(data, lfmc=="full1")
ful1_eraModis <- subset(ful1, meteo_source == "ERA5" & lai_source == "MODIS")
ful1_interModis <- subset(ful1, meteo_source == "INTER" & lai_source == "MODIS")
ful1_eraAllom <- subset(ful1, meteo_source == "ERA5" & lai_source == "ALLOM")
ful1_interAllom <- subset(ful1, meteo_source == "INTER" & lai_source == "ALLOM")  
# Full-2
ful2 <- subset(data, lfmc=="full2")
ful2_eraModis <- subset(ful2, meteo_source == "ERA5" & lai_source == "MODIS")
ful2_interModis <- subset(ful2, meteo_source == "INTER" & lai_source == "MODIS")
ful2_eraAllom <- subset(ful2, meteo_source == "ERA5" & lai_source == "ALLOM")
ful2_interAllom <- subset(ful2, meteo_source == "INTER" & lai_source == "ALLOM")  
# Full-3
ful3 <- subset(data, lfmc=="full3")
ful3_eraModis <- subset(ful3, meteo_source == "ERA5" & lai_source == "MODIS")
ful3_interModis <- subset(ful3, meteo_source == "INTER" & lai_source == "MODIS")
ful3_eraAllom <- subset(ful3, meteo_source == "ERA5" & lai_source == "ALLOM")
ful3_interAllom <- subset(ful3, meteo_source == "INTER" & lai_source == "ALLOM")  
##############Plots
# Semi ERA MODIS
#Labels
lm<-lm(semi_eraModis$LFMC_observed~semi_eraModis$LFMC_semi)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(semi_eraModis$LFMC_semi, semi_eraModis$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.1 <- ggplot(data=semi_eraModis, aes(x= LFMC_semi, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#FDE333FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("Obs. LFMC (%)") +
  xlab(LFMC[semi]~"(%)") +
  ggtitle("Era5-Modis")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.1

# Semi ERA Allom
#Labels
lm<-lm(semi_eraAllom$LFMC_observed~semi_eraAllom$LFMC_semi)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(semi_eraAllom$LFMC_semi, semi_eraAllom$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.2 <- ggplot(data=semi_eraAllom, aes(x= LFMC_semi, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#D7E12DFF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[semi]~"(%)") +
  ggtitle("Era5-Allom")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.2

# Semi Inter Modis
#Labels
lm<-lm(semi_interModis$LFMC_observed~semi_interModis$LFMC_semi)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(semi_interModis$LFMC_semi, semi_interModis$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.3 <- ggplot(data=semi_interModis, aes(x= LFMC_semi, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#ADDB3FFF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[semi]~"(%)") +
  ggtitle("Inter-Modis")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.3

# Semi Inter Modis
#Labels
lm<-lm(semi_interAllom$LFMC_observed~semi_interAllom$LFMC_semi)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(semi_interAllom$LFMC_semi, semi_interAllom$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.4 <- ggplot(data=semi_interAllom, aes(x= LFMC_semi, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#7ED357FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[semi]~"(%)") +
  ggtitle("Inter-Allom")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.4

###
# ful1 ERA MODIS
#Labels
lm<-lm(ful1_eraModis$LFMC_observed~ful1_eraModis$LFMC_full1)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful1_eraModis$LFMC_full1, ful1_eraModis$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.5 <- ggplot(data=ful1_eraModis, aes(x= LFMC_full1, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#41CA6CFF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("Obs. LFMC (%)") +
  xlab(LFMC[full1]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.5

# ful1 ERA Allom
#Labels
lm<-lm(ful1_eraAllom$LFMC_observed~ful1_eraAllom$LFMC_full1)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful1_eraAllom$LFMC_full1, ful1_eraAllom$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.6 <- ggplot(data=ful1_eraAllom, aes(x= LFMC_full1, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#00BE7DFF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[full1]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.6

# ful1 Inter Modis
#Labels
lm<-lm(ful1_interModis$LFMC_observed~ful1_interModis$LFMC_full1)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful1_interModis$LFMC_full1, ful1_interModis$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.7 <- ggplot(data=ful1_interModis, aes(x= LFMC_full1, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#00B28AFF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[full1]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.7

# ful1 Inter Modis
#Labels
lm<-lm(ful1_interAllom$LFMC_observed~ful1_interAllom$LFMC_full1)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful1_interAllom$LFMC_full1, ful1_interAllom$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.8 <- ggplot(data=ful1_interAllom, aes(x= LFMC_full1, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#00A392FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[full1]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.8

###
# ful2 ERA MODIS
#Labels
lm<-lm(ful2_eraModis$LFMC_observed~ful2_eraModis$LFMC_full2)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful2_eraModis$LFMC_full2, ful2_eraModis$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.9 <- ggplot(data=ful2_eraModis, aes(x= LFMC_full2, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#009397FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("Obs. LFMC (%)") +
  xlab(LFMC[full2]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.9

# ful2 ERA Allom
#Labels
lm<-lm(ful2_eraAllom$LFMC_observed~ful2_eraAllom$LFMC_full2)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful2_eraAllom$LFMC_full2, ful2_eraAllom$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.10 <- ggplot(data=ful2_eraAllom, aes(x= LFMC_full2, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#008298FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[full2]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.10

# ful2 Inter Modis
#Labels
lm<-lm(ful2_interModis$LFMC_observed~ful2_interModis$LFMC_full2)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful2_interModis$LFMC_full2, ful2_interModis$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.11 <- ggplot(data=ful2_interModis, aes(x= LFMC_full2, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#007094FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[full2]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.11

# ful2 Inter Modis
#Labels
lm<-lm(ful2_interAllom$LFMC_observed~ful2_interAllom$LFMC_full2)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful2_interAllom$LFMC_full2, ful2_interAllom$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.12 <- ggplot(data=ful2_interAllom, aes(x= LFMC_full2, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#005D8DFF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[full2]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.12

###
# ful3 ERA MODIS
#Labels
lm<-lm(ful3_eraModis$LFMC_observed~ful3_eraModis$LFMC_full3)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful3_eraModis$LFMC_full3, ful3_eraModis$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.13 <- ggplot(data=ful3_eraModis, aes(x= LFMC_full3, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#274983FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("Obs. LFMC (%)") +
  xlab(LFMC[full3]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.13

# ful3 ERA Allom
#Labels
lm<-lm(ful3_eraAllom$LFMC_observed~ful3_eraAllom$LFMC_full3)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful3_eraAllom$LFMC_full3, ful3_eraAllom$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.14 <- ggplot(data=ful3_eraAllom, aes(x= LFMC_full3, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#3E3375FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[full3]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.14

# ful3 Inter Modis
#Labels
lm<-lm(ful3_interModis$LFMC_observed~ful3_interModis$LFMC_full3)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful3_interModis$LFMC_full3, ful3_interModis$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.15 <- ggplot(data=ful3_interModis, aes(x= LFMC_full3, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#481B65FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[full3]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.15

# ful3 Inter Modis
#Labels
lm<-lm(ful3_interAllom$LFMC_observed~ful3_interAllom$LFMC_full3)
r2<-summary(lm)$r.squared
mae<-tdr::tdStats(ful3_interAllom$LFMC_full3, ful3_interAllom$LFMC_observed, functions =  "mae")
mylabel_1 = bquote(italic(R)^2==.(format(r2, digits = 2)))
grob_1 <- grobTree(textGrob(mylabel_1,x=0.74,  y=0.12, gp = gpar(fontsize = 22)))
mylabel_2 = bquote(italic(MAE)==.(format(mae, digits = 3)))
grob_2 <- grobTree(textGrob(mylabel_2,x=0.5,  y=0.92, gp = gpar(fontsize = 22)))
#Plots
F3.16 <- ggplot(data=ful3_interAllom, aes(x= LFMC_full3, y=LFMC_observed))+
  geom_point(alpha=0.25, color= "#4B0055FF") +
  geom_abline(lty=2) +
  stat_smooth(method = "lm", se = FALSE, formula = "y ~ x", col = "red")+
  scale_y_continuous(limits = c(0,230))+
  scale_x_continuous(limits = c(0,230))+
  annotation_custom(grob_1)  +
  annotation_custom(grob_2)  +
  ylab("") +
  xlab(LFMC[full3]~"(%)") +
  ggtitle("")+
  theme_classic() +
  theme(text = element_text(size=26), axis.title = element_text(size=24),
        plot.title = element_text(size = 26, hjust = 0.5))
F3.16

# One plot
ggarrange(F3.1,F3.2,F3.3,F3.4,F3.5,F3.6,F3.7,F3.8,F3.9,F3.10,F3.11,F3.12,F3.13,F3.14,F3.15,F3.16,
          nrow=4, ncol = 4, align = "hv", font.label = list(size = 26),
          labels = c("a)","b)","c)","d)","e)","f)","g)","h)","i)","j)","k)","l)","m)","n)","o)","p)"))
#
# Save
ggsave("plots/figures/Fig3/Fig3.png", width = 40, height = 40, units = "cm") 

