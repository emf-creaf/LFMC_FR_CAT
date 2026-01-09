#Librerias
library(ggplot2)
library(viridis)
library(ggpattern)

#Load data for plot a)
data <- readRDS("data/evaluation_table_all.rds")

# Plot A)
F2.1 <- ggplot(data, aes(x=taw, y=r2, group = taw, fill=taw, colour = taw))+
  geom_jitter(alpha=0.1, show.legend = F)+
  geom_violin(width=10, alpha=0.7, show.legend = F) +
  geom_boxplot(width=3, color="black", show.legend = F) +
  scale_fill_viridis(direction = -1, option = "viridis", begin =  0.1) +
  scale_colour_viridis(direction = -1, option = "viridis", begin =  0.1) +
  ggtitle("") +
  #ylab("Mean absolute error (MAE)") +
  ylab(expression(paste("Coeficient of determination (", R^2 , ")")))+
  xlab("Total available water (mm)") +
  scale_x_continuous(breaks=c(40,60,80,100,120,140,160,180,200), expand = c(0.01,0))+
  scale_y_continuous(expand = c(0,0.01)) +
  theme_minimal() +
  theme(axis.text=element_text(size=12, color = "black"), 
        axis.title=element_text(size=14))
F2.1

# Save plot
ggsave("plots/figures/Fig2/Fig2.1.svg", width = 20, height = 10, units = "cm") 


# Supplementary Figure 2
FS2 <- ggplot(data, aes(x=taw, y=MAE, group = taw, fill=taw, colour = taw))+
  geom_jitter(alpha=0.1, show.legend = F)+
  geom_violin(width=10, alpha=0.7, show.legend = F) +
  geom_boxplot(width=3, color="black", show.legend = F) +
  scale_fill_viridis(direction = -1, option = "viridis", begin =  0.1) +
  scale_colour_viridis(direction = -1, option = "viridis", begin =  0.1) +
  ggtitle("") +
  #ylab("Mean absolute error (MAE)") +
  ylab("Mean abolsute error (MAE %)")+
  xlab("Total available water (mm)") +
  scale_x_continuous(breaks=c(40,60,80,100,120,140,160,180,200), expand = c(0.01,0))+
  scale_y_continuous(expand = c(0,0.01)) +
  theme_minimal() +
  theme(axis.text=element_text(size=12, color = "black"), 
        axis.title=element_text(size=14))
FS2

# Save plot
ggsave("plots/figures/Suppleentary/FigS2.png", width = 20, height = 10, units = "cm") 

#Lad data for plot b)
data2 <- readRDS("data/evaluation_table_best_all.rds")
data2 <- data2[-c(3,7,48,29,40),] # delete repeated species-site combinations

# Create dataframes for plotting each TAW bar
#
d30s <- subset(data2, data2$taw == 30 & data2$lfmc == "semi")
d30s <- data.frame(x1 = 30, x2 = 30, y1 = 0, y2 = nrow(d30s))
d30f1 <- subset(data2, data2$taw == 30 & data2$lfmc == "full1")
d30f1 <- data.frame(x1 = 30, x2 = 30, y1 = d30s$y2, y2 = d30s$y2 + nrow(d30f1))
d30f2 <- subset(data2, data2$taw == 30 & data2$lfmc == "full2")
d30f2 <- data.frame(x1 = 30, x2 = 30, y1 = d30f1$y2, y2 = d30f1$y2 + nrow(d30f2))
d30f3 <- subset(data2, data2$taw == 30 & data2$lfmc == "full3")
d30f3 <- data.frame(x1 = 30, x2 = 30, y1 = d30f2$y2, y2 = d30f2$y2 + nrow(d30f3))
#
d40s <- subset(data2, data2$taw == 40 & data2$lfmc == "semi")
d40s <- data.frame(x1 = 40, x2 = 40, y1 = 0, y2 = nrow(d40s))
d40f1 <- subset(data2, data2$taw == 40 & data2$lfmc == "full1")
d40f1 <- data.frame(x1 = 40, x2 = 40, y1 = d40s$y2, y2 = d40s$y2 + nrow(d40f1))
d40f2 <- subset(data2, data2$taw == 40 & data2$lfmc == "full2")
d40f2 <- data.frame(x1 = 40, x2 = 40, y1 = d40f1$y2, y2 = d40f1$y2 + nrow(d40f2))
d40f3 <- subset(data2, data2$taw == 40 & data2$lfmc == "full3")
d40f3 <- data.frame(x1 = 40, x2 = 40, y1 = d40f2$y2, y2 = d40f2$y2 + nrow(d40f3))
#
d50s <- subset(data2, data2$taw == 50 & data2$lfmc == "semi")
d50s <- data.frame(x1 = 50, x2 = 50, y1 = 0, y2 = nrow(d50s))
d50f1 <- subset(data2, data2$taw == 50 & data2$lfmc == "full1")
d50f1 <- data.frame(x1 = 50, x2 = 50, y1 = d50s$y2, y2 = d50s$y2 + nrow(d50f1))
d50f2 <- subset(data2, data2$taw == 50 & data2$lfmc == "full2")
d50f2 <- data.frame(x1 = 50, x2 = 50, y1 = d50f1$y2, y2 = d50f1$y2 + nrow(d50f2))
d50f3 <- subset(data2, data2$taw == 50 & data2$lfmc == "full3")
d50f3 <- data.frame(x1 = 50, x2 = 50, y1 = d50f2$y2, y2 = d50f2$y2 + nrow(d50f3))
#
d60s <- subset(data2, data2$taw == 60 & data2$lfmc == "semi")
d60s <- data.frame(x1 = 60, x2 = 60, y1 = 0, y2 = nrow(d60s))
d60f1 <- subset(data2, data2$taw == 60 & data2$lfmc == "full1")
d60f1 <- data.frame(x1 = 60, x2 = 60, y1 = d60s$y2, y2 = d60s$y2 + nrow(d60f1))
d60f2 <- subset(data2, data2$taw == 60 & data2$lfmc == "full2")
d60f2 <- data.frame(x1 = 60, x2 = 60, y1 = d60f1$y2, y2 = d60f1$y2 + nrow(d60f2))
d60f3 <- subset(data2, data2$taw == 60 & data2$lfmc == "full3")
d60f3 <- data.frame(x1 = 60, x2 = 60, y1 = d60f2$y2, y2 = d60f2$y2 + nrow(d60f3))
#
d70s <- subset(data2, data2$taw == 70 & data2$lfmc == "semi")
d70s <- data.frame(x1 = 70, x2 = 70, y1 = 0, y2 = nrow(d70s))
d70f1 <- subset(data2, data2$taw == 70 & data2$lfmc == "full1")
d70f1 <- data.frame(x1 = 70, x2 = 70, y1 = d70s$y2, y2 = d70s$y2 + nrow(d70f1))
d70f2 <- subset(data2, data2$taw == 70 & data2$lfmc == "full2")
d70f2 <- data.frame(x1 = 70, x2 = 70, y1 = d70f1$y2, y2 = d70f1$y2 + nrow(d70f2))
d70f3 <- subset(data2, data2$taw == 70 & data2$lfmc == "full3")
d70f3 <- data.frame(x1 = 70, x2 = 70, y1 = d70f2$y2, y2 = d70f2$y2 + nrow(d70f3))
#
d80s <- subset(data2, data2$taw == 80 & data2$lfmc == "semi")
d80s <- data.frame(x1 = 80, x2 = 80, y1 = 0, y2 = nrow(d80s))
d80f1 <- subset(data2, data2$taw == 80 & data2$lfmc == "full1")
d80f1 <- data.frame(x1 = 80, x2 = 80, y1 = d80s$y2, y2 = d80s$y2 + nrow(d80f1))
d80f2 <- subset(data2, data2$taw == 80 & data2$lfmc == "full2")
d80f2 <- data.frame(x1 = 80, x2 = 80, y1 = d80f1$y2, y2 = d80f1$y2 + nrow(d80f2))
d80f3 <- subset(data2, data2$taw == 80 & data2$lfmc == "full3")
d80f3 <- data.frame(x1 = 80, x2 = 80, y1 = d80f2$y2, y2 = d80f2$y2 + nrow(d80f3))
#
d90s <- subset(data2, data2$taw == 90 & data2$lfmc == "semi")
d90s <- data.frame(x1 = 90, x2 = 90, y1 = 0, y2 = nrow(d90s))
d90f1 <- subset(data2, data2$taw == 90 & data2$lfmc == "full1")
d90f1 <- data.frame(x1 = 90, x2 = 90, y1 = d90s$y2, y2 = d90s$y2 + nrow(d90f1))
d90f2 <- subset(data2, data2$taw == 90 & data2$lfmc == "full2")
d90f2 <- data.frame(x1 = 90, x2 = 90, y1 = d90f1$y2, y2 = d90f1$y2 + nrow(d90f2))
d90f3 <- subset(data2, data2$taw == 90 & data2$lfmc == "full3")
d90f3 <- data.frame(x1 = 90, x2 = 90, y1 = d90f2$y2, y2 = d90f2$y2 + nrow(d90f3))
#
d100s <- subset(data2, data2$taw == 100 & data2$lfmc == "semi")
d100s <- data.frame(x1 = 100, x2 = 100, y1 = 0, y2 = nrow(d100s))
d100f1 <- subset(data2, data2$taw == 100 & data2$lfmc == "full1")
d100f1 <- data.frame(x1 = 100, x2 = 100, y1 = d100s$y2, y2 = d100s$y2 + nrow(d100f1))
d100f2 <- subset(data2, data2$taw == 100 & data2$lfmc == "full2")
d100f2 <- data.frame(x1 = 100, x2 = 100, y1 = d100f1$y2, y2 = d100f1$y2 + nrow(d100f2))
d100f3 <- subset(data2, data2$taw == 100 & data2$lfmc == "full3")
d100f3 <- data.frame(x1 = 100, x2 = 100, y1 = d100f2$y2, y2 = d100f2$y2 + nrow(d100f3))
#
d110s <- subset(data2, data2$taw == 110 & data2$lfmc == "semi")
d110s <- data.frame(x1 = 110, x2 = 110, y1 = 0, y2 = nrow(d110s))
d110f1 <- subset(data2, data2$taw == 110 & data2$lfmc == "full1")
d110f1 <- data.frame(x1 = 110, x2 = 110, y1 = d110s$y2, y2 = d110s$y2 + nrow(d110f1))
d110f2 <- subset(data2, data2$taw == 110 & data2$lfmc == "full2")
d110f2 <- data.frame(x1 = 110, x2 = 110, y1 = d110f1$y2, y2 = d110f1$y2 + nrow(d110f2))
d110f3 <- subset(data2, data2$taw == 110 & data2$lfmc == "full3")
d110f3 <- data.frame(x1 = 110, x2 = 110, y1 = d110f2$y2, y2 = d110f2$y2 + nrow(d110f3))
#
d120s <- subset(data2, data2$taw == 120 & data2$lfmc == "semi")
d120s <- data.frame(x1 = 120, x2 = 120, y1 = 0, y2 = nrow(d120s))
d120f1 <- subset(data2, data2$taw == 120 & data2$lfmc == "full1")
d120f1 <- data.frame(x1 = 120, x2 = 120, y1 = d120s$y2, y2 = d120s$y2 + nrow(d120f1))
d120f2 <- subset(data2, data2$taw == 120 & data2$lfmc == "full2")
d120f2 <- data.frame(x1 = 120, x2 = 120, y1 = d120f1$y2, y2 = d120f1$y2 + nrow(d120f2))
d120f3 <- subset(data2, data2$taw == 120 & data2$lfmc == "full3")
d120f3 <- data.frame(x1 = 120, x2 = 120, y1 = d120f2$y2, y2 = d120f2$y2 + nrow(d120f3))
#
d130s <- subset(data2, data2$taw == 130 & data2$lfmc == "semi")
d130s <- data.frame(x1 = 130, x2 = 130, y1 = 0, y2 = nrow(d130s))
d130f1 <- subset(data2, data2$taw == 130 & data2$lfmc == "full1")
d130f1 <- data.frame(x1 = 130, x2 = 130, y1 = d130s$y2, y2 = d130s$y2 + nrow(d130f1))
d130f2 <- subset(data2, data2$taw == 130 & data2$lfmc == "full2")
d130f2 <- data.frame(x1 = 130, x2 = 130, y1 = d130f1$y2, y2 = d130f1$y2 + nrow(d130f2))
d130f3 <- subset(data2, data2$taw == 130 & data2$lfmc == "full3")
d130f3 <- data.frame(x1 = 130, x2 = 130, y1 = d130f2$y2, y2 = d130f2$y2 + nrow(d130f3))
#
d140s <- subset(data2, data2$taw == 140 & data2$lfmc == "semi")
d140s <- data.frame(x1 = 140, x2 = 140, y1 = 0, y2 = nrow(d140s))
d140f1 <- subset(data2, data2$taw == 140 & data2$lfmc == "full1")
d140f1 <- data.frame(x1 = 140, x2 = 140, y1 = d140s$y2, y2 = d140s$y2 + nrow(d140f1))
d140f2 <- subset(data2, data2$taw == 140 & data2$lfmc == "full2")
d140f2 <- data.frame(x1 = 140, x2 = 140, y1 = d140f1$y2, y2 = d140f1$y2 + nrow(d140f2))
d140f3 <- subset(data2, data2$taw == 140 & data2$lfmc == "full3")
d140f3 <- data.frame(x1 = 140, x2 = 140, y1 = d140f2$y2, y2 = d140f2$y2 + nrow(d140f3))
#
d150s <- subset(data2, data2$taw == 150 & data2$lfmc == "semi")
d150s <- data.frame(x1 = 150, x2 = 150, y1 = 0, y2 = nrow(d150s))
d150f1 <- subset(data2, data2$taw == 150 & data2$lfmc == "full1")
d150f1 <- data.frame(x1 = 150, x2 = 150, y1 = d150s$y2, y2 = d150s$y2 + nrow(d150f1))
d150f2 <- subset(data2, data2$taw == 150 & data2$lfmc == "full2")
d150f2 <- data.frame(x1 = 150, x2 = 150, y1 = d150f1$y2, y2 = d150f1$y2 + nrow(d150f2))
d150f3 <- subset(data2, data2$taw == 150 & data2$lfmc == "full3")
d150f3 <- data.frame(x1 = 150, x2 = 150, y1 = d150f2$y2, y2 = d150f2$y2 + nrow(d150f3))
#
d160s <- subset(data2, data2$taw == 160 & data2$lfmc == "semi")
d160s <- data.frame(x1 = 160, x2 = 160, y1 = 0, y2 = nrow(d160s))
d160f1 <- subset(data2, data2$taw == 160 & data2$lfmc == "full1")
d160f1 <- data.frame(x1 = 160, x2 = 160, y1 = d160s$y2, y2 = d160s$y2 + nrow(d160f1))
d160f2 <- subset(data2, data2$taw == 160 & data2$lfmc == "full2")
d160f2 <- data.frame(x1 = 160, x2 = 160, y1 = d160f1$y2, y2 = d160f1$y2 + nrow(d160f2))
d160f3 <- subset(data2, data2$taw == 160 & data2$lfmc == "full3")
d160f3 <- data.frame(x1 = 160, x2 = 160, y1 = d160f2$y2, y2 = d160f2$y2 + nrow(d160f3))
#
d170s <- subset(data2, data2$taw == 170 & data2$lfmc == "semi")
d170s <- data.frame(x1 = 170, x2 = 170, y1 = 0, y2 = nrow(d170s))
d170f1 <- subset(data2, data2$taw == 170 & data2$lfmc == "full1")
d170f1 <- data.frame(x1 = 170, x2 = 170, y1 = d170s$y2, y2 = d170s$y2 + nrow(d170f1))
d170f2 <- subset(data2, data2$taw == 170 & data2$lfmc == "full2")
d170f2 <- data.frame(x1 = 170, x2 = 170, y1 = d170f1$y2, y2 = d170f1$y2 + nrow(d170f2))
d170f3 <- subset(data2, data2$taw == 170 & data2$lfmc == "full3")
d170f3 <- data.frame(x1 = 170, x2 = 170, y1 = d170f2$y2, y2 = d170f2$y2 + nrow(d170f3))
#
d180s <- subset(data2, data2$taw == 180 & data2$lfmc == "semi")
d180s <- data.frame(x1 = 180, x2 = 180, y1 = 0, y2 = nrow(d180s))
d180f1 <- subset(data2, data2$taw == 180 & data2$lfmc == "full1")
d180f1 <- data.frame(x1 = 180, x2 = 180, y1 = d180s$y2, y2 = d180s$y2 + nrow(d180f1))
d180f2 <- subset(data2, data2$taw == 180 & data2$lfmc == "full2")
d180f2 <- data.frame(x1 = 180, x2 = 180, y1 = d180f1$y2, y2 = d180f1$y2 + nrow(d180f2))
d180f3 <- subset(data2, data2$taw == 180 & data2$lfmc == "full3")
d180f3 <- data.frame(x1 = 180, x2 = 180, y1 = d180f2$y2, y2 = d180f2$y2 + nrow(d180f3))
#
d200s <- subset(data2, data2$taw == 200 & data2$lfmc == "semi")
d200s <- data.frame(x1 = 200, x2 = 200, y1 = 0, y2 = nrow(d200s))
d200f1 <- subset(data2, data2$taw == 200 & data2$lfmc == "full1")
d200f1 <- data.frame(x1 = 200, x2 = 200, y1 = d200s$y2, y2 = d200s$y2 + nrow(d200f1))
d200f2 <- subset(data2, data2$taw == 200 & data2$lfmc == "full2")
d200f2 <- data.frame(x1 = 200, x2 = 200, y1 = d200f1$y2, y2 = d200f1$y2 + nrow(d200f2))
d200f3 <- subset(data2, data2$taw == 200 & data2$lfmc == "full3")
d200f3 <- data.frame(x1 = 200, x2 = 200, y1 = d200f2$y2, y2 = d200f2$y2 + nrow(d200f3))
#


# Plot B)
F2.2 <- ggplot()+
  geom_col_pattern(data = d30f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#FDE333FF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d30s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#FDE333FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d40f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#DFE22BFF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d40f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#DFE22BFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d40s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#DFE22BFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d50f2, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.5,  
                   pattern = 'crosshatch', fill = "#BFDE36FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d50s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#BFDE36FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d60f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#9CD948FF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d60f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#9CD948FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d60s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#9CD948FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d70f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#76D25AFF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d70f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#76D25AFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d70s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#76D25AFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d80f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#46CA6BFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d80s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#46CA6BFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d90f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#00C279FF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d90f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#00C279FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d90s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#00C279FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d100f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#00B884FF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d100f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#00B884FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d110f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#00AD8DFF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d110s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#00AD8DFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d120f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#009597FF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d120f2, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.5,  
                   pattern = 'crosshatch', fill = "#009597FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d120f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#009597FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d120s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#009597FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d130s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#008898FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d140f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#007A96FF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d140f2, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.5,  
                   pattern = 'crosshatch', fill = "#007A96FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d140f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#007A96FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d140s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#007A96FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d150f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#006B93FF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d150f2, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.5,  
                   pattern = 'crosshatch', fill = "#006B93FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d150f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#006B93FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d160f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#005C8DFF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d160f2, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.5,  
                   pattern = 'crosshatch', fill = "#005C8DFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d160s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#005C8DFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d170f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#214C85FF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d180f3, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.25,  
                   pattern = 'circle', fill = "#383B7AFF", pattern_density = 0.2, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d180f1, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'none', fill = "#383B7AFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  geom_col_pattern(data = d180s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=1,  
                   pattern = 'stripe', fill = "#383B7AFF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  geom_col_pattern(data = d200s, aes(x=x1, y=y2),inherit.aes=FALSE, show.legend = F, alpha=0.9,  
                   pattern = 'stripe', fill = "#491562FF", pattern_density = 0.1, colour = "black",
                   pattern_fill="grey90", width=8) +
  #
  scale_x_continuous(breaks=c(40,60,80,100,120,140,160,180,200), expand = c(.01,0))+
  scale_y_continuous(expand = c(0,.01), breaks = c(0,2,4,6,8,10,12)) +
  ylab("N best species-site simulations")+
  xlab("Total available water (mm)") +
  theme_classic() +
  theme(axis.text=element_text(size=12), 
        axis.title=element_text(size=14))
F2.2

# Save
ggsave("plots/figures/Fig2/Fig2.2.svg", width = 20, height = 8, units = "cm") 
#
#Arrange and legend out of R
#