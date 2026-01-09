#Libreraias
library(tidyverse)
library(ggplot2)
library(ggpubr)
# Load data
data <- readRDS("data/evaluation_table_best.rds")
#Subset repeated spicies site combinations
data <- subset(data, data$site != "D07S1" | data$species!="Arbutus unedo" | lai_source != "ALLOM")
data <- subset(data, data$site != "D07S1" | data$species!="Buxus sempervirens" | lai_source != "ALLOM")
data <- subset(data, data$site != "D07S1" | data$species!="Hippocrepis emerus" | lai_source != "ALLOM")
data <- subset(data, data$site != "D48S1" | data$species!="Cytisus oromediterraneus" | lai_source != "ALLOM")
data <- subset(data, data$site != "D48S1" | data$species!="Erica cinerea" | lai_source != "ALLOM")
#Agregate
# Group by specie
# data <- data %>% group_by(lfmc) %>% summarise(Avg = mean(r2))
# data$r2 <- data$Avg
# Add plant trait imputed variable
sp <- levels(as.factor(data$species))
new <- data.frame(
  species = as.factor(sp),
  LeafP10_imp = NA,
  VcStemP50_imp = NA
)
new$LeafP10_imp <- c("YES","NO","NO","YES","NO","NO","YES","YES","NO","NO","YES","NO","NO","NO","YES","YES","NO","NO","NO","NO")
new$VcStemP50_imp <- c("YES","NO","NO","YES","NO","NO","YES","YES","NO","NO","YES","NO","NO","NO","YES","YES","NO","NO","NO","NO")
data$Trait_imp <- NA
for (i in 1:nrow(data)) {
  sp.name <- data$species[i]
  val <- subset(new, new$species==sp.name)
  data$Trait_imp[i] <- val$LeafP10_imp
}


# Subset data plant traits imputed Yes
data_yes <- subset(data, data$Trait_imp == "YES")
data_yes_semi <- subset(data_yes, data_yes$lfmc == "semi")
data_yes_lfmc1 <- subset(data_yes, data_yes$lfmc == "full1")
data_yes_lfmc2 <- subset(data_yes, data_yes$lfmc == "full2")
data_yes_lfmc3 <- subset(data_yes, data_yes$lfmc == "full3")
# Subset data plant traits imputed NO
data_no <- subset(data, data$Trait_imp == "NO")
data_no_semi <- subset(data_no, data_no$lfmc == "semi")
data_no_lfmc1 <- subset(data_no, data_no$lfmc == "full1")
data_no_lfmc2 <- subset(data_no, data_no$lfmc == "full2")
data_no_lfmc3 <- subset(data_no, data_no$lfmc == "full3")
# Significant differences in R2 p-values
t.test(x = data_yes_semi$r2, y = data_no_semi$r2, alternative = "less")
t.test(x = data_yes_lfmc1$r2, y = data_no_lfmc1$r2, , alternative = "less")
t.test(x = data_yes_lfmc2$r2, y = data_no_lfmc2$r2, , alternative = "less")
t.test(x = data_yes_lfmc3$r2, y = data_no_lfmc3$r2, , alternative = "less")

# Rename mechanistic approaches order
data <- data %>% mutate(lfmc = case_when(
  lfmc == "semi" ~ "1_Semi",
  lfmc == "full1" ~ "Full-1",
  lfmc == "full2" ~ "Full-2",
  lfmc == "full3" ~ "Full-3",
  TRUE ~ "NA"))

# Plot R2
ggplot(data, aes(x = lfmc, y = r2, fill = interaction(lfmc, Trait_imp), group = interaction(lfmc, Trait_imp))) +
  geom_boxplot(color="black",  width=0.65, position = position_dodge(0.75), show.legend = T) +
  #geom_jitter(alpha=0.5, aes(colour  = interaction(lfmc, Trait_imp)))+
  xlab("LFMC mdeling approach") +
  ylab(expression(paste("Coeficient of determination (", R^2 , ")")))+
  scale_y_continuous(expand = c(0,0.05))+
  scale_fill_manual(values = c("#FDE333FF","#00BE7DFF","#007094FF","#4B0055FF" ,"grey90","grey90","grey90","grey90")) +
  #scale_colour_manual(values = c("#00BE7DFF","#007094FF", "#4B0055FF" ,"#FDE333FF","#FFFFE0","#FFFFE0","#FFFFE0","#FFFFE0")) +
  geom_bracket(data = data, aes(xmin = 0.75, xmax = 1.25), y.position = 0.8, label = "p = 0.37") +
  geom_bracket(data = data, aes(xmin = 1.75, xmax = 2.25), y.position = 0.8, label = "p = 0.32") +
  geom_bracket(data = data, aes(xmin = 2.75, xmax = 3.25), y.position = 0.8, label = "p = 0.49") +
  geom_bracket(data = data, aes(xmin = 3.75, xmax = 4.25), y.position = 0.8, label = "p = 0.01*") +
  theme_classic()+
  theme(text = element_text(size=16), axis.title = element_text(size=14))
# Save plot
ggsave("plots/figures/Fig3/Fig3.png", width = 20, height = 10, units = "cm") 


## Supplementary Figure S3
# values p-valor
t.test(x = data_yes_semi$MAE, y = data_no_semi$MAE, alternative = "less")
t.test(x = data_yes_lfmc1$MAE, y = data_no_lfmc1$MAE, , alternative = "less")
t.test(x = data_yes_lfmc2$MAE, y = data_no_lfmc2$MAE, , alternative = "less")
t.test(x = data_yes_lfmc3$MAE, y = data_no_lfmc3$MAE, , alternative = "less")
t.test(x = data_yes$MAE, y = data_no$MAE, , alternative = "less")
# Plot MAE
ggplot(data, aes(x = lfmc, y = MAE, fill = interaction(lfmc, Trait_imp), group = interaction(lfmc, Trait_imp))) +
  geom_boxplot(color="black",  width=0.65, position = position_dodge(0.75), show.legend = T) +
  #geom_jitter(alpha=0.5, aes(colour  = interaction(lfmc, Trait_imp)))+
  xlab("LFMC mdeling approach") +
  ylab("Mean Absolute Error (MAE)")+
  scale_y_continuous(expand = c(0,0.05), limits = c(0,35))+
  scale_fill_manual(values = c("#FDE333FF","#00BE7DFF","#007094FF","#4B0055FF" ,"grey90","grey90","grey90","grey90")) +
  #scale_colour_manual(values = c("#00BE7DFF","#007094FF", "#4B0055FF" ,"#FDE333FF","#FFFFE0","#FFFFE0","#FFFFE0","#FFFFE0")) +
  geom_bracket(data = data, aes(xmin = 0.75, xmax = 1.25), y.position = 32, label = "p = 0.69") +
  geom_bracket(data = data, aes(xmin = 1.75, xmax = 2.25), y.position = 32, label = "p = 0.26") +
  geom_bracket(data = data, aes(xmin = 2.75, xmax = 3.25), y.position = 32, label = "p = 0.12") +
  geom_bracket(data = data, aes(xmin = 3.75, xmax = 4.25), y.position = 32, label = "p = 0.53") +
  theme_classic()+
  theme(text = element_text(size=16), axis.title = element_text(size=14))

# Save plot
ggsave("plots/figures/Suppleentary/FigS3.png", width = 20, height = 10, units = "cm") 