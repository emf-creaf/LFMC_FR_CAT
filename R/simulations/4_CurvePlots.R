library(medfate)
library(ggplot2)

SpParams_imputed <- readxl::read_excel("data/inputs/SpParamsAlbert.xlsx", sheet = "SpParams_final_imputed") |> 
  mutate(Name = ifelse(Name == "Erica scoparia subsp. scoparia", "Erica scoparia", Name))|> 
  mutate(Name = ifelse(Name == "Juniperus oxycedrus subsp. oxycedrus", "Juniperus oxycedrus", Name)) |>
  mutate(LeafPI0 = as.numeric(LeafPI0))

n_sp <- nrow(SpParams_imputed)
psi_seq <- seq(0, -15, by=-0.1)

df <- data.frame(species = gl(n_sp, length(psi_seq), labels = SpParams_imputed$Name), 
                 Psi = rep(psi_seq, n_sp)) |>
  dplyr::left_join(SpParams_imputed[,c("Name", "VCstem_P50", "VCstem_slope", "LeafPI0", "LeafEPS", "StemPI0", "StemEPS")], by=c("species"="Name")) |>
  dplyr::mutate(PLC = 0, LeafRWC = 0, StemRWC = 0)
for(i in 1:nrow(df)) {
  df$PLC[i] = 100*(1 - hydraulics_xylemConductanceSigmoid(df$Psi[i], 1.0, df$VCstem_P50[i], df$VCstem_slope[i]))
  df$LeafRWC[i] = 100*moisture_symplasticRWC(df$Psi[i], df$LeafPI0[i], df$LeafEPS[i])
  df$StemRWC[i] = 100*moisture_symplasticRWC(df$Psi[i], df$StemPI0[i], df$StemEPS[i])
}

trees <- c("Acacia dealbata", "Buxus sempervirens", "Quercus ilex", "Pinus halepensis", "Arbutus unedo", 
           "Juniperus oxycedrus", "Erica arborea")
resprouters <- c("Erica cinerea", "Erica scoparia",
                 "Genista cinerea", "Genista scorpius", "Hippocrepis emerus", "Quercus coccifera")
seeders <- c("Calicotome spinosa", "Cistus albidus", "Cistus monspeliensis", "Cytisophyllum sessilifolium",
             "Cytisus oromediterraneus", "Cytisus scoparius", "Rosmarinus officinalis")
plc_trees <- ggplot(data =df[df$species %in% trees, ])+
  geom_line(aes(x = -Psi, y = PLC, col = species), linewidth = 1.1) +
  theme_bw()+
  xlab("Water potential (-MPa)")+ylab("PLC (%)")+ labs(title="Hydraulic vulnerability curves")+
  theme(legend.position = "inside", legend.position.inside = c(0.85,0.25), legend.title = element_blank(),
        legend.background = element_blank())
plc_resp <- ggplot(data =df[df$species %in% resprouters, ])+
  geom_line(aes(x = -Psi, y = PLC, col = species), linewidth = 1.1) +
  theme_bw()+
  xlab("Water potential (-MPa)")+ylab("PLC (%)")+ labs(title=" ")+
  theme(legend.position = "inside", legend.position.inside = c(0.80,0.25), legend.title = element_blank(),
        legend.background = element_blank())
plc_seed <- ggplot(data =df[df$species %in% seeders, ])+
  geom_line(aes(x = -Psi, y = PLC, col = species), linewidth = 1.1) +
  theme_bw()+
  xlab("Water potential (-MPa)")+ylab("PLC (%)")+labs(title=" ")+
  theme(legend.position = "inside", legend.position.inside = c(0.80,0.25), legend.title = element_blank(),
        legend.background = element_blank())

rwcleaf_trees <- ggplot(data =df[df$species %in% trees & df$Psi > -10, ])+
  geom_line(aes(x = -Psi, y = LeafRWC, col = species), linewidth = 1.1) +
  theme_bw()+
  xlab("Water potential (-MPa)")+ylab("Leaf RWC (%)")+ labs(title="Leaf pressure-volume curves")+
  theme(legend.position = "inside", legend.position.inside = c(0.75,0.75), legend.title = element_blank(),
        legend.background = element_blank())
rwcleaf_resp <- ggplot(data =df[df$species %in% resprouters & df$Psi > -10, ])+
  geom_line(aes(x = -Psi, y = LeafRWC, col = species), linewidth = 1.1) +
  theme_bw()+
  xlab("Water potential (-MPa)")+ylab("Leaf RWC (%)")+ labs(title=" ")+
  theme(legend.position = "inside", legend.position.inside = c(0.75,0.75), legend.title = element_blank(),
        legend.background = element_blank())
rwcleaf_seed <- ggplot(data =df[df$species %in% seeders & df$Psi > -10, ])+
  geom_line(aes(x = -Psi, y = LeafRWC, col = species), linewidth = 1.1) +
  theme_bw()+
  xlab("Water potential (-MPa)")+ylab("Leaf RWC (%)")+labs(title=" ")+
  theme(legend.position = "inside", legend.position.inside = c(0.75,0.75), legend.title = element_blank(),
        legend.background = element_blank())


rwcstem_trees <- ggplot(data =df[df$species %in% trees & df$Psi > -10, ])+
  geom_line(aes(x = -Psi, y = StemRWC, col = species), linewidth = 1.1) +
  theme_bw()+
  xlab("Water potential (-MPa)")+ylab("Stem RWC (%)")+ labs(title="Stem pressure-volume curves")+
  theme(legend.position = "inside", legend.position.inside = c(0.75,0.75), legend.title = element_blank(),
        legend.background = element_blank())
rwcstem_resp <- ggplot(data =df[df$species %in% resprouters & df$Psi > -10, ])+
  geom_line(aes(x = -Psi, y = StemRWC, col = species), linewidth = 1.1) +
  theme_bw()+
  xlab("Water potential (-MPa)")+ylab("Stem RWC (%)")+ labs(title=" ")+
  theme(legend.position = "inside", legend.position.inside = c(0.75,0.75), legend.title = element_blank(),
        legend.background = element_blank())
rwcstem_seed <- ggplot(data =df[df$species %in% seeders & df$Psi > -10, ])+
  geom_line(aes(x = -Psi, y = StemRWC, col = species), linewidth = 1.1) +
  theme_bw()+
  xlab("Water potential (-MPa)")+ylab("Stem RWC (%)")+labs(title=" ")+
  theme(legend.position = "inside", legend.position.inside = c(0.75,0.75), legend.title = element_blank(),
        legend.background = element_blank())

p <- cowplot::plot_grid(plc_trees, plc_resp, plc_seed, 
                   rwcleaf_trees, rwcleaf_resp, rwcleaf_seed,
                   rwcstem_trees, rwcstem_resp, rwcstem_seed, 
                   nrow=3, ncol=3)

ggsave("plots/curve_plots.png", p, width = 20, height = 15)
