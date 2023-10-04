## Spatial figure
data_complete <- merge(MME_Merged_data, species_traits, by = "species") %>% 
  dplyr::select(species, year, ecoregion, location, latitude, longitude, damaged_percentatge, damaged_qualitative, drivers_abiotic, 
                drivers_abiotic_other, drivers_biotic_group, drivers_biotic, drivers_biotic_other, `1.morphology`, 
                `2.solitary.colonial`, `3.longevity`, `4.height`, `5.energetic.resource`, `6.feeding`, `7.growth.rates`,
                `8.calcification`, `9.mobility`, `10.carbon.storage`, `structural.role`)

data_FE_sp <- table_sp_and_fe %>% dplyr::select(Species, FE) %>% rename(species = Species)
data_complete <- merge(data_complete, data_FE_sp, by = "species")

coord_PCA <- FEs_cat_dataset %>% 
  mutate(FE = recode(FEs, "fe_1" = "fe_01", "fe_2" = "fe_02", "fe_3" = "fe_03", 
                     "fe_4" = "fe_04", "fe_5" = "fe_05", "fe_6" = "fe_06", 
                     "fe_7" = "fe_07", "fe_8" = "fe_08", "fe_9" = "fe_09")) %>% 
  dplyr::select(FE, PC1, PC2, PC3, PC4, PC5, PC6)

data_complete <- merge(data_complete, coord_PCA, by = "FE")

dataset_western <- data_complete %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean"))
dataset_central <- data_complete %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra"))
dataset_eastern <- data_complete %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea"))

length(unique(data_complete$FE)) # 56 FEs in total
length(unique(dataset_western$FE)) # 44 FEs affected in the western
length(unique(dataset_central$FE)) # 33 FEs affected in the central
length(unique(dataset_eastern$FE)) # 25 FEs affected in the eastern

(stat = MME_Merged_data %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")))
length(unique(stat$species))

FE_affected_western <- unique(dataset_western$FE)
FE_affected_western_df <- data.frame(FE = FE_affected_western, FE_affected_western = rep("yes", length(unique(dataset_western$FE))))
FE_affected_central <- unique(dataset_central$FE)
FE_affected_central_df <- data.frame(FE = FE_affected_central, FE_affected_central = rep("yes", length(unique(dataset_central$FE))))
FE_affected_eastern <- unique(dataset_eastern$FE)
FE_affected_eastern_df <- data.frame(FE = FE_affected_eastern, FE_affected_eastern = rep("yes", length(unique(dataset_eastern$FE))))

data_complete <- merge(data_complete, FE_affected_western_df, by = "FE", all.x = T)
data_complete <- merge(data_complete, FE_affected_central_df, by = "FE", all.x = T)
data_complete <- merge(data_complete, FE_affected_eastern_df, by = "FE", all.x = T)

data_spatial <- data_complete %>% 
  group_by(`1.morphology`, `2.solitary.colonial`, `3.longevity`, `4.height`, `5.energetic.resource`, `6.feeding`, 
           `7.growth.rates`,`8.calcification`, `9.mobility`, `10.carbon.storage`,
           PC1, PC2, PC3, PC4, PC5, PC6, FE_affected_western, FE_affected_central, FE_affected_eastern) %>% 
  distinct(FE) 

data_spatial_ws <- data_spatial %>% dplyr::filter(FE_affected_western == "yes")
data_spatial_ct <- data_spatial %>% dplyr::filter(FE_affected_central == "yes")
data_spatial_es <- data_spatial %>% dplyr::filter(FE_affected_eastern == "yes")

conv_hull_ws = data_spatial_ws %>% data.frame %>% slice(chull(PC1, PC2))
conv_hull_ct = data_spatial_ct %>% data.frame %>% slice(chull(PC1, PC2))
conv_hull_es = data_spatial_es %>% data.frame %>% slice(chull(PC1, PC2))

str(data_spatial_ws %>% data.frame)

### Plot
PCA_Western <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.4, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_ws, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#6D4F4B") +
  geom_point(data = data_spatial, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_point(data = data_spatial_ws, aes(x = PC1, y = PC2), col = "black", fill = "#6D4F4B", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid = element_line(colour = NA)) + 
  ggtitle("Western Basin")

PCA_Central <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.4, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_ct, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#6D4F4B") +
  geom_point(data = data_spatial, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_point(data = data_spatial_ct, aes(x = PC1, y = PC2), col = "black", fill = "#6D4F4B", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid = element_line(colour = NA)) + 
  ggtitle("Central Basin")

PCA_Eastern <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.4, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_es, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#6D4F4B") +
  geom_point(data = data_spatial, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_point(data = data_spatial_es, aes(x = PC1, y = PC2), col = "black", fill = "#6D4F4B", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), panel.grid = element_line(colour = NA)) + 
  ggtitle("Eastern Basin")

(Spatial_PCA <- PCA_Western + PCA_Central + PCA_Eastern)

round(1 / (length(unique(dataset_western$FE)) / length(dataset_western$FE))) # Each 30 observations, 1FE afected observed in the western
round(1 / (length(unique(dataset_central$FE)) / length(dataset_central$FE))) # Each 12 observations, 1FE afected observed in the central
round(1 / (length(unique(dataset_eastern$FE)) / length(dataset_eastern$FE))) # Each 08 observations, 1FE afected observed in the eastern

# Barplot_Spatial
data_barplot_spatial <- data_complete %>% 
  group_by(`1.morphology`, `2.solitary.colonial`, `3.longevity`, `4.height`, `5.energetic.resource`, `6.feeding`, 
           `7.growth.rates`,`8.calcification`, `9.mobility`, `10.carbon.storage`,
           PC1, PC2, PC3, PC4, PC5, PC6, FE_affected_western, FE_affected_central, FE_affected_eastern)

Morp_tot <- data_barplot_spatial %>% group_by(`1.morphology`)         %>% summarise(morpho_tot   = n())
Soli_tot <- data_barplot_spatial %>% group_by(`2.solitary.colonial`)  %>% summarise(social_tot   = n())
Long_tot <- data_barplot_spatial %>% group_by(`3.longevity`)          %>% summarise(age_tot      = n())
Heig_tot <- data_barplot_spatial %>% group_by(`4.height`)             %>% summarise(height_tot   = n())
Ener_tot <- data_barplot_spatial %>% group_by(`5.energetic.resource`) %>% summarise(energy_tot   = n())
Feed_tot <- data_barplot_spatial %>% group_by(`6.feeding`)            %>% summarise(feeding_tot  = n())
grow_tot <- data_barplot_spatial %>% group_by(`7.growth.rates`)       %>% summarise(growth_tot   = n())
calc_tot <- data_barplot_spatial %>% group_by(`8.calcification`)      %>% summarise(calcif_tot   = n())
mobi_tot <- data_barplot_spatial %>% group_by(`9.mobility`)           %>% summarise(mobility_tot = n())
stor_tot <- data_barplot_spatial %>% group_by(`10.carbon.storage`)    %>% summarise(storage_tot  = n())

data_barplot_spatial_ws <- data_barplot_spatial %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) 
Morp_ws <- data_barplot_spatial_ws %>% group_by(`1.morphology`)         %>% summarise(morpho_ws   = n())
Soli_ws <- data_barplot_spatial_ws %>% group_by(`2.solitary.colonial`)  %>% summarise(social_ws   = n())
Long_ws <- data_barplot_spatial_ws %>% group_by(`3.longevity`)          %>% summarise(age_ws      = n())
Heig_ws <- data_barplot_spatial_ws %>% group_by(`4.height`)             %>% summarise(height_ws   = n())
Ener_ws <- data_barplot_spatial_ws %>% group_by(`5.energetic.resource`) %>% summarise(energy_ws   = n())
Feed_ws <- data_barplot_spatial_ws %>% group_by(`6.feeding`)            %>% summarise(feeding_ws  = n())
grow_ws <- data_barplot_spatial_ws %>% group_by(`7.growth.rates`)       %>% summarise(growth_ws   = n())
calc_ws <- data_barplot_spatial_ws %>% group_by(`8.calcification`)      %>% summarise(calcif_ws   = n())
mobi_ws <- data_barplot_spatial_ws %>% group_by(`9.mobility`)           %>% summarise(mobility_ws = n())
stor_ws <- data_barplot_spatial_ws %>% group_by(`10.carbon.storage`)    %>% summarise(storage_ws  = n())

Morp_ws <- merge(Morp_ws, Morp_tot, by = "1.morphology", all.y = T)
Soli_ws <- merge(Soli_ws, Soli_tot, by = "2.solitary.colonial", all.y = T)
Long_ws <- merge(Long_ws, Long_tot, by = "3.longevity", all.y = T)
Heig_ws <- merge(Heig_ws, Heig_tot, by = "4.height", all.y = T)
Ener_ws <- merge(Ener_ws, Ener_tot, by = "5.energetic.resource", all.y = T)
Feed_ws <- merge(Feed_ws, Feed_tot, by = "6.feeding", all.y = T)
grow_ws <- merge(grow_ws, grow_tot, by = "7.growth.rates", all.y = T)
calc_ws <- merge(calc_ws, calc_tot, by = "8.calcification", all.y = T)
mobi_ws <- merge(mobi_ws, mobi_tot, by = "9.mobility", all.y = T)
stor_ws <- merge(stor_ws, stor_tot, by = "10.carbon.storage", all.y = T)

data_barplot_spatial_ct <- data_barplot_spatial %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) 
Morp_ct <- data_barplot_spatial_ct %>% group_by(`1.morphology`)         %>% summarise(morpho_ct   = n())
Soli_ct <- data_barplot_spatial_ct %>% group_by(`2.solitary.colonial`)  %>% summarise(social_ct   = n())
Long_ct <- data_barplot_spatial_ct %>% group_by(`3.longevity`)          %>% summarise(age_ct      = n())
Heig_ct <- data_barplot_spatial_ct %>% group_by(`4.height`)             %>% summarise(height_ct   = n())
Ener_ct <- data_barplot_spatial_ct %>% group_by(`5.energetic.resource`) %>% summarise(energy_ct   = n())
Feed_ct <- data_barplot_spatial_ct %>% group_by(`6.feeding`)            %>% summarise(feeding_ct  = n())
grow_ct <- data_barplot_spatial_ct %>% group_by(`7.growth.rates`)       %>% summarise(growth_ct   = n())
calc_ct <- data_barplot_spatial_ct %>% group_by(`8.calcification`)      %>% summarise(calcif_ct   = n())
mobi_ct <- data_barplot_spatial_ct %>% group_by(`9.mobility`)           %>% summarise(mobility_ct = n())
stor_ct <- data_barplot_spatial_ct %>% group_by(`10.carbon.storage`)    %>% summarise(storage_ct  = n())

Morp_ct <- merge(Morp_ct, Morp_tot, by = "1.morphology", all.y = T)
Soli_ct <- merge(Soli_ct, Soli_tot, by = "2.solitary.colonial", all.y = T)
Long_ct <- merge(Long_ct, Long_tot, by = "3.longevity", all.y = T)
Heig_ct <- merge(Heig_ct, Heig_tot, by = "4.height", all.y = T)
Ener_ct <- merge(Ener_ct, Ener_tot, by = "5.energetic.resource", all.y = T)
Feed_ct <- merge(Feed_ct, Feed_tot, by = "6.feeding", all.y = T)
grow_ct <- merge(grow_ct, grow_tot, by = "7.growth.rates", all.y = T)
calc_ct <- merge(calc_ct, calc_tot, by = "8.calcification", all.y = T)
mobi_ct <- merge(mobi_ct, mobi_tot, by = "9.mobility", all.y = T)
stor_ct <- merge(stor_ct, stor_tot, by = "10.carbon.storage", all.y = T)

data_barplot_spatial_es <- data_barplot_spatial %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) 
Morp_es <- data_barplot_spatial_es %>% group_by(`1.morphology`)         %>% summarise(morpho_es   = n())
Soli_es <- data_barplot_spatial_es %>% group_by(`2.solitary.colonial`)  %>% summarise(social_es   = n())
Long_es <- data_barplot_spatial_es %>% group_by(`3.longevity`)          %>% summarise(age_es      = n())
Heig_es <- data_barplot_spatial_es %>% group_by(`4.height`)             %>% summarise(height_es   = n())
Ener_es <- data_barplot_spatial_es %>% group_by(`5.energetic.resource`) %>% summarise(energy_es   = n())
Feed_es <- data_barplot_spatial_es %>% group_by(`6.feeding`)            %>% summarise(feeding_es  = n())
grow_es <- data_barplot_spatial_es %>% group_by(`7.growth.rates`)       %>% summarise(growth_es   = n())
calc_es <- data_barplot_spatial_es %>% group_by(`8.calcification`)      %>% summarise(calcif_es   = n())
mobi_es <- data_barplot_spatial_es %>% group_by(`9.mobility`)           %>% summarise(mobility_es = n())
stor_es <- data_barplot_spatial_es %>% group_by(`10.carbon.storage`)    %>% summarise(storage_es  = n())

Morp_es <- merge(Morp_es, Morp_tot, by = "1.morphology", all.y = T)
Soli_es <- merge(Soli_es, Soli_tot, by = "2.solitary.colonial", all.y = T)
Long_es <- merge(Long_es, Long_tot, by = "3.longevity", all.y = T)
Heig_es <- merge(Heig_es, Heig_tot, by = "4.height", all.y = T)
Ener_es <- merge(Ener_es, Ener_tot, by = "5.energetic.resource", all.y = T)
Feed_es <- merge(Feed_es, Feed_tot, by = "6.feeding", all.y = T)
grow_es <- merge(grow_es, grow_tot, by = "7.growth.rates", all.y = T)
calc_es <- merge(calc_es, calc_tot, by = "8.calcification", all.y = T)
mobi_es <- merge(mobi_es, mobi_tot, by = "9.mobility", all.y = T)
stor_es <- merge(stor_es, stor_tot, by = "10.carbon.storage", all.y = T)

##

Morp_tot = cbind(Morp_ws[,1:2], morpho_ct = Morp_ct$morpho_ct, Morp_es[,2:3])
Soli_tot = cbind(Soli_ws[,1:2], social_ct = Soli_ct$social_ct, Soli_es[,2:3])
Long_tot = cbind(Long_ws[,1:2], age_ct = Long_ct$age_ct, Long_es[,2:3])
Heig_tot = cbind(Heig_ws[,1:2], height_ct = Heig_ct$height_ct, Heig_es[,2:3])
Ener_tot = cbind(Ener_ws[,1:2], energy_ct = Ener_ct$energy_ct, Ener_es[,2:3])
Feed_tot = cbind(Feed_ws[,1:2], feeding_ct = Feed_ct$feeding_ct, Feed_es[,2:3])
Grow_tot = cbind(grow_ws[,1:2], growth_ct = grow_ct$growth_ct, grow_es[,2:3])
Calc_tot = cbind(calc_ws[,1:2], calcif_ct = calc_ct$calcif_ct, calc_es[,2:3])
Mobi_tot = cbind(mobi_ws[,1:2], mobility_ct = mobi_ct$mobility_ct, mobi_es[,2:3])
Stor_tot = cbind(stor_ws[,1:2], storage_ct = stor_ct$storage_ct, stor_es[,2:3])

# Recode
Morp_tot = Morp_tot %>% mutate(`1.morphology` = recode(`1.morphology`, "b" = "1", "c" = "2", "f" = "3", "h" = "4", "i" = "5", "j" = "6", "k" = "7", "l" = "8"))
Feed_tot = Feed_tot %>% mutate(`6.feeding` = recode(`6.feeding`, "a" = "1", "b" = "2", "c" = "3", "d" = "4", "e" = "5", "f" = "6", "g" = "7"))
Stor_tot = Stor_tot %>% mutate(`10.carbon.storage` = recode(`10.carbon.storage`, "a" = "1", "b" = "2", "c" = "3"))
Calc_tot = Calc_tot %>% mutate(`8.calcification` = recode(`8.calcification`, "a" = "1", "b" = "2"))
Mobi_tot = Mobi_tot %>% mutate(`9.mobility` = recode(`9.mobility`, "a" = "1", "b" = "2"))

# Plot
Nb_of_obs <- sum(Morp_tot$morpho_tot)

#### WESTERN
# Feeding  
WT1 <- Feed_tot %>% mutate(feeding_ws_perc = (feeding_ws / Nb_of_obs)*100, 
                           feeding_ct_perc = (feeding_ct / Nb_of_obs)*100, 
                           feeding_es_perc = (feeding_es / Nb_of_obs)*100, 
                           feeding_tot_perc = (feeding_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`6.feeding`, y=feeding_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`6.feeding`, y=feeding_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Longevity  
WT2 <- Long_tot %>% mutate(age_ws_perc = (age_ws / Nb_of_obs)*100, 
                           age_ct_perc = (age_ct / Nb_of_obs)*100, 
                           age_es_perc = (age_es / Nb_of_obs)*100, 
                           age_tot_perc = (age_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`3.longevity`, y=age_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`3.longevity`, y=age_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Solitary  
WT3 <- Soli_tot %>% mutate(social_ws_perc = (social_ws / Nb_of_obs)*100, 
                           social_ct_perc = (social_ct / Nb_of_obs)*100, 
                           social_es_perc = (social_es / Nb_of_obs)*100, 
                           social_tot_perc = (social_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 30))
# Morphology
WT4 <- Morp_tot %>% mutate(morpho_ws_perc = (morpho_ws / Nb_of_obs)*100, 
                           morpho_ct_perc = (morpho_ct / Nb_of_obs)*100, 
                           morpho_es_perc = (morpho_es / Nb_of_obs)*100, 
                           morpho_tot_perc = (morpho_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`1.morphology`, y=morpho_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`1.morphology`, y=morpho_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Storage
WT5 <- Stor_tot %>% mutate(storage_ws_perc = (storage_ws / Nb_of_obs)*100, 
                           storage_ct_perc = (storage_ct / Nb_of_obs)*100, 
                           storage_es_perc = (storage_es / Nb_of_obs)*100, 
                           storage_tot_perc = (storage_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Energy
WT6 <- Ener_tot %>% mutate(energy_ws_perc = (energy_ws / Nb_of_obs)*100, 
                           energy_ct_perc = (energy_ct / Nb_of_obs)*100, 
                           energy_es_perc = (energy_es / Nb_of_obs)*100, 
                           energy_tot_perc = (energy_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 30))
# Height
WT7 <- Heig_tot %>% dplyr::filter(`4.height` != 0) %>% 
  mutate(height_ws_perc = (height_ws / Nb_of_obs)*100, 
                           height_ct_perc = (height_ct / Nb_of_obs)*100, 
                           height_es_perc = (height_es / Nb_of_obs)*100, 
                           height_tot_perc = (height_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`4.height`, y=height_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`4.height`, y=height_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Growth
WT8 <- Grow_tot %>% 
  mutate(growth_ws_perc = (growth_ws / Nb_of_obs)*100, 
         growth_ct_perc = (growth_ct / Nb_of_obs)*100, 
         growth_es_perc = (growth_es / Nb_of_obs)*100, 
         growth_tot_perc = (growth_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Calcification
WT9 <- Calc_tot %>% 
  mutate(calcif_ws_perc = (calcif_ws / Nb_of_obs)*100, 
         calcif_ct_perc = (calcif_ct / Nb_of_obs)*100, 
         calcif_es_perc = (calcif_es / Nb_of_obs)*100, 
         calcif_tot_perc = (calcif_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`8.calcification`, y=calcif_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`8.calcification`, y=calcif_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Mobility
WT10 <- Mobi_tot %>% 
  mutate(mobility_ws_perc = (mobility_ws / Nb_of_obs)*100, 
         mobility_ct_perc = (mobility_ct / Nb_of_obs)*100, 
         mobility_es_perc = (mobility_es / Nb_of_obs)*100, 
         mobility_tot_perc = (mobility_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`9.mobility`, y=mobility_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`9.mobility`, y=mobility_ws_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 30))
### All merged
WT_Panel_0 <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.4, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_ws, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "goldenrod4") +
  geom_point(data = data_spatial, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_point(data = data_spatial_ws, aes(x = PC1, y = PC2), col = "black", fill = "goldenrod4", size = 5, shape = 21) +
  theme_minimal() +  
  scale_x_continuous(name = "") + scale_y_continuous(name = "") +
  theme(legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1), 
        panel.grid = element_line(colour = NA),
        axis.text = element_blank()) 
WT_Panel_1 <- (WT1 + WT2 + WT3) + plot_layout(heights = c(7,5,2), nrow = 3) + plot_annotation(tag_levels = 'A')
WT_Panel_2 <- (WT4 + WT5 + WT6) + plot_layout(heights = c(8,3,3), nrow = 3) + plot_annotation(tag_levels = list(c("D", "E", "F")))
WT_Panel_3 <- (WT7 + WT8 + WT9 + WT10) + plot_layout(heights = c(5,5,2,2), nrow = 4) + plot_annotation(tag_levels = list(c("G", "H", "I", "J")))

plot_title <- cowplot::ggdraw() + cowplot::draw_text("Western Basin", x = 0, y = 0.25, hjust = 0, vjust = 1, angle = 0, size = 16) +
  theme_void() + theme(plot.margin = margin(20, 20, 20, 20))
WESTERN <- cowplot::plot_grid(plot_title, cowplot::plot_grid(WT_Panel_0, WT_Panel_1, WT_Panel_2, WT_Panel_3, ncol = 4, rel_widths = c(0.4, 0.2, 0.2, 0.2)), ncol = 1, rel_heights = c(0.1, 0.9))

#### CENTRAL
# Feeding  
CT1 <- Feed_tot %>% mutate(feeding_ws_perc = (feeding_ws / Nb_of_obs)*100, 
                           feeding_ct_perc = (feeding_ct / Nb_of_obs)*100, 
                           feeding_es_perc = (feeding_es / Nb_of_obs)*100, 
                           feeding_tot_perc = (feeding_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`6.feeding`, y=feeding_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`6.feeding`, y=feeding_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Longevity  
CT2 <- Long_tot %>% mutate(age_ws_perc = (age_ws / Nb_of_obs)*100, 
                           age_ct_perc = (age_ct / Nb_of_obs)*100, 
                           age_es_perc = (age_es / Nb_of_obs)*100, 
                           age_tot_perc = (age_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`3.longevity`, y=age_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`3.longevity`, y=age_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Solitary  
CT3 <- Soli_tot %>% mutate(social_ws_perc = (social_ws / Nb_of_obs)*100, 
                           social_ct_perc = (social_ct / Nb_of_obs)*100, 
                           social_es_perc = (social_es / Nb_of_obs)*100, 
                           social_tot_perc = (social_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 30))
# Morphology
CT4 <- Morp_tot %>% mutate(morpho_ws_perc = (morpho_ws / Nb_of_obs)*100, 
                           morpho_ct_perc = (morpho_ct / Nb_of_obs)*100, 
                           morpho_es_perc = (morpho_es / Nb_of_obs)*100, 
                           morpho_tot_perc = (morpho_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`1.morphology`, y=morpho_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`1.morphology`, y=morpho_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Storage
CT5 <- Stor_tot %>% mutate(storage_ws_perc = (storage_ws / Nb_of_obs)*100, 
                           storage_ct_perc = (storage_ct / Nb_of_obs)*100, 
                           storage_es_perc = (storage_es / Nb_of_obs)*100, 
                           storage_tot_perc = (storage_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Energy
CT6 <- Ener_tot %>% mutate(energy_ws_perc = (energy_ws / Nb_of_obs)*100, 
                           energy_ct_perc = (energy_ct / Nb_of_obs)*100, 
                           energy_es_perc = (energy_es / Nb_of_obs)*100, 
                           energy_tot_perc = (energy_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 30))
# Height
CT7 <- Heig_tot %>% dplyr::filter(`4.height` != 0) %>% 
  mutate(height_ws_perc = (height_ws / Nb_of_obs)*100, 
         height_ct_perc = (height_ct / Nb_of_obs)*100, 
         height_es_perc = (height_es / Nb_of_obs)*100, 
         height_tot_perc = (height_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`4.height`, y=height_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`4.height`, y=height_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Growth
CT8 <- Grow_tot %>% 
  mutate(growth_ws_perc = (growth_ws / Nb_of_obs)*100, 
         growth_ct_perc = (growth_ct / Nb_of_obs)*100, 
         growth_es_perc = (growth_es / Nb_of_obs)*100, 
         growth_tot_perc = (growth_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Calcification
CT9 <- Calc_tot %>% 
  mutate(calcif_ws_perc = (calcif_ws / Nb_of_obs)*100, 
         calcif_ct_perc = (calcif_ct / Nb_of_obs)*100, 
         calcif_es_perc = (calcif_es / Nb_of_obs)*100, 
         calcif_tot_perc = (calcif_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`8.calcification`, y=calcif_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`8.calcification`, y=calcif_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Mobility
CT10 <- Mobi_tot %>% 
  mutate(mobility_ws_perc = (mobility_ws / Nb_of_obs)*100, 
         mobility_ct_perc = (mobility_ct / Nb_of_obs)*100, 
         mobility_es_perc = (mobility_es / Nb_of_obs)*100, 
         mobility_tot_perc = (mobility_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`9.mobility`, y=mobility_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`9.mobility`, y=mobility_ct_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 30))
### All merged
CT_Panel_0 <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.4, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_ct, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "goldenrod4") +
  geom_point(data = data_spatial, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_point(data = data_spatial_ct, aes(x = PC1, y = PC2), col = "black", fill = "goldenrod4", size = 5, shape = 21) +
  theme_minimal() +  
  scale_x_continuous(name = "") + scale_y_continuous(name = "") +
  theme(legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1), 
        panel.grid = element_line(colour = NA),
        axis.text = element_blank()) 
CT_Panel_1 <- (CT1 + CT2 + CT3) + plot_layout(heights = c(7,5,2), nrow = 3) + plot_annotation(tag_levels = 'A')
CT_Panel_2 <- (CT4 + CT5 + CT6) + plot_layout(heights = c(8,3,3), nrow = 3) + plot_annotation(tag_levels = list(c("D", "E", "F")))
CT_Panel_3 <- (CT7 + CT8 + CT9 + CT10) + plot_layout(heights = c(5,5,2,2), nrow = 4) + plot_annotation(tag_levels = list(c("G", "H", "I", "J")))

plot_title <- cowplot::ggdraw() + cowplot::draw_text("Central Basin", x = 0, y = 0.25, hjust = 0, vjust = 1, angle = 0, size = 16) +
  theme_void() + theme(plot.margin = margin(20, 20, 20, 20))
CENTRAL <- cowplot::plot_grid(plot_title, cowplot::plot_grid(CT_Panel_0, CT_Panel_1, CT_Panel_2, CT_Panel_3, ncol = 4, rel_widths = c(0.4, 0.2, 0.2, 0.2)), ncol = 1, rel_heights = c(0.1, 0.9))

#### CENTRAL
# Feeding  
ES1 <- Feed_tot %>% mutate(feeding_ws_perc = (feeding_ws / Nb_of_obs)*100, 
                           feeding_ct_perc = (feeding_ct / Nb_of_obs)*100, 
                           feeding_es_perc = (feeding_es / Nb_of_obs)*100, 
                           feeding_tot_perc = (feeding_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`6.feeding`, y=feeding_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`6.feeding`, y=feeding_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Longevity  
ES2 <- Long_tot %>% mutate(age_ws_perc = (age_ws / Nb_of_obs)*100, 
                           age_ct_perc = (age_ct / Nb_of_obs)*100, 
                           age_es_perc = (age_es / Nb_of_obs)*100, 
                           age_tot_perc = (age_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`3.longevity`, y=age_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`3.longevity`, y=age_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Solitary  
ES3 <- Soli_tot %>% mutate(social_ws_perc = (social_ws / Nb_of_obs)*100, 
                           social_ct_perc = (social_ct / Nb_of_obs)*100, 
                           social_es_perc = (social_es / Nb_of_obs)*100, 
                           social_tot_perc = (social_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 30))
# Morphology
ES4 <- Morp_tot %>% mutate(morpho_ws_perc = (morpho_ws / Nb_of_obs)*100, 
                           morpho_ct_perc = (morpho_ct / Nb_of_obs)*100, 
                           morpho_es_perc = (morpho_es / Nb_of_obs)*100, 
                           morpho_tot_perc = (morpho_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`1.morphology`, y=morpho_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`1.morphology`, y=morpho_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Storage
ES5 <- Stor_tot %>% mutate(storage_ws_perc = (storage_ws / Nb_of_obs)*100, 
                           storage_ct_perc = (storage_ct / Nb_of_obs)*100, 
                           storage_es_perc = (storage_es / Nb_of_obs)*100, 
                           storage_tot_perc = (storage_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Energy
ES6 <- Ener_tot %>% mutate(energy_ws_perc = (energy_ws / Nb_of_obs)*100, 
                           energy_ct_perc = (energy_ct / Nb_of_obs)*100, 
                           energy_es_perc = (energy_es / Nb_of_obs)*100, 
                           energy_tot_perc = (energy_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 30))
# Height
ES7 <- Heig_tot %>% dplyr::filter(`4.height` != 0) %>% 
  mutate(height_ws_perc = (height_ws / Nb_of_obs)*100, 
         height_ct_perc = (height_ct / Nb_of_obs)*100, 
         height_es_perc = (height_es / Nb_of_obs)*100, 
         height_tot_perc = (height_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`4.height`, y=height_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`4.height`, y=height_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Growth
ES8 <- Grow_tot %>% 
  mutate(growth_ws_perc = (growth_ws / Nb_of_obs)*100, 
         growth_ct_perc = (growth_ct / Nb_of_obs)*100, 
         growth_es_perc = (growth_es / Nb_of_obs)*100, 
         growth_tot_perc = (growth_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Calcification
ES9 <- Calc_tot %>% 
  mutate(calcif_ws_perc = (calcif_ws / Nb_of_obs)*100, 
         calcif_ct_perc = (calcif_ct / Nb_of_obs)*100, 
         calcif_es_perc = (calcif_es / Nb_of_obs)*100, 
         calcif_tot_perc = (calcif_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`8.calcification`, y=calcif_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`8.calcification`, y=calcif_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        text = element_text(size = 30))
# Mobility
ES10 <- Mobi_tot %>% 
  mutate(mobility_ws_perc = (mobility_ws / Nb_of_obs)*100, 
         mobility_ct_perc = (mobility_ct / Nb_of_obs)*100, 
         mobility_es_perc = (mobility_es / Nb_of_obs)*100, 
         mobility_tot_perc = (mobility_tot / Nb_of_obs)*100) %>% 
  ggplot( ) +
  coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`9.mobility`, y=mobility_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`9.mobility`, y=mobility_es_perc), fill = "goldenrod4", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25,75,25), name = "", limits = c(0,100)) +
  scale_x_discrete(name = "") +
  theme(axis.line = element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 30))
### All merged
ES_Panel_0 <- ggplot() +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.4, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_es, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "goldenrod4") +
  geom_point(data = data_spatial, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_point(data = data_spatial_es, aes(x = PC1, y = PC2), col = "black", fill = "goldenrod4", size = 5, shape = 21) +
  theme_minimal() +  
  scale_x_continuous(name = "") + scale_y_continuous(name = "") +
  theme(legend.position = "none", 
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1), 
        panel.grid = element_line(colour = NA),
        axis.text = element_blank()) 
ES_Panel_1 <- (ES1 + ES2 + ES3) + plot_layout(heights = c(7,5,2), nrow = 3) + plot_annotation(tag_levels = 'A')
ES_Panel_2 <- (ES4 + ES5 + ES6) + plot_layout(heights = c(8,3,3), nrow = 3) + plot_annotation(tag_levels = list(c("D", "E", "F")))
ES_Panel_3 <- (ES7 + ES8 + ES9 + ES10) + plot_layout(heights = c(5,5,2,2), nrow = 4) + plot_annotation(tag_levels = list(c("G", "H", "I", "J")))

plot_title <- cowplot::ggdraw() + cowplot::draw_text("Eastern Basin", x = 0, y = 0.25, hjust = 0, vjust = 1, angle = 0, size = 16) +
  theme_void() + theme(plot.margin = margin(20, 20, 20, 20))
EASTERN <- cowplot::plot_grid(plot_title, cowplot::plot_grid(ES_Panel_0, ES_Panel_1, ES_Panel_2, ES_Panel_3, ncol = 4, rel_widths = c(0.4, 0.2, 0.2, 0.2)), ncol = 1, rel_heights = c(0.1, 0.9))

### Save Figures
# ggsave(WESTERN, filename = "../Figures/Figure_5_West.png", device = "png", width = 18.00, height = 9.50, units = "in", dpi = 300)
# ggsave(CENTRAL, filename = "../Figures/Figure_5_Cent.png", device = "png", width = 18.00, height = 9.50, units = "in", dpi = 300)
# ggsave(EASTERN, filename = "../Figures/Figure_5_East.png", device = "png", width = 18.00, height = 9.50, units = "in", dpi = 300)

