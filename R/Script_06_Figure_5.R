#### Figure 5
data_complete <- merge(MME_Merged_data, species_traits, by = "species") %>% 
  dplyr::select(species, year, ecoregion, location, latitude, longitude, damaged_percentatge, damaged_qualitative, drivers_abiotic, 
                drivers_abiotic_other, drivers_biotic_group, drivers_biotic, drivers_biotic_other, `1.morphology`, 
                `2.solitary.colonial`, `3.longevity`, `4.height`, `5.energetic.resource`, `6.feeding`, `7.growth.rates`,
                `8.calcification`, `9.mobility`, `10.carbon.storage`, `structural.role`)
data_FE_sp    <- table_sp_and_fe %>% dplyr::select(Species, FE) %>% rename(species = Species)
data_complete <- merge(data_complete, data_FE_sp, by = "species")

coord_PCA     <- FEs_cat_dataset %>% mutate(FE = recode(FEs, "fe_1" = "fe_01", "fe_2" = "fe_02", "fe_3" = "fe_03", "fe_4" = "fe_04", "fe_5" = "fe_05", 
                                                        "fe_6" = "fe_06", "fe_7" = "fe_07", "fe_8" = "fe_08", "fe_9" = "fe_09")) %>% 
  dplyr::select(FE, PC1, PC2, PC3, PC4, PC5, PC6)
data_complete <- merge(data_complete, coord_PCA, by = "FE")
# Subset by ecoregions
dataset_western <- data_complete %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean"))
dataset_central <- data_complete %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra"))
dataset_eastern <- data_complete %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea"))
# define the number of Affected FEs
length(unique(data_complete$FE))   # 56 FEs in total
length(unique(dataset_western$FE)) # 44 FEs affected in the western
length(unique(dataset_central$FE)) # 33 FEs affected in the central
length(unique(dataset_eastern$FE)) # 25 FEs affected in the eastern
# Prepare datasets
FE_affected_western_df <- data.frame(FE = unique(dataset_western$FE), FE_affected_western = rep("yes", length(unique(dataset_western$FE))))
FE_affected_central_df <- data.frame(FE = unique(dataset_central$FE), FE_affected_central = rep("yes", length(unique(dataset_central$FE))))
FE_affected_eastern_df <- data.frame(FE = unique(dataset_eastern$FE), FE_affected_eastern = rep("yes", length(unique(dataset_eastern$FE))))
data_complete          <- merge(data_complete, FE_affected_western_df, by = "FE", all.x = T)
data_complete          <- merge(data_complete, FE_affected_central_df, by = "FE", all.x = T)
data_complete          <- merge(data_complete, FE_affected_eastern_df, by = "FE", all.x = T)
data_spatial           <- data_complete %>% group_by(`1.morphology`, `2.solitary.colonial`, `3.longevity`, `4.height`, `5.energetic.resource`, `6.feeding`, 
           `7.growth.rates`,`8.calcification`, `9.mobility`, `10.carbon.storage`, PC1, PC2, PC3, PC4, PC5, PC6, FE_affected_western, FE_affected_central, 
           FE_affected_eastern) %>% distinct(FE) 
data_spatial$FE_affected_western[is.na(data_spatial$FE_affected_western)] = "no"
data_spatial$FE_affected_central[is.na(data_spatial$FE_affected_central)] = "no"
data_spatial$FE_affected_eastern[is.na(data_spatial$FE_affected_eastern)] = "no"
data_spatial_ws <- data_spatial %>% dplyr::filter(FE_affected_western == "yes")
data_spatial_ct <- data_spatial %>% dplyr::filter(FE_affected_central == "yes")
data_spatial_es <- data_spatial %>% dplyr::filter(FE_affected_eastern == "yes")
conv_hull_ws    <- data_spatial_ws %>% data.frame %>% slice(chull(PC1, PC2))
conv_hull_ct    <- data_spatial_ct %>% data.frame %>% slice(chull(PC1, PC2))
conv_hull_es    <- data_spatial_es %>% data.frame %>% slice(chull(PC1, PC2))

### Figure 5C
Figure_5C1 <- ggplot() + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white", linewidth = 1) +
  geom_polygon(data = conv_hull_ws, aes(x = PC1, y = PC2), alpha = .95, fill = "#CC3399", linewidth = 2, color = "#CC0099") +
  geom_point(data = data_spatial, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 12, shape = 21, alpha = .75) +
  geom_point(data = data_spatial_ws, aes(x = PC1, y = PC2), col = "black", fill = "#CC0099", size = 12, shape = 21) +
  theme_minimal() + scale_x_continuous(name = "") + scale_y_continuous(name = "") +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), 
        panel.grid = element_line(colour = NA), axis.text = element_blank()) 

Figure_5C2 <- ggplot() + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_ct, aes(x = PC1, y = PC2), alpha = .95, fill = "#CC3399", linewidth = 2, color = "#CC0099") +
  geom_point(data = data_spatial, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 12, shape = 21, alpha = .75) +
  geom_point(data = data_spatial_ct, aes(x = PC1, y = PC2), col = "black", fill = "#CC0099", size = 12, shape = 21) +
  theme_minimal() +  scale_x_continuous(name = "") + scale_y_continuous(name = "") +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), 
        panel.grid = element_line(colour = NA), axis.text = element_blank()) 

Figure_5C3 <- ggplot() + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_es, aes(x = PC1, y = PC2), alpha = .95, fill = "#CC3399", linewidth = 2, color = "#CC0099") +
  geom_point(data = data_spatial, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 12, shape = 21, alpha = .75) +
  geom_point(data = data_spatial_es, aes(x = PC1, y = PC2), col = "black", fill = "#CC0099", size = 12, shape = 21) +
  theme_minimal() +  scale_x_continuous(name = "") + scale_y_continuous(name = "") +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), 
        panel.grid = element_line(colour = NA), axis.text = element_blank()) 

### Figure 5B
# General
data_barplot_spatial <- data_complete %>% group_by(`1.morphology`, `2.solitary.colonial`, `3.longevity`, `4.height`, `5.energetic.resource`, `6.feeding`, 
           `7.growth.rates`,`8.calcification`, `9.mobility`, `10.carbon.storage`, PC1, PC2, PC3, PC4, PC5, PC6, FE_affected_western, FE_affected_central, 
           FE_affected_eastern)
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
# Western
data_barplot_spatial_ws <- data_barplot_spatial %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) 
Morp_ws   <- merge(data_barplot_spatial_ws %>% group_by(`1.morphology`)         %>% summarise(morpho_ws   = n()), Morp_tot, by = "1.morphology",         all.y = T)
Soli_ws   <- merge(data_barplot_spatial_ws %>% group_by(`2.solitary.colonial`)  %>% summarise(social_ws   = n()), Soli_tot, by = "2.solitary.colonial",  all.y = T)
Long_ws   <- merge(data_barplot_spatial_ws %>% group_by(`3.longevity`)          %>% summarise(age_ws      = n()), Long_tot, by = "3.longevity",          all.y = T)
Heig_ws   <- merge(data_barplot_spatial_ws %>% group_by(`4.height`)             %>% summarise(height_ws   = n()), Heig_tot, by = "4.height",             all.y = T)
Ener_ws   <- merge(data_barplot_spatial_ws %>% group_by(`5.energetic.resource`) %>% summarise(energy_ws   = n()), Ener_tot, by = "5.energetic.resource", all.y = T)
Feed_ws   <- merge(data_barplot_spatial_ws %>% group_by(`6.feeding`)            %>% summarise(feeding_ws  = n()), Feed_tot, by = "6.feeding",            all.y = T)
grow_ws   <- merge(data_barplot_spatial_ws %>% group_by(`7.growth.rates`)       %>% summarise(growth_ws   = n()), grow_tot, by = "7.growth.rates",       all.y = T)
calc_ws   <- merge(data_barplot_spatial_ws %>% group_by(`8.calcification`)      %>% summarise(calcif_ws   = n()), calc_tot, by = "8.calcification",      all.y = T)
mobi_ws   <- merge(data_barplot_spatial_ws %>% group_by(`9.mobility`)           %>% summarise(mobility_ws = n()), mobi_tot, by = "9.mobility",           all.y = T)
stor_ws   <- merge(data_barplot_spatial_ws %>% group_by(`10.carbon.storage`)    %>% summarise(storage_ws  = n()), stor_tot, by = "10.carbon.storage",    all.y = T)
# Central
data_barplot_spatial_ct <- data_barplot_spatial %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) 
Morp_ct   <- merge(data_barplot_spatial_ct %>% group_by(`1.morphology`)         %>% summarise(morpho_ct   = n()), Morp_tot, by = "1.morphology",         all.y = T)
Soli_ct   <- merge(data_barplot_spatial_ct %>% group_by(`2.solitary.colonial`)  %>% summarise(social_ct   = n()), Soli_tot, by = "2.solitary.colonial",  all.y = T)
Long_ct   <- merge(data_barplot_spatial_ct %>% group_by(`3.longevity`)          %>% summarise(age_ct      = n()), Long_tot, by = "3.longevity",          all.y = T)
Heig_ct   <- merge(data_barplot_spatial_ct %>% group_by(`4.height`)             %>% summarise(height_ct   = n()), Heig_tot, by = "4.height",             all.y = T)
Ener_ct   <- merge(data_barplot_spatial_ct %>% group_by(`5.energetic.resource`) %>% summarise(energy_ct   = n()), Ener_tot, by = "5.energetic.resource", all.y = T)
Feed_ct   <- merge(data_barplot_spatial_ct %>% group_by(`6.feeding`)            %>% summarise(feeding_ct  = n()), Feed_tot, by = "6.feeding",            all.y = T)
grow_ct   <- merge(data_barplot_spatial_ct %>% group_by(`7.growth.rates`)       %>% summarise(growth_ct   = n()), grow_tot, by = "7.growth.rates",       all.y = T)
calc_ct   <- merge(data_barplot_spatial_ct %>% group_by(`8.calcification`)      %>% summarise(calcif_ct   = n()), calc_tot, by = "8.calcification",      all.y = T)
mobi_ct   <- merge(data_barplot_spatial_ct %>% group_by(`9.mobility`)           %>% summarise(mobility_ct = n()), mobi_tot, by = "9.mobility",           all.y = T)
stor_ct   <- merge(data_barplot_spatial_ct %>% group_by(`10.carbon.storage`)    %>% summarise(storage_ct  = n()), stor_tot, by = "10.carbon.storage",    all.y = T)
# Eastern
data_barplot_spatial_es <- data_barplot_spatial %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) 
Morp_es   <- merge(data_barplot_spatial_es %>% group_by(`1.morphology`)         %>% summarise(morpho_es   = n()), Morp_tot, by = "1.morphology",         all.y = T)
Soli_es   <- merge(data_barplot_spatial_es %>% group_by(`2.solitary.colonial`)  %>% summarise(social_es   = n()), Soli_tot, by = "2.solitary.colonial",  all.y = T)
Long_es   <- merge(data_barplot_spatial_es %>% group_by(`3.longevity`)          %>% summarise(age_es      = n()), Long_tot, by = "3.longevity",          all.y = T)
Heig_es   <- merge(data_barplot_spatial_es %>% group_by(`4.height`)             %>% summarise(height_es   = n()), Heig_tot, by = "4.height",             all.y = T)
Ener_es   <- merge(data_barplot_spatial_es %>% group_by(`5.energetic.resource`) %>% summarise(energy_es   = n()), Ener_tot, by = "5.energetic.resource", all.y = T)
Feed_es   <- merge(data_barplot_spatial_es %>% group_by(`6.feeding`)            %>% summarise(feeding_es  = n()), Feed_tot, by = "6.feeding",            all.y = T)
grow_es   <- merge(data_barplot_spatial_es %>% group_by(`7.growth.rates`)       %>% summarise(growth_es   = n()), grow_tot, by = "7.growth.rates",       all.y = T)
calc_es   <- merge(data_barplot_spatial_es %>% group_by(`8.calcification`)      %>% summarise(calcif_es   = n()), calc_tot, by = "8.calcification",      all.y = T)
mobi_es   <- merge(data_barplot_spatial_es %>% group_by(`9.mobility`)           %>% summarise(mobility_es = n()), mobi_tot, by = "9.mobility",           all.y = T)
stor_es   <- merge(data_barplot_spatial_es %>% group_by(`10.carbon.storage`)    %>% summarise(storage_es  = n()), stor_tot, by = "10.carbon.storage",    all.y = T)
# Combine everything again
Morp_tot  <- cbind(Morp_ws[,1:2], morpho_ct   = Morp_ct$morpho_ct,   Morp_es[,2:3])
Soli_tot  <- cbind(Soli_ws[,1:2], social_ct   = Soli_ct$social_ct,   Soli_es[,2:3])
Long_tot  <- cbind(Long_ws[,1:2], age_ct      = Long_ct$age_ct,      Long_es[,2:3])
Heig_tot  <- cbind(Heig_ws[,1:2], height_ct   = Heig_ct$height_ct,   Heig_es[,2:3])
Ener_tot  <- cbind(Ener_ws[,1:2], energy_ct   = Ener_ct$energy_ct,   Ener_es[,2:3])
Feed_tot  <- cbind(Feed_ws[,1:2], feeding_ct  = Feed_ct$feeding_ct,  Feed_es[,2:3])
Grow_tot  <- cbind(grow_ws[,1:2], growth_ct   = grow_ct$growth_ct,   grow_es[,2:3])
Calc_tot  <- cbind(calc_ws[,1:2], calcif_ct   = calc_ct$calcif_ct,   calc_es[,2:3])
Mobi_tot  <- cbind(mobi_ws[,1:2], mobility_ct = mobi_ct$mobility_ct, mobi_es[,2:3])
Stor_tot  <- cbind(stor_ws[,1:2], storage_ct  = stor_ct$storage_ct,  stor_es[,2:3])
# Recode
Morp_tot  <- Morp_tot %>% mutate(`1.morphology` = recode(`1.morphology`, "b" = "a", "c" = "b", "f" = "c", "h" = "d", "i" = "e", "j" = "f", "k" = "g", "l" = "h"))
Nb_of_obs <- sum(Morp_tot$morpho_tot)

## Figure 5B1
F5Ba1 <- Feed_tot %>% mutate(feeding_ws_perc = (feeding_ws / Nb_of_obs) * 100, feeding_ct_perc = (feeding_ct / Nb_of_obs) * 100, 
                           feeding_es_perc = (feeding_es / Nb_of_obs) * 100, feeding_tot_perc = (feeding_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`6.feeding`, y=feeding_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`6.feeding`, y=feeding_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), text = element_text(size = 30))
F5Bb1 <- Long_tot %>% mutate(age_ws_perc = (age_ws / Nb_of_obs) * 100, age_ct_perc = (age_ct / Nb_of_obs) * 100,  age_es_perc = (age_es / Nb_of_obs) * 100, 
                           age_tot_perc = (age_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`3.longevity`, y=age_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`3.longevity`, y=age_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), text = element_text(size = 30))
F5Bc1 <- Soli_tot %>% mutate(social_ws_perc = (social_ws / Nb_of_obs) * 100, social_ct_perc = (social_ct / Nb_of_obs) * 100, 
                           social_es_perc = (social_es / Nb_of_obs) * 100, social_tot_perc = (social_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bd1 <- Morp_tot %>% mutate(morpho_ws_perc = (morpho_ws / Nb_of_obs) * 100, morpho_ct_perc = (morpho_ct / Nb_of_obs) * 100, 
                           morpho_es_perc = (morpho_es / Nb_of_obs) * 100, morpho_tot_perc = (morpho_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`1.morphology`, y=morpho_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`1.morphology`, y=morpho_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Be1 <- Stor_tot %>% mutate(storage_ws_perc = (storage_ws / Nb_of_obs) * 100, storage_ct_perc = (storage_ct / Nb_of_obs) * 100, 
                           storage_es_perc = (storage_es / Nb_of_obs) * 100, storage_tot_perc = (storage_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bf1 <- Ener_tot %>% mutate(energy_ws_perc = (energy_ws / Nb_of_obs) * 100, energy_ct_perc = (energy_ct / Nb_of_obs) * 100, 
                           energy_es_perc = (energy_es / Nb_of_obs) * 100, energy_tot_perc = (energy_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bg1 <- Heig_tot %>% dplyr::filter(`4.height` != 0) %>% mutate(height_ws_perc = (height_ws / Nb_of_obs) * 100, height_ct_perc = (height_ct / Nb_of_obs) * 100, 
                           height_es_perc = (height_es / Nb_of_obs) * 100, height_tot_perc = (height_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`4.height`, y=height_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`4.height`, y=height_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bh1 <- Grow_tot %>% mutate(growth_ws_perc = (growth_ws / Nb_of_obs)*100, growth_ct_perc = (growth_ct / Nb_of_obs)*100, 
                             growth_es_perc = (growth_es / Nb_of_obs)*100, growth_tot_perc = (growth_tot / Nb_of_obs)*100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bi1 <- Calc_tot %>% mutate(calcif_ws_perc = (calcif_ws / Nb_of_obs) * 100, calcif_ct_perc = (calcif_ct / Nb_of_obs) * 100, 
         calcif_es_perc = (calcif_es / Nb_of_obs) * 100, calcif_tot_perc = (calcif_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`8.calcification`, y=calcif_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`8.calcification`, y=calcif_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bj1 <- Mobi_tot %>% mutate(mobility_ws_perc = (mobility_ws / Nb_of_obs) * 100, mobility_ct_perc = (mobility_ct / Nb_of_obs) * 100, 
         mobility_es_perc = (mobility_es / Nb_of_obs) * 100, mobility_tot_perc = (mobility_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`9.mobility`, y=mobility_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`9.mobility`, y=mobility_ws_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
### Figure_5B1_5C1
F5B1_Panel_1 <- (F5Ba1 + F5Bb1 + F5Bc1) + plot_layout(heights = c(7,5,2), nrow = 3) + plot_annotation(tag_levels = 'A')
F5B1_Panel_2 <- (F5Bd1 + F5Be1 + F5Bf1) + plot_layout(heights = c(8,3,3), nrow = 3) + plot_annotation(tag_levels = list(c("D", "E", "F")))
F5B1_Panel_3 <- (F5Bg1 + F5Bh1 + F5Bi1 + F5Bj1) + plot_layout(heights = c(5,5,2,2), nrow = 4) + plot_annotation(tag_levels = list(c("G", "H", "I", "J")))
plot_title   <- cowplot::ggdraw() + cowplot::draw_text("Western Basin", x = 0, y = 0.25, hjust = 0, vjust = 1, angle = 0, size = 16) + theme_void() + 
  theme(plot.margin = margin(20, 20, 20, 20))
F5B1_and_C1  <- cowplot::plot_grid(plot_title, cowplot::plot_grid(Figure_5C1, F5B1_Panel_1, F5B1_Panel_2, F5B1_Panel_3, ncol = 4, 
                                                        rel_widths = c(0.4, 0.2, 0.2, 0.2)), ncol = 1, rel_heights = c(0.1, 0.9))

## Figure 5B2
F5Ba2 <- Feed_tot %>% mutate(feeding_ws_perc = (feeding_ws / Nb_of_obs) * 100, feeding_ct_perc = (feeding_ct / Nb_of_obs) * 100, 
                             feeding_es_perc = (feeding_es / Nb_of_obs) * 100, feeding_tot_perc = (feeding_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`6.feeding`, y=feeding_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`6.feeding`, y=feeding_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), text = element_text(size = 30))
F5Bb2 <- Long_tot %>% mutate(age_ws_perc = (age_ws / Nb_of_obs) * 100, age_ct_perc = (age_ct / Nb_of_obs) * 100,  age_es_perc = (age_es / Nb_of_obs) * 100, 
                             age_tot_perc = (age_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`3.longevity`, y=age_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`3.longevity`, y=age_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), text = element_text(size = 30))
F5Bc2 <- Soli_tot %>% mutate(social_ws_perc = (social_ws / Nb_of_obs) * 100, social_ct_perc = (social_ct / Nb_of_obs) * 100, 
                             social_es_perc = (social_es / Nb_of_obs) * 100, social_tot_perc = (social_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bd2 <- Morp_tot %>% mutate(morpho_ws_perc = (morpho_ws / Nb_of_obs) * 100, morpho_ct_perc = (morpho_ct / Nb_of_obs) * 100, 
                             morpho_es_perc = (morpho_es / Nb_of_obs) * 100, morpho_tot_perc = (morpho_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`1.morphology`, y=morpho_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`1.morphology`, y=morpho_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Be2 <- Stor_tot %>% mutate(storage_ws_perc = (storage_ws / Nb_of_obs) * 100, storage_ct_perc = (storage_ct / Nb_of_obs) * 100, 
                             storage_es_perc = (storage_es / Nb_of_obs) * 100, storage_tot_perc = (storage_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bf2 <- Ener_tot %>% mutate(energy_ws_perc = (energy_ws / Nb_of_obs) * 100, energy_ct_perc = (energy_ct / Nb_of_obs) * 100, 
                             energy_es_perc = (energy_es / Nb_of_obs) * 100, energy_tot_perc = (energy_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bg2 <- Heig_tot %>% dplyr::filter(`4.height` != 0) %>% mutate(height_ws_perc = (height_ws / Nb_of_obs) * 100, height_ct_perc = (height_ct / Nb_of_obs) * 100, 
                                                                height_es_perc = (height_es / Nb_of_obs) * 100, height_tot_perc = (height_tot / Nb_of_obs) * 100) %>% 
  ggplot( ) + coord_flip() + geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`4.height`, y=height_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`4.height`, y=height_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bh2 <- Grow_tot %>% mutate(growth_ws_perc = (growth_ws / Nb_of_obs)*100, growth_ct_perc = (growth_ct / Nb_of_obs)*100, 
                             growth_es_perc = (growth_es / Nb_of_obs)*100, growth_tot_perc = (growth_tot / Nb_of_obs)*100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bi2 <- Calc_tot %>% mutate(calcif_ws_perc = (calcif_ws / Nb_of_obs) * 100, calcif_ct_perc = (calcif_ct / Nb_of_obs) * 100, 
                             calcif_es_perc = (calcif_es / Nb_of_obs) * 100, calcif_tot_perc = (calcif_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`8.calcification`, y=calcif_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`8.calcification`, y=calcif_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bj2 <- Mobi_tot %>% mutate(mobility_ws_perc = (mobility_ws / Nb_of_obs) * 100, mobility_ct_perc = (mobility_ct / Nb_of_obs) * 100, 
                             mobility_es_perc = (mobility_es / Nb_of_obs) * 100, mobility_tot_perc = (mobility_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`9.mobility`, y=mobility_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`9.mobility`, y=mobility_ct_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
### Figure_5B2_5C2
F5B2_Panel_1 <- (F5Ba2 + F5Bb2 + F5Bc2) + plot_layout(heights = c(7,5,2), nrow = 3) + plot_annotation(tag_levels = 'A')
F5B2_Panel_2 <- (F5Bd2 + F5Be2 + F5Bf2) + plot_layout(heights = c(8,3,3), nrow = 3) + plot_annotation(tag_levels = list(c("D", "E", "F")))
F5B2_Panel_3 <- (F5Bg2 + F5Bh2 + F5Bi2 + F5Bj2) + plot_layout(heights = c(5,5,2,2), nrow = 4) + plot_annotation(tag_levels = list(c("G", "H", "I", "J")))
plot_title   <- cowplot::ggdraw() + cowplot::draw_text("Central Basin", x = 0, y = 0.25, hjust = 0, vjust = 1, angle = 0, size = 16) + theme_void() + 
  theme(plot.margin = margin(20, 20, 20, 20))
F5B2_and_C2  <- cowplot::plot_grid(plot_title, cowplot::plot_grid(Figure_5C2, F5B2_Panel_1, F5B2_Panel_2, F5B2_Panel_3, ncol = 4, 
                                                                  rel_widths = c(0.4, 0.2, 0.2, 0.2)), ncol = 1, rel_heights = c(0.1, 0.9))

## Figure 5B3
F5Ba3 <- Feed_tot %>% mutate(feeding_ws_perc = (feeding_ws / Nb_of_obs) * 100, feeding_ct_perc = (feeding_ct / Nb_of_obs) * 100, 
                             feeding_es_perc = (feeding_es / Nb_of_obs) * 100, feeding_tot_perc = (feeding_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`6.feeding`, y=feeding_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`6.feeding`, y=feeding_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), text = element_text(size = 30))
F5Bb3 <- Long_tot %>% mutate(age_ws_perc = (age_ws / Nb_of_obs) * 100, age_ct_perc = (age_ct / Nb_of_obs) * 100,  age_es_perc = (age_es / Nb_of_obs) * 100, 
                             age_tot_perc = (age_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + theme_classic() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`3.longevity`, y=age_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`3.longevity`, y=age_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), axis.text.x=element_blank(), text = element_text(size = 30))
F5Bc3 <- Soli_tot %>% mutate(social_ws_perc = (social_ws / Nb_of_obs) * 100, social_ct_perc = (social_ct / Nb_of_obs) * 100, 
                             social_es_perc = (social_es / Nb_of_obs) * 100, social_tot_perc = (social_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`2.solitary.colonial`, y=social_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bd3 <- Morp_tot %>% mutate(morpho_ws_perc = (morpho_ws / Nb_of_obs) * 100, morpho_ct_perc = (morpho_ct / Nb_of_obs) * 100, 
                             morpho_es_perc = (morpho_es / Nb_of_obs) * 100, morpho_tot_perc = (morpho_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`1.morphology`, y=morpho_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`1.morphology`, y=morpho_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Be3 <- Stor_tot %>% mutate(storage_ws_perc = (storage_ws / Nb_of_obs) * 100, storage_ct_perc = (storage_ct / Nb_of_obs) * 100, 
                             storage_es_perc = (storage_es / Nb_of_obs) * 100, storage_tot_perc = (storage_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() +
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`10.carbon.storage`, y=storage_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bf3 <- Ener_tot %>% mutate(energy_ws_perc = (energy_ws / Nb_of_obs) * 100, energy_ct_perc = (energy_ct / Nb_of_obs) * 100, 
                             energy_es_perc = (energy_es / Nb_of_obs) * 100, energy_tot_perc = (energy_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`5.energetic.resource`, y=energy_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bg3 <- Heig_tot %>% dplyr::filter(`4.height` != 0) %>% mutate(height_ws_perc = (height_ws / Nb_of_obs) * 100, height_ct_perc = (height_ct / Nb_of_obs) * 100, 
                                                                height_es_perc = (height_es / Nb_of_obs) * 100, height_tot_perc = (height_tot / Nb_of_obs) * 100) %>% 
  ggplot( ) + coord_flip() + geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`4.height`, y=height_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`4.height`, y=height_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bh3 <- Grow_tot %>% mutate(growth_ws_perc = (growth_ws / Nb_of_obs)*100, growth_ct_perc = (growth_ct / Nb_of_obs)*100, 
                             growth_es_perc = (growth_es / Nb_of_obs)*100, growth_tot_perc = (growth_tot / Nb_of_obs)*100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`7.growth.rates`, y=growth_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bi3 <- Calc_tot %>% mutate(calcif_ws_perc = (calcif_ws / Nb_of_obs) * 100, calcif_ct_perc = (calcif_ct / Nb_of_obs) * 100, 
                             calcif_es_perc = (calcif_es / Nb_of_obs) * 100, calcif_tot_perc = (calcif_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`8.calcification`, y=calcif_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`8.calcification`, y=calcif_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
F5Bj3 <- Mobi_tot %>% mutate(mobility_ws_perc = (mobility_ws / Nb_of_obs) * 100, mobility_ct_perc = (mobility_ct / Nb_of_obs) * 100, 
                             mobility_es_perc = (mobility_es / Nb_of_obs) * 100, mobility_tot_perc = (mobility_tot / Nb_of_obs) * 100) %>% ggplot( ) + coord_flip() + 
  geom_segment(aes(y = 0, x = -Inf, yend = 0, xend = Inf), linetype = "solid") + theme_classic() +
  geom_segment(aes(y = 25, x = -Inf, yend = 25, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 50, x = -Inf, yend = 50, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_segment(aes(y = 75, x = -Inf, yend = 75, xend = Inf), linetype = "dotted", alpha = .25) +
  geom_bar(aes(x=`9.mobility`, y=mobility_tot_perc), color = "black", fill = "white", stat="identity", width = 0.5) +
  geom_bar(aes(x=`9.mobility`, y=mobility_es_perc), fill = "#CC66CC", color = "black", stat="identity", width = 0.5) +
  scale_y_continuous(breaks = seq(25, 75, 25), name = "", limits = c(0, 100)) + scale_x_discrete(name = "") +
  theme(axis.line = element_blank(), axis.ticks.x=element_blank(), axis.ticks.y=element_blank(), text = element_text(size = 30))
### Figure_5B3_5C3
F5B3_Panel_1 <- (F5Ba3 + F5Bb3 + F5Bc3) + plot_layout(heights = c(7,5,2), nrow = 3) + plot_annotation(tag_levels = 'A')
F5B3_Panel_2 <- (F5Bd3 + F5Be3 + F5Bf3) + plot_layout(heights = c(8,3,3), nrow = 3) + plot_annotation(tag_levels = list(c("D", "E", "F")))
F5B3_Panel_3 <- (F5Bg3 + F5Bh3 + F5Bi3 + F5Bj3) + plot_layout(heights = c(5,5,2,2), nrow = 4) + plot_annotation(tag_levels = list(c("G", "H", "I", "J")))
plot_title   <- cowplot::ggdraw() + cowplot::draw_text("Central Basin", x = 0, y = 0.25, hjust = 0, vjust = 1, angle = 0, size = 16) + theme_void() + 
  theme(plot.margin = margin(20, 20, 20, 20))
F5B3_and_C3  <- cowplot::plot_grid(plot_title, cowplot::plot_grid(Figure_5C3, F5B3_Panel_1, F5B3_Panel_2, F5B3_Panel_3, ncol = 4, 
                                                                  rel_widths = c(0.4, 0.2, 0.2, 0.2)), ncol = 1, rel_heights = c(0.1, 0.9))

### Figure 5D
data_heatmap_FE_ER <- Global_dataset %>% inner_join(fe_6D_coord_df, by = "FE") %>% dplyr::select(FE, year, ecoregion, `damaged_percentatge`, `damaged_qualitative`,
                drivers_abiotic, drivers_abiotic_other, drivers_biotic_group, drivers_biotic, drivers_biotic_other, PC1, PC2, PC3, PC4, PC5, PC6) 

### F5Da1 – Abiotic group
data_heatmap_FE_abiotic_summ_ER_WT     <- data_heatmap_FE_ER %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None"),
                ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% dplyr::select(., -n) %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% 
  group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 26,1))
data_heatmap_FE_abiotic_summ_sel_ER_WT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 26,1)) %>% dplyr::filter(n > 4)
data_heatmap_FE_abiotic_summ_sel_ER_WT <- data_heatmap_FE_abiotic_summ_sel_ER_WT$ID
data_heatmap_FE_abiotic_ER_WT          <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()
V = c() ; for (i in data_heatmap_FE_abiotic_summ_sel_ER_WT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic_ER_WT[[i]][,2:5] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_abiotic_summ_sel_ER_WT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 26,1)) %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_abiotic_summ_sel_ER_WT <- data_heatmap_FE_abiotic_summ_sel_ER_WT$ID
for (i in data_heatmap_FE_abiotic_summ_sel_ER_WT) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_abiotic_ER_WT[[i]][,2:3] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^2) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_abiotic_summ_ER_WT     <- data_heatmap_FE_abiotic_summ_ER_WT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_abiotic = "A")

F5Da1 <- ggplot(data_heatmap_FE_abiotic_summ_ER_WT, aes(year, drivers_abiotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0, 8)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Da2 – Biotic group
data_heatmap_FE_biotic_summ_ER_WT     <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 13, 1))
data_heatmap_FE_biotic_summ_sel_ER_WT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 13,1)) %>% 
  dplyr::filter(n > 4)
data_heatmap_FE_biotic_summ_sel_ER_WT <- data_heatmap_FE_biotic_summ_sel_ER_WT$ID
data_heatmap_FE_biotic_ER_WT          <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()
V = c() ; for (i in data_heatmap_FE_biotic_summ_sel_ER_WT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_biotic_ER_WT[[i]][,2:5] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_biotic_summ_sel_ER_WT <- data_heatmap_FE_ER %>% group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 13,1)) %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_biotic_summ_sel_ER_WT <- c(data_heatmap_FE_biotic_summ_sel_ER_WT$ID)
for (i in data_heatmap_FE_biotic_summ_sel_ER_WT) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_biotic_ER_WT[[i]][,2:3] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^2) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_biotic_summ_ER_WT     <- data_heatmap_FE_biotic_summ_ER_WT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_biotic = "B")

F5Da2 <- ggplot(data_heatmap_FE_biotic_summ_ER_WT, aes(year, drivers_biotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0, 8)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Da3 – Abiotic & biotic groups
data_heatmap_FE_all_summ_ER_WT     <- rbind((data_heatmap_FE_biotic_ER_WT %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_WT %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 27, 1))
data_heatmap_FE_all_summ_sel_ER_WT <- data_heatmap_FE_all_summ_ER_WT %>% dplyr::filter(n > 4)
data_heatmap_FE_all_summ_sel_ER_WT <- data_heatmap_FE_all_summ_sel_ER_WT$ID
data_heatmap_FE_all_ER_WT          <- rbind((data_heatmap_FE_biotic_ER_WT %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_WT %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% group_split()
V = c() ; for (i in data_heatmap_FE_all_summ_sel_ER_WT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all_ER_WT[[i]][,3:6] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_all_summ_sel_ER_WT <- data_heatmap_FE_all_summ_ER_WT %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_all_summ_sel_ER_WT <- c(data_heatmap_FE_all_summ_sel_ER_WT$ID)
for (i in data_heatmap_FE_all_summ_sel_ER_WT) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_all_ER_WT[[i]][,3:4] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>%  as.matrix())$volume)^2) * 100, 2))}
data_heatmap_FE_all_summ_ER_WT     <- data_heatmap_FE_all_summ_ER_WT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V,0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_all = "C")

F5Da3 <- ggplot(data_heatmap_FE_all_summ_ER_WT, aes(year, drivers_all, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0, 8)) +
  scale_x_continuous(breaks = seq(1990, 2020, 10), name = "") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = .5)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(size = 15, face="bold"), axis.text.x = element_text(size = 20))

### F5Da 
F5Da <- F5Da1 / F5Da2 / F5Da3 + plot_layout(heights = c(1, 1, 1), guides = 'collect') & theme(legend.position = 'none') 

### F5Db1 – Abiotic group
data_heatmap_FE_abiotic_summ_ER_CT     <- data_heatmap_FE_ER %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None"),
                ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 17,1))
data_heatmap_FE_abiotic_summ_sel_ER_CT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 17,1)) %>% 
  dplyr::filter(n > 4)
data_heatmap_FE_abiotic_summ_sel_ER_CT <- data_heatmap_FE_abiotic_summ_sel_ER_CT$ID
data_heatmap_FE_abiotic_ER_CT          <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()
V = c() ; for (i in data_heatmap_FE_abiotic_summ_sel_ER_CT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic_ER_CT[[i]][,2:5] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_abiotic_summ_sel_ER_CT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 17,1)) %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_abiotic_summ_sel_ER_CT <- data_heatmap_FE_abiotic_summ_sel_ER_CT$ID
for (i in data_heatmap_FE_abiotic_summ_sel_ER_CT) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_abiotic_ER_CT[[i]][,2:3] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^2) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_abiotic_summ_ER_CT     <- data_heatmap_FE_abiotic_summ_ER_CT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_abiotic = "A")

F5Db1 <- ggplot(data_heatmap_FE_abiotic_summ_ER_CT, aes(year, drivers_abiotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Db2 – Biotic group
data_heatmap_FE_biotic_summ_ER_CT     <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 7, 1))
data_heatmap_FE_biotic_summ_sel_ER_CT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 7,1)) %>% dplyr::filter(n > 4)
data_heatmap_FE_biotic_summ_sel_ER_CT <- data_heatmap_FE_biotic_summ_sel_ER_CT$ID
data_heatmap_FE_biotic_ER_CT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()
V = c() ; for (i in data_heatmap_FE_biotic_summ_sel_ER_CT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_biotic_ER_CT[[i]][,2:5] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_biotic_summ_sel_ER_CT <- data_heatmap_FE_ER %>% group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 7,1)) %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_biotic_summ_sel_ER_CT <- c(data_heatmap_FE_biotic_summ_sel_ER_CT$ID)
for (i in data_heatmap_FE_biotic_summ_sel_ER_CT) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_biotic_ER_CT[[i]][,2:3] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^2) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_biotic_summ_ER_CT     <- data_heatmap_FE_biotic_summ_ER_CT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_biotic = "B")

F5Db2 <- ggplot(data_heatmap_FE_biotic_summ_ER_CT, aes(year, drivers_biotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Db3 – Abiotic & biotic groups
data_heatmap_FE_all_summ_ER_CT     <- rbind((data_heatmap_FE_biotic_ER_CT %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_CT %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 17, 1))
data_heatmap_FE_all_summ_sel_ER_CT <- data_heatmap_FE_all_summ_ER_CT %>% dplyr::filter(n > 4)
data_heatmap_FE_all_summ_sel_ER_CT <- data_heatmap_FE_all_summ_sel_ER_CT$ID
data_heatmap_FE_all_ER_CT          <- rbind((data_heatmap_FE_biotic_ER_CT %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_CT %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>%  group_split()
V = c() ; for (i in data_heatmap_FE_all_summ_sel_ER_CT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all_ER_CT[[i]][,3:6] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_all_summ_sel_ER_CT <- data_heatmap_FE_all_summ_ER_CT %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_all_summ_sel_ER_CT <- c(data_heatmap_FE_all_summ_sel_ER_CT$ID)
for (i in data_heatmap_FE_all_summ_sel_ER_CT) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_all_ER_CT[[i]][,3:4] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^2) * 100, 2))}
data_heatmap_FE_all_summ_ER_CT     <- data_heatmap_FE_all_summ_ER_CT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V,0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_all = "C")

F5Db3 <- ggplot(data_heatmap_FE_all_summ_ER_CT, aes(year, drivers_all, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1990,2020,10), name = "") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = .5)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(size = 15, face="bold"), axis.text.x = element_text(size = 20))

### F5Db
F5Db <- F5Db1 / F5Db2 / F5Db3 + plot_layout(heights = c(1, 1, 1), guides = 'collect') & theme(legend.position = 'none') 

### F5Dc1 – Abiotic group
data_heatmap_FE_abiotic_summ_ER_ES     <- data_heatmap_FE_ER %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None"), 
                                                                               ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 9,1))
data_heatmap_FE_abiotic_summ_sel_ER_ES <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 9,1)) %>% 
  dplyr::filter(n > 4)
data_heatmap_FE_abiotic_summ_sel_ER_ES <- data_heatmap_FE_abiotic_summ_sel_ER_ES$ID
data_heatmap_FE_abiotic_ER_ES          <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()
V = c() ; for (i in data_heatmap_FE_abiotic_summ_sel_ER_ES[-2]) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic_ER_ES[[i]][,2:5] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_abiotic_summ_sel_ER_ES <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 9,1)) %>% 
  dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_abiotic_summ_sel_ER_ES <- data_heatmap_FE_abiotic_summ_sel_ER_ES$ID
for (i in c(data_heatmap_FE_abiotic_summ_sel_ER_ES,8)) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_abiotic_ER_ES[[i]][,2:3] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^2) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_abiotic_summ_ER_ES     <- data_heatmap_FE_abiotic_summ_ER_ES %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$.)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_abiotic = "A")

F5Dc1 <- ggplot(data_heatmap_FE_abiotic_summ_ER_ES, aes(year, drivers_abiotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Dc2 – Biotic group
data_heatmap_FE_biotic_summ_ER_ES     <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 4, 1))
data_heatmap_FE_biotic_summ_sel_ER_ES <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 4,1)) %>% 
  dplyr::filter(n > 4)
data_heatmap_FE_biotic_summ_sel_ER_ES <- data_heatmap_FE_biotic_summ_sel_ER_ES$ID
data_heatmap_FE_biotic_ER_ES          <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()
data_heatmap_FE_biotic_summ_ER_ES     <- data.frame(year   = seq(1986, 2020, 1), 
                                                    n = c(rep(NA,26), 1, rep(NA, 2), 2, rep(NA, 2), 1, NA, 1),
                                                    Volume = c(rep(NA,26), 0, rep(NA, 2), 0, rep(NA, 2), 0, NA, 0))
data_heatmap_FE_biotic_summ_ER_ES     <- data_heatmap_FE_biotic_summ_ER_ES %>% data.frame() %>% full_join(data_to_fill, by = "year") %>% complete(year) %>% 
  mutate(drivers_biotic = "B")

F5Dc2 <- ggplot(data_heatmap_FE_biotic_summ_ER_ES, aes(year, drivers_biotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Dc3 – Abiotic & biotic groups
data_heatmap_FE_all_summ_ER_ES     <- rbind((data_heatmap_FE_biotic_ER_ES %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_ES %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 10, 1))
data_heatmap_FE_all_summ_sel_ER_ES <- data_heatmap_FE_all_summ_ER_ES %>% dplyr::filter(n > 4)
data_heatmap_FE_all_summ_sel_ER_ES <- data_heatmap_FE_all_summ_sel_ER_ES$ID
data_heatmap_FE_all_ER_ES          <- rbind((data_heatmap_FE_biotic_ER_ES %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_ES %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>%  group_split()
V = c() ; for (i in data_heatmap_FE_all_summ_sel_ER_ES) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all_ER_ES[[i]][,3:6] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_all_summ_sel_ER_ES <- data_heatmap_FE_all_summ_ER_ES %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_all_summ_sel_ER_ES <- c(data_heatmap_FE_all_summ_sel_ER_ES$ID)
for (i in data_heatmap_FE_all_summ_sel_ER_ES) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_all_ER_ES[[i]][,3:4] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^2) * 100, 2))}
data_heatmap_FE_all_summ_ER_ES     <- data_heatmap_FE_all_summ_ER_ES %>% dplyr::select(year, n) %>% cbind(., Volume = c(V,0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_all = "C")

F5Dc3 <- ggplot(data_heatmap_FE_all_summ_ER_ES, aes(year, drivers_all, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0, 8)) +
  scale_x_continuous(breaks = seq(1990, 2020, 10), name = "") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(size = 15, face="bold"), axis.text.x = element_text(size = 20))

### F5Dc
F5Dc <- F5Dc1 / F5Dc2 / F5Dc3 + plot_layout(heights = c(1, 1, 1), guides = 'collect') & theme(legend.position = 'none') 