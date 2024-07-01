#### Figure_4
fe_6D_coord_df     <- fe_6D_coord %>% data.frame() %>% rownames_to_column("FE") 
data_complete_all  <- data_complete %>% left_join(data_filtered_MME_Merged_data %>% rename(Species = species)) 
data_mortality_from_complete_all  <- data_mortality_from_complete %>% 
  dplyr::select(FE, year, `damaged_percentatge`, `damaged_qualitative`, drivers_abiotic, drivers_abiotic_other, drivers_biotic_group,
                drivers_biotic, drivers_biotic_other, PC1, PC2, PC3, PC4, PC5, PC6, ecoregion) %>% 
  mutate(drivers_abiotic = ifelse(drivers_abiotic == "Increase of turbidity / sedimentation", "Increase of turbidity", drivers_abiotic)) 

# Subset by ecoregions
dataset_western <- data_mortality_from_complete_all %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean"))
dataset_central <- data_mortality_from_complete_all %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra"))
dataset_eastern <- data_mortality_from_complete_all %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea"))
# define the number of Affected FEs
length(unique(data_mortality_from_complete_all$FE))   # 56 FEs in total
length(unique(dataset_western$FE)) # 44 FEs affected in the western
length(unique(dataset_central$FE)) # 33 FEs affected in the central
length(unique(dataset_eastern$FE)) # 25 FEs affected in the eastern

# Prepare datasets
FE_affected_western_df <- data.frame(FE = unique(dataset_western$FE), FE_affected_western = rep("yes", length(unique(dataset_western$FE))))
FE_affected_central_df <- data.frame(FE = unique(dataset_central$FE), FE_affected_central = rep("yes", length(unique(dataset_central$FE))))
FE_affected_eastern_df <- data.frame(FE = unique(dataset_eastern$FE), FE_affected_eastern = rep("yes", length(unique(dataset_eastern$FE))))
data_complete          <- merge(data_mortality_from_complete_all, FE_affected_western_df, by = "FE", all.x = T)
data_complete          <- merge(data_complete,                    FE_affected_central_df, by = "FE", all.x = T)
data_complete          <- merge(data_complete,                    FE_affected_eastern_df, by = "FE", all.x = T)
data_spatial           <- data_complete %>% group_by(PC1, PC2, PC3, PC4, PC5, PC6, FE_affected_western, FE_affected_central, 
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

### Here we need an additional analysis to get the same sampling effort – Post review request
data_mortality_from_complete_all_Review <- data_mortality_from_complete_all %>%
  mutate(Region = case_when(
    ecoregion %in% c("Alboran Sea", "Western Mediterranean") ~ "Western",
    ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra") ~ "Central",
    ecoregion %in% c("Aegean Sea", "Levantine Sea") ~ "Eastern",
    TRUE ~ NA_character_))

bootstrap_number = 1000
sampled_data = vector(mode = "list", length = bootstrap_number) 
V_WS = vector(length = length(sampled_data)) ; V_CT = V_WS ; V_ES = V_WS
FE_WS = vector(length = length(sampled_data)) ; FE_CT = FE_WS ; FE_ES = FE_WS
for (i in 1:length(sampled_data)) {
  sampled_data[[i]] <- data_mortality_from_complete_all_Review %>% 
    group_by(Region) %>% sample_n(size = 100, replace = TRUE) %>% ungroup()
  V_WS[i] <- round((cxhull::cxhull(sampled_data[[i]][201:300,10:13] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2)
  V_CT[i] <- round((cxhull::cxhull(sampled_data[[i]][  1:100,10:13] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2)
  V_ES[i] <- round((cxhull::cxhull(sampled_data[[i]][101:200,10:13] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2)
  FE_WS[i] <- dim(unique(sampled_data[[i]][201:300,1]))[1]
  FE_CT[i] <- dim(unique(sampled_data[[i]][  1:100,1]))[1]
  FE_ES[i] <- dim(unique(sampled_data[[i]][101:200,1]))[1]
}

(Standardized_impaired_volume_WS = mean(V_WS)) ; (Standardized_impaired_volume_WS_sd = sd(V_WS))
(Standardized_impaired_volume_CT = mean(V_CT)) ; (Standardized_impaired_volume_CT_sd = sd(V_CT)) 
(Standardized_impaired_volume_ES = mean(V_ES)) ; (Standardized_impaired_volume_ES_sd = sd(V_ES))

(Standardized_impaired_FE_WS = mean(FE_WS)) ; (Standardized_impaired_FE_WS_sd = sd(FE_WS)) 
(Standardized_impaired_FE_CT = mean(FE_CT)) ; (Standardized_impaired_FE_CT_sd = sd(FE_CT)) 
(Standardized_impaired_FE_ES = mean(FE_ES)) ; (Standardized_impaired_FE_ES_sd = sd(FE_ES))


### Figure 5C
Figure_5C1 <- ggplot(data_spatial_ws) + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_ws, aes(x = PC1, y = PC2), alpha = .95, fill = "#CC3399", linewidth = 2, color = "#CC0099") +
  geom_point(data = data_spatial_ws, aes(x = PC1, y = PC2), col = "black", fill = "#CC0099", size = 12, shape = 21) +
  scale_x_continuous(name = "", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), 
        panel.grid = element_line(colour = NA), axis.text = element_blank()) 

Figure_5C2 <- ggplot(data_spatial_ct) + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_ct, aes(x = PC1, y = PC2), alpha = .95, fill = "#CC3399", linewidth = 2, color = "#CC0099") +
  geom_point(data = data_spatial_ct, aes(x = PC1, y = PC2), col = "black", fill = "#CC0099", size = 12, shape = 21) +
  scale_x_continuous(name = "", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + 
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), 
        panel.grid = element_line(colour = NA), axis.text = element_blank()) 

Figure_5C3 <- ggplot(data_spatial_es) + geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  scale_x_continuous(name = "", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  geom_polygon(data = conv_hull_es, aes(x = PC1, y = PC2), alpha = .95, fill = "#CC3399", linewidth = 2, color = "#CC0099") +
  geom_point(data = data_spatial_es, aes(x = PC1, y = PC2), col = "black", fill = "#CC0099", size = 12, shape = 21) +
  theme_minimal() + 
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), 
        panel.grid = element_line(colour = NA), axis.text = element_blank()) 

### Figure 5D
data_heatmap_FE_ER <- data_mortality_from_complete_all %>% dplyr::select(FE, year, ecoregion, `damaged_percentatge`, `damaged_qualitative`,
                                                                         drivers_abiotic, drivers_abiotic_other, drivers_biotic_group, drivers_biotic, drivers_biotic_other, PC1, PC2, PC3, PC4, PC5, PC6) 

### F5Da1 – Abiotic group
data_heatmap_FE_abiotic_summ_ER_WT     <- dataset_western %>% group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% dplyr::select(., -n) %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% 
  group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 27,1))
data_heatmap_FE_abiotic_summ_sel_ER_WT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 27,1)) %>% dplyr::filter(n > 4)
data_heatmap_FE_abiotic_summ_sel_ER_WT <- data_heatmap_FE_abiotic_summ_sel_ER_WT$ID
data_heatmap_FE_abiotic_ER_WT          <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()
V = c() ; for (i in data_heatmap_FE_abiotic_summ_sel_ER_WT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic_ER_WT[[i]][,2:5] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_abiotic_summ_sel_ER_WT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 27,1)) %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_abiotic_summ_sel_ER_WT <- data_heatmap_FE_abiotic_summ_sel_ER_WT$ID
for (i in data_heatmap_FE_abiotic_summ_sel_ER_WT[-5]) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic_ER_WT[[i]][,2:3] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_abiotic_summ_ER_WT     <- data_heatmap_FE_abiotic_summ_ER_WT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_abiotic = "A")

F5Da1 <- ggplot(data_heatmap_FE_abiotic_summ_ER_WT, aes(year, drivers_abiotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0, 6)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Da2 – Biotic group
data_heatmap_FE_biotic_summ_ER_WT     <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 17, 1))
data_heatmap_FE_biotic_summ_sel_ER_WT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Alboran Sea", "Western Mediterranean")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 17,1)) %>% 
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
  mutate(ID = seq(1, 17,1)) %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_biotic_summ_sel_ER_WT <- c(data_heatmap_FE_biotic_summ_sel_ER_WT$ID)
for (i in data_heatmap_FE_biotic_summ_sel_ER_WT[-4]) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_biotic_ER_WT[[i]][,2:3] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_biotic_summ_ER_WT     <- data_heatmap_FE_biotic_summ_ER_WT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$., 0.5, 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_biotic = "B")

F5Da2 <- ggplot(data_heatmap_FE_biotic_summ_ER_WT, aes(year, drivers_biotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0, 6)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Da3 – Abiotic & biotic groups
data_heatmap_FE_all_summ_ER_WT     <- rbind((data_heatmap_FE_biotic_ER_WT %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_WT %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 28, 1))
data_heatmap_FE_all_summ_sel_ER_WT <- data_heatmap_FE_all_summ_ER_WT %>% dplyr::filter(n > 4)
data_heatmap_FE_all_summ_sel_ER_WT <- data_heatmap_FE_all_summ_sel_ER_WT$ID
data_heatmap_FE_all_ER_WT          <- rbind((data_heatmap_FE_biotic_ER_WT %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_WT %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% group_split()
V = c() ; for (i in data_heatmap_FE_all_summ_sel_ER_WT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all_ER_WT[[i]][,3:6] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_all_summ_sel_ER_WT <- data_heatmap_FE_all_summ_ER_WT %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_all_summ_sel_ER_WT <- c(data_heatmap_FE_all_summ_sel_ER_WT$ID)
for (i in data_heatmap_FE_all_summ_sel_ER_WT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all_ER_WT[[i]][,3:4] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
data_heatmap_FE_all_summ_ER_WT     <- data_heatmap_FE_all_summ_ER_WT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V,0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_all = "C")

F5Da3 <- ggplot(data_heatmap_FE_all_summ_ER_WT, aes(year, drivers_all, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0, 6)) +
  scale_x_continuous(breaks = seq(1990, 2020, 10), name = "") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = .5)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(size = 15, face="bold"), axis.text.x = element_text(size = 20))

### F5Da 
F5Da <- F5Da1 / F5Da2 / F5Da3 + plot_layout(heights = c(1, 1, 1), guides = 'collect') & theme(legend.position = 'none') 

### F5Db1 – Abiotic group
data_heatmap_FE_abiotic_summ_ER_CT     <- dataset_central %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 19,1))
data_heatmap_FE_abiotic_summ_sel_ER_CT <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 19,1)) %>% 
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
  mutate(ID = seq(1, 19,1)) %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 4)
data_heatmap_FE_abiotic_summ_sel_ER_CT <- data_heatmap_FE_abiotic_summ_sel_ER_CT$ID
for (i in data_heatmap_FE_abiotic_summ_sel_ER_CT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic_ER_CT[[i]][,2:4] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2, PC3) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_abiotic_summ_ER_CT     <- data_heatmap_FE_abiotic_summ_ER_CT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_abiotic = "A")

F5Db1 <- ggplot(data_heatmap_FE_abiotic_summ_ER_CT, aes(year, drivers_abiotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,6)) +
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
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_biotic_summ_ER_CT     <- data_heatmap_FE_biotic_summ_ER_CT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_biotic = "B")

F5Db2 <- ggplot(data_heatmap_FE_biotic_summ_ER_CT, aes(year, drivers_biotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,6)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Db3 – Abiotic & biotic groups
data_heatmap_FE_all_summ_ER_CT     <- rbind((data_heatmap_FE_biotic_ER_CT %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_CT %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 19, 1))
data_heatmap_FE_all_summ_sel_ER_CT <- data_heatmap_FE_all_summ_ER_CT %>% dplyr::filter(n > 4)
data_heatmap_FE_all_summ_sel_ER_CT <- data_heatmap_FE_all_summ_sel_ER_CT$ID
data_heatmap_FE_all_ER_CT          <- rbind((data_heatmap_FE_biotic_ER_CT %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_CT %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>%  group_split()
V = c() ; for (i in data_heatmap_FE_all_summ_sel_ER_CT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all_ER_CT[[i]][,3:6] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_all_summ_sel_ER_CT <- data_heatmap_FE_all_summ_ER_CT %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 4)
data_heatmap_FE_all_summ_sel_ER_CT <- c(data_heatmap_FE_all_summ_sel_ER_CT$ID)
for (i in data_heatmap_FE_all_summ_sel_ER_CT) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all_ER_CT[[i]][,3:5] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2, PC3) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
data_heatmap_FE_all_summ_ER_CT     <- data_heatmap_FE_all_summ_ER_CT %>% dplyr::select(year, n) %>% cbind(., Volume = c(V,0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_all = "C")

F5Db3 <- ggplot(data_heatmap_FE_all_summ_ER_CT, aes(year, drivers_all, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,6)) +
  scale_x_continuous(breaks = seq(1990,2020,10), name = "") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = .5)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(size = 15, face="bold"), axis.text.x = element_text(size = 20))

### F5Db
F5Db <- F5Db1 / F5Db2 / F5Db3 + plot_layout(heights = c(1, 1, 1), guides = 'collect') & theme(legend.position = 'none') 

### F5Dc1 – Abiotic group
data_heatmap_FE_abiotic_summ_ER_ES     <- dataset_eastern %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 13,1))
data_heatmap_FE_abiotic_summ_sel_ER_ES <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 13,1)) %>% 
  dplyr::filter(n > 4)
data_heatmap_FE_abiotic_summ_sel_ER_ES <- data_heatmap_FE_abiotic_summ_sel_ER_ES$ID
data_heatmap_FE_abiotic_ER_ES          <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()
V = c() ; for (i in data_heatmap_FE_abiotic_summ_sel_ER_ES[-3]) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic_ER_ES[[i]][,2:5] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_abiotic_summ_sel_ER_ES <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 13,1)) %>% 
  dplyr::filter(n <= 4) %>% dplyr::filter(n >= 4)
data_heatmap_FE_abiotic_summ_sel_ER_ES <- data_heatmap_FE_abiotic_summ_sel_ER_ES$ID
for (i in c(data_heatmap_FE_abiotic_summ_sel_ER_ES)) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic_ER_ES[[i]][,2:4] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2, PC3) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_abiotic_summ_ER_ES     <- data_heatmap_FE_abiotic_summ_ER_ES %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$.)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_abiotic = "A")

F5Dc1 <- ggplot(data_heatmap_FE_abiotic_summ_ER_ES, aes(year, drivers_abiotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,6)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Dc2 – Biotic group
data_heatmap_FE_biotic_summ_ER_ES     <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 5, 1))
data_heatmap_FE_biotic_summ_sel_ER_ES <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 5,1)) %>% 
  dplyr::filter(n > 4)
data_heatmap_FE_biotic_summ_sel_ER_ES <- data_heatmap_FE_biotic_summ_sel_ER_ES$ID
data_heatmap_FE_biotic_ER_ES          <- data_heatmap_FE_ER %>% dplyr::filter(ecoregion %in% c("Aegean Sea", "Levantine Sea")) %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()
data_heatmap_FE_biotic_summ_ER_ES     <- data.frame(year   = seq(1986, 2020, 1), 
                                                    n = c(rep(NA,13), 1, rep(NA,12), 1, rep(NA, 2), 2, rep(NA, 2), 1, NA, 1),
                                                    Volume = c(rep(NA,13), 0, rep(NA,12), 0, rep(NA, 2), 0, rep(NA, 2), 0, NA, 0))
data_heatmap_FE_biotic_summ_ER_ES     <- data_heatmap_FE_biotic_summ_ER_ES %>% data.frame() %>% full_join(data_to_fill, by = "year") %>% complete(year) %>% 
  mutate(drivers_biotic = "B")

F5Dc2 <- ggplot(data_heatmap_FE_biotic_summ_ER_ES, aes(year, drivers_biotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0,6)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15), axis.text.x = element_blank(), axis.ticks.x = element_blank())

### F5Dc3 – Abiotic & biotic groups
data_heatmap_FE_all_summ_ER_ES     <- rbind((data_heatmap_FE_biotic_ER_ES %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_ES %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 14, 1))
data_heatmap_FE_all_summ_sel_ER_ES <- data_heatmap_FE_all_summ_ER_ES %>% dplyr::filter(n > 4)
data_heatmap_FE_all_summ_sel_ER_ES <- data_heatmap_FE_all_summ_sel_ER_ES$ID
data_heatmap_FE_all_ER_ES          <- rbind((data_heatmap_FE_biotic_ER_ES %>% bind_rows())[,-1], (data_heatmap_FE_abiotic_ER_ES %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>%  group_split()
V = c() ; for (i in data_heatmap_FE_all_summ_sel_ER_ES) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all_ER_ES[[i]][,3:6] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_all_summ_sel_ER_ES <- data_heatmap_FE_all_summ_ER_ES %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 4)
data_heatmap_FE_all_summ_sel_ER_ES <- c(data_heatmap_FE_all_summ_sel_ER_ES$ID)
for (i in data_heatmap_FE_all_summ_sel_ER_ES) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all_ER_ES[[i]][,3:5] %>% distinct() %>% as.matrix())$volume / 
                        cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2, PC3) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
data_heatmap_FE_all_summ_ER_ES     <- data_heatmap_FE_all_summ_ER_ES %>% dplyr::select(year, n) %>% cbind(., Volume = c(V,0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_all = "C")

F5Dc3 <- ggplot(data_heatmap_FE_all_summ_ER_ES, aes(year, drivers_all, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "Reds", begin = 0, end = 1, limits = c(0, 6)) +
  scale_x_continuous(breaks = seq(1990, 2020, 10), name = "") + theme(axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(size = 15, face="bold"), axis.text.x = element_text(size = 20))

### F5Dc
F5Dc <- F5Dc1 / F5Dc2 / F5Dc3 + plot_layout(heights = c(1, 1, 1), guides = 'collect') & theme(legend.position = 'none') 

### Basic statistics
# Volume
round((cxhull::cxhull(dataset_western[,10:13] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2)
round((cxhull::cxhull(dataset_central[,10:13] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2)
round((cxhull::cxhull(dataset_eastern[,10:13] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2)

# Observations
length(dataset_western$FE)
length(dataset_central$FE)
length(dataset_eastern$FE)

# Number of FE impaired per decade
data_complete_impaired %>% mutate(species = Species) %>% left_join(MME_Merged_data) %>% group_by(year, FE) %>% distinct(year, FE) %>% 
  dplyr::filter(year < 2000, year >= 1990) %>% group_by(year) %>% summarise(FE = n()) %>% rbind(data.frame(year = c(1990, 1994), FE = c(0, 0))) %>% 
  summarise(mean = mean(FE), sd = sd(FE))

# Impaired volume per decade
Figure_4C$data %>% dplyr::mutate(Volume = replace_na(Volume, 0), Volume = Volume^2) %>% 
  dplyr::filter(year < 2000, year >= 1990) %>% summarise(V = mean(Volume))
