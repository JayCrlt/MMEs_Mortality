#### Figure_3
data_FE_Affected_PA <- type.convert(as.data.frame(ifelse(t(data_FE_Affected) > 0, "affected", ifelse(t(data_FE_Affected) == 0, 1, 0))), as.is = TRUE)
data_FE_Affected    <- data.frame(type.convert(t(data_FE_Affected)))
fe_6D_coord_df      <- fe_6D_coord %>% data.frame() %>% rownames_to_column("FEs")
data_FE_Affected_PA <- data_FE_Affected_PA %>% rownames_to_column("FEs") %>% inner_join(fe_6D_coord_df, by = "FEs") %>% mutate_all(~ ifelse(. == "affected", 0, .))
data_FE_Affected    <- data_FE_Affected %>% rownames_to_column("FEs") %>% inner_join(fe_6D_coord_df, by = "FEs") 

# Plot FE hypervolume space
### Define convex hull
conv_hull_tot = data_FE_Affected_PA %>%  slice(chull(PC1, PC2))
conv_hull_010 = data_FE_Affected_PA %>% filter(`[0,10]`   == 1) %>% slice(chull(PC1, PC2))
conv_hull_020 = data_FE_Affected_PA %>% filter(`(10,20]`  == 1) %>% slice(chull(PC1, PC2))
conv_hull_030 = data_FE_Affected_PA %>% filter(`(20,30]`  == 1) %>% slice(chull(PC1, PC2))
conv_hull_040 = data_FE_Affected_PA %>% filter(`(30,40]`  == 1) %>% slice(chull(PC1, PC2))
conv_hull_050 = data_FE_Affected_PA %>% filter(`(40,50]`  == 1) %>% slice(chull(PC1, PC2))
conv_hull_060 = data_FE_Affected_PA %>% filter(`(50,60]`  == 1) %>% slice(chull(PC1, PC2))
conv_hull_070 = data_FE_Affected_PA %>% filter(`(60,70]`  == 1) %>% slice(chull(PC1, PC2))
conv_hull_080 = data_FE_Affected_PA %>% filter(`(70,80]`  == 1) %>% slice(chull(PC1, PC2))
conv_hull_090 = data_FE_Affected_PA %>% filter(`(80,90]`  == 1) %>% slice(chull(PC1, PC2))
conv_hull_100 = data_FE_Affected_PA %>% filter(`(90,100]` == 1) %>% slice(chull(PC1, PC2))

### Define corresponding datasets
dataset_010 = data_FE_Affected_PA %>% filter(`[0,10]`   == 1) 
dataset_020 = data_FE_Affected_PA %>% filter(`(10,20]`  == 1) 
dataset_030 = data_FE_Affected_PA %>% filter(`(20,30]`  == 1) 
dataset_040 = data_FE_Affected_PA %>% filter(`(30,40]`  == 1) 
dataset_050 = data_FE_Affected_PA %>% filter(`(40,50]`  == 1) 
dataset_060 = data_FE_Affected_PA %>% filter(`(50,60]`  == 1)
dataset_070 = data_FE_Affected_PA %>% filter(`(60,70]`  == 1) 
dataset_080 = data_FE_Affected_PA %>% filter(`(70,80]`  == 1)
dataset_090 = data_FE_Affected_PA %>% filter(`(80,90]`  == 1) 
dataset_100 = data_FE_Affected_PA %>% filter(`(90,100]` == 1) 

### Volume
VTot = cxhull::cxhull(data_FE_Affected_PA[,12:15] %>% as.matrix())$volume
V30  = round(((cxhull::cxhull(dataset_030[,12:13] %>% as.matrix())$volume / cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^3) * 100, 2)
V40  = round(((cxhull::cxhull(dataset_040[,12:13] %>% as.matrix())$volume / cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^3) * 100, 2)
V50  = round((cxhull::cxhull(dataset_050[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V60  = round((cxhull::cxhull(dataset_060[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V70  = round((cxhull::cxhull(dataset_070[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V80  = round((cxhull::cxhull(dataset_080[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V90  = round((cxhull::cxhull(dataset_090[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V100 = round((cxhull::cxhull(dataset_100[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)

data_FE_Affected <- data_sp_to_fe %>% count(FE) %>% rename(FEs = FE, nb_sp_within_FE = n) %>% right_join(data_FE_Affected, by = "FEs") %>%
  right_join(., (data_sp_to_fe %>% rename(FEs = FE)), by = "FEs") %>% 
  right_join(species_traits %>% select(species, group) %>% rename(Species = species, Taxa = group), by = "Species")

# Rename the Phylum
data_FE_Affected$Phylum = NA
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("red.algae", "cca")]                                              = "Rhodophyta"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("brown.algae")]                                                   = "Ochrophyta"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("actinaria", "octocorals", "gorgonians", "corals", "zooanthids")] = "Cnidaria"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("echinoderms")]                                                   = "Echinodermata"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("sponges")]                                                       = "Porifera"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("tunicates")]                                                     = "Chordata"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("bryozoans")]                                                     = "Bryozoan"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("bivalves", "gasteropods")]                                       = "Mollusca"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("seagrass")]                                                      = "Tracheophyta"

# Figure 3A
## Base Plot
Base <- ggplot(data = data_FE_Affected, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_point(aes(x = PC1, y = PC2, size = nb_sp_within_FE, fill = Phylum), col = "black", shape = 21) +
  scale_x_continuous(name = "PC1 (28.6%)", breaks = seq(-0.45, 0.35, 0.8)) + scale_y_continuous(name = "PC2 (18.8%)", breaks = seq(-0.2, 0.6, 0.8)) + 
  theme_minimal() + scale_size_continuous(range = c(1,8), breaks = seq(1,10,1), limits = c(0, 8)) + 
  scale_fill_manual(values = Fig_3a_col) + guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_text(size = 14),
        axis.title = element_text(size = 16), title = element_text(size = 14)) + ggtitle("")

# Starting Figure 3B
Conv_010_020 <- ggplot(data = dataset_030, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#f1f292") +
  geom_polygon(data = conv_hull_020, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), col = "black", fill = "#f1f292", size = 5, shape = 21, alpha = .05) +
  geom_point(data = dataset_020, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.3, 0.7, 1), limits = c(-0.3, 0.7)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), axis.text = element_blank(), 
        title = element_text(size = 14)) + 
  geom_label(label = paste("FE = ", 56 - dim(dataset_020)[1], "\n", "V   = ", "100%", sep = ""), fill = "#f1f292", x = 0.161, y = 0.41, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", dim(dataset_020)[1], "\n", "V   = ", "0.00%", sep = ""), fill = "white", x = 0.161, y = 0.64, hjust = 0, size = 4) +
  ggtitle("[20% – 10%[")

Conv_020_030 <- ggplot(data = dataset_030, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ffdc54") +
  geom_polygon(data = conv_hull_030, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), col = "black", fill = "#ffdc54", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.3, 0.7, 1), limits = c(-0.3, 0.7)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), axis.text = element_blank(),
        title = element_text(size = 14)) + 
  geom_label(label = paste("FE = ", 56 - dim(dataset_030)[1], "\n", "V   = ", "99.9%", sep = ""), fill = "#ffdc54", x = 0.161, y = 0.41, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", dim(dataset_030)[1], "\n", "V   = ", "0.12%", sep = ""), fill = "white", x = 0.161, y = 0.64, hjust = 0, size = 4) +
  ggtitle("[30% – 20%[")

Conv_030_040 <- ggplot(data = dataset_040, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ffa654") +
  geom_polygon(data = conv_hull_040, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), col = "black", fill = "#ffa654", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.3, 0.7, 1), limits = c(-0.3, 0.7)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), axis.text = element_blank(),
        title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", 56 - dim(dataset_040)[1], "\n", "V   = ", "99.7%", sep = ""), fill = "#ffa654", x = 0.161, y = 0.41, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", dim(dataset_040)[1], "\n", "V   = ", "0.33%", sep = ""), fill = "white", x = 0.161, y = 0.64, hjust = 0, size = 4) +
  ggtitle("[40% – 30%[")

Conv_040_050 <- ggplot(data = dataset_050, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ff8c24") +
  geom_polygon(data = conv_hull_050, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), col = "black", fill = "#ff8c24", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.3, 0.7, 1), limits = c(-0.3, 0.7)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), axis.text = element_blank(),
        title = element_text(size = 14)) + 
  geom_label(label = paste("FE = ", 56 - dim(dataset_050)[1], "\n", "V   = ", "97.4%", sep = ""), fill = "#ff8c24", x = 0.161, y = 0.41, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", dim(dataset_050)[1], "\n", "V   = ", "2.57%", sep = ""), fill = "white", x = 0.161, y = 0.64, hjust = 0, size = 4) +
  ggtitle("[50% – 40%[")

Conv_050_060 <- ggplot(data = dataset_060, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ca663a") +
  geom_polygon(data = conv_hull_060, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), col = "black", fill = "#ca663a", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.3, 0.7, 1), limits = c(-0.3, 0.7)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), axis.text = element_blank(),
        title = element_text(size = 14)) + 
  geom_label(label = paste("FE = ", 56 - dim(dataset_060)[1], "\n", "V   = ", "91.8%", sep = ""), fill = "#ca663a", x = 0.161, y = 0.41, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", dim(dataset_060)[1], "\n", "V   = ", "8.21%", sep = ""), fill = "white", x = 0.161, y = 0.64, hjust = 0, size = 4) +
  ggtitle("[60% – 50%[")

Conv_060_070 <- ggplot(data = dataset_070, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#c85250") +
  geom_polygon(data = conv_hull_070, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), col = "black", fill = "#c85250", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.3, 0.7, 1), limits = c(-0.3, 0.7)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), axis.text = element_blank(),
        title = element_text(size = 14)) + 
  geom_label(label = paste("FE = ", 56 - dim(dataset_070)[1], "\n", "V   = ", "90.9%", sep = ""), fill = "#c85250", x = 0.161, y = 0.41, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", dim(dataset_070)[1], "\n", "V   = ", "9.13%", sep = ""), fill = "white", x = 0.161, y = 0.64, hjust = 0, size = 4) +
  ggtitle("[70% – 60%[")

Conv_070_080 <- ggplot(data = dataset_080, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ca3a3a") +
  geom_polygon(data = conv_hull_080, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), col = "black", fill = "#ca3a3a", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.3, 0.7, 1), limits = c(-0.3, 0.7)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), axis.text = element_blank(),
        title = element_text(size = 14)) + 
  geom_label(label = paste("FE = ", 56 - dim(dataset_080)[1], "\n", "V   = ", "88.8%", sep = ""), fill = "#ca3a3a", x = 0.161, y = 0.41, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", dim(dataset_080)[1], "\n", "V   = ", "11.2%", sep = ""), fill = "white", x = 0.161, y = 0.64, hjust = 0, size = 4) +
  ggtitle("[80% – 70%[")

Conv_080_090 <- ggplot(data = dataset_090, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#bd0909") +
  geom_polygon(data = conv_hull_090, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), col = "black", fill = "#bd0909", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.3, 0.7, 1), limits = c(-0.3, 0.7)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), axis.text = element_blank(),
        title = element_text(size = 14)) + 
  geom_label(label = paste("FE = ", 56 - dim(dataset_090)[1], "\n", "V   = ", "77.7%", sep = ""), fill = "#bd0909", x = 0.161, y = 0.41, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", dim(dataset_090)[1], "\n", "V   = ", "22.3%", sep = ""), fill = "white", x = 0.161, y = 0.64, hjust = 0, size = 4) +
  ggtitle("[90% – 80%[")

Conv_090_100 <- ggplot(data = dataset_100, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#a20000") +
  geom_polygon(data = conv_hull_100, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), col = "black", fill = "#a20000", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.3, 0.7, 1), limits = c(-0.3, 0.7)) +
  theme(legend.position = "none", panel.border = element_rect(colour = "black", fill = NA, linewidth = 1), axis.text = element_blank(),
        title = element_text(size = 14)) + 
  geom_label(label = paste("FE = ", 56 - dim(dataset_100)[1], "\n", "V   = ", "62.9%", sep = ""), fill = "#a20000", x = 0.161, y = 0.41, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", dim(dataset_100)[1], "\n", "V   = ", "37.1%", sep = ""), fill = "white", x = 0.161, y = 0.64, hjust = 0, size = 4) +
  ggtitle("[100% – 90%[")

Figure_3 <- Base + ((Conv_090_100 + Conv_080_090 + Conv_070_080) / (Conv_060_070 + Conv_050_060 + Conv_040_050) / (Conv_030_040 + Conv_020_030 + Conv_010_020))