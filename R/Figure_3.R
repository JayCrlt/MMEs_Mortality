#### Figure_3
grid_dataset <- grid_dataset %>% 
  dplyr::select(-c(lon_rounded_0, lon_rounded_1, lat_rounded_0, lat_rounded_1, x.y, geometry, x))

# Refine percentage
grid_dataset <- grid_dataset %>% dplyr::filter(damaged_percentatge > 0) # Problem with 0 occurences
grid_dataset$damage_sub_cat <- cut_width(grid_dataset$damaged_percentatge, 10, boundary = 0)

# Refine cumulative – If affected at further level, it'd be affected correspondingly
data_FE_Affected <- table(grid_dataset$damage_sub_cat, grid_dataset$FEs) %>% as.data.frame.matrix() 

{data_FE_Affected[1,]  <- data_FE_Affected[1,] + data_FE_Affected[2,] + data_FE_Affected[3,] +
    data_FE_Affected[4,] + data_FE_Affected[5,] + data_FE_Affected[6,] + data_FE_Affected[7,] +
    data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[2,]  <- data_FE_Affected[2,] + data_FE_Affected[3,] + data_FE_Affected[4,] + 
    data_FE_Affected[5,] + data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + 
    data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[3,]  <- data_FE_Affected[3,] + data_FE_Affected[4,] + data_FE_Affected[5,] + 
    data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + 
    data_FE_Affected[10,]
  data_FE_Affected[4,]  <- data_FE_Affected[4,] + data_FE_Affected[5,] + data_FE_Affected[6,] + 
    data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[5,]  <- data_FE_Affected[5,] + data_FE_Affected[6,] + data_FE_Affected[7,] + 
    data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[6,]  <- data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + 
    data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[7,]  <- data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + 
    data_FE_Affected[10,]
  data_FE_Affected[8,]  <- data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[9,]  <- data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[10,] <- data_FE_Affected[10,]}

# rowSums(data_FE_Affected)[1] # 1532 occurrences considered

# Convert in P/A
data_FE_Affected_PA = data_FE_Affected
data_FE_Affected_PA[data_FE_Affected > 0] = "affected"
data_FE_Affected_PA[data_FE_Affected == 0] = 1
data_FE_Affected_PA[data_FE_Affected_PA == "affected"] = 0

data_FE_Affected_PA <- t(data_FE_Affected_PA)
data_FE_Affected_PA <- type.convert(data_FE_Affected_PA, as.is = TRUE)

data_FE_Affected <- t(data_FE_Affected)
data_FE_Affected <- type.convert(data_FE_Affected, as.is = TRUE)
data_FE_Affected <- data_FE_Affected %>% data.frame()

# Merge Data_FE_Affected with PCoA coords
data_FE_Affected_PA <- data_FE_Affected_PA %>% data.frame() %>% rownames_to_column("FEs") %>% 
  inner_join((fe_6D_coord %>% data.frame() %>% rownames_to_column("FEs")), by = "FEs")

data_FE_Affected <- data_FE_Affected %>% data.frame() %>% rownames_to_column("FEs") %>% 
  inner_join((fe_6D_coord %>% data.frame() %>% rownames_to_column("FEs")), by = "FEs")

# Plot FE hypervolume space

### Define convex hull
conv_hull_tot = data_FE_Affected_PA %>%  slice(chull(PC1, PC2))
conv_hull_010 = data_FE_Affected_PA %>% filter(X.0.10.   == 1) %>% slice(chull(PC1, PC2))
conv_hull_020 = data_FE_Affected_PA %>% filter(X.10.20.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_030 = data_FE_Affected_PA %>% filter(X.20.30.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_040 = data_FE_Affected_PA %>% filter(X.30.40.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_050 = data_FE_Affected_PA %>% filter(X.40.50.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_060 = data_FE_Affected_PA %>% filter(X.50.60.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_070 = data_FE_Affected_PA %>% filter(X.60.70.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_080 = data_FE_Affected_PA %>% filter(X.70.80.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_090 = data_FE_Affected_PA %>% filter(X.80.90.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_100 = data_FE_Affected_PA %>% filter(X.90.100. == 1) %>% slice(chull(PC1, PC2))

### Define corresponding datasets
dataset_010 = data_FE_Affected_PA %>% filter(X.0.10.   == 1) 
dataset_020 = data_FE_Affected_PA %>% filter(X.10.20.  == 1) 
dataset_030 = data_FE_Affected_PA %>% filter(X.20.30.  == 1) 
dataset_040 = data_FE_Affected_PA %>% filter(X.30.40.  == 1) 
dataset_050 = data_FE_Affected_PA %>% filter(X.40.50.  == 1) 
dataset_060 = data_FE_Affected_PA %>% filter(X.50.60.  == 1)
dataset_070 = data_FE_Affected_PA %>% filter(X.60.70.  == 1) 
dataset_080 = data_FE_Affected_PA %>% filter(X.70.80.  == 1)
dataset_090 = data_FE_Affected_PA %>% filter(X.80.90.  == 1) 
dataset_100 = data_FE_Affected_PA %>% filter(X.90.100. == 1) 

### Volume
VTot = cxhull::cxhull(data_FE_Affected_PA[,12:15] %>% as.matrix())$volume
V30  = round(((cxhull::cxhull(dataset_030[,12:13] %>% as.matrix())$volume / 
                 cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^3) * 100, 2)
V40  = round(((cxhull::cxhull(dataset_040[,12:13] %>% as.matrix())$volume / 
                 cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^3) * 100, 2)
V50  = round((cxhull::cxhull(dataset_050[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V60  = round((cxhull::cxhull(dataset_060[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V70  = round((cxhull::cxhull(dataset_070[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V80  = round((cxhull::cxhull(dataset_080[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V90  = round((cxhull::cxhull(dataset_090[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V100 = round((cxhull::cxhull(dataset_100[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)

data_FE_Affected <- table(data_sp_to_fe$FE) %>% data.frame() %>% 
  rename(FEs = Var1, nb_sp_within_FE = Freq) %>% 
  right_join(data_FE_Affected, by = "FEs")

data_FE_Affected <- data_sp_to_fe %>% rename(species = Species) %>%  right_join(species_traits, by = "species") %>% 
  dplyr::select(FE, group) %>% rename(FEs = FE, Taxa = group) %>% 
  right_join(data_FE_Affected, by = "FEs")

data_FE_Affected$Phylum = NA
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("red.algae", "cca")] = "Rhodophyta"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("brown.algae")] = "Seaweeds"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("actinaria", "octocorals", "gorgonians", "corals", "zooanthids")] = "Cnidaria"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("echinoderms")] = "Echinoderms"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("sponges")] = "Porifera"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("tunicates")] = "Chordata"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("bryozoans")] = "Bryozoan"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("bivalves", "gasteropods")] = "Mollusca"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("seagrass")] = "Tracheophyta"

data_FE_Affected$FEs[data_FE_Affected$Taxa == "red.algae"]

## Base Plot
Base <- ggplot(data = data_FE_Affected, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_point(aes(x = PC1, y = PC2, size = nb_sp_within_FE, fill = Phylum), col = "black", shape = 21) +
  theme_minimal() + scale_size_continuous(range = c(1,8), 
                                          breaks = seq(1,10,1),
                                          limits = c(0, 8)) + 
  scale_fill_manual(values = c("gray60", "darkolivegreen3", "salmon", "darkslateblue", "sienna3",
                               "goldenrod", "violetred3", "brown", "palegreen4")) +
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  ggtitle("Functional entities hypervolume space")

# Low Damage
Conv_010_020 <- ggplot(data = dataset_030, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#a20000") +
  geom_polygon(data = conv_hull_020, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#a20000", size = 5, shape = 21, alpha = .05) +
  geom_point(data = dataset_020, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_020)[1], "\n", "V   = ", "0.00%",
                           sep = ""), fill = "#a20000",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[20% – 10%[")

Conv_020_030 <- ggplot(data = dataset_030, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#bd0909") +
  geom_polygon(data = conv_hull_030, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#bd0909", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_030)[1], "\n", "V   = ", "0.12%",
                           sep = ""), fill = "#bd0909",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[30% – 20%[")

# Moderate Damage
Conv_030_040 <- ggplot(data = dataset_040, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ca3a3a") +
  geom_polygon(data = conv_hull_040, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ca3a3a", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) +
  geom_label(label = paste("FE = ", dim(dataset_040)[1], "\n", "V   = ", "0.33%",
                           sep = ""), fill = "#ca3a3a",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[40% – 30%[")

Conv_040_050 <- ggplot(data = dataset_050, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#c85250") +
  geom_polygon(data = conv_hull_050, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#c85250", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_050)[1], "\n", "V   = ", "2.57%",
                           sep = ""), fill = "#c85250",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[50% – 40%[")

Conv_050_060 <- ggplot(data = dataset_060, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ca663a") +
  geom_polygon(data = conv_hull_060, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ca663a", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_060)[1], "\n", "V   = ", "8.21%",
                           sep = ""), fill = "#ca663a",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[60% – 50%[")

# High damage
Conv_060_070 <- ggplot(data = dataset_070, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ff8c24") +
  geom_polygon(data = conv_hull_070, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ff8c24", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_070)[1], "\n", "V   = ", "9.13%",
                           sep = ""), fill = "#ff8c24",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[70% – 60%[")

Conv_070_080 <- ggplot(data = dataset_080, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ffa654") +
  geom_polygon(data = conv_hull_080, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ffa654", size = 5, shape = 21, alpha = .05) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_label(label = paste("FE = ", dim(dataset_080)[1], "\n", "V   = ", "11.2%",
                           sep = ""), fill = "#ffa654",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[80% – 70%[")

Conv_080_090 <- ggplot(data = dataset_090, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ffdc54") +
  geom_polygon(data = conv_hull_090, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ffdc54", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_label(label = paste("FE = ", dim(dataset_090)[1], "\n", "V   = ", "22.3%",
                           sep = ""), fill = "#ffdc54",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[90% – 80%[")

Conv_090_100 <- ggplot(data = dataset_100, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#f1f292") +
  geom_polygon(data = conv_hull_100, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#f1f292", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_100)[1], "\n", "V   = ", "37.1%",
                           sep = ""), fill = "#f1f292",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[100% – 90%[")

Figure_3 <- Base + ((Conv_090_100 + Conv_080_090 + Conv_070_080) / (Conv_060_070 + Conv_050_060 + Conv_040_050) /
                      (Conv_030_040 + Conv_020_030 + Conv_010_020))

#ggsave(Figure_3, filename = "Figure_3.png", device = "png", width = 18.00, height = 9.50, units = "in", dpi = 300)