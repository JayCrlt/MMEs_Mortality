Complete_traits <- read_excel("../Data/R/Complete_Traits.xlsx", sheet = "Sheet1", col_types = rep("text", 13)) %>% arrange(Species)

### Figure S2 Post Review

Figure_S2a <- Complete_traits %>% count(Feeding) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Feeding, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "Distribution of trait categories (%)", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2b <- Complete_traits %>% count(Longevity) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Longevity, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2c <- Complete_traits %>% count(Coloniality) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Coloniality, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2d <- Complete_traits %>% count(Morphology) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Morphology, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2e <- Complete_traits %>% count(Storage) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Storage, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2f <- Complete_traits %>% count(Energy) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Energy, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "Distribution of trait categories (%)", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2g <- Complete_traits %>% dplyr::filter(Size != 0) %>% 
  count(Size) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Size, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2h <- Complete_traits %>% count(Growth) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Growth, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2i <- Complete_traits %>% count(Calcification) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Calcification, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2j <- Complete_traits %>% count(Motility) %>% mutate(percentage = n / sum(n) * 100) %>% 
  ggplot(aes(x = reorder(Motility, -percentage), y = percentage)) +
  geom_bar(stat = "identity", color = "black") + theme_classic() +
  scale_x_discrete(name = "") + scale_y_continuous(name = "", limits = c(0,100)) +
  theme(axis.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

Figure_S2 <- Figure_S2a + Figure_S2b + Figure_S2c + Figure_S2d + Figure_S2e +
  Figure_S2f + Figure_S2g + Figure_S2h + Figure_S2i + Figure_S2j + plot_layout(ncol = 5)

## Clean Complete_traits
# Check duplicate species
Complete_traits$duplicate = NA ; for (i in 2:length(Complete_traits$duplicate)) {
if (Complete_traits$Species[i-1] == Complete_traits$Species[i]) {
  Complete_traits$duplicate[i] = "TRUE"} else {Complete_traits$duplicate[i] = "FALSE"} }
dataset_duplicated    <- Complete_traits %>% dplyr::filter(Species %in% Complete_traits$Species[Complete_traits$duplicate == "TRUE"])
# Keep the last most up to date version
# dataset_duplicated$priority = NA ; for (i in 1:length(dataset_duplicated$priority)) {
# if (dataset_duplicated$Dataset[i] == "Mortality_MME") {
#  dataset_duplicated$priority[i] = 1} else if (dataset_duplicated$Dataset[i] == "Nuria_GCB_2024"){
#    dataset_duplicated$priority[i] = 2} else if (dataset_duplicated$Dataset[i] == "Golo et al 2024"){
#      dataset_duplicated$priority[i] = 3} else if (dataset_duplicated$Dataset[i] == "Galobart et al 2023"){
#        dataset_duplicated$priority[i] = 4} else { dataset_duplicated$priority[i] = 5 }}
# Final dataset cleaned
dataset_trait_cleaned <- Complete_traits %>% dplyr::filter(Species %notin% Complete_traits$Species[Complete_traits$duplicate == "TRUE"]) %>% select(-duplicate) %>% 
  full_join(dataset_duplicated %>% arrange(Species) %>% group_by(Species) %>% slice(1) %>% select(-c(duplicate))) %>% arrange(Species)

## Define the FE number
### Set up the data frames and define the FEs richness
tr_cat             <- data.frame(trait_name = colnames(dataset_trait_cleaned[3:12]), trait_type = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N"),
                                 fuzzy_name = rep(NA, 10))
sp_tr              <- dataset_trait_cleaned %>% column_to_rownames("Species") %>% dplyr::select(., 2:11)
sp_tr              <- sp_tr %>% dplyr::mutate_all(as.factor)
sp_to_fe           <- mFD::sp.to.fe(sp_tr = sp_tr, tr_cat = tr_cat) 
fe_nm              <- unique(sp_to_fe$fe_nm) 

### List of species in each FE
fe_sp              <- list() 
for (k in fe_nm) {
  fe_sp[[k]]       <- names(sp_to_fe$sp_fe[which(sp_to_fe$sp_fe == k)]) }
### Trait values of FE
fe_tr              <- sp_to_fe$fe_tr
data_sp_to_fe      <- sp_to_fe$sp_fe %>% data.frame() %>% rownames_to_column(., var = "Species") %>% rename(., FE = .)
fe_tr              <- fe_tr %>% data.frame() %>% rownames_to_column(., var = "FE") 
colnames(fe_tr)    <- c("FE", "Morphology", "Coloniality", "Longevity", "Height", "Energy", "Feeding", "Growth", "Calcification", "Mobility", "Storage")
table_sp_and_fe    <- inner_join(data_sp_to_fe, fe_tr, by = "FE") 

## Play with mortality rates
FE_matrix          <- FE_dataset %>% group_by(ID, FEs) %>% summarise(occurence = n()) %>% reshape2::acast(., ID~FEs, value.var = "occurence")

### Convert in Presence absence
FE_matrix[is.na(FE_matrix)] <- 0 ; FE_matrix[FE_matrix > 0] <- 1

### computing Gower distance between FEs
tr_cat[,1] <- colnames(fe_tr)[-1] ; colnames(sp_tr) <- colnames(fe_tr)[-1]
fe_tr <- fe_tr %>% column_to_rownames("FE")
fe_fe_dist <- funct.dist(fe_tr, tr_cat[,-3], metric = "gower")
fe_fspaces <- mFD::quality.fspaces(sp_dist = fe_fe_dist)

### Looking for how many axes
# print(round(fe_fspaces$quality_fspaces,2)) #6 dimensions | mAD = 0.055
fe_6D_coord <- fe_fspaces$details_fspaces$sp_pc_coord[,1:6] %>% data.frame()
conv_hull_tot = fe_6D_coord %>% slice(chull(PC1, PC2))

# Define the complete dataset
data_complete <- fe_6D_coord %>% rownames_to_column(var = "FE") %>% left_join(data_sp_to_fe) %>% data.frame() %>% mutate(Impaired = NA)
for (i in 1:length(data_complete$Impaired)) {
  if (data_complete$Species[i] %in% Complete_traits$Species[Complete_traits$Dataset == "Current study"]) {
  data_complete$Impaired[i] = "YES"} else {
  data_complete$Impaired[i] = "NO"}}

### Data mortality
data_mortality_from_complete  <- data_complete %>% dplyr::filter(Impaired == "YES")
data_filtered_MME_Merged_data <- MME_Merged_data %>% dplyr::select(year, ecoregion, `sub-ecoregion`, taxa, species, damaged_percentatge, damaged_qualitative,
                                                                   drivers_abiotic, drivers_abiotic_other, drivers_biotic, drivers_biotic_group, drivers_biotic_other)
data_mortality_from_complete  <- data_mortality_from_complete %>% left_join(data_filtered_MME_Merged_data %>% rename(Species = species)) %>%
  mutate(row = seq(1, 1856, 1))

### Clean dataset
data_mortality_from_complete_mistakes_1 <- data_mortality_from_complete %>% dplyr::filter(damaged_qualitative == "Low", damaged_percentatge >= 30)
data_mortality_from_complete_mistakes_2 <- data_mortality_from_complete %>% dplyr::filter(damaged_qualitative == "Moderate", damaged_percentatge < 30)
data_mortality_from_complete_mistakes_3 <- data_mortality_from_complete %>% dplyr::filter(damaged_qualitative == "Severe", damaged_percentatge < 60)
# Correct according to percentages
data_mortality_from_complete_mistakes_1$damaged_qualitative[data_mortality_from_complete_mistakes_1$damaged_percentatge > 30] = "Moderate"
data_mortality_from_complete_mistakes_3$damaged_qualitative[data_mortality_from_complete_mistakes_3$damaged_percentatge > 40] = "Moderate"
# Correct according to quality information
data_mortality_from_complete_mistakes_2$damaged_percentatge[data_mortality_from_complete_mistakes_2$damaged_qualitative == "Moderate"] = 30
data_mortality_from_complete_mistakes_3$damaged_percentatge[data_mortality_from_complete_mistakes_3$damaged_percentatge == 0] = 60
# remove incoherent information
rows_to_remove <- c(data_mortality_from_complete_mistakes_1$row, data_mortality_from_complete_mistakes_2$row, data_mortality_from_complete_mistakes_3$row)
data_mortality_from_complete <- data_mortality_from_complete %>% dplyr::filter(row %notin% rows_to_remove) %>% 
  rbind(data_mortality_from_complete_mistakes_1, data_mortality_from_complete_mistakes_2, data_mortality_from_complete_mistakes_3)
## Although this information is interesting, we defined MMEs >= 10% of mortality rate
# data_mortality_from_complete <- data_mortality_from_complete %>% dplyr::filter(damaged_percentatge > 0)

## Redefine mortality records as matrix
data_FE_Affected <- data_mortality_from_complete %>% mutate(damage_sub_cat = cut_width(damaged_percentatge, 10, boundary = 0))
data_FE_Affected <- table(data_FE_Affected$damage_sub_cat, data_FE_Affected$FE) %>% as.data.frame.matrix() 

{data_FE_Affected[1,]   <- data_FE_Affected[1,] + data_FE_Affected[2,] + data_FE_Affected[3,] + data_FE_Affected[4,] + data_FE_Affected[5,] + 
    data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[2,]  <- data_FE_Affected[2,] + data_FE_Affected[3,] + data_FE_Affected[4,] + data_FE_Affected[5,] + data_FE_Affected[6,] + 
    data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[3,]  <- data_FE_Affected[3,] + data_FE_Affected[4,] + data_FE_Affected[5,] + data_FE_Affected[6,] + data_FE_Affected[7,] + 
    data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[4,]  <- data_FE_Affected[4,] + data_FE_Affected[5,] + data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + 
    data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[5,]  <- data_FE_Affected[5,] + data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + 
    data_FE_Affected[10,]
  data_FE_Affected[6,]  <- data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[7,]  <- data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[8,]  <- data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[9,]  <- data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[10,] <- data_FE_Affected[10,]}

data_trait_occurences <- data_FE_Affected %>% t() %>% data.frame() %>% rownames_to_column(var = "FE") %>% 
  arrange(FE) %>% rename(., `[0-10]` = X.0.10., `]10-20]` = X.10.20., `]20-30]` = X.20.30., `]30-40]` = X.30.40., `]40-50]` = X.40.50., `]50-60]` = X.50.60.,
                          `]60-70]` = X.60.70., `]70-80]` = X.70.80., `]80-90]` = X.80.90., `]90-100]` = X.90.100.) %>% 
  inner_join(data_mortality_from_complete) %>% arrange(row) %>% group_by(FE) %>% slice(1)

### Define corresponding datasets
dataset_010 = data_trait_occurences %>% dplyr::filter(`[0-10]`   > 0)
dataset_020 = data_trait_occurences %>% dplyr::filter(`]10-20]`  > 0)
dataset_030 = data_trait_occurences %>% dplyr::filter(`]20-30]`  > 0)
dataset_040 = data_trait_occurences %>% dplyr::filter(`]30-40]`  > 0)
dataset_050 = data_trait_occurences %>% dplyr::filter(`]40-50]`  > 0)
dataset_060 = data_trait_occurences %>% dplyr::filter(`]50-60]`  > 0)
dataset_070 = data_trait_occurences %>% dplyr::filter(`]60-70]`  > 0)
dataset_080 = data_trait_occurences %>% dplyr::filter(`]70-80]`  > 0)
dataset_090 = data_trait_occurences %>% dplyr::filter(`]80-90]`  > 0)
dataset_100 = data_trait_occurences %>% dplyr::filter(`]90-100]` > 0)

### Define convex hull
conv_hull_tot = data_complete %>%  slice(chull(PC1, PC2))
conv_hull_010 = dataset_010 %>% data.frame() %>% slice(chull(PC1, PC2))
conv_hull_020 = dataset_020 %>% data.frame() %>% slice(chull(PC1, PC2))
conv_hull_030 = dataset_030 %>% data.frame() %>% slice(chull(PC1, PC2))
conv_hull_040 = dataset_040 %>% data.frame() %>% slice(chull(PC1, PC2))
conv_hull_050 = dataset_050 %>% data.frame() %>% slice(chull(PC1, PC2))
conv_hull_060 = dataset_060 %>% data.frame() %>% slice(chull(PC1, PC2))
conv_hull_070 = dataset_070 %>% data.frame() %>% slice(chull(PC1, PC2))
conv_hull_080 = dataset_080 %>% data.frame() %>% slice(chull(PC1, PC2))
conv_hull_090 = dataset_090 %>% data.frame() %>% slice(chull(PC1, PC2))
conv_hull_100 = dataset_100 %>% data.frame() %>% slice(chull(PC1, PC2))

### Volume
VTot = cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2, PC3, PC4) %>% distinct() %>% as.matrix())$volume
V10  = round((cxhull::cxhull(dataset_010[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V20  = round((cxhull::cxhull(dataset_020[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V30  = round((cxhull::cxhull(dataset_030[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V40  = round((cxhull::cxhull(dataset_040[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V50  = round((cxhull::cxhull(dataset_050[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V60  = round((cxhull::cxhull(dataset_060[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V70  = round((cxhull::cxhull(dataset_070[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V80  = round((cxhull::cxhull(dataset_080[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V90  = round((cxhull::cxhull(dataset_090[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V100 = round((cxhull::cxhull(dataset_100[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)

# Complete the dataset
data_complete              <- data_complete %>% left_join(Complete_traits %>% dplyr::select(Species, Taxonomy))
nb_sp_within_FE            <- data_complete %>% select(Species, FE) %>% distinct() %>% group_by(FE) %>% summarise(nb_sp_within_FE = n())
data_complete              <- data_complete %>% left_join(nb_sp_within_FE)
data_complete_impaired     <- data_complete %>% dplyr::filter(Impaired == "YES") 
data_complete_non_impaired <- data_complete %>% dplyr::filter(Impaired == "NO")

# Building the Figure 3
Base <- ggplot() + 
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, aes(x = PC1, y = PC2), alpha = .8, col = "black", fill = "white") +
  geom_point(data = data_complete_non_impaired, aes(x = PC1, y = PC2, size = nb_sp_within_FE), fill = "white", shape = 21, alpha = .25) +
  geom_point(data = data_complete_impaired, aes(x = PC1, y = PC2, fill = Taxonomy, size = nb_sp_within_FE), shape = 21) +
  scale_x_continuous(name = "PC1 (34.8%)", breaks = seq(-0.4, 0.4, 0.8), limits = c(-0.4, 0.45)) + 
  scale_y_continuous(name = "PC2 (19.4%)", breaks = seq(-0.4, 0.4, 0.8), limits = c(-0.4, 0.45)) + 
  theme_minimal() + scale_size_continuous(range = c(1,12), breaks = seq(1,12,1), limits = c(0, 12)) + 
  scale_fill_manual(values = Fig_3a_col) + guides(size = guide_legend(nrow = 1), fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_text(size = 14),
        axis.title = element_text(size = 16), title = element_text(size = 14), panel.grid.major = element_blank()) + ggtitle("")

Conv_010_020  <- ggplot(data = dataset_010, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_010, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#f1f292") +
  geom_point(data = dataset_010, aes(x = PC1, y = PC2), col = "black", fill = "#f1f292", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + scale_size_continuous(range = c(1,11), breaks = seq(1,11,1), limits = c(0, 11)) + 
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_blank(),
        axis.title = element_text(size = 16), title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", 55, "\n", "V   = ", "47.4%", sep = ""), fill = "#f1f292", x = 0.155, y = -0.44, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", length(fe_nm) - 55, "\n", "V   = ", "52.6%", sep = ""), fill = "white", x = -0.55, y = -0.44, hjust = 0, size = 4) +
  ggtitle("≥ 10%")

Conv_020_030 <- ggplot(data = dataset_030, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_030, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#ffdc54") +
  geom_point(data = dataset_030, aes(x = PC1, y = PC2), col = "black", fill = "#ffdc54", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + scale_size_continuous(range = c(1,11), breaks = seq(1,11,1), limits = c(0, 11)) + 
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_blank(),
        axis.title = element_text(size = 16), title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", dim(dataset_030)[1], "\n", "V   = ", "43.9%", sep = ""), fill = "#ffdc54", x = 0.155, y = -0.44, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", length(fe_nm) - dim(dataset_030)[1], "\n", "V   = ", "56.1%", sep = ""), fill = "white", x = -0.55, y = -0.44, hjust = 0, size = 4) +
  ggtitle("> 20%")

Conv_030_040 <- ggplot(data = dataset_040, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_040, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#ffa654") +
  geom_point(data = dataset_040, aes(x = PC1, y = PC2), col = "black", fill = "#ffa654", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + scale_size_continuous(range = c(1,11), breaks = seq(1,11,1), limits = c(0, 11)) + 
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_blank(),
        axis.title = element_text(size = 16), title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", dim(dataset_040)[1], "\n", "V   = ", "43.9%", sep = ""), fill = "#ffa654", x = 0.155, y = -0.44, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", length(fe_nm) - dim(dataset_040)[1], "\n", "V   = ", "56.1%", sep = ""), fill = "white", x = -0.55, y = -0.44, hjust = 0, size = 4) +
  ggtitle("> 30%")

Conv_040_050 <- ggplot(data = dataset_050, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_050, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#ff8c24") +
  geom_point(data = dataset_050, aes(x = PC1, y = PC2), col = "black", fill = "#ff8c24", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + scale_size_continuous(range = c(1,11), breaks = seq(1,11,1), limits = c(0, 11)) + 
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_blank(),
        axis.title = element_text(size = 16), title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", dim(dataset_050)[1], "\n", "V   = ", "41.0%", sep = ""), fill = "#ff8c24", x = 0.155, y = -0.44, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", length(fe_nm) - dim(dataset_050)[1], "\n", "V   = ", "59.0%", sep = ""), fill = "white", x = -0.55, y = -0.44, hjust = 0, size = 4) +
  ggtitle("> 40%")

Conv_050_060 <- ggplot(data = dataset_060, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_060, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#ca663a") +
  geom_point(data = dataset_060, aes(x = PC1, y = PC2), col = "black", fill = "#ca663a", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + scale_size_continuous(range = c(1,11), breaks = seq(1,11,1), limits = c(0, 11)) + 
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_blank(),
        axis.title = element_text(size = 16), title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", dim(dataset_060)[1], "\n", "V   = ", "38.8%", sep = ""), fill = "#ca663a", x = 0.155, y = -0.44, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", length(fe_nm) - dim(dataset_060)[1], "\n", "V   = ", "61.2%", sep = ""), fill = "white", x = -0.55, y = -0.44, hjust = 0, size = 4) +
  ggtitle("> 50%")

Conv_060_070 <- ggplot(data = dataset_070, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_070, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#c85250") +
  geom_point(data = dataset_070, aes(x = PC1, y = PC2), col = "black", fill = "#c85250", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + scale_size_continuous(range = c(1,11), breaks = seq(1,11,1), limits = c(0, 11)) + 
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_blank(),
        axis.title = element_text(size = 16), title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", dim(dataset_070)[1], "\n", "V   = ", "37.9%", sep = ""), fill = "#c85250", x = 0.155, y = -0.44, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", length(fe_nm) - dim(dataset_070)[1], "\n", "V   = ", "62.1%", sep = ""), fill = "white", x = -0.55, y = -0.44, hjust = 0, size = 4) +
  ggtitle("> 60%")

Conv_070_080 <- ggplot(data = dataset_080, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_080, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#ca3a3a") +
  geom_point(data = dataset_080, aes(x = PC1, y = PC2), col = "black", fill = "#ca3a3a", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + scale_size_continuous(range = c(1,11), breaks = seq(1,11,1), limits = c(0, 11)) + 
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_blank(),
        axis.title = element_text(size = 16), title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", dim(dataset_080)[1], "\n", "V   = ", "28.2%", sep = ""), fill = "#ca3a3a", x = 0.155, y = -0.44, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", length(fe_nm) - dim(dataset_080)[1], "\n", "V   = ", "71.8%", sep = ""), fill = "white", x = -0.55, y = -0.44, hjust = 0, size = 4) +
  ggtitle("> 70%")

Conv_080_090 <- ggplot(data = dataset_090, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_090, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#bd0909") +
  geom_point(data = dataset_090, aes(x = PC1, y = PC2), col = "black", fill = "#bd0909", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + scale_size_continuous(range = c(1,11), breaks = seq(1,11,1), limits = c(0, 11)) + 
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_blank(),
        axis.title = element_text(size = 16), title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", dim(dataset_090)[1], "\n", "V   = ", "22.7%", sep = ""), fill = "#bd0909", x = 0.155, y = -0.44, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", length(fe_nm) - dim(dataset_090)[1], "\n", "V   = ", "77.3%", sep = ""), fill = "white", x = -0.55, y = -0.44, hjust = 0, size = 4) +
  ggtitle("> 80%")

Conv_090_100 <- ggplot(data = dataset_100, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_polygon(data = conv_hull_100, aes(x = PC1, y = PC2), alpha = .95, col = "black", fill = "#a20000") +
  geom_point(data = dataset_100, aes(x = PC1, y = PC2), col = "black", fill = "#a20000", size = 5, shape = 21) + theme_minimal() +  
  scale_x_continuous(name = "PC1", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  scale_y_continuous(name = "PC2", breaks = seq(-0.5, 0.5, 1.0), limits = c(-0.5, 0.5)) + 
  theme_minimal() + scale_size_continuous(range = c(1,11), breaks = seq(1,11,1), limits = c(0, 11)) + 
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom", panel.border = element_rect(colour = "black", fill=NA, linewidth=1), axis.text = element_blank(),
        axis.title = element_text(size = 16), title = element_text(size = 14)) +
  geom_label(label = paste("FE = ", dim(dataset_100)[1], "\n", "V   = ", "19.1%", sep = ""), fill = "#a20000", x = 0.155, y = -0.44, hjust = 0, size = 4) +
  geom_label(label = paste("FE = ", length(fe_nm) - dim(dataset_100)[1], "\n", "V   = ", "80.9%", sep = ""), fill = "white", x = -0.55, y = -0.44, hjust = 0, size = 4) +
  ggtitle("> 90%")

Figure_3 <- Base +  ((Conv_010_020 + Conv_020_030 + Conv_030_040) / (Conv_040_050 + Conv_050_060 + Conv_060_070) / (Conv_070_080 + Conv_080_090 + Conv_090_100))