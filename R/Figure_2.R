#### Figure_2
Global_dataset <- table_sp_and_fe_sankey %>% rename(species = Species) %>% data.frame() %>% 
  inner_join(MME_Mortality, by = "species")
data_trait_occurences <- data_FE_Affected %>% 
  mutate(FEs = recode(FEs, "fe_1" = "fe_01", "fe_2" = "fe_02", "fe_3" = "fe_03", 
                      "fe_4" = "fe_04", "fe_5" = "fe_05", "fe_6" = "fe_06", 
                      "fe_7" = "fe_07", "fe_8" = "fe_08", "fe_9" = "fe_09")) %>% 
  arrange(FEs) %>% rename(., `[0-10]` = X.0.10., `]10-20]` = X.10.20., `]20-30]` = X.20.30.,
                          `]30-40]` = X.30.40., `]40-50]` = X.40.50., `]50-60]` = X.50.60.,
                          `]60-70]` = X.60.70., `]70-80]` = X.70.80., `]80-90]` = X.80.90.,
                          `]90-100]` = X.90.100.) %>% 
  dplyr::select(-c(PC1, PC2, PC3, PC4, PC5, PC6))

Traits_Code <- Global_dataset[,2:12] %>% distinct() %>% rename(., FEs = FE)

## Data_traits_occurences
data_trait_occurences <- data_trait_occurences %>% inner_join(Traits_Code, by = "FEs")

## Traits
Morpho_trait <- data_trait_occurences[c(4:13,15)] %>% group_by(Morphology) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Morphology") %>% t() %>% data.frame() %>% rownames_to_column("group")

Morpho_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Morpho_trait[c(1, 4, 7), 2], 2),
  b = round(Morpho_trait[c(1, 4, 7), 3], 2),
  c = round(Morpho_trait[c(1, 4, 7), 4], 2),
  d = round(Morpho_trait[c(1, 4, 7), 5], 2),
  e = round(Morpho_trait[c(1, 4, 7), 6], 2),
  f = round(Morpho_trait[c(1, 4, 7), 7], 2),
  g = round(Morpho_trait[c(1, 4, 7), 8], 2),
  h = round(Morpho_trait[c(1, 4, 7), 9], 2))
Morpho_trait <- Morpho_trait %>% rownames_to_column("group")

F2_A <- ggradar(Morpho_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Encrusting", "Filamentous", "Foliose", "Cup-like",
                                "Massive-\nencrusting", "massive-\nhemispheric",
                                "Massive-\nerect", "Tree-like"),
                legend.text.size = 10) +
  ggtitle("Morphology") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Coloniality_trait <- data_trait_occurences[,c(4:13, 16)] %>% group_by(Coloniality) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Coloniality") %>% t() %>% data.frame() %>% rownames_to_column("group")

Colon_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = c(0, 0, 0),
  b = c(0, 0, 0),
  c = round(Coloniality_trait[c(1, 4, 7), 2], 2),
  d = c(0, 0, 0),
  e = c(0, 0, 0),
  f = c(0, 0, 0),
  g = round(Coloniality_trait[c(1, 4, 7), 3], 2),
  h = c(0, 0, 0))
Colon_trait <- Colon_trait %>% rownames_to_column("group")

F2_B <- ggradar(Colon_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("", "", "Solitary", "",
                                "", "", "Colonial", ""),
                legend.text.size = 10) +
  ggtitle("Coloniality") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Longevity_trait <- data_trait_occurences[,c(4:13, 17)] %>% group_by(Longevity) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Longevity") %>% t() %>% data.frame() %>% rownames_to_column("group")

Longevity_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Longevity_trait[c(1, 4, 7), 2], 2),
  b = round(Longevity_trait[c(1, 4, 7), 3], 2),
  c = round(Longevity_trait[c(1, 4, 7), 4], 2),
  d = round(Longevity_trait[c(1, 4, 7), 5], 2),
  e = round(Longevity_trait[c(1, 4, 7), 6], 2))
Longevity_trait <- Longevity_trait %>% rownames_to_column("group")

F2_C <- ggradar(Longevity_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("[0; 1] year", "[1; 5[ years", "[5; 10[ years", 
                                "[10; 20[ years",
                                "≥ 20 years"),
                legend.text.size = 10) +
  ggtitle("Longevity") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Height_trait <- data_trait_occurences[,c(4:13, 18)] %>% group_by(Height) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Height") %>% t() %>% data.frame() %>% rownames_to_column("group")

Height_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Height_trait[c(1, 4, 7), 3], 2),
  b = round(Height_trait[c(1, 4, 7), 4], 2),
  c = round(Height_trait[c(1, 4, 7), 5], 2),
  d = round(Height_trait[c(1, 4, 7), 6], 2),
  e = round(Height_trait[c(1, 4, 7), 7], 2))
Height_trait <- Height_trait %>% rownames_to_column("group")

F2_D <- ggradar(Height_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Extreme low", "Low", "Moderate", "High",
                                "Very high"),
                legend.text.size = 10) +
  ggtitle("Height") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Energy_trait <- data_trait_occurences[,c(4:13, 19)] %>% group_by(Energy) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Energy") %>% t() %>% data.frame() %>% rownames_to_column("group")

Energy_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Energy_trait[c(1, 4, 7), 2], 2),
  b = round(Energy_trait[c(1, 4, 7), 3], 2),
  c = round(Energy_trait[c(1, 4, 7), 4], 2))
Energy_trait <- Energy_trait %>% rownames_to_column("group")

F2_E <- ggradar(Energy_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Photosynthetic \nautotroph", "Photo-\nheterotroph", 
                                "Heterotroph"),
                legend.text.size = 10) +
  ggtitle("Energetic resources") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Feeding_trait <- data_trait_occurences[,c(4:13, 20)] %>% group_by(Feeding) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Feeding") %>% t() %>% data.frame() %>% rownames_to_column("group")

Feeding_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Feeding_trait[c(1, 4, 7), 2], 2),
  b = round(Feeding_trait[c(1, 4, 7), 3], 2),
  c = round(Feeding_trait[c(1, 4, 7), 4], 2),
  d = round(Feeding_trait[c(1, 4, 7), 5], 2),
  e = round(Feeding_trait[c(1, 4, 7), 6], 2),
  f = round(Feeding_trait[c(1, 4, 7), 7], 2),
  g = round(Feeding_trait[c(1, 4, 7), 8], 2))
Feeding_trait <- Feeding_trait %>% rownames_to_column("group")

F2_F <- ggradar(Feeding_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Autotrophy", "Filter-feeders \nwith cilliae", 
                                "Filter-feeders \nby pumping", "Passive \nfilter-feeders", 
                                "Grazers", "Carnivores", "Detritivores"),
                legend.text.size = 10) +
  ggtitle("Feeding") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Growth_trait <- data_trait_occurences[,c(4:13, 21)] %>% group_by(Growth) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Growth") %>% t() %>% data.frame() %>% rownames_to_column("group")

Growth_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Growth_trait[c(1, 4, 7), 2], 2),
  b = round(Growth_trait[c(1, 4, 7), 3], 2),
  c = round(Growth_trait[c(1, 4, 7), 4], 2),
  d = round(Growth_trait[c(1, 4, 7), 5], 2),
  e = round(Growth_trait[c(1, 4, 7), 6], 2))
Growth_trait <- Growth_trait %>% rownames_to_column("group")

F2_G <- ggradar(Growth_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Extreme slow", "Slow", "Moderate", "High",
                                "Very high"),
                legend.text.size = 10) +
  ggtitle("Growth") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Calcification_trait <- data_trait_occurences[,c(4:13, 22)] %>% group_by(Calcification) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Calcification") %>% t() %>% data.frame() %>% rownames_to_column("group")

Calcification_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = c(0, 0, 0),
  b = c(0, 0, 0),
  c = round(Calcification_trait[c(1, 4, 7), 2], 2),
  d = c(0, 0, 0),
  e = c(0, 0, 0),
  f = c(0, 0, 0),
  g = round(Calcification_trait[c(1, 4, 7), 3], 2),
  h = c(0, 0, 0))
Calcification_trait <- Calcification_trait %>% rownames_to_column("group")

F2_H <- ggradar(Calcification_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("", "", "Without \ncalcareous \nstructures", "",
                                "", "", "With \ncalcareous \nstructures", ""),
                legend.text.size = 10) +
  ggtitle("Calcification") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Mobility_trait <- data_trait_occurences[,c(4:13, 23)] %>% group_by(Mobility) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Mobility") %>% t() %>% data.frame() %>% rownames_to_column("group")

Mobility_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = c(0, 0, 0),
  b = c(0, 0, 0),
  c = round(Mobility_trait[c(1, 4, 7), 2], 2),
  d = c(0, 0, 0),
  e = c(0, 0, 0),
  f = c(0, 0, 0),
  g = round(Mobility_trait[c(1, 4, 7), 3], 2),
  h = c(0, 0, 0))
Mobility_trait <- Mobility_trait %>% rownames_to_column("group")

F2_I <- ggradar(Mobility_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("", "", "Sessile", "",
                                "", "", "Vagile", ""),
                legend.text.size = 10) +
  ggtitle("Mobility") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Storage_trait <- data_trait_occurences[,c(4:13, 24)] %>% group_by(Storage) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./20.73)) %>% data.frame() %>% 
  column_to_rownames("Storage") %>% t() %>% data.frame() %>% rownames_to_column("group")

Storage_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Storage_trait[c(1, 4, 7), 2], 2),
  b = round(Storage_trait[c(1, 4, 7), 3], 2),
  c = round(Storage_trait[c(1, 4, 7), 4], 2))
Storage_trait <- Storage_trait %>% rownames_to_column("group")

F2_J <- ggradar(Storage_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Storing", "Likely-\nstoring", "Non-\nstoring"),
                legend.text.size = 10) +
  ggtitle("Carbon storage") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Figure_2 = F2_A + F2_B + F2_C + F2_D + F2_E + F2_F + F2_G + F2_H + F2_I + F2_J +
  plot_layout(guides = "collect", ncol = 5) & 
  theme(legend.position = 'bottom') 