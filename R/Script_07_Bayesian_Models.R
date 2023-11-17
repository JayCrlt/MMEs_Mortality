Perturbations <- rep(NA, length = length(Global_dataset$species))
Perturbations <- paste(Global_dataset$drivers_abiotic, Global_dataset$drivers_biotic_group, sep = " and ")
Perturbations[Perturbations == "NA and NA"] = NA
Perturbations[Perturbations %in% c("Other and NA")] = "Abiotic others"
Perturbations[Perturbations %in% c("NA and Other", "None and Other")] = "Biotic others"
Perturbations[Perturbations %in% c("Other and Other")] = "Biotic others and Abiotic others"
Perturbations[Perturbations %in% c("NA and Mucilage coverage")] = "Mucilage coverage"
Perturbations[Perturbations %in% c("Other and Mucilage coverage")] = "Abiotic others and Mucilage coverage"
Perturbations[Perturbations == "Storms and NA"] = "Storms"
Perturbations[Perturbations == "Pollution and NA"] = "Pollution"
Perturbations[Perturbations %in% c("NA and Disease", "None and Disease")] = "Disease"
Perturbations[Perturbations %in% c("Temperature anomaly and None", "Temperature anomaly and NA")] = "Temperature anomaly"
Perturbations[Perturbations %in% c("Temperature anomaly and Other")] = "Temperature anomaly and Biotic others"
Perturbations[Perturbations == "Increase of turbidity / sedimentation and NA"] = "Increase of turbidity & sedimentation"
Perturbations[Perturbations == "Increase of turbidity / sedimentation and Disease"] = "Increase of turbidity & sedimentation and Disease"

Zone <- Global_dataset$ecoregion
Zone[Zone %in% c("Alboran Sea", "Western Mediterranean")] = "Western_Bassin"
Zone[Zone %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")] = "Central_Bassin"
Zone[Zone %in% c("Aegean Sea", "Levantine Sea")] = "Eastern_Bassin"




data_model <- Global_dataset %>% 
  mutate(Perturbations = Perturbations, Zone = Zone, damaged_percentage = damaged_percentatge/100) %>% 
  dplyr::select(species, FE, Perturbations, Zone, Morphology, Coloniality, Longevity, Height, Energy, Feeding, 
                Growth, Calcification, Mobility, Storage, damaged_percentage) %>% drop_na(Perturbations)

### General Models
beta_model_ord <- brms::brm(damaged_percentage ~ Morphology + Coloniality + Longevity + Height + Energy + Feeding + Growth + Calcification +
                            Mobility + Storage + Perturbations, 
                            data = data_model,
                            iter = 3000, warmup = 1000, chains = 3, cores = 3, family = "zero_one_inflated_beta", 
                            #control = list(adapt_delta = 0.9, max_treedepth = 15),
                            backend = "cmdstanr") # Lasts 40 minutes

#### Single Models
### Morphology
# Calculate the number of zero rows to add for morphology class
num_zeros_to_add_to_b <- max(table(beta_model_ord$data[,2])) - table(beta_model_ord$data[,2])[1]
num_zeros_to_add_to_f <- max(table(beta_model_ord$data[,2])) - table(beta_model_ord$data[,2])[2]
num_zeros_to_add_to_h <- max(table(beta_model_ord$data[,2])) - table(beta_model_ord$data[,2])[3]
num_zeros_to_add_to_i <- max(table(beta_model_ord$data[,2])) - table(beta_model_ord$data[,2])[4]
num_zeros_to_add_to_j <- max(table(beta_model_ord$data[,2])) - table(beta_model_ord$data[,2])[5]
num_zeros_to_add_to_k <- max(table(beta_model_ord$data[,2])) - table(beta_model_ord$data[,2])[6]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_b), 
                                               rep(0, num_zeros_to_add_to_f), 
                                               rep(0, num_zeros_to_add_to_h), 
                                               rep(0, num_zeros_to_add_to_i), 
                                               rep(0, num_zeros_to_add_to_j), 
                                               rep(0, num_zeros_to_add_to_k)), 
                        Morphology = c(rep('b', num_zeros_to_add_to_b),
                                       rep('f', num_zeros_to_add_to_f),
                                       rep('h', num_zeros_to_add_to_h),
                                       rep('i', num_zeros_to_add_to_i),
                                       rep('j', num_zeros_to_add_to_j),
                                       rep('k', num_zeros_to_add_to_k)))
# Combine the original dataset with the new rows
data_model_morph <- data_model %>% select(damaged_percentage, Morphology)
balanced_data_model <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Morphology_model <- brms::brm(transformed_damage ~ Morphology, data = balanced_data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                                 family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Morphology <- expand.grid(unique(beta_model_ord$data[,2]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Morphology) <- c("Morphology","Perturbations")
# Plot coefficient
Morphology_predicted <- cbind(theoretical_dataset_Morphology, predict(Morphology_model, theoretical_dataset_Morphology)) %>% 
  group_by(Morphology) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_d <- Morphology_predicted %>% 
  ggplot(aes(y=Morphology, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Morphology), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Morphology), show.legend = F) +
  scale_fill_manual(values = as.character(Morphology_predicted$Color)) +
  scale_color_manual(values = as.character(Morphology_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("d") ~ " Morphology")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

### Coloniality
# Calculate the number of zero rows to add for Coloniality class
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,3])) - table(beta_model_ord$data[,3])[1]
# Create new rows with zeros for class '1'
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_1)),
                        Coloniality = c(rep('1', num_zeros_to_add_to_1)))
# Combine the original dataset with the new rows
data_model_colo <- data_model %>% select(damaged_percentage, Coloniality)
balanced_data_model <- rbind(data_model_colo, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Coloniality_model <- brms::brm(transformed_damage ~ Coloniality, data = balanced_data_model,
                              iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                              family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Coloniality <- expand.grid(unique(beta_model_ord$data[,3]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Coloniality) <- c("Coloniality","Perturbations")
# Plot coefficient
Coloniality_predicted <- cbind(theoretical_dataset_Coloniality, predict(Coloniality_model, theoretical_dataset_Coloniality)) %>% 
  group_by(Coloniality) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_c <- Coloniality_predicted %>% 
  ggplot(aes(y=Coloniality, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Coloniality), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Coloniality), show.legend = F) +
  scale_fill_manual(values = as.character(Coloniality_predicted$Color)) +
  scale_color_manual(values = as.character(Coloniality_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20), limits = c(0,1)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("c") ~ " Coloniality")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

### Longevity
# Calculate the number of zero rows to add for Longevity class
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,4])) - table(beta_model_ord$data[,4])[1]
num_zeros_to_add_to_2 <- max(table(beta_model_ord$data[,4])) - table(beta_model_ord$data[,4])[2]
num_zeros_to_add_to_3 <- max(table(beta_model_ord$data[,4])) - table(beta_model_ord$data[,4])[3]
num_zeros_to_add_to_4 <- max(table(beta_model_ord$data[,4])) - table(beta_model_ord$data[,4])[4]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_1), 
                                               rep(0, num_zeros_to_add_to_2), 
                                               rep(0, num_zeros_to_add_to_3), 
                                               rep(0, num_zeros_to_add_to_4)), 
                        Longevity = c(rep('1', num_zeros_to_add_to_1),
                                      rep('2', num_zeros_to_add_to_2),
                                      rep('3', num_zeros_to_add_to_3),
                                      rep('4', num_zeros_to_add_to_4)))
# Combine the original dataset with the new rows
data_model_morph <- data_model %>% select(damaged_percentage, Longevity)
balanced_data_model <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Longevity_model <- brms::brm(transformed_damage ~ Longevity, data = balanced_data_model,
                             iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                             family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Longevity <- expand.grid(unique(beta_model_ord$data[,4]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Longevity) <- c("Longevity","Perturbations")
# Plot coefficient
Longevity_predicted <- cbind(theoretical_dataset_Longevity, predict(Longevity_model, theoretical_dataset_Longevity)) %>% 
  group_by(Longevity) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_b <- Longevity_predicted %>% 
  ggplot(aes(y=Longevity, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Longevity), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Longevity), show.legend = F) +
  scale_fill_manual(values = as.character(Longevity_predicted$Color)) +
  scale_color_manual(values = as.character(Longevity_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("b") ~ " Longevity")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

### Height
# Calculate the number of zero rows to add for Height class
num_zeros_to_add_to_0 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[1]
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[2]
num_zeros_to_add_to_2 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[3]
num_zeros_to_add_to_3 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[4]
num_zeros_to_add_to_4 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[5]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_0), 
                                               rep(0, num_zeros_to_add_to_1), 
                                               rep(0, num_zeros_to_add_to_2), 
                                               rep(0, num_zeros_to_add_to_3), 
                                               rep(0, num_zeros_to_add_to_4)), 
                        Height = c(rep('0', num_zeros_to_add_to_0),
                                   rep('1', num_zeros_to_add_to_1),
                                   rep('2', num_zeros_to_add_to_2),
                                   rep('3', num_zeros_to_add_to_3),
                                   rep('4', num_zeros_to_add_to_4)))
# Combine the original dataset with the new rows
data_model_morph <- data_model %>% select(damaged_percentage, Height)
balanced_data_model <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Height_model <- brms::brm(transformed_damage ~ Height, data = balanced_data_model,
                          iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                          family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Height <- expand.grid(unique(beta_model_ord$data[,5]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Height) <- c("Height","Perturbations")
# Plot coefficient
Height_predicted <- cbind(theoretical_dataset_Height, predict(Height_model, theoretical_dataset_Height)) %>% 
  group_by(Height) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_g <- Height_predicted %>% 
  ggplot(aes(y=Height, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Height), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Height), show.legend = F) +
  scale_fill_manual(values = as.character(Height_predicted$Color)) +
  scale_color_manual(values = as.character(Height_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("g") ~ " Height")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

### Energy
# Calculate the number of zero rows to add for Energy class
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,6])) - table(beta_model_ord$data[,6])[1]
num_zeros_to_add_to_2 <- max(table(beta_model_ord$data[,6])) - table(beta_model_ord$data[,6])[2]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_1), 
                                               rep(0, num_zeros_to_add_to_2)), 
                        Energy = c(rep('1', num_zeros_to_add_to_1),
                                   rep('2', num_zeros_to_add_to_2)))
# Combine the original dataset with the new rows
data_model_morph <- data_model %>% select(damaged_percentage, Energy)
balanced_data_model <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Energy_model <- brms::brm(transformed_damage ~ Energy, data = balanced_data_model,
                          iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                          family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Energy <- expand.grid(unique(beta_model_ord$data[,6]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Energy) <- c("Energy","Perturbations")
# Plot coefficient
Energy_predicted <- cbind(theoretical_dataset_Energy, predict(Energy_model, theoretical_dataset_Energy)) %>% 
  group_by(Energy) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_f <- Energy_predicted %>% 
  ggplot(aes(y=Energy, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Energy), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Energy), show.legend = F) +
  scale_fill_manual(values = as.character(Energy_predicted$Color)) +
  scale_color_manual(values = as.character(Energy_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("f") ~ " Energetic ressources")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

### Feeding
# Calculate the number of zero rows to add for Feeding class
num_zeros_to_add_to_a <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[1]
num_zeros_to_add_to_b <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[2]
num_zeros_to_add_to_c <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[3]
num_zeros_to_add_to_e <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[5]
num_zeros_to_add_to_f <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[6]
num_zeros_to_add_to_g <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[7]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_a), 
                                               rep(0, num_zeros_to_add_to_b), 
                                               rep(0, num_zeros_to_add_to_c), 
                                               rep(0, num_zeros_to_add_to_f), 
                                               rep(0, num_zeros_to_add_to_e), 
                                               rep(0, num_zeros_to_add_to_g)), 
                        Feeding = c(rep('a', num_zeros_to_add_to_a),
                                    rep('b', num_zeros_to_add_to_b),
                                    rep('c', num_zeros_to_add_to_c),
                                    rep('e', num_zeros_to_add_to_e),
                                    rep('f', num_zeros_to_add_to_f),
                                    rep('g', num_zeros_to_add_to_g)))
# Combine the original dataset with the new rows
data_model_Feeding <- data_model %>% select(damaged_percentage, Feeding)
balanced_data_model <- rbind(data_model_Feeding, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Feeding_model <- brms::brm(transformed_damage ~ Feeding, data = balanced_data_model,
                           iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                           family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Feeding <- expand.grid(unique(beta_model_ord$data[,7]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Feeding) <- c("Feeding","Perturbations")
# Plot coefficient
Feeding_predicted <- cbind(theoretical_dataset_Feeding, predict(Feeding_model, theoretical_dataset_Feeding)) %>% 
  group_by(Feeding) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_a <- Feeding_predicted %>% 
  ggplot(aes(y=Feeding, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Feeding), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Feeding), show.legend = F) +
  scale_fill_manual(values = as.character(Feeding_predicted$Color)) +
  scale_color_manual(values = as.character(Feeding_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("a") ~ " Feeding")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

#### Growth
# Calculate the number of zero rows to add for Growth class
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,8])) - table(beta_model_ord$data[,8])[1]
num_zeros_to_add_to_3 <- max(table(beta_model_ord$data[,8])) - table(beta_model_ord$data[,8])[3]
num_zeros_to_add_to_4 <- max(table(beta_model_ord$data[,8])) - table(beta_model_ord$data[,8])[4]
num_zeros_to_add_to_5 <- max(table(beta_model_ord$data[,8])) - table(beta_model_ord$data[,8])[5]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_1), 
                                               rep(0, num_zeros_to_add_to_3), 
                                               rep(0, num_zeros_to_add_to_4), 
                                               rep(0, num_zeros_to_add_to_5)), 
                        Growth = c(rep('1', num_zeros_to_add_to_1),
                                   rep('3', num_zeros_to_add_to_3),
                                   rep('4', num_zeros_to_add_to_4),
                                   rep('5', num_zeros_to_add_to_5)))
# Combine the original dataset with the new rows
data_model_Growth <- data_model %>% select(damaged_percentage, Growth)
balanced_data_model <- rbind(data_model_Growth, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Growth_model <- brms::brm(transformed_damage ~ Growth, data = balanced_data_model,
                          iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                          family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Growth <- expand.grid(unique(beta_model_ord$data[,8]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Growth) <- c("Growth","Perturbations")
# Plot coefficient
Growth_predicted <- cbind(theoretical_dataset_Growth, predict(Growth_model, theoretical_dataset_Growth)) %>% 
  group_by(Growth) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_h <- Growth_predicted %>% 
  ggplot(aes(y=Growth, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Growth), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Growth), show.legend = F) +
  scale_fill_manual(values = as.character(Growth_predicted$Color)) +
  scale_color_manual(values = as.character(Growth_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("h") ~ " Growth")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

### Calcification
# Calculate the number of zero rows to add for class 'a'
num_zeros_to_add <- max(table(beta_model_ord$data[,9])) - min(table(beta_model_ord$data[,9])) 
# Create new rows with zeros for class 'a'
zero_rows <- data.frame(damaged_percentage = rep(0, num_zeros_to_add), 
                        Calcification = rep('a', num_zeros_to_add))
# Combine the original dataset with the new rows
data_model_calc <- data_model %>% select(damaged_percentage, Calcification)
balanced_data_model <- rbind(data_model_calc, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Calcification_model <- brms::brm(transformed_damage ~ Calcification, data = balanced_data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                                 family = "zero_one_inflated_beta", backend = "cmdstanr")
# Prediected
theoretical_dataset_Calcification <- expand.grid(unique(beta_model_ord$data[,9]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Calcification) <- c("Calcification","Perturbations")
# Plot coefficient
Calcification_predicted <- cbind(theoretical_dataset_Calcification, predict(Calcification_model, theoretical_dataset_Calcification)) %>% 
  group_by(Calcification) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_i <- Calcification_predicted %>% 
  ggplot(aes(y=Calcification, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Color), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Color), show.legend = F) +
  scale_fill_manual(values = as.character(Calcification_predicted$Color)) +
  scale_color_manual(values = as.character(Calcification_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("i") ~ " Calcification")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

#### Motility
# Calculate the number of zero rows to add for Motility class
num_zeros_to_add_to_b <- max(table(beta_model_ord$data[,10])) - table(beta_model_ord$data[,10])[2]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_b)), 
                        Mobility = c(rep('b', num_zeros_to_add_to_b)))
# Combine the original dataset with the new rows
data_model_Motility <- data_model %>% select(damaged_percentage, Mobility)
balanced_data_model <- rbind(data_model_Motility, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Motility_model <- brms::brm(transformed_damage ~ Mobility, data = balanced_data_model,
                            iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                            family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Motility <- expand.grid(unique(beta_model_ord$data[,10]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Motility) <- c("Mobility","Perturbations")
# Plot coefficient
Motility_predicted <- cbind(theoretical_dataset_Motility, predict(Motility_model, theoretical_dataset_Motility)) %>% 
  group_by(Mobility) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_j <- Motility_predicted %>% 
  ggplot(aes(y=Mobility, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Mobility), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Mobility), show.legend = F) +
  scale_fill_manual(values = as.character(Motility_predicted$Color)) +
  scale_color_manual(values = as.character(Motility_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("j") ~ " Motility")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

### Storage
# Calculate the number of zero rows to add for Storage class
num_zeros_to_add_to_a <- max(table(beta_model_ord$data[,11])) - table(beta_model_ord$data[,11])[1]
num_zeros_to_add_to_b <- max(table(beta_model_ord$data[,11])) - table(beta_model_ord$data[,11])[2]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_a), 
                                               rep(0, num_zeros_to_add_to_b)), 
                        Storage = c(rep('a', num_zeros_to_add_to_a),
                                    rep('b', num_zeros_to_add_to_b)))
# Combine the original dataset with the new rows
data_model_morph <- data_model %>% select(damaged_percentage, Storage)
balanced_data_model <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
Storage_model <- brms::brm(transformed_damage ~ Storage, data = balanced_data_model,
                           iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                           family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Storage <- expand.grid(unique(beta_model_ord$data[,11]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Storage) <- c("Storage","Perturbations")
# Plot coefficient
Storage_predicted <- cbind(theoretical_dataset_Storage, predict(Storage_model, theoretical_dataset_Storage)) %>% 
  group_by(Storage) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% 
  mutate(High = pmin(High, 1), Low = pmax(Low, 0)) %>% 
  mutate(Color = cut(Estimate, breaks = seq(0, 1, 0.1), 
                     labels = c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                                "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")))
model_e <- Storage_predicted %>% 
  ggplot(aes(y=Storage, x = Estimate)) +
  geom_errorbarh(aes(xmin = Low, xmax = High, color = Storage), height = 0, position = position_dodge(width = .7), size = 2, show.legend = F) +
  geom_point(shape = 21, size = 5, aes(fill = Storage), show.legend = F) +
  scale_fill_manual(values = as.character(Storage_predicted$Color)) +
  scale_color_manual(values = as.character(Storage_predicted$Color)) +
  scale_x_continuous(name = "Damaged cover (%)", breaks = seq(0, 1, 0.2), labels = seq(0,100,20)) +
  scale_y_discrete(name = "") +
  theme_classic() +
  ggtitle(expression(bold("e") ~ "Carbon storage")) + 
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16))

model_a + model_b + model_c + model_d + model_e + model_f + model_g + model_h + model_i + model_j + plot_layout(ncol = 2)

max(Longevity_predicted$Estimate) / Longevity_predicted$Estimate ; Longevity_predicted
max(Storage_predicted$Estimate) / Storage_predicted$Estimate ; Storage_predicted
max(Energy_predicted$Estimate) / Energy_predicted$Estimate ; Energy_predicted
max(Height_predicted$Estimate) / Height_predicted$Estimate ; Height_predicted
max(Growth_predicted$Estimate) / Growth_predicted$Estimate ; Growth_predicted
max(Calcification_predicted$Estimate) / Calcification_predicted$Estimate ; Calcification_predicted
max(Motility_predicted$Estimate) / Motility_predicted$Estimate ; Motility_predicted

max(Feeding_predicted$Estimate) / Feeding_predicted$Estimate ; Feeding_predicted
max(Feeding_predicted$Estimate[Feeding_predicted$Estimate != max(Feeding_predicted$Estimate)]) / Feeding_predicted$Estimate ; Feeding_predicted
max(Morphology_predicted$Estimate) / Morphology_predicted$Estimate ; Morphology_predicted
max(Morphology_predicted$Estimate[Morphology_predicted$Estimate != max(Morphology_predicted$Estimate)]) / Morphology_predicted$Estimate ; Morphology_predicted


Coloniality_predicted


# Check models
rstantools::bayes_R2(Motility_model)
rstanarm::pp_check(Motility_model, type = "scatter_avg") 



### Perturbations
data_model <- Global_dataset %>% 
  mutate(Perturbations = Perturbations, Zone = Zone, damaged_percentage = damaged_percentatge/100) %>% 
  dplyr::select(species, FE, year, Perturbations, Zone, Morphology, Coloniality, Longevity, Height, Energy, Feeding, 
                Growth, Calcification, Mobility, Storage, damaged_percentage) %>% drop_na(Perturbations)

table_pert <- table(data_model$Perturbations, data_model$FE, data_model$year) %>% data.frame()
table_pert_split_1 <- table_pert %>% 
  dplyr::filter(Var1 != "Increase of turbidity & sedimentation") %>% 
  mutate(Var1 = str_split(Var1, " and ")) %>% 
  unnest(Var1)
table_pert_split_2 <- table_pert %>% 
  dplyr::filter(Var1 == "Increase of turbidity & sedimentation") 
table_pert_split = rbind(table_pert_split_1, table_pert_split_2)
table_pert_split$Freq[table_pert_split$Freq >= 1] = 1
table_pert <- table_pert_split %>% group_by(Var1, Var3) %>% summarise(Freq = sum(Freq))

pivot_table <- table_pert %>% pivot_wider(names_from = Var1, values_from = Freq)

pivot_table <- pivot_table %>% mutate(Volume = (data_heatmap_FE_all_summ$Volume %>% na.omit())^2 / 100)
pivot_table$Volume <- pmax(0.00001, pmin(0.99999, pivot_table$Volume))

colnames(pivot_table) = c("Year", "Abiotic_others", "Biotic_others", "Disease", "Turbidity_and_Sedimentation", 
                          "Mucillage", "Pollution", "Predator_outbreaks", "Storms", "Temperature_anomaly", "Volume")

volume_trait_quantification <- brms::brm(Volume ~ Abiotic_others + Biotic_others + Disease + Turbidity_and_Sedimentation + Mucillage + Pollution + 
                                           Predator_outbreaks + Storms + Temperature_anomaly, 
                                         data = pivot_table,
                                         iter = 2000, warmup = 1000, chains = 2, cores = 2, 
                                         family = "zero_one_inflated_beta", backend = "cmdstanr") 

pivot_table <- pivot_table %>% mutate(Volume_predicted = predict(volume_trait_quantification, pivot_table)[,1],
                                      Volume_Q2.5 = predict(volume_trait_quantification, pivot_table)[,3],
                                      Volume_Q97.5 = predict(volume_trait_quantification, pivot_table)[,4],
                                      Volume_sd = predict(volume_trait_quantification, pivot_table)[,2])

rstantools::bayes_R2(volume_trait_quantification)
rstanarm::pp_check(volume_trait_quantification, type = "scatter_avg") 

###
stressors <- colnames(pivot_table)[c(2:10)]
# Generate all possible combinations
combinations <- expand.grid(lapply(stressors, function(x) c(0, 1)))
combinations <- combinations %>% mutate(Volume_predicted = predict(volume_trait_quantification, combinations)[,1],
                                      Volume_Q2.5 = predict(volume_trait_quantification, combinations)[,3],
                                      Volume_Q97.5 = predict(volume_trait_quantification, combinations)[,4],
                                      Volume_sd = predict(volume_trait_quantification, combinations)[,2])

# Rename the columns to match your factors
colnames(combinations) <- stressors
single_stressors <- pivot_table %>% dplyr::filter(Year %in% c(1986, 1992, 1995:1998, 2002, 2004:2006, 2011))
double_stressors <- pivot_table %>% dplyr::filter(Year %in% c(1993, 1999, 2001, 2007:2008, 2010, 2020))
triple_stressors <- pivot_table %>% dplyr::filter(Year %in% c(1987:1988, 2003, 2009, 2012, 2014, 2016))
multi_stressors <- pivot_table %>% dplyr::filter(Year %notin% c(single_stressors$Year, double_stressors$Year, triple_stressors$Year))

MHW <- pivot_table
MHW[,c(2,4:5,7,9)] = 0
MHW %>% mutate(Volume_predicted = predict(volume_trait_quantification, MHW)[,1],
           Volume_Q2.5 = predict(volume_trait_quantification, MHW)[,3],
           Volume_Q97.5 = predict(volume_trait_quantification, MHW)[,4],
           Volume_sd = predict(volume_trait_quantification, MHW)[,2]) %>% View()


for (i in 1:9) {
max_values <- apply(pivot_table[c(2:10)], MARGIN = 2, FUN = max)
max_values[i] = 0
print(predict(volume_trait_quantification ,data.frame(max_values) %>% t() %>% data.frame())) }
pivot_table %>% View()