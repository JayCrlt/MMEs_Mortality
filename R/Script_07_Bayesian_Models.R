#### Bayesian Models
Perturbations <- rep(NA, length = length(Global_dataset$species))
Perturbations <- paste(Global_dataset$drivers_abiotic, Global_dataset$drivers_biotic_group, sep = " and ")
# Build Perturbations vector
Perturbations[Perturbations == "NA and NA"]                                                       = NA
Perturbations[Perturbations %in% c("Other and NA")]                                               = "Abiotic others"
Perturbations[Perturbations %in% c("NA and Other", "None and Other")]                             = "Biotic others"
Perturbations[Perturbations %in% c("Other and Other")]                                            = "Biotic others and Abiotic others"
Perturbations[Perturbations %in% c("NA and Mucilage coverage")]                                   = "Mucilage coverage"
Perturbations[Perturbations %in% c("Other and Mucilage coverage")]                                = "Abiotic others and Mucilage coverage"
Perturbations[Perturbations == "Storms and NA"]                                                   = "Storms"
Perturbations[Perturbations == "Pollution and NA"]                                                = "Pollution"
Perturbations[Perturbations %in% c("NA and Disease", "None and Disease")]                         = "Disease"
Perturbations[Perturbations %in% c("Temperature anomaly and None", "Temperature anomaly and NA")] = "Temperature anomaly"
Perturbations[Perturbations %in% c("Temperature anomaly and Other")]                              = "Temperature anomaly and Biotic others"
Perturbations[Perturbations == "Increase of turbidity / sedimentation and NA"]                    = "Increase of turbidity & sedimentation"
Perturbations[Perturbations == "Increase of turbidity / sedimentation and Disease"]               = "Increase of turbidity & sedimentation and Disease"
# Build Ecoregion vector
Zone <- Global_dataset$ecoregion
Zone[Zone %in% c("Alboran Sea", "Western Mediterranean")]                                         = "Western_Bassin"
Zone[Zone %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")]                 = "Central_Bassin"
Zone[Zone %in% c("Aegean Sea", "Levantine Sea")]                                                  = "Eastern_Bassin"

# Build Data Model
data_model <- Global_dataset %>% mutate(Perturbations = Perturbations, Zone = Zone, damaged_percentage = damaged_percentatge/100) %>% 
  dplyr::select(year, species, FE, Perturbations, Zone, Morphology, Coloniality, Longevity, Height, Energy, Feeding, 
                Growth, Calcification, Mobility, Storage, damaged_percentage) %>% drop_na(Perturbations)

### General Model ### Exploration ###
# beta_model_ord <- brms::brm(damaged_percentage ~ Morphology + Coloniality + Longevity + Height + Energy + Feeding + Growth + Calcification +
#                             Mobility + Storage + Perturbations, data = data_model,
#                             iter = 3000, warmup = 1000, chains = 3, cores = 3, family = "zero_one_inflated_beta", 
#                             control = list(adapt_delta = 0.9, max_treedepth = 15), backend = "cmdstanr") # Lasts > 40 minutes

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
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_b), rep(0, num_zeros_to_add_to_f), rep(0, num_zeros_to_add_to_h), 
                                               rep(0, num_zeros_to_add_to_i), rep(0, num_zeros_to_add_to_j), rep(0, num_zeros_to_add_to_k)), 
                        Morphology = c(rep('b', num_zeros_to_add_to_b), rep('f', num_zeros_to_add_to_f), rep('h', num_zeros_to_add_to_h),
                                       rep('i', num_zeros_to_add_to_i), rep('j', num_zeros_to_add_to_j), rep('k', num_zeros_to_add_to_k)))
# Combine the original dataset with the new rows
data_model_morph    <- data_model %>% select(damaged_percentage, Morphology)
balanced_data_model <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Morphology_model    <- brms::brm(transformed_damage ~ Morphology, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                                  family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Morphology <- expand.grid(Morphology = unique(beta_model_ord$data[,2]), Perturbations = unique(beta_model_ord$data[,12]))
# Plot coefficient
Morphology_predicted <- cbind(theoretical_dataset_Morphology, predict(Morphology_model, theoretical_dataset_Morphology)) %>% 
  group_by(Morphology) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

### Coloniality
# Calculate the number of zero rows to add for Coloniality class
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,3])) - table(beta_model_ord$data[,3])[1]
# Create new rows with zeros for class '1'
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_1)), Coloniality = c(rep('1', num_zeros_to_add_to_1)))
# Combine the original dataset with the new rows
data_model_colo       <- data_model %>% select(damaged_percentage, Coloniality)
balanced_data_model   <- rbind(data_model_colo, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Coloniality_model     <- brms::brm(transformed_damage ~ Coloniality, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                                    family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Coloniality <- expand.grid(Coloniality = unique(beta_model_ord$data[,3]), Perturbations = unique(beta_model_ord$data[,12]))
# Plot coefficient
Coloniality_predicted <- cbind(theoretical_dataset_Coloniality, predict(Coloniality_model, theoretical_dataset_Coloniality)) %>% 
  group_by(Coloniality) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

### Longevity
# Calculate the number of zero rows to add for Longevity class
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,4])) - table(beta_model_ord$data[,4])[1]
num_zeros_to_add_to_2 <- max(table(beta_model_ord$data[,4])) - table(beta_model_ord$data[,4])[2]
num_zeros_to_add_to_3 <- max(table(beta_model_ord$data[,4])) - table(beta_model_ord$data[,4])[3]
num_zeros_to_add_to_4 <- max(table(beta_model_ord$data[,4])) - table(beta_model_ord$data[,4])[4]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_1), rep(0, num_zeros_to_add_to_2), rep(0, num_zeros_to_add_to_3), 
                                               rep(0, num_zeros_to_add_to_4)), 
                        Longevity = c(rep('1', num_zeros_to_add_to_1), rep('2', num_zeros_to_add_to_2), rep('3', num_zeros_to_add_to_3),
                                      rep('4', num_zeros_to_add_to_4)))
# Combine the original dataset with the new rows
data_model_morph      <- data_model %>% select(damaged_percentage, Longevity)
balanced_data_model   <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Longevity_model <- brms::brm(transformed_damage ~ Longevity, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                              family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Longevity <- expand.grid(Longevity = unique(beta_model_ord$data[,4]), Perturbations = unique(beta_model_ord$data[,12]))
# Plot coefficient
Longevity_predicted <- cbind(theoretical_dataset_Longevity, predict(Longevity_model, theoretical_dataset_Longevity)) %>% 
  group_by(Longevity) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

### Height
# Calculate the number of zero rows to add for Height class
num_zeros_to_add_to_0 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[1]
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[2]
num_zeros_to_add_to_2 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[3]
num_zeros_to_add_to_3 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[4]
num_zeros_to_add_to_4 <- max(table(beta_model_ord$data[,5])) - table(beta_model_ord$data[,5])[5]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_0), rep(0, num_zeros_to_add_to_1), rep(0, num_zeros_to_add_to_2), 
                                               rep(0, num_zeros_to_add_to_3), rep(0, num_zeros_to_add_to_4)), 
                        Height = c(rep('0', num_zeros_to_add_to_0), rep('1', num_zeros_to_add_to_1), rep('2', num_zeros_to_add_to_2),
                                   rep('3', num_zeros_to_add_to_3), rep('4', num_zeros_to_add_to_4)))
# Combine the original dataset with the new rows
data_model_morph    <- data_model %>% select(damaged_percentage, Height)
balanced_data_model <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Height_model <- brms::brm(transformed_damage ~ Height, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                           family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Height <- expand.grid(Height = unique(beta_model_ord$data[,5]), Perturbations = unique(beta_model_ord$data[,12]))
# Plot coefficient
Height_predicted <- cbind(theoretical_dataset_Height, predict(Height_model, theoretical_dataset_Height)) %>% 
  group_by(Height) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

### Energy
# Calculate the number of zero rows to add for Energy class
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,6])) - table(beta_model_ord$data[,6])[1]
num_zeros_to_add_to_2 <- max(table(beta_model_ord$data[,6])) - table(beta_model_ord$data[,6])[2]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_1), rep(0, num_zeros_to_add_to_2)), 
                        Energy = c(rep('1', num_zeros_to_add_to_1), rep('2', num_zeros_to_add_to_2)))
# Combine the original dataset with the new rows
data_model_morph <- data_model %>% select(damaged_percentage, Energy)
balanced_data_model <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Energy_model <- brms::brm(transformed_damage ~ Energy, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                           family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Energy <- expand.grid(Energy = unique(beta_model_ord$data[,6]), Perturbations = unique(beta_model_ord$data[,12]))
# Plot coefficient
Energy_predicted <- cbind(theoretical_dataset_Energy, predict(Energy_model, theoretical_dataset_Energy)) %>% 
  group_by(Energy) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

### Feeding
# Calculate the number of zero rows to add for Feeding class
num_zeros_to_add_to_a <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[1]
num_zeros_to_add_to_b <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[2]
num_zeros_to_add_to_c <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[3]
num_zeros_to_add_to_e <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[5]
num_zeros_to_add_to_f <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[6]
num_zeros_to_add_to_g <- max(table(beta_model_ord$data[,7])) - table(beta_model_ord$data[,7])[7]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_a), rep(0, num_zeros_to_add_to_b), rep(0, num_zeros_to_add_to_c), 
                                               rep(0, num_zeros_to_add_to_f), rep(0, num_zeros_to_add_to_e), rep(0, num_zeros_to_add_to_g)), 
                        Feeding = c(rep('a', num_zeros_to_add_to_a), rep('b', num_zeros_to_add_to_b), rep('c', num_zeros_to_add_to_c),
                                    rep('e', num_zeros_to_add_to_e), rep('f', num_zeros_to_add_to_f), rep('g', num_zeros_to_add_to_g)))
# Combine the original dataset with the new rows
data_model_Feeding  <- data_model %>% select(damaged_percentage, Feeding)
balanced_data_model <- rbind(data_model_Feeding, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Feeding_model <- brms::brm(transformed_damage ~ Feeding, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                            family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Feeding <- expand.grid(Feeding = unique(beta_model_ord$data[,7]), Perturbations = unique(beta_model_ord$data[,12]))
# Plot coefficient
Feeding_predicted <- cbind(theoretical_dataset_Feeding, predict(Feeding_model, theoretical_dataset_Feeding)) %>% 
  group_by(Feeding) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

#### Growth
# Calculate the number of zero rows to add for Growth class
num_zeros_to_add_to_1 <- max(table(beta_model_ord$data[,8])) - table(beta_model_ord$data[,8])[1]
num_zeros_to_add_to_3 <- max(table(beta_model_ord$data[,8])) - table(beta_model_ord$data[,8])[3]
num_zeros_to_add_to_4 <- max(table(beta_model_ord$data[,8])) - table(beta_model_ord$data[,8])[4]
num_zeros_to_add_to_5 <- max(table(beta_model_ord$data[,8])) - table(beta_model_ord$data[,8])[5]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_1), rep(0, num_zeros_to_add_to_3), rep(0, num_zeros_to_add_to_4), 
                                               rep(0, num_zeros_to_add_to_5)), 
                        Growth = c(rep('1', num_zeros_to_add_to_1), rep('3', num_zeros_to_add_to_3), rep('4', num_zeros_to_add_to_4),
                                   rep('5', num_zeros_to_add_to_5)))
# Combine the original dataset with the new rows
data_model_Growth   <- data_model %>% select(damaged_percentage, Growth)
balanced_data_model <- rbind(data_model_Growth, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Growth_model <- brms::brm(transformed_damage ~ Growth, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                           family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Growth <- expand.grid(Growth = unique(beta_model_ord$data[,8]), Perturbations = unique(beta_model_ord$data[,12]))
# Plot coefficient
Growth_predicted <- cbind(theoretical_dataset_Growth, predict(Growth_model, theoretical_dataset_Growth)) %>% 
  group_by(Growth) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

### Calcification
# Calculate the number of zero rows to add for class 'a'
num_zeros_to_add <- max(table(beta_model_ord$data[,9])) - min(table(beta_model_ord$data[,9])) 
# Create new rows with zeros for class 'a'
zero_rows <- data.frame(damaged_percentage = rep(0, num_zeros_to_add), Calcification = rep('a', num_zeros_to_add))
# Combine the original dataset with the new rows
data_model_calc     <- data_model %>% select(damaged_percentage, Calcification)
balanced_data_model <- rbind(data_model_calc, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Calcification_model <- brms::brm(transformed_damage ~ Calcification, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                                  family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Calcification <- expand.grid(Calcification = unique(beta_model_ord$data[,9]), Perturbations = unique(beta_model_ord$data[,12]))
# Plot coefficient
Calcification_predicted <- cbind(theoretical_dataset_Calcification, predict(Calcification_model, theoretical_dataset_Calcification)) %>% 
  group_by(Calcification) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

#### Motility
# Calculate the number of zero rows to add for Motility class
num_zeros_to_add_to_b <- max(table(beta_model_ord$data[,10])) - table(beta_model_ord$data[,10])[2]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_b)), Mobility = c(rep('b', num_zeros_to_add_to_b)))
# Combine the original dataset with the new rows
data_model_Motility <- data_model %>% select(damaged_percentage, Mobility)
balanced_data_model <- rbind(data_model_Motility, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Motility_model <- brms::brm(transformed_damage ~ Mobility, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                             family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Motility <- expand.grid(Mobility = unique(beta_model_ord$data[,10]), Perturbations = unique(beta_model_ord$data[,12]))
# Plot coefficient
Motility_predicted <- cbind(theoretical_dataset_Motility, predict(Motility_model, theoretical_dataset_Motility)) %>% 
  group_by(Mobility) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

### Storage
# Calculate the number of zero rows to add for Storage class
num_zeros_to_add_to_a <- max(table(beta_model_ord$data[,11])) - table(beta_model_ord$data[,11])[1]
num_zeros_to_add_to_b <- max(table(beta_model_ord$data[,11])) - table(beta_model_ord$data[,11])[2]
# Create new rows with zeros 
zero_rows <- data.frame(damaged_percentage = c(rep(0, num_zeros_to_add_to_a), rep(0, num_zeros_to_add_to_b)), 
                        Storage = c(rep('a', num_zeros_to_add_to_a), rep('b', num_zeros_to_add_to_b)))
# Combine the original dataset with the new rows
data_model_morph    <- data_model %>% select(damaged_percentage, Storage)
balanced_data_model <- rbind(data_model_morph, zero_rows)
# Build the model with custom priors and weights
balanced_data_model$transformed_damage <- pmax(0.00001, pmin(0.99999, balanced_data_model$damaged_percentage))
# Storage_model <- brms::brm(transformed_damage ~ Storage, data = balanced_data_model, iter = 2000, warmup = 1000, chains = 2, cores = 2, 
#                            family = "zero_one_inflated_beta", backend = "cmdstanr")
# Predicted
theoretical_dataset_Storage <- expand.grid(unique(beta_model_ord$data[,11]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset_Storage) <- c("Storage","Perturbations")
# Plot coefficient
Storage_predicted <- cbind(theoretical_dataset_Storage, predict(Storage_model, theoretical_dataset_Storage)) %>% 
  group_by(Storage) %>% summarise(Estimate = mean(Estimate), Est.Error = mean(Est.Error)) %>% 
  mutate(Low = Estimate - Est.Error, High = Estimate + Est.Error) %>% mutate(High = pmin(High, 1), Low = pmax(Low, 0)) 

### Analyzing outputs
# Single category
max(Longevity_predicted$Estimate)     / Longevity_predicted$Estimate     ; Longevity_predicted
max(Storage_predicted$Estimate)       / Storage_predicted$Estimate       ; Storage_predicted
max(Energy_predicted$Estimate)        / Energy_predicted$Estimate        ; Energy_predicted
max(Height_predicted$Estimate)        / Height_predicted$Estimate        ; Height_predicted
max(Growth_predicted$Estimate)        / Growth_predicted$Estimate        ; Growth_predicted
max(Calcification_predicted$Estimate) / Calcification_predicted$Estimate ; Calcification_predicted
max(Motility_predicted$Estimate)      / Motility_predicted$Estimate      ; Motility_predicted
# Dual categories
max(Feeding_predicted$Estimate)       / Feeding_predicted$Estimate       ; Feeding_predicted
max(Morphology_predicted$Estimate)    / Morphology_predicted$Estimate    ; Morphology_predicted
max(Feeding_predicted$Estimate[Feeding_predicted$Estimate != max(Feeding_predicted$Estimate)]) / Feeding_predicted$Estimate ; Feeding_predicted
max(Morphology_predicted$Estimate[Morphology_predicted$Estimate != max(Morphology_predicted$Estimate)]) / Morphology_predicted$Estimate ; Morphology_predicted

# Check models
Single_models = list(Feeding_model, Longevity_model, Coloniality_model, Morphology_model, Storage_model, Energy_model, Height_model, 
                     Growth_model, Calcification_model, Motility_model)
for (i in 1:10) {print(rstantools::bayes_R2(Single_models[[i]])) ; rstanarm::pp_check(Single_models[[i]], type = "scatter_avg") }

### Perturbations
table_pert         <- table(data_model$Perturbations, data_model$FE, data_model$year) %>% data.frame()
table_pert_split_1 <- table_pert %>% dplyr::filter(Var1 != "Increase of turbidity & sedimentation") %>% mutate(Var1 = str_split(Var1, " and ")) %>% unnest(Var1)
table_pert_split_2 <- table_pert %>% dplyr::filter(Var1 == "Increase of turbidity & sedimentation") 
table_pert         <- bind_rows(table_pert_split_1, table_pert_split_2) %>% mutate(Freq = ifelse(Freq >= 1, 1, Freq)) %>% group_by(Var1, Var3) %>%
  summarise(Freq = sum(Freq))
pivot_table        <- table_pert %>% pivot_wider(names_from = Var1, values_from = Freq) %>% 
  mutate(Volume = pmax(0.00001, pmin(0.99999, na.omit(data_heatmap_FE_all_summ$Volume)^2 / 100)))
colnames(pivot_table) = c("Year", "Abiotic_others", "Biotic_others", "Disease", "Turbidity_and_Sedimentation", "Mucillage", "Pollution", "Predator_outbreaks", 
                          "Storms", "Temperature_anomaly", "Volume")

# Make the model
# volume_trait_quantification <- brms::brm(Volume ~ Abiotic_others + Biotic_others + Disease + Turbidity_and_Sedimentation + Mucillage + Pollution + 
#                                          Predator_outbreaks + Storms + Temperature_anomaly, data = pivot_table, iter = 3000, warmup = 1000, chains = 2, cores = 2, 
#                                          family = "zero_one_inflated_beta", backend = "cmdstanr") 
# Predict
pivot_table <- pivot_table %>% mutate(Volume_predicted = predict(volume_trait_quantification, pivot_table)[,1],
                                      Volume_Q2.5      = predict(volume_trait_quantification, pivot_table)[,3],
                                      Volume_Q97.5     = predict(volume_trait_quantification, pivot_table)[,4],
                                      Volume_sd        = predict(volume_trait_quantification, pivot_table)[,2])
# Quality
rstantools::bayes_R2(volume_trait_quantification) ; rstanarm::pp_check(volume_trait_quantification, type = "scatter_avg") 

# Analyzing this model
single_stressors <- pivot_table %>% dplyr::filter(Year %in% c(1986, 1992, 1995:1998, 2002, 2004:2006, 2011))
double_stressors <- pivot_table %>% dplyr::filter(Year %in% c(1993, 1999, 2001, 2007:2008, 2010, 2020))
triple_stressors <- pivot_table %>% dplyr::filter(Year %in% c(1987:1988, 2003, 2009, 2012, 2014, 2016))
multi_stressors  <- pivot_table %>% dplyr::filter(Year %notin% c(single_stressors$Year, double_stressors$Year, triple_stressors$Year))
MHW <- pivot_table ; MHW[,c(2,4:5,7,9)] = 0
MHW %>% mutate(Volume_predicted = predict(volume_trait_quantification, MHW)[,1],
           Volume_Q2.5          = predict(volume_trait_quantification, MHW)[,3],
           Volume_Q97.5         = predict(volume_trait_quantification, MHW)[,4],
           Volume_sd            = predict(volume_trait_quantification, MHW)[,2]) 

for (i in 1:9) { max_values <- apply(pivot_table[c(2:10)], MARGIN = 2, FUN = max) ; max_values[i] = 0
print(predict(volume_trait_quantification ,data.frame(max_values) %>% t() %>% data.frame())) }

# Define max for each variable
max_abiotic_others = max(pivot_table$Abiotic_others)
max_biotic_others  = max(pivot_table$Biotic_others)
max_disease        = max(pivot_table$Disease)
max_turbidity      = max(pivot_table$Turbidity_and_Sedimentation)
max_mucillage      = max(pivot_table$Mucillage)
max_pollution      = max(pivot_table$Pollution)
max_predator       = max(pivot_table$Predator_outbreaks)
max_storms         = max(pivot_table$Storms)
max_temperature    = max(pivot_table$Temperature_anomaly)

# Define min for each variable
min_abiotic_others = min(pivot_table$Abiotic_others)
min_biotic_others  = min(pivot_table$Biotic_others)
min_disease        = min(pivot_table$Disease)
min_turbidity      = min(pivot_table$Turbidity_and_Sedimentation)
min_mucillage      = min(pivot_table$Mucillage)
min_pollution      = min(pivot_table$Pollution)
min_predator       = min(pivot_table$Predator_outbreaks)
min_storms         = min(pivot_table$Storms)
min_temperature    = min(pivot_table$Temperature_anomaly)

# Define range for each variable
range_abiotic_others = seq(0, max(pivot_table$Abiotic_others), 1)
range_biotic_others  = seq(0, max(pivot_table$Biotic_others), 1)
range_disease        = seq(0, max(pivot_table$Disease), 1)
range_turbidity      = seq(0, max(pivot_table$Turbidity_and_Sedimentation), 1)
range_mucillage      = seq(0, max(pivot_table$Mucillage), 1)
range_pollution      = seq(0, max(pivot_table$Pollution), 1)
range_predator       = seq(0, max(pivot_table$Predator_outbreaks), 1)
range_storms         = seq(0, max(pivot_table$Storms), 1)
range_temperature    = seq(0, max(pivot_table$Temperature_anomaly), 1)

# Estimates for perturbation model
predicted_data <- expand.grid(Abiotic_others = range_abiotic_others, Biotic_others = min_biotic_others, Disease = min_disease, 
                              Turbidity_and_Sedimentation = min_turbidity, Mucillage = min_mucillage, Pollution = min_pollution, 
                              Predator_outbreaks = min_predator, Storms = min_storms, Temperature_anomaly = min_temperature) 
(th.abiotic    <- predicted_data %>% mutate(Volume_predicted = predict(volume_trait_quantification, predicted_data)[,1], 
                                         Volume_Q2.5         = predict(volume_trait_quantification, predicted_data)[,3],
                                         Volume_Q97.5        = predict(volume_trait_quantification, predicted_data)[,4], 
                                         Volume_sd           = predict(volume_trait_quantification, predicted_data)[,2]) %>% 
    dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% summarise_all(mean))

predicted_data <- expand.grid(Abiotic_others = min_abiotic_others, Biotic_others = range_biotic_others, Disease = min_disease, 
                              Turbidity_and_Sedimentation = min_turbidity, Mucillage = min_mucillage, Pollution = min_pollution, 
                              Predator_outbreaks = min_predator, Storms = min_storms, Temperature_anomaly = min_temperature) 
(th.biotic     <- predicted_data %>% mutate(Volume_predicted = predict(volume_trait_quantification, predicted_data)[,1], 
                                         Volume_Q2.5         = predict(volume_trait_quantification, predicted_data)[,3],
                                         Volume_Q97.5        = predict(volume_trait_quantification, predicted_data)[,4], 
                                         Volume_sd           = predict(volume_trait_quantification, predicted_data)[,2]) %>% 
    dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% summarise_all(mean))

predicted_data <- expand.grid(Abiotic_others = min_abiotic_others, Biotic_others = min_biotic_others, Disease = range_disease, 
                              Turbidity_and_Sedimentation = min_turbidity, Mucillage = min_mucillage, Pollution = min_pollution, 
                              Predator_outbreaks = min_predator, Storms = min_storms, Temperature_anomaly = min_temperature) 
(th.disease    <- predicted_data %>% mutate(Volume_predicted = predict(volume_trait_quantification, predicted_data)[,1], 
                                        Volume_Q2.5          = predict(volume_trait_quantification, predicted_data)[,3],
                                        Volume_Q97.5         = predict(volume_trait_quantification, predicted_data)[,4], 
                                        Volume_sd            = predict(volume_trait_quantification, predicted_data)[,2]) %>% 
    dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% summarise_all(mean))

predicted_data <- expand.grid(Abiotic_others = min_abiotic_others, Biotic_others = min_biotic_others, Disease = min_disease, 
                              Turbidity_and_Sedimentation = range_turbidity, Mucillage = min_mucillage, Pollution = min_pollution, 
                              Predator_outbreaks = min_predator, Storms = min_storms, Temperature_anomaly = min_temperature) 
(th.turbid     <- predicted_data %>% mutate(Volume_predicted = predict(volume_trait_quantification, predicted_data)[,1], 
                                         Volume_Q2.5         = predict(volume_trait_quantification, predicted_data)[,3],
                                         Volume_Q97.5        = predict(volume_trait_quantification, predicted_data)[,4], 
                                         Volume_sd           = predict(volume_trait_quantification, predicted_data)[,2]) %>% 
    dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% summarise_all(mean))

predicted_data <- expand.grid(Abiotic_others = min_abiotic_others, Biotic_others = min_biotic_others, Disease = min_disease, 
                              Turbidity_and_Sedimentation = min_turbidity, Mucillage = range_mucillage, Pollution = min_pollution, 
                              Predator_outbreaks = min_predator, Storms = min_storms, Temperature_anomaly = min_temperature) 
(th.mucillage  <- predicted_data %>% mutate(Volume_predicted = predict(volume_trait_quantification, predicted_data)[,1], 
                                            Volume_Q2.5      = predict(volume_trait_quantification, predicted_data)[,3],
                                            Volume_Q97.5     = predict(volume_trait_quantification, predicted_data)[,4], 
                                            Volume_sd        = predict(volume_trait_quantification, predicted_data)[,2]) %>% 
    dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% summarise_all(mean))

predicted_data <- expand.grid(Abiotic_others = min_abiotic_others, Biotic_others = min_biotic_others, Disease = min_disease, 
                              Turbidity_and_Sedimentation = min_turbidity, Mucillage = min_mucillage, Pollution = range_pollution, 
                              Predator_outbreaks = min_predator, Storms = min_storms, Temperature_anomaly = min_temperature) 
(th.pollution  <- predicted_data %>% mutate(Volume_predicted = predict(volume_trait_quantification, predicted_data)[,1], 
                                            Volume_Q2.5      = predict(volume_trait_quantification, predicted_data)[,3],
                                            Volume_Q97.5     = predict(volume_trait_quantification, predicted_data)[,4], 
                                            Volume_sd        = predict(volume_trait_quantification, predicted_data)[,2]) %>% 
    dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% summarise_all(mean))

predicted_data <- expand.grid(Abiotic_others = min_abiotic_others, Biotic_others = min_biotic_others, Disease = min_disease, 
                              Turbidity_and_Sedimentation = min_turbidity, Mucillage = min_mucillage, Pollution = min_pollution, 
                              Predator_outbreaks = range_predator, Storms = min_storms, Temperature_anomaly = min_temperature) 
(th.predator   <- predicted_data %>% mutate(Volume_predicted = predict(volume_trait_quantification, predicted_data)[,1], 
                                            Volume_Q2.5      = predict(volume_trait_quantification, predicted_data)[,3],
                                            Volume_Q97.5     = predict(volume_trait_quantification, predicted_data)[,4], 
                                            Volume_sd        = predict(volume_trait_quantification, predicted_data)[,2]) %>% 
    dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% summarise_all(mean))

predicted_data <- expand.grid(Abiotic_others = min_abiotic_others, Biotic_others = min_biotic_others, Disease = min_disease, 
                              Turbidity_and_Sedimentation = min_turbidity, Mucillage = min_mucillage, Pollution = min_pollution, 
                              Predator_outbreaks = min_predator, Storms = range_storms, Temperature_anomaly = min_temperature) 
(th.storms     <- predicted_data %>% mutate(Volume_predicted = predict(volume_trait_quantification, predicted_data)[,1], 
                                           Volume_Q2.5       = predict(volume_trait_quantification, predicted_data)[,3],
                                           Volume_Q97.5      = predict(volume_trait_quantification, predicted_data)[,4], 
                                           Volume_sd         = predict(volume_trait_quantification, predicted_data)[,2]) %>% 
    dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% summarise_all(mean))

predicted_data <- expand.grid(Abiotic_others = min_abiotic_others, Biotic_others = min_biotic_others, Disease = min_disease, 
                              Turbidity_and_Sedimentation = min_turbidity, Mucillage = min_mucillage, Pollution = min_pollution, 
                              Predator_outbreaks = min_predator, Storms = min_storms, Temperature_anomaly = range_temperature) 
(th.temper     <- predicted_data %>% mutate(Volume_predicted = predict(volume_trait_quantification, predicted_data)[,1], 
                                            Volume_Q2.5      = predict(volume_trait_quantification, predicted_data)[,3],
                                            Volume_Q97.5     = predict(volume_trait_quantification, predicted_data)[,4], 
                                            Volume_sd        = predict(volume_trait_quantification, predicted_data)[,2]) %>% 
    dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% summarise_all(mean))

predicted_pert <- rbind(th.abiotic, th.biotic, th.disease, th.turbid, th.mucillage, th.pollution, th.predator, th.storms, th.temper) %>% 
  dplyr::select(Volume_predicted, Volume_Q2.5, Volume_Q97.5, Volume_sd) %>% data.frame()