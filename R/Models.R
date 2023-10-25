Perturbations <- rep(NA, length = length(Global_dataset$species))
Perturbations <- paste(Global_dataset$drivers_abiotic, Global_dataset$drivers_biotic_group, sep = " and ")
Perturbations[Perturbations == "NA and NA"] = NA
Perturbations[Perturbations %in% c("Other and NA", "NA and Other", "Other and Other", "None and Other")] = "Others"
Perturbations[Perturbations %in% c("NA and Mucilage coverage", "Other and Mucilage coverage")] = "Mucilage coverage"
Perturbations[Perturbations == "Storms and NA"] = "Storms"
Perturbations[Perturbations == "Pollution and NA"] = "Pollution"
Perturbations[Perturbations %in% c("NA and Disease", "None and Disease")] = "Disease"
Perturbations[Perturbations %in% c("Temperature anomaly and Other", "Temperature anomaly and None", "Temperature anomaly and NA")] = "Temperature anomaly"
Perturbations[Perturbations == "Increase of turbidity / sedimentation and NA"] = "Increase of turbidity and sedimentation"
Perturbations[Perturbations == "Increase of turbidity / sedimentation and Disease"] = "Increase of turbidity and sedimentation and disease"

Zone <- Global_dataset$ecoregion
Zone[Zone %in% c("Alboran Sea", "Western Mediterranean")] = "Western_Bassin"
Zone[Zone %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")] = "Central_Bassin"
Zone[Zone %in% c("Aegean Sea", "Levantine Sea")] = "Eastern_Bassin"

data_model <- Global_dataset %>% 
  mutate(Perturbations = Perturbations, Zone = Zone, damaged_percentage = damaged_percentatge/100) %>% 
  dplyr::select(species, FE, Perturbations, Zone, Morphology, Coloniality, Longevity, Height, Energy, Feeding, 
                Growth, Calcification, Mobility, Storage, damaged_percentage) %>% drop_na(Perturbations)
  
#beta_model <- brms::brm(damaged_percentage ~ Morphology + Coloniality + Longevity + Height + Energy + Feeding + Growth + Calcification +
#                          Mobility + Storage + Perturbations + (1 | Zone), data = data_model,
#          iter = 5000, warmup = 1000, chains = 3, cores = 3, family = "zero_one_inflated_beta", 
#          #control = list(adapt_delta = 0.9, max_treedepth = 15),
#          backend = "cmdstanr") # Lasts 1 hour and 5 minutes

# remove low samples
# data_model = data_model %>% dplyr::filter(Morphology != "f", Feeding %notin% c("f", "g"))
# data_model$Morphology    <- relevel(data_model$Morphology,    ref = "h")
# data_model$Coloniality   <- relevel(data_model$Coloniality,   ref = "2")
# data_model$Longevity     <- relevel(data_model$Longevity,     ref = "5")
# data_model$Height        <- relevel(data_model$Height,        ref = "0")
# data_model$Energy        <- relevel(data_model$Energy,        ref = "2")
# data_model$Feeding       <- relevel(data_model$Feeding,       ref = "e")
# data_model$Growth        <- relevel(data_model$Growth,        ref = "3")
# data_model$Calcification <- relevel(data_model$Calcification, ref = "a")
# data_model$Mobility      <- relevel(data_model$Mobility,      ref = "a")
# data_model$Storage       <- relevel(data_model$Storage,       ref = "a")

# data_model$Perturbations <- relevel(factor(data_model$Perturbations, levels = unique(data_model$Perturbations)), 
#                                     ref = "Temperature anomaly and Mucilage coverage")

theoretical_dataset <- expand.grid(unique(beta_model_ord$data[,2]), unique(beta_model_ord$data[,3]), unique(beta_model_ord$data[,4]),
                                   unique(beta_model_ord$data[,5]), unique(beta_model_ord$data[,6]), unique(beta_model_ord$data[,7]),
                                   unique(beta_model_ord$data[,8]), unique(beta_model_ord$data[,9]), unique(beta_model_ord$data[,10]),
                                   unique(beta_model_ord$data[,11]), unique(beta_model_ord$data[,12]))
colnames(theoretical_dataset) <- c("Morphology", "Coloniality", "Longevity", "Height", "Energy", "Feeding", "Growth", "Calcification",
                                     "Mobility", "Storage", "Perturbations")

# Not enough RAM >
# Create 1000 equally size folds
folds <- theoretical_dataset %>% group_by((row_number()-1) %/% (n()/1000)) %>% nest %>% pull(data)
data_model_predicted = vector("list", 1000)
# Set the number of cores to use
num_cores <- paralleldetectCores()
# Create a cluster of parallel workers
cl <- makeCluster(num_cores)
# Export necessary objects and functions to the workers
clusterExport(cl, c("folds", "beta_model_ord"))
# Parallelize the loop
data_model_predicted <- parLapply(cl, 1:1000, function(i) {cbind(folds[[i]], predict(beta_model_ord, folds[[i]]))})

data_model_predicted_all <- bind_rows(data_model_predicted)
save(data_model_predicted_all, file = "data_model_predicted_all.RData")
save(beta_model_ord_no_perturb, file = "beta_model_ord_no_perturb.RData")

beta_model_ord <- brms::brm(damaged_percentage ~ Morphology + Coloniality + Longevity + Height + Energy + Feeding + Growth + Calcification +
                            Mobility + Storage + Perturbations, 
                            data = data_model,
                            iter = 3000, warmup = 1000, chains = 3, cores = 3, family = "zero_one_inflated_beta", 
                            #control = list(adapt_delta = 0.9, max_treedepth = 15),
                            backend = "cmdstanr") # Lasts 40 minutes

beta_model_ord_no_perturb <- brms::brm(damaged_percentage ~ Morphology + Coloniality + Longevity + Height + Energy + Feeding + Growth + Calcification +
                              Mobility + Storage , 
                            data = data_model,
                            iter = 2000, warmup = 1000, chains = 3, cores = 3, family = "zero_one_inflated_beta", 
                            #control = list(adapt_delta = 0.9, max_treedepth = 15),
                            backend = "cmdstanr") # Lasts 40 minutes

data_model_predicted <- cbind(beta_model_ord$data, predict(beta_model_ord, beta_model_ord$data))
data_model_predicted_no_pert <- cbind(beta_model_ord_no_perturb$data, predict(beta_model_ord_no_perturb, beta_model_ord_no_perturb$data))
data_model_predicted_morph <- cbind(Morphology_model$data, predict(Morphology_model, Morphology_model$data))

(plot_1A <- data_model_predicted_all %>%
  mutate(Morphology = fct_reorder(Morphology, desc(Estimate))) %>%
  ggplot(aes(x = Morphology, y = Estimate, fill = Morphology)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1), name = "Effect Size") +
  theme_linedraw() +
  coord_flip())

(plot_1B <- data_model_predicted_no_pert %>%
    mutate(Morphology = fct_reorder(Morphology, desc(Estimate))) %>%
    ggplot(aes(x = Morphology, y = Estimate, fill = Morphology)) + geom_boxplot() +
    scale_y_continuous(limits = c(0,1), name = "Effect Size") +
    theme_linedraw() +
    coord_flip())

plot_1A + plot_1B + plot_1C + plot_layout(guides = "collect")

plot_2 <- data_model_predicted_all %>% ggplot(aes(x = Coloniality, y = Estimate, fill = Coloniality)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))
plot_3 <- data_model_predicted_all %>% ggplot(aes(x = Longevity, y = Estimate, fill = Longevity)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))
plot_4 <- data_model_predicted %>% ggplot(aes(x = Height, y = Estimate, fill = Height)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))
plot_5 <- data_model_predicted %>% ggplot(aes(x = Energy, y = Estimate, fill = Energy)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))
plot_6 <- data_model_predicted %>% ggplot(aes(x = Feeding, y = Estimate, fill = Feeding)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))
plot_7 <- data_model_predicted %>% ggplot(aes(x = Growth, y = Estimate, fill = Growth)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))
plot_8 <- data_model_predicted %>% ggplot(aes(x = Calcification, y = Estimate, fill = Calcification)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))
plot_9 <- data_model_predicted %>% ggplot(aes(x = Mobility, y = Estimate, fill = Mobility)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))
plot_10 <- data_model_predicted %>% ggplot(aes(x = Storage, y = Estimate, fill = Storage)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))

plot_11 <- data_model_predicted %>% ggplot(aes(x = Perturbations, y = Estimate, fill = Perturbations)) + geom_boxplot() +
  scale_y_continuous(limits = c(0,1))

plot_1 + plot_2 + plot_3 + plot_4 + plot_5 + 
  plot_6 + plot_7 + plot_8 + plot_9 + plot_10 + plot_layout(ncol = 5)

Morphology_model    <- brms::brm(damaged_percentage ~ Morphology, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min
Coloniality_model   <- brms::brm(damaged_percentage ~ Coloniality, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min
Longevity_model     <- brms::brm(damaged_percentage ~ Longevity, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min
Height_model        <- brms::brm(damaged_percentage ~ Height, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min
Energy_model        <- brms::brm(damaged_percentage ~ Energy, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min
Feeding_model       <- brms::brm(damaged_percentage ~ Feeding, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min
Calcification_model <- brms::brm(damaged_percentage ~ Calcification, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min
Growth_model        <- brms::brm(damaged_percentage ~ Growth, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min
Mobility_model      <- brms::brm(damaged_percentage ~ Mobility, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min
Storage_model       <- brms::brm(damaged_percentage ~ Storage, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min

Perturbations_model <- brms::brm(damaged_percentage ~ Perturbations, data = data_model,
                                 iter = 2000, warmup = 1000, chains = 2, cores = 2, family = "zero_one_inflated_beta", 
                                 backend = "cmdstanr") # Lasts <1min





mean(posterior_samples$b_Morphologyh)

rstantools::bayes_R2(beta_model_ord)
# plot(beta_model) 
rstanarm::pp_check(beta_model_ord, type = "scatter_avg") 
beta_model_df = cbind(fitted(beta_model_ord),beta_model_ord$data) 

unique(data_model_Temp$Morphology)

coef_summary <- brms::fixef(beta_model_ord)

