#### Bayesian Models
Perturbations <- rep(NA, length = length(data_mortality_from_complete$Species))
Perturbations <- paste(data_mortality_from_complete$drivers_abiotic, data_mortality_from_complete$drivers_biotic_group, sep = " and ")
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
Zone <- data_mortality_from_complete$ecoregion
Zone[Zone %in% c("Alboran Sea", "Western Mediterranean")]                                         = "Western_Bassin"
Zone[Zone %in% c("Adriatic Sea", "Ionian Sea", "Tunisian Plateau/Gulf of Sidra")]                 = "Central_Bassin"
Zone[Zone %in% c("Aegean Sea", "Levantine Sea")]                                                  = "Eastern_Bassin"

# Build Data Model
data_model <- data_mortality_from_complete %>% mutate(Perturbations = Perturbations, Zone = Zone, damaged_percentage = damaged_percentatge/100) %>% 
  dplyr::select(year, Species, FE, Perturbations, Zone, damaged_percentage) %>% drop_na(Perturbations) %>% dplyr::filter(year > 1985)

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
volume_trait_quantification <- brms::brm(Volume ~ Abiotic_others + Biotic_others + Disease + Turbidity_and_Sedimentation + Mucillage + Pollution + 
                                          Predator_outbreaks + Storms + Temperature_anomaly, data = pivot_table, iter = 3000, warmup = 1000, chains = 2, cores = 2, 
                                          family = "zero_one_inflated_beta", backend = "cmdstanr") 
# Predict
pivot_table <- pivot_table %>% mutate(Volume_predicted = predict(volume_trait_quantification, pivot_table)[,1],
                                      Volume_Q2.5      = predict(volume_trait_quantification, pivot_table)[,3],
                                      Volume_Q97.5     = predict(volume_trait_quantification, pivot_table)[,4],
                                      Volume_sd        = predict(volume_trait_quantification, pivot_table)[,2])
# Quality
rstantools::bayes_R2(volume_trait_quantification) ; rstanarm::pp_check(volume_trait_quantification, type = "scatter_avg") 

# Analyzing this model
single_stressors <- pivot_table %>% dplyr::filter(Year %in% c(1986, 1992, 1996:1998, 2004:2005, 2011))
double_stressors <- pivot_table %>% dplyr::filter(Year %in% c(1993, 1995, 2001:2002, 2006:2007, 2020))
triple_stressors <- pivot_table %>% dplyr::filter(Year %in% c(1987:1988, 1999, 2003, 2008:2010, 2014, 2016))
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
  dplyr::select(Volume_predicted, Volume_sd, Volume_Q2.5, Volume_Q97.5) %>% data.frame()

xlsx::write.xlsx(predicted_pert, "predicted_pert.xlsx")
