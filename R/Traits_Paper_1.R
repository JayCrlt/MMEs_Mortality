data_barplot_spatial$`1.morphology`

A <- table(data_barplot_spatial$`3.longevity`) %>% data.frame() %>% rename(Trait_cat = Var1) %>% mutate(., Trait = rep("Longevity", 5))
B <- table(data_barplot_spatial$`7.growth.rates`) %>% data.frame() %>% rename(Trait_cat = Var1) %>% mutate(., Trait = rep("Growth", 5))
C <- table(data_barplot_spatial$`1.morphology`) %>% data.frame() %>% rename(Trait_cat = Var1) %>% mutate(., Trait = rep("Morphology", 8))

data_trait_Paper_1 <- A %>% 
  mutate(Freq = round((Freq / 1858) * 100, 2)) %>% 
  ggplot(aes(x = Trait_cat, y = Freq)) +
  geom_segment(aes(xend = Trait_cat), yend = 0, size = 0.5, linetype = "dashed") +
  geom_point(size = 6, shape = 21, fill = "#990066") +
  scale_y_continuous(limits = c(0,75), breaks = seq(0,75,25), name = "Frequency (%)", expand = c(0,0)) +
  scale_x_discrete(name = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 0),
        plot.title = element_text(size = 20)) +
  ggtitle("1. Longevity (yr)")

data_trait_Paper_2 <- B %>% 
  mutate(Freq = round((Freq / 1858) * 100, 2)) %>% 
  ggplot(aes(x = Trait_cat, y = Freq)) +
  geom_segment(aes(xend = Trait_cat), yend = 0, size = 0.5, linetype = "dashed") +
  geom_point(size = 6, shape = 21, fill = "#990066") +
  scale_y_continuous(limits = c(0,75), breaks = seq(0,75,25), name = "", expand = c(0,0)) +
  scale_x_discrete(name = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 0),
        plot.title = element_text(size = 20)) +
  ggtitle("2. Growth (cm/yr)")

data_trait_Paper_3 <- C %>% 
  mutate(Freq = round((Freq / 1858) * 100, 2)) %>% 
  ggplot(aes(x = Trait_cat, y = Freq)) +
  geom_segment(aes(xend = Trait_cat), yend = 0, size = 0.5, linetype = "dashed") +
  geom_point(size = 6, shape = 21, fill = "#990066") +
  scale_y_continuous(limits = c(0,75), breaks = seq(0,75,25), name = "", expand = c(0,0)) +
  scale_x_discrete(name = "") +
  theme_classic() +
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 0),
        plot.title = element_text(size = 20)) +
  ggtitle("3. Morphology")

(data_trait_Paper = data_trait_Paper_1 + data_trait_Paper_2 + data_trait_Paper_3 + plot_layout(widths = c(5,5,8)))
#ggsave(data_trait_Paper, file = "../Figures/data_trait_Paper", device = "png", width = 10.00, height = 5.0, units = "in")

data_conservation <- data_barplot_spatial %>% left_join(., species_traits) 
data_cons <- table(data_conservation$Conservation.status, data_conservation$year) %>% data.frame() %>% 
  mutate(Var2 = as.numeric(Var2))
ID_yr <- data.frame(Var2 = seq(1, 35, 1),
                    year = sort(unique(data_conservation$year)))
data_cons <- data_cons %>% left_join(ID_yr)

new_species_counts <- data_conservation %>% group_by(year) %>% dplyr::filter(Conservation.status != "g") %>% 
  distinct(species) %>% arrange(year)
new_species_counts <- new_species_counts %>%
  group_by(species) %>%
  mutate(new_species = year - lag(year) > 1 | is.na(lag(year)))
new_species_counts <- new_species_counts %>%
  group_by(year) %>%
  summarize(new_species_count = sum(new_species)) 

years_to_keep = new_species_counts$year
new_species_counts_y = data_cons %>% group_by(year) %>% dplyr::filter(year %in% years_to_keep) %>% summarise(y = sum(Freq) + 25) 
new_species_counts <- new_species_counts %>% mutate(y = new_species_counts_y$y) %>% 
  dplyr::filter(new_species_count > 0)

data_cons <- data_cons %>% mutate(Var1 = recode(Var1,
                                                "a" = "Critically endangered",
                                                "b" = "Endangered",
                                                "c" = "Vulnerable",
                                                "d" = "Near threatened",
                                                "e" = "Least concern",
                                                "f" = "Data deficient",
                                                "g" = "Not evaluated"))

New_species <- ggplot() + 
  geom_bar(data = data_cons, aes(x = year, y = Freq, fill = Var1), stat = "identity", color = "black", na.rm = TRUE) +
  scale_fill_manual(values = c("#6f0000", "#bd0909", "#ca3a3a", "#ff8c24", "#ffdc54","#f1f292", "#ffffff")) +
  theme_classic() +
  geom_text(data = new_species_counts, aes(x = year, y = y, label = new_species_count)) +
  geom_point(data = new_species_counts, aes(x = year, y = y), size = 6, shape = 21) +
  scale_x_continuous(breaks = seq(1980, 2020, 5), name = "") +
  scale_y_continuous(name = "Mortality records", expand = c(0,0), limits = c(0,375)) +
  labs(fill = "Conservation Status") +
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
        plot.title = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) 

new_species_counts <- data_conservation %>% group_by(year) %>% dplyr::filter(Conservation.status != "g") %>% 
  distinct(species) %>% arrange(year)
new_species_counts <- new_species_counts %>%
  group_by(species) %>%
  mutate(new_species = year - lag(year) > 0 | is.na(lag(year)))
new_species_counts <- new_species_counts %>%
  group_by(year) %>%
  summarize(new_species_count = sum(new_species)) 
years_to_keep = new_species_counts$year
new_species_counts_y = data_cons %>% group_by(year) %>% dplyr::filter(year %in% years_to_keep) %>% summarise(y = sum(Freq) + 25) 
new_species_counts <- new_species_counts %>% mutate(y = new_species_counts_y$y) %>% 
  dplyr::filter(new_species_count > 0)

data_cons <- data_cons %>% mutate(Var1 = recode(Var1,
                                                "a" = "Critically endangered",
                                                "b" = "Endangered",
                                                "c" = "Vulnerable",
                                                "d" = "Near threatened",
                                                "e" = "Least concern",
                                                "f" = "Data deficient",
                                                "g" = "Not evaluated"))

Tot_species <- ggplot() + 
  geom_bar(data = data_cons, aes(x = year, y = Freq, fill = Var1), stat = "identity", color = "black", na.rm = TRUE) +
  scale_fill_manual(values = c("#6f0000", "#bd0909", "#ca3a3a", "#ff8c24", "#ffdc54","#f1f292", "#ffffff")) +
  theme_classic() +
  geom_text(data = new_species_counts, aes(x = year, y = y, label = new_species_count)) +
  geom_point(data = new_species_counts, aes(x = year, y = y), size = 6, shape = 21) +
  scale_x_continuous(breaks = seq(1980, 2020, 5), name = "") +
  scale_y_continuous(name = "Mortality records", expand = c(0,0), limits = c(0,375)) +
  labs(fill = "Conservation Status") +
  theme(axis.text.y = element_text(size = 14),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 16, angle = 45, hjust = 1),
        plot.title = element_text(size = 20),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14)) 
