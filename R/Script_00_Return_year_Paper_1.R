library(tidyverse)
library(brms)

MME      <- readxl::read_excel("/Users/jeremycarlot/Desktop/MME.xlsx")
MME_melt <- reshape2::melt(MME, id = c("sub-ecoregion", "id.hexagon", 
                           "#Years with MME", "#Records MMEs_All_years_")) %>% 
  mutate(year = 1979 + as.numeric(variable) - 1,
         num_year = rev(year)-1979) %>% select(-"variable") %>% 
  dplyr::filter(value != 0) 
MME_melt <- MME_melt %>% full_join(data.frame(id.hexagon = unique(MME_melt$id.hexagon),
                                              year = 2020, value = 0, num_year = 0))

hex_data <- MME_melt %>% group_by(id.hexagon) %>% group_split()
new_hex_data <- list() ; for (i in seq_along(hex_data)) {group <- hex_data[[i]]  
  years_with_value_1 <- group$year[group$value == 1]
  value_diff <- c(diff(years_with_value_1), NA)
  if (any(is.na(value_diff))) {last_year_with_value_1 <- max(years_with_value_1, na.rm = TRUE)
    value_diff[is.na(value_diff)] <- NA } #2020 - last_year_with_value_1}
  new_df <- data.frame(ID = rep(group$id.hexagon[1], length(years_with_value_1)),
    year = years_with_value_1,value = value_diff)
  new_hex_data[[i]] <- new_df}

# Datasets
new_hex_data = new_hex_data %>% bind_rows() %>% rename(return_year = value, id.hexagon = ID) %>% 
  right_join(MME_melt %>% select(id.hexagon, year, `sub-ecoregion`), 
             by = c("id.hexagon", "year"), relationship = "many-to-many") %>% 
  dplyr::filter(return_year > 0) %>% 
  mutate(year_rev = 2020 - year)
new_hex_data_summary <- new_hex_data %>% group_by(year, return_year) %>% 
  summarise(tot_return_years = n()) 
new_hex_data_mean <- new_hex_data %>% group_by(year) %>% 
  summarise(mean_return_year = mean(return_year)) 

# Fit the model using brms
exp_model <- brm(formula = bf(return_year ~ a * exp(b * year_rev), a ~ 1, b ~ 1, nl = TRUE),
  data = new_hex_data, family = gaussian(), iter = 2000, warmup = 1000, chains = 4,
  prior = c(prior(normal(1, 0.5), nlpar = "a"), prior(normal(0.1, 0.05), nlpar = "b")),
  cores = 4) #parallel::detectCores())
bayes_R2(exp_model)

plot(exp_model)

# Predictions
Return_years = cbind(data.frame(year_rev = seq(0,41,0.001)), 
                     predict(exp_model, data.frame(year_rev = seq(0,41,0.001))))

model_exp_pred = Return_years %>% 
  mutate(max_return_year = max(Estimate),
         Return_Year_sc = scales::rescale(Estimate) * max_return_year,
         Est.Error_sc_inf = Return_Year_sc - Est.Error,
         Est.Error_sc_hig = Return_Year_sc + Est.Error,
         Year = 2020 - Return_years$year_rev)

model_exp_pred$Est.Error_sc_inf[model_exp_pred$Est.Error_sc_inf <= 0] = 0
model_exp_pred$Est.Error_sc_hig[model_exp_pred$Est.Error_sc_hig >= 42] = 42

# Model a white polygon
white_polygon <- data.frame(year = seq(1979, 2020, 0.1),
           Est.Error_sc_inf = -1*seq(0, 41, 0.1) + 41,
           Est.Error_sc_hig = -1*seq(0, 41, 0.1) + 51)

white_polygon$Est.Error_sc_hig[white_polygon$Est.Error_sc_hig >= 41] = 41

new_hex_data_summary = new_hex_data_summary %>% 
  mutate(cut_interval = cut(tot_return_years, breaks = seq(0,48,8)))

Figure_4b <- ggplot() + 
  geom_ribbon(data = model_exp_pred, aes(ymin = Est.Error_sc_inf, ymax = Est.Error_sc_hig, x = Year), alpha = .3, fill = "firebrick3") + 
  geom_ribbon(data = white_polygon, aes(ymin = Est.Error_sc_inf, ymax = Est.Error_sc_hig, x = year), fill = "white") + 
  geom_point(data = new_hex_data_summary, aes(x = year, y = return_year, size = tot_return_years), fill = "gold", shape = 21, alpha = .7) +
  geom_point(data = new_hex_data_mean, aes(x = year, y = mean_return_year), fill = "darkorange3", shape = 21, alpha = .7, size = 3) +
  stat_smooth(data = model_exp_pred, aes(x = Year, y = Return_Year_sc), color = "firebrick3", alpha = 1, size = 1) +
  geom_segment(aes(x = 1979, y = 41, xend = 2020, yend = 0), color = "black", linewidth = 1) +
  scale_size_continuous(name = "N° of Events", breaks = seq(8, 40, 8)) +
  scale_y_continuous(limits = c(0, 41)) + 
  scale_x_continuous(limits = c(1979, 2020), breaks = seq(1979, 2020, 1), 
                     labels = c("", "1980", "", "", "", "", "1985", "", "", "", "", "1990", "", "", "", "", "1995",
                                "", "", "", "", "2000", "", "", "", "", "2005", "", "", "", "", "2010", 
                                "", "", "", "", "2015", "", "", "", "", "2020")) +
  theme_classic() + labs(x = "Year", y = "Return Years") +
  theme(legend.position = "bottom",
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14))

ggsave(Figure_4b, file = "../Figures/raw/Figure_4b.png", width = 12, height = 12, unit = 'cm')
