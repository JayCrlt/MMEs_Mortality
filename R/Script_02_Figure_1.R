our_nc_data <- nc_open("../Data/GIS/adaptor.mars.internal-1719404690.4140854-1379-11-46ebdeb5-626b-42aa-8d65-d8ad537b6eda.nc")

#### Figure_1
mypalette <- colorBin(palette = Fig_1_col, domain = unique(MME_Merged_data$damaged_percentatge), 
                      na.color = "transparent", bins = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

(Figure_1 <- MME_Merged_data %>% leaflet(options = leafletOptions(attributionControl = F)) %>% addTiles() %>% 
    setView(lat = 40, lng = 18 , zoom = 4) %>% addProviderTiles("Esri.WorldImagery") %>% addCircles(~longitude, ~latitude,  
               fillColor = ~ mypalette(damaged_percentatge), fillOpacity = .5, color = "white", radius = ~sqrt(damaged_percentatge) * 4e3, stroke = F, weight = 1) %>%
    addLegend(pal = mypalette, values = ~damaged_percentatge, opacity = .9, title = "Damaged due to MMEs (%)", position = "topright" ))

# Figure S1 Post Review
MME_Merged_data_S1 <- MME_Merged_data %>% dplyr::filter(year >= 1986)
mypalette <- colorBin(palette = Fig_1_col, domain = unique(MME_Merged_data_S1$year), 
                      na.color = "transparent", bins = c(1985, 1990, 1995, 2000, 2005, 2010, 2015, 2020))


(Figure_S1 <- MME_Merged_data_S1 %>% leaflet(options = leafletOptions(attributionControl = F)) %>% addTiles() %>% 
    setView(lat = 40, lng = 18 , zoom = 4) %>% addProviderTiles("Esri.WorldImagery") %>% addCircles(~longitude, ~latitude,  
                                                                                                    fillColor = ~ mypalette(year), fillOpacity = .5, color = "white", radius = ~sqrt(year) * 8e2, stroke = F, weight = 1) %>%
    addLegend(pal = mypalette, values = ~year, opacity = .9, title = "Range of year observed", position = "topright" ))

# Figure 1 Part 2 Post review
attributes(our_nc_data$var)
attributes(our_nc_data$dim)
lat <- ncvar_get(our_nc_data, "latitude")
nlat <- dim(lat) #to check it matches the metadata: 53
lon <- ncvar_get(our_nc_data, "longitude")
nlon <- dim(lon) #to check, should be 161
time <- ncvar_get(our_nc_data, "time")
tunits <- ncatt_get(our_nc_data, "time", "units") 
time_obs <- as.POSIXct(time * 3600, origin = "1900-01-01", tz="GMT")
ntime <- dim(time_obs) #to check, should be 6912

lswt_array <- ncvar_get(our_nc_data, "sst") 
fillvalue <- ncatt_get(our_nc_data, "sst", "_FillValue")
dim(lswt_array) #to check; this should give you 161 53 6912
lswt_array[lswt_array==fillvalue$value] <- NA
lswt_array = lswt_array - 273.15

lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))
lswt_vec_long <- as.vector(lswt_array)
length(lswt_vec_long) #to check; this should give you 161*53*6912 = 58980096
lswt_obs <- data.frame(cbind(lonlattime, lswt_vec_long))
lswt_obs$Var3 <- as.Date(lswt_obs$Var3)

lswt_obs_avg <- lswt_obs %>%
  mutate(Year = year(Var3), Decade = sapply(Year, assign_decade)) %>% 
  dplyr::filter(lswt_vec_long > 0) %>% 
  mutate(Var1 = round(as.numeric(Var1), 0), Var2 = round(as.numeric(Var2), 0)) %>% 
  group_by(Var1, Var2, Var3, Year, Decade) %>% 
  summarise(lswt_vec_long = max(as.numeric(lswt_vec_long))) 

mean_sst <- lswt_obs_avg %>% 
  group_by(Decade) %>% summarise(lswt_vec_long_avg = mean(lswt_vec_long), lswt_vec_long_sd = sd(lswt_vec_long)) %>% 
  mutate(xmin = lswt_vec_long_avg - lswt_vec_long_sd, xmax = lswt_vec_long_avg + lswt_vec_long_sd)
lswt_obs_avg = lswt_obs_avg %>% dplyr::filter(lswt_vec_long <= 40)

Figure_1A <- ggplot() +
  geom_segment(data = mean_sst, aes(x = xmin, xend = xmax, y = Decade, yend = Decade, color = lswt_vec_long_avg), size = 1) +
  geom_density_ridges(data = lswt_obs_avg, aes(x = lswt_vec_long, y = Decade, height = after_stat(ndensity)), fill = "#ffcccc",
                      scale = 2.5, alpha = 0.6, bandwidth = 0.25, linewidth = 1) +
  scale_x_continuous(limits = c(0, 32)) +
  geom_vline(xintercept = 19, linetype = "dotted", size = 1) +
  geom_point(data = mean_sst, aes(x = lswt_vec_long_avg, y = Decade, fill = lswt_vec_long_avg), 
             shape = 21, color = "black", size = 4) +
  labs(x = expression("Mean Sea Surface Temperature (°C)"), y = "") + 
  scale_fill_gradient2(low = "white", mid = "#ff9999", high = "#cc3333", midpoint = 19) +
  scale_color_gradient2(low = "white", mid = "#ff9999", high = "#cc3333", midpoint = 19) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

lswt_array <- ncvar_get(our_nc_data, "i10fg") 
fillvalue <- ncatt_get(our_nc_data, "i10fg", "_FillValue")
dim(lswt_array) #to check; this should give you 161 53 432
lswt_array[lswt_array==fillvalue$value] <- NA

lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))
lswt_vec_long <- as.vector(lswt_array)
length(lswt_vec_long) #to check; this should give you 161*53*432 = 3686256
lswt_obs <- data.frame(cbind(lonlattime, lswt_vec_long))
lswt_obs$Var3 <- as.Date(lswt_obs$Var3)

lswt_obs_avg <- lswt_obs %>%
  mutate(Year = year(Var3), Decade = sapply(Year, assign_decade), lswt_vec_long = as.numeric(lswt_vec_long)) %>% 
  dplyr::filter(lswt_vec_long > 15) %>% 
  mutate(Var1 = round(as.numeric(Var1), 0), Var2 = round(as.numeric(Var2), 0)) %>% 
  group_by(Var1, Var2, Var3, Year, Decade) %>% 
  summarise(lswt_vec_long = max(lswt_vec_long)) 

mean_storm <- lswt_obs_avg %>% 
  group_by(Decade) %>% summarise(lswt_vec_long_avg = mean(lswt_vec_long), lswt_vec_long_sd = sd(lswt_vec_long)) %>% 
  mutate(xmin = lswt_vec_long_avg - lswt_vec_long_sd, xmax = lswt_vec_long_avg + lswt_vec_long_sd)

Figure_1B <- ggplot() +
  geom_segment(data = mean_storm, aes(x = xmin, xend = xmax, y = Decade, yend = Decade, color = lswt_vec_long_avg), size = 1) +
  geom_density_ridges(data = lswt_obs_avg, aes(x = lswt_vec_long, y = Decade, height = after_stat(ndensity)), fill = "#0099aa",
                      scale = 2.5, alpha = 0.6, bandwidth = 0.1, linewidth = 1) +
  scale_x_continuous(limits = c(15, 19)) +
  geom_vline(xintercept = 15.75, linetype = "dotted", linewidth = 1) +
  geom_point(data = mean_storm, aes(x = lswt_vec_long_avg, y = Decade, fill = lswt_vec_long_avg), 
             shape = 21, color = "black", size = 4) +
  labs(x = expression("Mean Wind guts ≥ 15m."*s^-1*"(m."*s^-1*")"), y = "") + 
  scale_fill_gradient2(low = "gray80", mid = "#0099cc", high = "#000099", midpoint = 15.8) +
  scale_color_gradient2(low = "gray80", mid = "#0099cc", high = "#000099", midpoint = 15.8) +
  theme_classic() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1),
        strip.text = element_blank(), 
        strip.background = element_blank(),
        legend.position = "none")

lswt_array <- ncvar_get(our_nc_data, "mtpr") 
fillvalue <- ncatt_get(our_nc_data, "mtpr", "_FillValue")
dim(lswt_array) #to check; this should give you 161 53 432
lswt_array[lswt_array==fillvalue$value] <- NA

lonlattime <- as.matrix(expand.grid(lon,lat,time_obs))
lswt_vec_long <- as.vector(lswt_array)
length(lswt_vec_long) #to check; this should give you 161*53*432 = 3686256
lswt_obs <- data.frame(cbind(lonlattime, lswt_vec_long))
lswt_obs$Var3 <- as.Date(lswt_obs$Var3)

lswt_obs_avg <- lswt_obs %>%
  mutate(Year = year(Var3), Decade = sapply(Year, assign_decade)) %>% 
  dplyr::filter(lswt_vec_long > 0) %>% 
  mutate(Var1 = round(as.numeric(Var1), 0), Var2 = round(as.numeric(Var2), 0)) %>% 
  group_by(Var1, Var2, Var3, Year, Decade) %>% 
  summarise(lswt_vec_long = max(as.numeric(lswt_vec_long)) * 10^6) 

mean_pr <- lswt_obs_avg %>% 
  group_by(Decade) %>% summarise(lswt_vec_long_avg = mean(lswt_vec_long), lswt_vec_long_sd = sd(lswt_vec_long)) %>% 
  mutate(xmin = lswt_vec_long_avg - lswt_vec_long_sd, xmax = lswt_vec_long_avg + lswt_vec_long_sd)
mean_pr$xmin[mean_pr$xmin < 0] = 0
lswt_obs_avg = lswt_obs_avg %>% dplyr::filter(lswt_vec_long <= 100)

Figure_1C <- ggplot() +
  geom_segment(data = mean_pr, aes(x = xmin, xend = xmax, y = Decade, yend = Decade, color = lswt_vec_long_avg), size = 1) +
  geom_density_ridges(data = lswt_obs_avg, aes(x = lswt_vec_long, y = Decade, height = after_stat(ndensity)), fill = "#99cc66",
                      scale = 2.5, alpha = 0.6, bandwidth = .25, linewidth = 1) +
  scale_x_continuous(limits = c(0, 100)) +
  geom_vline(xintercept = 23.7, linetype = "dotted", size = 1) +
  geom_point(data = mean_pr, aes(x = lswt_vec_long_avg, y = Decade, fill = lswt_vec_long_avg), 
             shape = 21, color = "black", size = 4) +
  labs(x = expression("Mean Nutrient Runoff (mg."*m^-2*"."*s^-1*")"), y = "") + 
  scale_fill_gradient2(low = "white", mid = "#ccff66", high = "#336600", midpoint = 23.7) +
  scale_color_gradient2(low = "white", mid = "#ccff66", high = "#336600", midpoint = 23.7) +
  theme_classic() +
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

(Figure_1b <- Figure_1A + Figure_1B + Figure_1C)