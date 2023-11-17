#### Figure_1
mypalette <- colorBin(palette = Fig_1_col, domain = unique(MME_Merged_data$damaged_percentatge), 
                      na.color = "transparent", bins = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100))

(Figure_1 <- MME_Merged_data %>% leaflet(options = leafletOptions(attributionControl = F)) %>% addTiles() %>% 
    setView(lat = 40, lng = 18 , zoom = 4) %>% addProviderTiles("Esri.WorldImagery") %>% addCircles(~longitude, ~latitude,  
               fillColor = ~ mypalette(damaged_percentatge), fillOpacity = .5, color = "white", radius = ~sqrt(damaged_percentatge) * 4e3, stroke = F, weight = 1) %>%
    addLegend(pal = mypalette, values = ~damaged_percentatge, opacity = .9, title = "Damaged due to MMEs (%)", position = "topright" ))