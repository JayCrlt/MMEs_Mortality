#### Figure_1
mycolors <- c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
              "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")

mybins    <- c(10,20,30,40,50,60,70,80,90,100)
mypalette <- colorBin(palette = mycolors, 
                      domain=unique(MME_Merged_data$damaged_percentatge), 
                      na.color="transparent", 
                      bins=mybins)

(Figure_1 <- MME_Merged_data %>% 
    leaflet(options = leafletOptions(
      attributionControl=FALSE)) %>% 
    addTiles() %>% 
    setView(lat = 40, lng = 18 , zoom = 4) %>%
    addProviderTiles("Esri.WorldImagery") %>%
    addCircles(~longitude, ~latitude,  
               fillColor = ~mypalette(damaged_percentatge), 
               fillOpacity = 0.5, color = "white", radius = ~sqrt(damaged_percentatge)*4000, 
               stroke=FALSE, weight = 1,) %>%
    addLegend( pal=mypalette, values=~damaged_percentatge, opacity=0.9, 
               title = "Damaged due to MMEs (%)", 
               position = "topright" ))