rm(list = ls()) ; options(warn = -1)

## Packages
library(readxl) ; library(tidyverse) ; library(viridis) ; library(hrbrthemes) ; library(rgdal) 
library(mapdata) ; library(leaflet) ; library(sf) ; library(rnaturalearth) ; library(rgeos)
library(rnaturalearthdata)  ; library(ggspatial) ; library(scales) ; library(patchwork)

## Functions
`%notin%` <- Negate(`%in%`)

## Load Datasets
# Dataset Mortality from Massimo Ponti et al.
MME_Mortality <- read_excel("Data/MME-Review data.xlsx", 
                            sheet = "MME2015-2019 cleaned", 
                            col_types = c("numeric", "text", "date", "date", "numeric", "text", 
                                          "numeric", "text", "text", "text", "text", "numeric", 
                                          "numeric", "numeric", "numeric", "text", "text", "text", 
                                          "numeric", "numeric", "text", "numeric", "text", "text", 
                                          "numeric", "text", "text", "numeric", "numeric", 
                                          "numeric", "numeric", "text", "numeric", "numeric", 
                                          "text", "text", "text", "text", "numeric", "text", 
                                          "numeric", "text", "text", "text", "numeric", "numeric",
                                          "numeric", "numeric", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text", "text", "text", 
                                          "text", "text", "text", "text", "text"))

# Dataset Functional traits from Nuria Teixido et al.
species_traits <- read_excel("Data/species_traits.xlsx", 
                             sheet = "4_species_traits_clean.v2", 
                             col_types = c("text", "text", "text", "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "text", "text", "text", "text", "text"))

## Unravel the missing species to code
# Dataset Mortality from Massimo Ponti et al.
MME_Merged_data <- read_excel("Data/MME-Review data.xlsx", 
                              sheet = "Merged datasets",
                              col_types = c("numeric", "text", "date", "date", "numeric", "text", "text", 
                                            "text", "text", "text", "text", "numeric", "numeric", "numeric", 
                                            "numeric", "text", "text", "text", "text", "numeric", "text", 
                                            "numeric", "text", "text", "text", "text", "text", "numeric", 
                                            "numeric", "numeric", "numeric", "text", "numeric", "numeric"))

table(MME_Merged_data$species)

# Extract species
all_species    = unique(MME_Merged_data$species)
species_subset = unique(species_traits$species)
# Dataset w/ missing species
data_species_to_add <- data.frame(species     = all_species[all_species %notin% species_subset],
                                  Keep_or_rmv = c("Y", "Y", "Y", "Y", "N", "N", "Y", "Y", "Y", "N", 
                                                  "N", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
                                                  "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", 
                                                  "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
                                                  "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", 
                                                  "Y", "Y", "Y", "Y", "Y", "N", "N", "N", "N", "Y",
                                                  "Y", "N", "N", "N", "Y", "Y", "Y", "Y", "Y", "Y", 
                                                  "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "Y", "Y",
                                                  "Y", "Y", "Y", "N", "N", "N", "N", "N", "N"),
                                  ID_sp_lvl   = c("Y", "N", "Y", "Y", "N", "N", "N", "N", "N", "N", 
                                                  "N", "Y", "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y",
                                                  "Y", "Y", "Y", "N", "Y", "Y", "N", "Y", "Y", "Y", 
                                                  "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
                                                  "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", 
                                                  "Y", "Y", "Y", "N", "Y", "Y", "Y", "Y", "Y", "Y",
                                                  "Y", "Y", "Y", "Y", "Y", "Y", "N", "Y", "N", "Y", 
                                                  "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y", "Y",
                                                  "Y", "Y", "Y", "Y", "Y", "N", "N", "N", "N")) %>% 
  arrange(Keep_or_rmv, ID_sp_lvl, species)
# Filtering only benthic species
data_species_to_add_w_sp_ID     = data_species_to_add %>% filter(Keep_or_rmv == "Y", ID_sp_lvl == "Y") %>% 
  select(species) %>% as.vector()
data_species_to_add_additional  = data_species_to_add %>% filter(Keep_or_rmv == "Y", ID_sp_lvl == "N") %>% 
  select(species) %>% as.vector()
data_species_to_add_w_sp_ID$species[data_species_to_add_w_sp_ID$species == "Epizoanthus  arenaceus"] = 
  "Epizoanthus arenaceus"
data_species_to_add %>% filter(Keep_or_rmv == "N", ID_sp_lvl == "N") %>% 
  select(species) %>% as.vector()

# Missing Species
(data_species_to_add_w_sp_ID    = data_species_to_add_w_sp_ID$species)
(data_species_to_add_additional = data_species_to_add_additional$species)

data_xlsx_species_to_add = c(data_species_to_add_w_sp_ID, data_species_to_add_additional)
xlsx::write.xlsx(data_xlsx_species_to_add, file = "data_xlsx_species_to_add.xlsx")

## Define the FE number
# Be sure that we have dataset w/ mortality only
MME_Mortality <- MME_Mortality %>% drop_na(., `Damaged percentage`)
# Look at the species into the dataset
species_MMEs       <- MME_Mortality %>% arrange(Species) %>% distinct(Species)
species_Fctl       <- species_traits %>% arrange(species) %>% distinct(species)
# Species mismatch
Species_mismatch   <- species_MMEs$Species[species_MMEs$Species %notin% species_Fctl$species]

# Remove the 4 missing species so far
MME_Mortality      <- MME_Mortality %>% filter(Species %notin% Species_mismatch)

# Plot the % damaged
mybins    = c(1,5,10,25,50,75,80,90,100)
mypalette = colorBin(palette="YlOrBr", 
                     domain=unique(MME_Mortality$`Damaged percentage`), 
                     na.color="transparent", 
                     bins=mybins)

# Huge effort w/ Pinna nobilis
Figure_1 <- MME_Mortality %>% 
leaflet() %>% 
  addTiles() %>% 
  setView(lat = 40, lng = 18 , zoom = 4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircles(~Longitude, ~Latitude,  
             fillColor = ~mypalette(`Damaged percentage`), 
             fillOpacity = 0.5, color = "white", radius = ~sqrt(`Damaged percentage`)*3000, 
             stroke=FALSE, weight = 1,) %>%
  addLegend( pal=mypalette, values=~`Damaged percentage`, opacity=0.9, title = "Damaged due to MMEs (%)", 
             position = "topright" )

Figure_2 <- MME_Mortality %>% filter(., Species != "Pinna nobilis") %>% 
  leaflet() %>% 
  addTiles() %>% 
  setView(lat = 40, lng = 18 , zoom = 4) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircles(~Longitude, ~Latitude,  
             fillColor = ~mypalette(`Damaged percentage`), 
             fillOpacity = 0.5, color = "white", radius = ~sqrt(`Damaged percentage`)*3000, 
             stroke=FALSE, weight = 1,) %>%
  addLegend( pal=mypalette, values=~`Damaged percentage`, opacity=0.9, title = "Damaged due to MMEs (%)", 
             position = "topright" )

# Work with cell 1° x 1°
Occurence <- MME_Mortality %>%
     mutate(Longitude = round(Longitude, 1)) %>%
     mutate(Latitude = round(Latitude, 1)) %>%
     group_by(Latitude, Longitude, Country, Species) %>%
     summarise(`Damaged percentage` = mean(`Damaged percentage`)) 

Occurence_species_per_cell <- Occurence %>% 
  mutate(Longitude = round(Longitude, 0)) %>%
  mutate(Latitude = round(Latitude, 0)) %>%
  group_by(Latitude, Longitude, Species) %>% 
  summarise(`Damaged percentage`  = mean(`Damaged percentage`))

# Functioning
tr_cat = data.frame(trait_name = colnames(species_traits[2:12]),
                    trait_type = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N"),
                    fuzzy_name = rep(NA, 11))
sp_tr  = species_traits %>% column_to_rownames("species") %>% dplyr::select(., 1:11)
sp_tr  = sp_tr %>% dplyr::mutate_all(as.factor)
sp_to_fe <- mFD::sp.to.fe(sp_tr = sp_tr, tr_cat = tr_cat) 
fe_nm <- unique(sp_to_fe$fe_nm) ; length(fe_nm) # 58 FE

# List of species in each FE
fe_sp <- list() ; for (k in fe_nm) {fe_sp[[k]]<-names(sp_to_fe$sp_fe[which(sp_to_fe$sp_fe==k)])}

# Trait values of FE
fe_tr <- sp_to_fe$fe_tr

### Grid

# Delineate a grid
quantile(MME_Mortality$Latitude) # 31 to 45
quantile(MME_Mortality$Longitude) # -6 to 36
Lat_grid = seq(31, 45, 1) ; Lon_grid = seq(-6, 36, 1)
grid = expand.grid(Lat_grid, Lon_grid) %>% data.frame() %>% rename(Lat = Var1, Lon = Var2)

function_dataset = sp_to_fe$sp_fe %>% data.frame() %>% rownames_to_column(var = "Species")

MME_Mortality_grid = MME_Mortality %>% mutate(lon_rounded_0 = floor(Longitude), lon_rounded_1 = ceiling(Longitude), lat_rounded_0 = floor(Latitude), lat_rounded_1 = ceiling(Latitude)) %>% 
  inner_join(function_dataset, by = "Species") %>% rename(., FEs = .)

Polygon       = vector("list", length(MME_Mortality_grid$ID_polygon))
Poly_Coord_df = vector("list", length(MME_Mortality_grid$ID_polygon))
grid_dataset  = vector("list", length(MME_Mortality_grid$ID_polygon))
for (i in 1:length(MME_Mortality_grid$ID_polygon)) {
  Poly_Coord_df[[i]]      = data.frame(lon = c(MME_Mortality_grid$lon_rounded_0[i], MME_Mortality_grid$lon_rounded_1[i]), 
                                  lat = c(MME_Mortality_grid$lat_rounded_0[i], MME_Mortality_grid$lat_rounded_1[i]))
  Polygon[[i]]            = Poly_Coord_df[[i]] %>% sf::st_as_sf(coords = c("lon", "lat")) %>% st_bbox() %>% st_as_sfc()
  grid_dataset[[i]]       = data.frame(MME_Mortality_grid[i,], Polygon[[i]])
}

sfc_combined = st_sfc()
for(i in 1:length(MME_Mortality_grid$ID_polygon)) {
  sfc_combined = rbind(sfc_combined, st_as_sf(Polygon[[i]]))
}

grid_dataset <- st_as_sf(x = MME_Mortality_grid, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
sfc_combined <- st_as_sf(x = sfc_combined, wkt = "x", crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
grid_dataset <- cbind(sfc_combined, grid_dataset) %>% st_as_sf(wkt = "x")
grid_dataset <- grid_dataset %>% st_set_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_transform(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

species_affected <- grid_dataset %>% group_by(x, Species) %>% summarise(occurence = n()) %>% group_by(x) %>% summarise(occurence = n())
FEs_affected     <- grid_dataset %>% group_by(x, FEs) %>% summarise(occurence = n()) %>% group_by(x) %>% summarise(occurence = n())

world <- ne_countries(scale = "medium", returnclass = "sf")
Med_sp <- ggplot(data = world) +
  geom_sf(data = species_affected, shape = 4, aes(fill = occurence), col = "black", size = 5) +
  geom_sf(color = "black", fill = "light grey") +
  scale_fill_gradient(low = "#FFFFD5", high = "#FF0000") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 45), expand = FALSE) +
  ggtitle("Number of taxa affected (spatial resolution 1°)") +
  theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2), panel.background = element_rect(fill = "light blue"))

Med_FE <- ggplot(data = world) +
  geom_sf(data = FEs_affected, shape = 4, aes(fill = occurence), col = "black", size = 5) +
  geom_sf(color = "black", fill = "light grey") +
  scale_fill_gradient(low = "#FFFFD5", high = "#FF0000") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 45), expand = FALSE) +
  ggtitle("Number of FEs affected (spatial resolution 1°)") +
  theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2), panel.background = element_rect(fill = "light blue"))

Med_Viz = Med_sp / Med_FE

