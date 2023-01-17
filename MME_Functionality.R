rm(list = ls()) ; options(warn = -1)

# 0️⃣ Set the environment 💻 ----

## Packages
library(readxl) ; library(tidyverse) ; library(viridis) ; library(hrbrthemes) ; library(rgdal) 
library(mapdata) ; library(leaflet) ; library(sf) ; library(rnaturalearth) ; library(rgeos)
library(rnaturalearthdata)  ; library(ggspatial) ; library(scales) ; library(patchwork)

## Functions and shp.
`%notin%` <- Negate(`%in%`)
world     <- ne_countries(scale = "medium", returnclass = "sf")

## Load Datasets
### Dataset Mortality from Massimo Ponti et al.
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

### Dataset Functional traits from Nuria Teixido et al.
species_traits <- read_excel("Data/species_traits.xlsx", 
                             sheet = "4_species_traits_clean.v2", 
                             col_types = c("text", "text", "text", "text", "text", "text", "text", "text", 
                                           "text", "text", "text", "text", "text", "text", "text", "text"))


### Dataset Mortality from Massimo Ponti et al.
MME_Merged_data <- read_excel("Data/MME-Review data.xlsx", 
                              sheet = "Merged datasets",
                              col_types = c("numeric", "text", "date", "date", "numeric", "text", "text", 
                                            "text", "text", "text", "text", "numeric", "numeric", "numeric", 
                                            "numeric", "text", "text", "text", "text", "numeric", "text", 
                                            "numeric", "text", "text", "text", "text", "text", "numeric", 
                                            "numeric", "numeric", "numeric", "text", "numeric", "numeric"))

# 1️⃣ Unravel the missing species to code 🐚 ----

## Extract species
all_species         <- unique(MME_Merged_data$species)
species_subset      <- unique(species_traits$species)

## Dataset w/ missing species
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

## Filtering only benthic species
data_species_to_add_w_sp_ID     <- data_species_to_add %>% filter(Keep_or_rmv == "Y", ID_sp_lvl == "Y") %>% 
  select(species) %>% as.vector()
data_species_to_add_additional  <- data_species_to_add %>% filter(Keep_or_rmv == "Y", ID_sp_lvl == "N") %>% 
  select(species) %>% as.vector()
data_species_to_add_w_sp_ID$species[data_species_to_add_w_sp_ID$species == "Epizoanthus  arenaceus"] = 
  "Epizoanthus arenaceus"

## Missing Species
(data_species_to_add_w_sp_ID    <- data_species_to_add_w_sp_ID$species)
(data_species_to_add_additional <- data_species_to_add_additional$species)

## Write down all missing species into a xlsx file
data_xlsx_species_to_add        <- c(data_species_to_add_w_sp_ID, data_species_to_add_additional)
xlsx::write.xlsx(data_xlsx_species_to_add, file = "Data/data_xlsx_species_to_add.xlsx")

# 2️⃣ Visualize the damaged sites 💥 ----
## Set the leaflet parameters
mybins    <- c(1,5,10,25,50,75,80,90,100)
mypalette <- colorBin(palette="YlOrBr", 
                     domain=unique(MME_Mortality$`Damaged percentage`), 
                     na.color="transparent", 
                     bins=mybins)

## First viz all species
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

#### Huge effort w/ Pinna nobilis
## Second viz without P. nobilis
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

# 3️⃣ Functioning richness definition 🌿 ----

## Define the FE number
### Be sure that we have dataset w/ mortality only
MME_Mortality      <- MME_Mortality %>% drop_na(., `Damaged percentage`)

### Look at the species into the dataset
species_MMEs       <- MME_Mortality %>% arrange(Species) %>% distinct(Species)
species_Fctl       <- species_traits %>% arrange(species) %>% distinct(species)

### Species mismatch
Species_mismatch   <- species_MMEs$Species[species_MMEs$Species %notin% species_Fctl$species]

### Remove the 4 missing species so far
MME_Mortality      <- MME_Mortality %>% filter(Species %notin% Species_mismatch)

### Set up the data frames and define the FEs richness
tr_cat             <- data.frame(trait_name = colnames(species_traits[2:12]),
                                 trait_type = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N"),
                                 fuzzy_name = rep(NA, 11))
sp_tr              <- species_traits %>% column_to_rownames("species") %>% dplyr::select(., 1:11)
sp_tr              <- sp_tr %>% dplyr::mutate_all(as.factor)
sp_to_fe           <- mFD::sp.to.fe(sp_tr = sp_tr, tr_cat = tr_cat) 
fe_nm              <- unique(sp_to_fe$fe_nm) ; length(fe_nm) # 58 FE

### List of species in each FE
fe_sp              <- list() 
for (k in fe_nm) {
  fe_sp[[k]]       <- names(sp_to_fe$sp_fe[which(sp_to_fe$sp_fe == k)]) }

### Trait values of FE
fe_tr              <- sp_to_fe$fe_tr

# 4️⃣ Visualize with a 1° resolution grid 🗺 ----

## Delineate a grid
quantile(MME_Mortality$Latitude) # 31 to 45
quantile(MME_Mortality$Longitude) # -6 to 36
Lat_grid <- seq(31, 45, 1) 
Lon_grid <- seq(-6, 36, 1)
grid     <- expand.grid(Lat_grid, Lon_grid) %>% data.frame() %>% rename(Lat = Var1, Lon = Var2)

## Add FEs information into the global dataset
function_dataset   <- sp_to_fe$sp_fe %>% data.frame() %>% rownames_to_column(var = "Species")
MME_Mortality_grid <- MME_Mortality %>% mutate(lon_rounded_0 = floor(Longitude), lon_rounded_1 = ceiling(Longitude), lat_rounded_0 = floor(Latitude), lat_rounded_1 = ceiling(Latitude)) %>% 
  inner_join(function_dataset, by = "Species") %>% rename(., FEs = .)

## Extract the needed information per cell
### Convert the dataset into a sf object – add the cell information for each row
#### Build empty vectors first
Polygon       <- vector("list", length(MME_Mortality_grid$ID_polygon))
Poly_Coord_df <- vector("list", length(MME_Mortality_grid$ID_polygon))
grid_dataset  <- vector("list", length(MME_Mortality_grid$ID_polygon))
sfc_combined  <- st_sfc()

#### Attribute a cell for each dataset row
for (i in 1:length(MME_Mortality_grid$ID_polygon)) {
  Poly_Coord_df[[i]]      <- data.frame(lon = c(MME_Mortality_grid$lon_rounded_0[i], MME_Mortality_grid$lon_rounded_1[i]), 
                                  lat = c(MME_Mortality_grid$lat_rounded_0[i], MME_Mortality_grid$lat_rounded_1[i]))
  Polygon[[i]]            <- Poly_Coord_df[[i]] %>% sf::st_as_sf(coords = c("lon", "lat")) %>% st_bbox() %>% st_as_sfc()
  grid_dataset[[i]]       <- data.frame(MME_Mortality_grid[i,], Polygon[[i]]) }

#### Collect all the information into a single vector
for(i in 1:length(MME_Mortality_grid$ID_polygon)) {
  sfc_combined            <- rbind(sfc_combined, st_as_sf(Polygon[[i]])) }

#### Merge dataset with cell (sf object) information into a single document (sf object)
grid_dataset <- st_as_sf(x = MME_Mortality_grid, coords = c("Longitude", "Latitude"), crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
sfc_combined <- st_as_sf(x = sfc_combined, wkt = "x", crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
grid_dataset <- cbind(sfc_combined, grid_dataset) %>% st_as_sf(wkt = "x")
grid_dataset <- grid_dataset %>% st_set_crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") %>% 
  st_transform(crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

## Extract statistics
species_affected <- grid_dataset %>% group_by(x, Species) %>% summarise(occurence = n()) %>% group_by(x) %>% summarise(`number of taxa` = n())
FEs_affected     <- grid_dataset %>% group_by(x, FEs) %>% summarise(occurence = n()) %>% group_by(x) %>% summarise(`number of FEs` = n())
dammaged_cell    <- grid_dataset %>% group_by(x, Species) %>% summarise(dammaged = mean(Damaged.percentage)) %>% group_by(x) %>% summarise(`% of dammage` = mean(dammaged))

## Build the maps with 1° cells
### Number of taxas affected
Med_spp <- ggplot(data = world) +
  geom_sf(data = species_affected, shape = 4, aes(fill = `number of taxa`), col = "black", size = 5) +
  geom_sf(color = "black", fill = "light grey") +
  scale_fill_gradient(low = "white", high = "red") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 45), expand = FALSE) +
  ggtitle("Number of taxa affected") +
  theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2), panel.background = element_rect(fill = "light blue"))

### Number of FEs affected
Med_FEs <- ggplot(data = world) +
  geom_sf(data = FEs_affected, shape = 4, aes(fill = `number of FEs`), col = "black", size = 5) +
  geom_sf(color = "black", fill = "light grey") +
  scale_fill_gradient(low = "white", high = "red") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 45), expand = FALSE) +
  ggtitle("Number of FEs affected") +
  theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2), panel.background = element_rect(fill = "light blue"))

### % of dammaged
Med_dam <- ggplot(data = world) +
  geom_sf(data = dammaged_cell, shape = 4, aes(fill = `% of dammage`), col = "black", size = 5) +
  geom_sf(color = "black", fill = "light grey") +
  scale_fill_gradient(low = "white", high = "red") +
  xlab("Longitude") + ylab("Latitude") +
  coord_sf(xlim = c(-10, 40), ylim = c(30, 45), expand = FALSE) +
  ggtitle("Percentage of dammage") +
  theme(panel.grid.major = element_line(color = gray(.25), linetype = "blank", size = 0.2), panel.background = element_rect(fill = "light blue"))

## Global map
Figure_3 = Med_dam / Med_spp / Med_FEs

# 5️⃣ Functional hypervolume 🛑 ----

# 6️⃣ Figures 📊 ----

Figure_1 # Repartition of the data points all species + dammaged information
Figure_2 # Repartition of the data points without P. nobilis + dammaged information
Figure_3 # Combined gridded maps (1°x1°) – % of dammmaged, number of taxa affected, number of FEs affected
