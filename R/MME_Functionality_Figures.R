rm(list = ls()) ; options(warn = -1)

## Packages
library(rnaturalearthdata)  
library(rnaturalearth) 
library(circlepackeR)
library(googledrive)
library(hrbrthemes) 
library(kableExtra)
library(tidyverse) 
library(data.tree)
library(ggspatial) 
library(patchwork)
library(networkD3)
library(sfheaders)
library(ggstream)
library(ggradar)
library(mapdata) 
library(leaflet) 
library(viridis) 
library(plotly)
library(readxl) 
library(scales) 
library(rgeos)
library(rgdal)
library(mFD)
library(sf) 

## Functions and shapefiles
`%notin%` <- Negate(`%in%`)
world     <- ne_countries(scale = "medium", returnclass = "sf")

## Download datasets from Google Drive
# drive_auth(email = gargle::gargle_oauth_email(),
#            scopes = "https://www.googleapis.com/auth/drive",
#            cache = gargle::gargle_oauth_cache(),
#            use_oob = gargle::gargle_oob_default())

# drive_download(file = "MED_MME_Review/functional_traits/species_traits",
#                path = "Data/species_traits", overwrite = T, type = "xlsx")
# drive_download(file = "MED_MME_Review/MME-Review data",
#                path = "Data/MME-Review data", overwrite = T, type = "xlsx")

## Load Datasets
### Dataset Functional traits from Nuria Teixido et al.
species_traits <- read_excel("../Data/species_traits.xlsx", 
                             sheet = "4_species_traits_clean.vcomplet", 
                             col_types = c("text", "text", "text", "text", "text", "text",
                                           "text", "text", "text", "text", "text", "text",
                                           "text", "text", "text", "text", "text"))


### Dataset Mortality from Massimo Ponti et al.
MME_Merged_data <- read_excel("../Data/MME-Review data.xlsx", 
                              sheet = "Merged datasets",
                              col_types = c("numeric", "text"   , "date"   , "date"   ,
                                            "numeric", "text"   , "numeric", "text"   ,
                                            "text"   , "text"   , "text"   , "text"   ,
                                            "numeric", "numeric", "numeric", "numeric",
                                            "text"   , "text"   , "text"   , "text"   ,
                                            "numeric", "text"   , "numeric", "text"   ,
                                            "text"   , "text"   , "text"   , "text"   , 
                                            "numeric", "numeric", "numeric", "numeric", 
                                            "text"   , "numeric", "numeric", "text"   , 
                                            "text"   , "text"   , "text"   , "text"   , 
                                            "text"   , "text"   , "text"   , "text"   , 
                                            "text"   , "numeric", "numeric", "numeric", 
                                            "numeric", "text"   , "text"   , "text"   ,
                                            "text"   , "text"   , "text"   , "text"   ,
                                            "text"   , "text"   , "text"   , "text"   ,
                                            "text"   , "text"   , "text"   , "text"   ,
                                            "text"))

## Define the FE number
### Be sure that we have dataset w/ mortality only
MME_Mortality      <- MME_Merged_data %>% drop_na(., damaged_percentatge)

### Look at the species into the dataset
species_MMEs       <- MME_Mortality %>% arrange(species) %>% distinct(species)
species_Fctl       <- species_traits %>% arrange(species) %>% distinct(species)

### Species mismatch
Species_mismatch   <- species_MMEs$species[species_MMEs$species %notin% species_Fctl$species]

### Remove the 4 missing species so far
MME_Mortality      <- MME_Mortality %>% filter(species %notin% Species_mismatch)

### Set up the data frames and define the FEs richness
tr_cat             <- data.frame(trait_name = colnames(species_traits[3:12]),
                                 trait_type = c("N", "N", "N", "N", "N", "N",
                                                "N", "N", "N", "N"),
                                 fuzzy_name = rep(NA, 10))
sp_tr              <- species_traits %>% column_to_rownames("species") %>% dplyr::select(., 2:11)
sp_tr              <- sp_tr %>% dplyr::mutate_all(as.factor)
sp_to_fe           <- mFD::sp.to.fe(sp_tr = sp_tr, tr_cat = tr_cat) 
fe_nm              <- unique(sp_to_fe$fe_nm) 
# length(fe_nm) # 101 FE

### List of species in each FE
fe_sp              <- list() 
for (k in fe_nm) {
  fe_sp[[k]]       <- names(sp_to_fe$sp_fe[which(sp_to_fe$sp_fe == k)]) }

### Trait values of FE
fe_tr              <- sp_to_fe$fe_tr

data_sp_to_fe      <- sp_to_fe$sp_fe %>% data.frame() %>% 
  rownames_to_column(., var = "Species") %>% rename(., FE = .)
fe_tr              <- fe_tr %>% data.frame() %>% rownames_to_column(., var = "FE") 
colnames(fe_tr)    <- c("FE", "Morphology", "Coloniality", "Longevity", "Height", 
                        "Energy", "Feeding", "Growth", "Calcification", "Mobility",
                        "Storage")
table_sp_and_fe    <- inner_join(data_sp_to_fe, fe_tr, by = "FE") %>% 
  mutate(FE = recode(FE, "fe_1" = "fe_01", "fe_2" = "fe_02", "fe_3" = "fe_03", 
                     "fe_4" = "fe_04", "fe_5" = "fe_05", "fe_6" = "fe_06", 
                     "fe_7" = "fe_07", "fe_8" = "fe_08", "fe_9" = "fe_09")) %>% 
  arrange(FE)
table_sp_and_fe_up <- table_sp_and_fe %>% dplyr::filter(FE %in% c("fe_100", "fe_101"))
table_sp_and_fe_dn <- table_sp_and_fe %>% dplyr::filter(FE %notin% c("fe_100", "fe_101"))
table_sp_and_fe    <- rbind(table_sp_and_fe_dn, table_sp_and_fe_up)
table_sp_and_fe_sankey = table_sp_and_fe

table_sp_and_fe$Species[table_sp_and_fe$Species == "Haliclona (Halichoclona) fulva"] =
  "Haliclona fulva"
table_sp_and_fe$Species[table_sp_and_fe$Species == "Hymedesmia (Hymedesmia) paupertas"] =
  "Hymedesmia paupertas"

## Delineate a grid
# quantile(MME_Mortality$latitude) # 31 to 45
# quantile(MME_Mortality$longitude) # -6 to 36
Lat_grid <- seq(31, 45, 1) 
Lon_grid <- seq(-6, 36, 1)
grid     <- expand.grid(Lat_grid, Lon_grid) %>% data.frame() %>% 
  rename(Lat = Var1, Lon = Var2)

## Add FEs information into the global dataset
function_dataset   <- sp_to_fe$sp_fe %>% data.frame() %>% rownames_to_column(var = "species")
MME_Mortality_grid <- MME_Mortality %>% mutate(lon_rounded_0 = floor(longitude),
                                               lon_rounded_1 = ceiling(longitude), 
                                               lat_rounded_0 = floor(latitude), 
                                               lat_rounded_1 = ceiling(latitude)) %>% 
  inner_join(function_dataset, by = "species") %>% rename(., FEs = .)

## Extract the needed information per cell
### Convert the dataset into a sf object – add the cell information for each row
#### Build empty vectors first
Polygon       <- vector("list", length(MME_Mortality_grid$ID_polygon))
Poly_Coord_df <- vector("list", length(MME_Mortality_grid$ID_polygon))
grid_dataset  <- vector("list", length(MME_Mortality_grid$ID_polygon))
sfc_combined  <- st_sfc()

#### Attribute a cell for each dataset row
for (i in 1:length(MME_Mortality_grid$ID_polygon)) {
  Poly_Coord_df[[i]]      <- data.frame(lon = c(MME_Mortality_grid$lon_rounded_0[i],
                                                MME_Mortality_grid$lon_rounded_1[i]), 
                                        lat = c(MME_Mortality_grid$lat_rounded_0[i],
                                                MME_Mortality_grid$lat_rounded_1[i]))
  Polygon[[i]]            <- Poly_Coord_df[[i]] %>% 
    sf::st_as_sf(coords = c("lon", "lat")) %>% st_bbox() %>% st_as_sfc()
  grid_dataset[[i]]       <- data.frame(MME_Mortality_grid[i,], Polygon[[i]]) }

#### Collect all the information into a single vector
for(i in 1:length(MME_Mortality_grid$ID_polygon)) {
  sfc_combined            <- rbind(sfc_combined, st_as_sf(Polygon[[i]])) }

#### Merge dataset with cell (sf object) information into a single document (sf object)
CRS_used = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
grid_dataset <- st_as_sf(x = MME_Mortality_grid, coords = c("longitude", "latitude"), 
                         crs = CRS_used)
sfc_combined <- st_as_sf(x = sfc_combined, 
                         wkt = "x", 
                         crs = CRS_used)
grid_dataset <- cbind(sfc_combined, grid_dataset) %>% st_as_sf(wkt = "x")
grid_dataset <- grid_dataset %>% st_set_crs(CRS_used) %>% st_transform(crs=CRS_used)

## Extract statistics
species_affected <- grid_dataset %>% group_by(x, species) %>% summarise(occurence = n()) %>%
  group_by(x) %>% summarise(`number of taxa` = n())
FEs_affected     <- grid_dataset %>% group_by(x, FEs) %>% summarise(occurence = n()) %>%
  group_by(x) %>% summarise(`number of FEs` = n())
dammaged_cell    <- grid_dataset %>% group_by(x, species) %>% 
  summarise(dammaged = mean(damaged_percentatge)) %>% group_by(x) %>% 
  summarise(`% of damage` = mean(dammaged))

Col_FE_gradient_tot <- c("#ffffff", "#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                         "#ca3a3a", "#bd0909", "#88132d", "#883399", "#5f236b", "#36143d", 
                         "#1b0a1e")

Col_FE_gradient_12  <- c("#ffffff", "#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", 
                         "#ca3a3a", "#bd0909", "#88132d", "#883399", "#5f236b", "#1b0a1e")
                                                                    
FEs_affected = FEs_affected %>% arrange(`number of FEs`) %>% 
  mutate(`FEs impacted` = as.factor(`number of FEs`))

## Split the dataset into categories
grid_dataset$damaged_qualitative[grid_dataset$damaged_qualitative == "moderate"] = "Moderate"
data_ID_Cell       <- data.frame(x = unique(paste(grid_dataset$lon_rounded_0,
                                                  grid_dataset$lon_rounded_1, 
                                                  grid_dataset$lat_rounded_0,
                                                  grid_dataset$lat_rounded_1, sep = "_")),
                                 ID_Cell = paste("Cell_",
                                                 seq(1, length(unique(grid_dataset$x)), 1),
                                                 sep = ""))
grid_dataset$rect  <- paste(grid_dataset$lon_rounded_0, grid_dataset$lon_rounded_1, 
                            grid_dataset$lat_rounded_0, grid_dataset$lat_rounded_1, sep="_")
grid_dataset       <- merge(data_ID_Cell, grid_dataset, by.x = "x", by.y = "rect")
FE_dataset         <- grid_dataset %>% 
  mutate(ID = paste(grid_dataset$ID_Cell, grid_dataset$damaged_qualitative, sep = "_"))

## Play with mortality rates
FE_matrix          <- FE_dataset %>% group_by(ID, FEs) %>% summarise(occurence = n())
FE_matrix          <- reshape2::acast(FE_matrix, ID~FEs, value.var = "occurence")

### Convert in Presence absence
FE_matrix[is.na(FE_matrix)] <- 0
FE_matrix[FE_matrix > 0]    <- 1

### computing Gower distance between FEs
tr_cat[,1] <- colnames(fe_tr)[-1]
colnames(sp_tr) <- colnames(fe_tr)[-1]
fe_tr <- fe_tr %>% column_to_rownames("FE")
fe_fe_dist <- funct.dist(fe_tr, tr_cat[,-3], metric = "gower")
fe_fspaces <- mFD::quality.fspaces(sp_dist = fe_fe_dist)

### Looking for how many axes
# print(round(fe_fspaces$quality_fspaces,3)) # > 6 dimensions
fe_6D_coord <- fe_fspaces$details_fspaces$sp_pc_coord[,1:6]

### Define the number of mortality occurences
habcat = FE_matrix %>% data.frame() %>% mutate_if(is.character,as.numeric) %>% 
  mutate(cat = sub("^[^_]*_", "", sub("^[^_]*_", "", rownames(FE_matrix)))) %>% 
  group_by(cat) %>% 
  summarise_all(sum)
FEs_cat_dataset = t(habcat) %>% data.frame() %>% janitor::row_to_names(row_number = 1) %>% 
  rownames_to_column(var = "FEs") %>% dplyr::select(-"NA")
colnames(FEs_cat_dataset) <- c("FEs", "Low_mor_occ", "Mod_mor_occ", "Sev_mor_occ") 

## Define the survival rates
grid_dataset$survival_rate <- 100 - grid_dataset$damaged_percentatge
FE_dataset         <- grid_dataset %>% 
  mutate(ID = paste(grid_dataset$ID_Cell, grid_dataset$damaged_qualitative, sep = "_"))
FE_matrix          <- FE_dataset %>% group_by(ID, FEs) %>% 
  summarise(survival_rate = mean(survival_rate))
FE_matrix          <- reshape2::acast(FE_matrix, ID~FEs, value.var = "survival_rate")

### Convert in survival rate matrix
FE_matrix[FE_matrix > 0]    <- 0
FE_matrix[is.na(FE_matrix)] <- 1

### Functional statistics
quadrats_multidimFD <- alpha.fd.multidim(sp_faxes_coord = fe_6D_coord, asb_sp_w = FE_matrix, 
                                         ind_vect = c("fdis", "fide", "fspe", "fori"), 
                                         scaling = TRUE, details_returned = TRUE, 
                                         verbose = FALSE)
quadrats_taxo_hill  <- alpha.fd.hill(asb_sp_w = FE_matrix, 
                                     sp_dist = fe_fe_dist, q = c(0,1), tau = "min",
                                     details_returned = FALSE)
colnames(quadrats_taxo_hill) <- c("FE_richness", "FE_shannon")
quadrats_funct_hill <- alpha.fd.hill(asb_sp_w = FE_matrix, sp_dist = fe_fe_dist, q = 1, 
                                     tau = "min", details_returned = FALSE)

### Merging all diversity indices with specific info
quadrats_biodiv     <- data.frame(Nb_sp = 
                                    quadrats_multidimFD$functional_diversity_indices[,1], 
                                  Impact_damage = sub("^[^_]*_", "", 
                                                      sub("^[^_]*_", "", 
                                                          rownames(quadrats_taxo_hill))),
                                  quadrats_taxo_hill, quadrats_funct_hill,
                                  quadrats_multidimFD$functional_diversity_indices[,-1]) 

### Merge PCoA coord with mortality records
pool_coord          <- quadrats_multidimFD$details$sp_faxes_coord %>% data.frame() %>%
  rownames_to_column(var = "FEs")
FEs_cat_dataset     <- merge(FEs_cat_dataset, pool_coord, by = 'FEs')

### Mortality Presence / Absence
Mortality_PA        <- FE_matrix %>% data.frame() %>% mutate_if(is.character,as.numeric) %>% 
  mutate(cat = quadrats_biodiv$Impact_damage) %>% group_by(cat) %>% 
  summarise_all(min)
Mortality_PA        <- t(Mortality_PA) %>% data.frame() %>% 
  janitor::row_to_names(row_number = 1) %>% 
  rownames_to_column(var = "FEs")

## Final dataset hypervolume
FEs_cat_dataset     <- merge(FEs_cat_dataset, Mortality_PA, by = 'FEs')

## Builds the different datasets layers for the figure
### General convex hull
conv_hull_tot = FEs_cat_dataset %>%  slice(chull(PC1, PC2))

### Low damage
FEs_cat_dataset$Low         <- as.numeric(FEs_cat_dataset$Low)
FEs_cat_dataset$Low_mor_occ <- as.numeric(FEs_cat_dataset$Low_mor_occ)
FEs_cat_dataset$Low_mor_log <- log(FEs_cat_dataset$Low_mor_occ + 1)
FEs_cat_dataset_Low_aliv    <- FEs_cat_dataset %>% filter(Low >  0) %>% 
  dplyr::select(-c(Moderate, Severe, Mod_mor_occ, Sev_mor_occ)) 
FEs_cat_dataset_Low_dead    <- FEs_cat_dataset %>% filter(Low == 0) %>% 
  dplyr::select(-c(Moderate, Severe, Mod_mor_occ, Sev_mor_occ))
conv_hull_low               <- FEs_cat_dataset_Low_aliv %>% slice(chull(PC1, PC2))

### Moderate damage
FEs_cat_dataset$Moderate    <- as.numeric(FEs_cat_dataset$Moderate)
FEs_cat_dataset$Mod_mor_occ <- as.numeric(FEs_cat_dataset$Sev_mor_occ)
FEs_cat_dataset$Mod_mor_log <- log(FEs_cat_dataset$Mod_mor_occ + 1)
FEs_cat_dataset_Mod_aliv    <- FEs_cat_dataset %>% filter(Moderate >  0) %>% 
  dplyr::select(-c(Low, Severe, Low_mor_occ, Sev_mor_occ)) 
FEs_cat_dataset_Mod_dead    <- FEs_cat_dataset %>% filter(Moderate == 0) %>% 
  dplyr::select(-c(Low, Severe, Low_mor_occ, Sev_mor_occ))
conv_hull_mod               <- FEs_cat_dataset_Mod_aliv %>% slice(chull(PC1, PC2))

### Moderate damage
FEs_cat_dataset$Severe      <- as.numeric(FEs_cat_dataset$Severe)
FEs_cat_dataset$Sev_mor_occ <- as.numeric(FEs_cat_dataset$Low_mor_occ)
FEs_cat_dataset$Sev_mor_log <- log(FEs_cat_dataset$Sev_mor_occ + 1)
FEs_cat_dataset_Sev_aliv    <- FEs_cat_dataset %>% filter(Severe >  0) %>% 
  dplyr::select(-c(Low, Moderate, Low_mor_occ, Mod_mor_occ)) 
FEs_cat_dataset_Sev_dead    <- FEs_cat_dataset %>% filter(Severe == 0) %>% 
  dplyr::select(-c(Low, Moderate, Low_mor_occ, Mod_mor_occ))
conv_hull_sev               <- FEs_cat_dataset_Sev_aliv %>% slice(chull(PC1, PC2))

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

#### Figure_3

grid_dataset <- grid_dataset %>% 
  dplyr::select(-c(lon_rounded_0, lon_rounded_1, lat_rounded_0, lat_rounded_1, x.y, geometry, x))

# Refine percentage
grid_dataset <- grid_dataset %>% dplyr::filter(damaged_percentatge > 0) # Problem with 0 occurences
grid_dataset$damage_sub_cat <- cut_width(grid_dataset$damaged_percentatge, 10, boundary = 0)

# Refine cumulative – If affected at further level, it'd be affected correspondingly
data_FE_Affected <- table(grid_dataset$damage_sub_cat, grid_dataset$FEs) %>% as.data.frame.matrix() 

{data_FE_Affected[1,]  <- data_FE_Affected[1,] + data_FE_Affected[2,] + data_FE_Affected[3,] +
    data_FE_Affected[4,] + data_FE_Affected[5,] + data_FE_Affected[6,] + data_FE_Affected[7,] +
    data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[2,]  <- data_FE_Affected[2,] + data_FE_Affected[3,] + data_FE_Affected[4,] + 
    data_FE_Affected[5,] + data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + 
    data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[3,]  <- data_FE_Affected[3,] + data_FE_Affected[4,] + data_FE_Affected[5,] + 
    data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + 
    data_FE_Affected[10,]
  data_FE_Affected[4,]  <- data_FE_Affected[4,] + data_FE_Affected[5,] + data_FE_Affected[6,] + 
    data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[5,]  <- data_FE_Affected[5,] + data_FE_Affected[6,] + data_FE_Affected[7,] + 
    data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[6,]  <- data_FE_Affected[6,] + data_FE_Affected[7,] + data_FE_Affected[8,] + 
    data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[7,]  <- data_FE_Affected[7,] + data_FE_Affected[8,] + data_FE_Affected[9,] + 
    data_FE_Affected[10,]
  data_FE_Affected[8,]  <- data_FE_Affected[8,] + data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[9,]  <- data_FE_Affected[9,] + data_FE_Affected[10,]
  data_FE_Affected[10,] <- data_FE_Affected[10,]}

# rowSums(data_FE_Affected)[1] # 1532 occurrences considered

# Convert in P/A
data_FE_Affected_PA = data_FE_Affected
data_FE_Affected_PA[data_FE_Affected > 0] = "affected"
data_FE_Affected_PA[data_FE_Affected == 0] = 1
data_FE_Affected_PA[data_FE_Affected_PA == "affected"] = 0

data_FE_Affected_PA <- t(data_FE_Affected_PA)
data_FE_Affected_PA <- type.convert(data_FE_Affected_PA, as.is = TRUE)

data_FE_Affected <- t(data_FE_Affected)
data_FE_Affected <- type.convert(data_FE_Affected, as.is = TRUE)
data_FE_Affected <- data_FE_Affected %>% data.frame()

# Merge Data_FE_Affected with PCoA coords
data_FE_Affected_PA <- data_FE_Affected_PA %>% data.frame() %>% rownames_to_column("FEs") %>% 
  inner_join((fe_6D_coord %>% data.frame() %>% rownames_to_column("FEs")), by = "FEs")

data_FE_Affected <- data_FE_Affected %>% data.frame() %>% rownames_to_column("FEs") %>% 
  inner_join((fe_6D_coord %>% data.frame() %>% rownames_to_column("FEs")), by = "FEs")

# Plot FE hypervolume space

### Define convex hull
conv_hull_tot = data_FE_Affected_PA %>%  slice(chull(PC1, PC2))
conv_hull_010 = data_FE_Affected_PA %>% filter(X.0.10.   == 1) %>% slice(chull(PC1, PC2))
conv_hull_020 = data_FE_Affected_PA %>% filter(X.10.20.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_030 = data_FE_Affected_PA %>% filter(X.20.30.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_040 = data_FE_Affected_PA %>% filter(X.30.40.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_050 = data_FE_Affected_PA %>% filter(X.40.50.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_060 = data_FE_Affected_PA %>% filter(X.50.60.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_070 = data_FE_Affected_PA %>% filter(X.60.70.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_080 = data_FE_Affected_PA %>% filter(X.70.80.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_090 = data_FE_Affected_PA %>% filter(X.80.90.  == 1) %>% slice(chull(PC1, PC2))
conv_hull_100 = data_FE_Affected_PA %>% filter(X.90.100. == 1) %>% slice(chull(PC1, PC2))

### Define corresponding datasets
dataset_010 = data_FE_Affected_PA %>% filter(X.0.10.   == 1) 
dataset_020 = data_FE_Affected_PA %>% filter(X.10.20.  == 1) 
dataset_030 = data_FE_Affected_PA %>% filter(X.20.30.  == 1) 
dataset_040 = data_FE_Affected_PA %>% filter(X.30.40.  == 1) 
dataset_050 = data_FE_Affected_PA %>% filter(X.40.50.  == 1) 
dataset_060 = data_FE_Affected_PA %>% filter(X.50.60.  == 1)
dataset_070 = data_FE_Affected_PA %>% filter(X.60.70.  == 1) 
dataset_080 = data_FE_Affected_PA %>% filter(X.70.80.  == 1)
dataset_090 = data_FE_Affected_PA %>% filter(X.80.90.  == 1) 
dataset_100 = data_FE_Affected_PA %>% filter(X.90.100. == 1) 

### Volume
VTot = cxhull::cxhull(data_FE_Affected_PA[,12:15] %>% as.matrix())$volume
V30  = round(((cxhull::cxhull(dataset_030[,12:13] %>% as.matrix())$volume / 
                 cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^3) * 100, 2)
V40  = round(((cxhull::cxhull(dataset_040[,12:13] %>% as.matrix())$volume / 
                 cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^3) * 100, 2)
V50  = round((cxhull::cxhull(dataset_050[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V60  = round((cxhull::cxhull(dataset_060[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V70  = round((cxhull::cxhull(dataset_070[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V80  = round((cxhull::cxhull(dataset_080[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V90  = round((cxhull::cxhull(dataset_090[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)
V100 = round((cxhull::cxhull(dataset_100[,12:15] %>% as.matrix())$volume / VTot) * 100, 2)

data_FE_Affected <- table(data_sp_to_fe$FE) %>% data.frame() %>% 
  rename(FEs = Var1, nb_sp_within_FE = Freq) %>% 
  right_join(data_FE_Affected, by = "FEs")

data_FE_Affected <- data_sp_to_fe %>% rename(species = Species) %>%  right_join(species_traits, by = "species") %>% 
  dplyr::select(FE, group) %>% rename(FEs = FE, Taxa = group) %>% 
  right_join(data_FE_Affected, by = "FEs")

data_FE_Affected$Phylum = NA
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("red.algae", "cca")] = "Rhodophyta"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("brown.algae")] = "Seaweeds"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("actinaria", "octocorals", "gorgonians", "corals", "zooanthids")] = "Cnidaria"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("echinoderms")] = "Echinoderms"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("sponges")] = "Porifera"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("tunicates")] = "Chordata"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("bryozoans")] = "Bryozoan"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("bivalves", "gasteropods")] = "Mollusca"
data_FE_Affected$Phylum[data_FE_Affected$Taxa %in% c("seagrass")] = "Tracheophyta"

data_FE_Affected$FEs[data_FE_Affected$Taxa == "red.algae"]

## Base Plot
Base <- ggplot(data = data_FE_Affected, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "white") +
  geom_point(aes(x = PC1, y = PC2, size = nb_sp_within_FE, fill = Phylum), col = "black", shape = 21) +
  theme_minimal() + scale_size_continuous(range = c(1,8), 
                                          breaks = seq(1,10,1),
                                          limits = c(0, 8)) + 
  scale_fill_manual(values = c("gray60", "darkolivegreen3", "salmon", "darkslateblue", "sienna3",
                                 "goldenrod", "violetred3", "brown", "palegreen4")) +
  guides(size = guide_legend(nrow = 1)) + guides(fill = guide_legend(nrow = 3)) + 
  theme(legend.position = "bottom",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  ggtitle("Functional entities hypervolume space")

# Low Damage
Conv_010_020 <- ggplot(data = dataset_030, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#a20000") +
  geom_polygon(data = conv_hull_020, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#a20000", size = 5, shape = 21, alpha = .05) +
  geom_point(data = dataset_020, aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_020)[1], "\n", "V   = ", "0.00%",
                           sep = ""), fill = "#a20000",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[20% – 10%[")

Conv_020_030 <- ggplot(data = dataset_030, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#bd0909") +
  geom_polygon(data = conv_hull_030, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#bd0909", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_030)[1], "\n", "V   = ", "0.12%",
                           sep = ""), fill = "#bd0909",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[30% – 20%[")

# Moderate Damage
Conv_030_040 <- ggplot(data = dataset_040, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ca3a3a") +
  geom_polygon(data = conv_hull_040, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ca3a3a", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) +
  geom_label(label = paste("FE = ", dim(dataset_040)[1], "\n", "V   = ", "0.33%",
                           sep = ""), fill = "#ca3a3a",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[40% – 30%[")

Conv_040_050 <- ggplot(data = dataset_050, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#c85250") +
  geom_polygon(data = conv_hull_050, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#c85250", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_050)[1], "\n", "V   = ", "0.49%",
                           sep = ""), fill = "#c85250",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[50% – 40%[")

Conv_050_060 <- ggplot(data = dataset_060, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ca663a") +
  geom_polygon(data = conv_hull_060, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ca663a", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_060)[1], "\n", "V   = ", "3.59%",
                           sep = ""), fill = "#ca663a",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[60% – 50%[")

# High damage
Conv_060_070 <- ggplot(data = dataset_070, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ff8c24") +
  geom_polygon(data = conv_hull_070, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ff8c24", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_070)[1], "\n", "V   = ", "4.25%",
                           sep = ""), fill = "#ff8c24",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[70% – 60%[")

Conv_070_080 <- ggplot(data = dataset_080, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ffa654") +
  geom_polygon(data = conv_hull_080, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ffa654", size = 5, shape = 21, alpha = .05) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_label(label = paste("FE = ", dim(dataset_080)[1], "\n", "V   = ", "5.79%",
                           sep = ""), fill = "#ffa654",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[80% – 70%[")

Conv_080_090 <- ggplot(data = dataset_090, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#ffdc54") +
  geom_polygon(data = conv_hull_090, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#ffdc54", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  geom_label(label = paste("FE = ", dim(dataset_090)[1], "\n", "V   = ", "13.7%",
                           sep = ""), fill = "#ffdc54",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[90% – 80%[")

Conv_090_100 <- ggplot(data = dataset_100, aes(x = PC1, y = PC2)) +
  geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf), 
            fill = "#deebf7", color = "NA", alpha = 0.5, inherit.aes = F) +
  geom_polygon(data = conv_hull_tot, alpha = .8, col = "black", fill = "#f1f292") +
  geom_polygon(data = conv_hull_100, aes(x = PC1, y = PC2), 
               alpha = .95, col = "black", fill = "white") +
  geom_point(data = data_FE_Affected, aes(x = PC1, y = PC2, group = FEs), 
             col = "black", fill = "#f1f292", size = 5, shape = 21, alpha = .05) +
  geom_point(aes(x = PC1, y = PC2), col = "black", fill = "white", size = 5, shape = 21) +
  theme_minimal() +  
  theme(legend.position = "none",
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1)) + 
  geom_label(label = paste("FE = ", dim(dataset_100)[1], "\n", "V   = ", "27.3%",
                           sep = ""), fill = "#f1f292",
             x = 0.05, y = 0.55, hjust = 0, size = 4) +
  ggtitle("[100% – 90%[")

Figure_3 <- Base + ((Conv_090_100 + Conv_080_090 + Conv_070_080) / (Conv_060_070 + Conv_050_060 + Conv_040_050) /
                      (Conv_030_040 + Conv_020_030 + Conv_010_020))

#### Figure_2
Global_dataset <- table_sp_and_fe_sankey %>% rename(species = Species) %>% data.frame() %>% 
  inner_join(MME_Mortality, by = "species")
data_trait_occurences <- data_FE_Affected %>% 
  mutate(FEs = recode(FEs, "fe_1" = "fe_01", "fe_2" = "fe_02", "fe_3" = "fe_03", 
                      "fe_4" = "fe_04", "fe_5" = "fe_05", "fe_6" = "fe_06", 
                      "fe_7" = "fe_07", "fe_8" = "fe_08", "fe_9" = "fe_09")) %>% 
  arrange(FEs) %>% rename(., `[0-10]` = X.0.10., `]10-20]` = X.10.20., `]20-30]` = X.20.30.,
                          `]30-40]` = X.30.40., `]40-50]` = X.40.50., `]50-60]` = X.50.60.,
                          `]60-70]` = X.60.70., `]70-80]` = X.70.80., `]80-90]` = X.80.90.,
                          `]90-100]` = X.90.100.) %>% 
  dplyr::select(-c(PC1, PC2, PC3, PC4, PC5, PC6))

Traits_Code <- Global_dataset[,2:12] %>% distinct() %>% rename(., FEs = FE)

## Data_traits_occurences
data_trait_occurences <- data_trait_occurences %>% inner_join(Traits_Code, by = "FEs")

## Traits
Morpho_trait <- data_trait_occurences[2:12] %>% group_by(Morphology) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Morphology") %>% t() %>% data.frame() %>% rownames_to_column("group")

Morpho_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Morpho_trait[c(1, 4, 7), 2], 2),
  b = round(Morpho_trait[c(1, 4, 7), 3], 2),
  c = round(Morpho_trait[c(1, 4, 7), 4], 2),
  d = round(Morpho_trait[c(1, 4, 7), 5], 2),
  e = round(Morpho_trait[c(1, 4, 7), 6], 2),
  f = round(Morpho_trait[c(1, 4, 7), 7], 2),
  g = round(Morpho_trait[c(1, 4, 7), 8], 2),
  h = round(Morpho_trait[c(1, 4, 7), 9], 2))
Morpho_trait <- Morpho_trait %>% rownames_to_column("group")

F2_A <- ggradar(Morpho_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Encrusting", "Filamentous", "Foliose", "Cup-like",
                                "Massive-\nencrusting", "massive-\nhemispheric",
                                "Massive-\nerect", "Tree-like"),
                legend.text.size = 10) +
  ggtitle("Morphology") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Coloniality_trait <- data_trait_occurences[,c(2:11, 13)] %>% group_by(Coloniality) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Coloniality") %>% t() %>% data.frame() %>% rownames_to_column("group")

Colon_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = c(0, 0, 0),
  b = c(0, 0, 0),
  c = round(Coloniality_trait[c(1, 4, 7), 2], 2),
  d = c(0, 0, 0),
  e = c(0, 0, 0),
  f = c(0, 0, 0),
  g = round(Coloniality_trait[c(1, 4, 7), 3], 2),
  h = c(0, 0, 0))
Colon_trait <- Colon_trait %>% rownames_to_column("group")

F2_B <- ggradar(Colon_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("", "", "Solitary", "",
                                "", "", "Colonial", ""),
                legend.text.size = 10) +
  ggtitle("Coloniality") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Longevity_trait <- data_trait_occurences[,c(2:11, 14)] %>% group_by(Longevity) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Longevity") %>% t() %>% data.frame() %>% rownames_to_column("group")

Longevity_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Longevity_trait[c(1, 4, 7), 2], 2),
  b = round(Longevity_trait[c(1, 4, 7), 3], 2),
  c = round(Longevity_trait[c(1, 4, 7), 4], 2),
  d = round(Longevity_trait[c(1, 4, 7), 5], 2),
  e = round(Longevity_trait[c(1, 4, 7), 6], 2))
Longevity_trait <- Longevity_trait %>% rownames_to_column("group")

F2_C <- ggradar(Longevity_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("[0; 1] year", "[1; 5[ years", "[5; 10[ years", 
                                "[10; 20[ years",
                                "≥ 20 years"),
                legend.text.size = 10) +
  ggtitle("Longevity") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Height_trait <- data_trait_occurences[,c(2:11, 15)] %>% group_by(Height) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Height") %>% t() %>% data.frame() %>% rownames_to_column("group")

Height_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Height_trait[c(1, 4, 7), 3], 2),
  b = round(Height_trait[c(1, 4, 7), 4], 2),
  c = round(Height_trait[c(1, 4, 7), 5], 2),
  d = round(Height_trait[c(1, 4, 7), 6], 2),
  e = round(Height_trait[c(1, 4, 7), 7], 2))
Height_trait <- Height_trait %>% rownames_to_column("group")

F2_D <- ggradar(Height_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Extreme low", "Low", "Moderate", "High",
                                "Very high"),
                legend.text.size = 10) +
  ggtitle("Height") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Energy_trait <- data_trait_occurences[,c(2:11, 16)] %>% group_by(Energy) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Energy") %>% t() %>% data.frame() %>% rownames_to_column("group")

Energy_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Energy_trait[c(1, 4, 7), 2], 2),
  b = round(Energy_trait[c(1, 4, 7), 3], 2),
  c = round(Energy_trait[c(1, 4, 7), 4], 2))
Energy_trait <- Energy_trait %>% rownames_to_column("group")

F2_E <- ggradar(Energy_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Photosynthetic \nautotroph", "Photo-\nheterotroph", 
                                "Heterotroph"),
                legend.text.size = 10) +
  ggtitle("Energetic resources") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Feeding_trait <- data_trait_occurences[,c(2:11, 17)] %>% group_by(Feeding) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Feeding") %>% t() %>% data.frame() %>% rownames_to_column("group")

Feeding_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Feeding_trait[c(1, 4, 7), 2], 2),
  b = round(Feeding_trait[c(1, 4, 7), 3], 2),
  c = round(Feeding_trait[c(1, 4, 7), 4], 2),
  d = round(Feeding_trait[c(1, 4, 7), 5], 2),
  e = round(Feeding_trait[c(1, 4, 7), 6], 2),
  f = round(Feeding_trait[c(1, 4, 7), 7], 2),
  g = round(Feeding_trait[c(1, 4, 7), 8], 2))
Feeding_trait <- Feeding_trait %>% rownames_to_column("group")

F2_F <- ggradar(Feeding_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Autotrophy", "Filter-feeders \nwith cilliae", 
                                "Filter-feeders \nby pumping", "Passive \nfilter-feeders", 
                                "Grazers", "Carnivores", "Detritivores"),
                legend.text.size = 10) +
  ggtitle("Feeding") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Growth_trait <- data_trait_occurences[,c(2:11, 18)] %>% group_by(Growth) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Growth") %>% t() %>% data.frame() %>% rownames_to_column("group")

Growth_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Growth_trait[c(1, 4, 7), 2], 2),
  b = round(Growth_trait[c(1, 4, 7), 3], 2),
  c = round(Growth_trait[c(1, 4, 7), 4], 2),
  d = round(Growth_trait[c(1, 4, 7), 5], 2),
  e = round(Growth_trait[c(1, 4, 7), 6], 2))
Growth_trait <- Growth_trait %>% rownames_to_column("group")

F2_G <- ggradar(Growth_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Extreme slow", "Slow", "Moderate", "High",
                                "Very high"),
                legend.text.size = 10) +
  ggtitle("Growth") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Calcification_trait <- data_trait_occurences[,c(2:11, 19)] %>% group_by(Calcification) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Calcification") %>% t() %>% data.frame() %>% rownames_to_column("group")

Calcification_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = c(0, 0, 0),
  b = c(0, 0, 0),
  c = round(Calcification_trait[c(1, 4, 7), 2], 2),
  d = c(0, 0, 0),
  e = c(0, 0, 0),
  f = c(0, 0, 0),
  g = round(Calcification_trait[c(1, 4, 7), 3], 2),
  h = c(0, 0, 0))
Calcification_trait <- Calcification_trait %>% rownames_to_column("group")

F2_H <- ggradar(Calcification_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("", "", "Without \ncalcareous \nstructures", "",
                                "", "", "With \ncalcareous \nstructures", ""),
                legend.text.size = 10) +
  ggtitle("Calcification") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Mobility_trait <- data_trait_occurences[,c(2:11, 20)] %>% group_by(Mobility) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Mobility") %>% t() %>% data.frame() %>% rownames_to_column("group")

Mobility_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = c(0, 0, 0),
  b = c(0, 0, 0),
  c = round(Mobility_trait[c(1, 4, 7), 2], 2),
  d = c(0, 0, 0),
  e = c(0, 0, 0),
  f = c(0, 0, 0),
  g = round(Mobility_trait[c(1, 4, 7), 3], 2),
  h = c(0, 0, 0))
Mobility_trait <- Mobility_trait %>% rownames_to_column("group")

F2_I <- ggradar(Mobility_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("", "", "Sessile", "",
                                "", "", "Vagile", ""),
                legend.text.size = 10) +
  ggtitle("Mobility") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Storage_trait <- data_trait_occurences[,c(2:11, 21)] %>% group_by(Storage) %>% 
  summarise_all(sum) %>% mutate_at(vars(`[0-10]`:`]90-100]`), list(~./15.32)) %>% data.frame() %>% 
  column_to_rownames("Storage") %>% t() %>% data.frame() %>% rownames_to_column("group")

Storage_trait = data.frame(
  row.names = c("1. Low", "2. Moderate", "3. High"),
  a = round(Storage_trait[c(1, 4, 7), 2], 2),
  b = round(Storage_trait[c(1, 4, 7), 3], 2),
  c = round(Storage_trait[c(1, 4, 7), 4], 2))
Storage_trait <- Storage_trait %>% rownames_to_column("group")

F2_J <- ggradar(Storage_trait, 
                values.radar = c("0", "50", "100"),
                grid.min = 0, grid.mid = 50, grid.max = 100,
                group.colours = c("#ffdc54", "#ff8c24", "#bd0909"),
                fill=TRUE, group.point.size = 1,
                gridline.mid.colour = "grey",
                legend.position = "bottom",
                axis.label.size = 3,
                axis.labels = c("Storing", "Likely-\nstoring", "Non-\nstoring"),
                legend.text.size = 10) +
  ggtitle("Carbon storage") +
  theme(plot.title = element_text(size = 15, face = "bold"))

Figure_2 = F2_A + F2_B + F2_C + F2_D + F2_E + F2_F + F2_G + F2_H + F2_I + F2_J +
  plot_layout(guides = "collect", ncol = 5) & 
  theme(legend.position = 'bottom') 

fe_6D_coord_df <- fe_6D_coord %>% data.frame() %>% rownames_to_column("FE") %>% 
  mutate(FE = recode(FE, "fe_1" = "fe_01", "fe_2" = "fe_02", "fe_3" = "fe_03", 
                     "fe_4" = "fe_04", "fe_5" = "fe_05", "fe_6" = "fe_06", 
                     "fe_7" = "fe_07", "fe_8" = "fe_08", "fe_9" = "fe_09"))

data_heatmap_FE <- Global_dataset %>% inner_join(fe_6D_coord_df, by = "FE") %>% 
  dplyr::select(FE, year, `damaged_percentatge`, `damaged_qualitative`,
                drivers_abiotic, drivers_abiotic_other, drivers_biotic_group,
                drivers_biotic, drivers_biotic_other, PC1, PC2, PC3, PC4, PC5, PC6) 

## ABIOTIC
### Abiotic subgroups
data_heatmap_FE_abiotic_summ = data_heatmap_FE %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% 
  group_by(year, drivers_abiotic) %>% 
  summarise(n = n()) %>% mutate(ID = paste(year, drivers_abiotic))

data_heatmap_FE_abiotic_summ_seq <- data_heatmap_FE_abiotic_summ %>% distinct(ID) %>% data.frame() %>% 
  mutate(dataset_ID = seq(1,45,1))

data_heatmap_FE_abiotic <- data_heatmap_FE %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% 
  mutate(ID = paste(year, drivers_abiotic)) %>% 
  inner_join(data_heatmap_FE_abiotic_summ_seq) %>% 
  group_by(dataset_ID) %>% 
  group_split()

data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ %>% 
  inner_join(data_heatmap_FE_abiotic_summ_seq) %>% 
  arrange(n) %>% 
  dplyr::filter(n > 4) %>% 
  arrange(dataset_ID) %>% 
  dplyr::select(dataset_ID) %>% 
  as.vector()
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ_sel$dataset_ID

V = c()
for (i in data_heatmap_FE_abiotic_summ_sel) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic[[i]][,4:7] %>% 
                                      as.matrix())$volume / VTot) * 100, 2))}

data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ %>% 
  inner_join(data_heatmap_FE_abiotic_summ_seq) %>% 
  arrange(n) %>% 
  dplyr::filter(n >= 3) %>% 
  dplyr::filter(n <= 4) %>% 
  arrange(dataset_ID) %>% 
  dplyr::select(dataset_ID) %>% 
  as.vector()
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ_sel$dataset_ID

for (i in data_heatmap_FE_abiotic_summ_sel) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_abiotic[[i]][,4:5] %>% 
                                       as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^2) * 100, 2))}

V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))

data_heatmap_FE_abiotic_summ <- data_heatmap_FE_abiotic_summ %>% 
  dplyr::select(year, drivers_abiotic, n) 
data_heatmap_FE_abiotic_summ <- cbind(data_heatmap_FE_abiotic_summ, c(V$., 0))
colnames(data_heatmap_FE_abiotic_summ)[4] <- "Volume"

data_to_fill <- data.frame(year = seq(1986,2020,1))

data_heatmap_FE_abiotic_summ <- data_heatmap_FE_abiotic_summ %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% 
  complete(year, drivers_abiotic) %>% 
  drop_na(drivers_abiotic)

Figure_4A <- ggplot(data_heatmap_FE_abiotic_summ, aes(year, drivers_abiotic, fill= Volume)) + 
  geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', 
                                               palette = "OrYel", 
                                               begin = 0, end = 1,
                                               limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1986,2020,1), name = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### Abiotic group
data_heatmap_FE_abiotic_summ = data_heatmap_FE %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% 
  distinct(., year, FE) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(ID = seq(1, 29,1))

data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% 
  distinct(., year, FE) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(ID = seq(1, 29,1)) %>% 
  dplyr::filter(n > 4)
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ_sel$ID

data_heatmap_FE_abiotic <- data_heatmap_FE %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% 
  distinct(., year, FE) %>% 
  group_by(year) %>% 
  group_split()

V = c()
for (i in data_heatmap_FE_abiotic_summ_sel) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic[[i]][,2:5] %>% distinct() %>% 
                                      as.matrix())$volume / VTot) * 100, 2))}

data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE %>% 
  group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% 
  distinct(., year, FE) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(ID = seq(1, 29,1)) %>% 
  dplyr::filter(n <= 4) %>% 
  dplyr::filter(n >= 3)
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ_sel$ID

for (i in data_heatmap_FE_abiotic_summ_sel[-7]) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_abiotic[[i]][,2:3] %>% distinct() %>% 
                                       as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% 
                                         as.matrix())$volume)^2) * 100, 2))}

V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))

data_heatmap_FE_abiotic_summ <- data_heatmap_FE_abiotic_summ %>% 
  dplyr::select(year, n) 
data_heatmap_FE_abiotic_summ <- cbind(data_heatmap_FE_abiotic_summ, c(V$., 0))
colnames(data_heatmap_FE_abiotic_summ)[3] <- "Volume"

data_heatmap_FE_abiotic_summ <- data_heatmap_FE_abiotic_summ %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% 
  complete(year) %>% 
  mutate(drivers_abiotic = "Overall abiotic pressures")

Figure_4B <- ggplot(data_heatmap_FE_abiotic_summ, aes(year, drivers_abiotic, fill= Volume)) + 
  geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', 
                                               palette = "OrYel", 
                                               begin = 0, end = 1,
                                               limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1986,2020,1), name = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

## BIOTIC
### biotic subgroups
data_heatmap_FE_biotic_summ = data_heatmap_FE %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% 
  group_by(year, drivers_biotic_group) %>% 
  summarise(n = n()) %>% mutate(ID = paste(year, drivers_biotic_group))

data_heatmap_FE_biotic_summ_seq <- data_heatmap_FE_biotic_summ %>% distinct(ID) %>% data.frame() %>% 
  mutate(dataset_ID = seq(1,24,1))

data_heatmap_FE_biotic <- data_heatmap_FE %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% 
  mutate(ID = paste(year, drivers_biotic_group)) %>% 
  inner_join(data_heatmap_FE_biotic_summ_seq) %>% 
  group_by(dataset_ID) %>% 
  group_split()

data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ %>% 
  inner_join(data_heatmap_FE_biotic_summ_seq) %>% 
  arrange(n) %>% 
  dplyr::filter(n > 4) %>% 
  arrange(dataset_ID) %>% 
  dplyr::select(dataset_ID) %>% 
  as.vector()
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ_sel$dataset_ID

V = c()
for (i in data_heatmap_FE_biotic_summ_sel) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_biotic[[i]][,4:7] %>% 
                                      as.matrix())$volume / VTot) * 100, 2))}

data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ %>% 
  inner_join(data_heatmap_FE_biotic_summ_seq) %>% 
  arrange(n) %>% 
  dplyr::filter(n >= 3) %>% 
  dplyr::filter(n <= 4) %>% 
  arrange(dataset_ID) %>% 
  dplyr::select(dataset_ID) %>% 
  as.vector()
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ_sel$dataset_ID

for (i in data_heatmap_FE_biotic_summ_sel) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_biotic[[i]][,4:5] %>% 
                                       as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% as.matrix())$volume)^2) * 100, 2))}

V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))

data_heatmap_FE_biotic_summ <- data_heatmap_FE_biotic_summ %>% 
  dplyr::select(year, drivers_biotic_group, n) 
data_heatmap_FE_biotic_summ <- cbind(data_heatmap_FE_biotic_summ, c(V$., rep(0, 2)))
colnames(data_heatmap_FE_biotic_summ)[4] <- "Volume"

data_heatmap_FE_biotic_summ <- data_heatmap_FE_biotic_summ %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% 
  complete(year, drivers_biotic_group) %>% 
  drop_na(drivers_biotic_group)

Figure_4C <- ggplot(data_heatmap_FE_biotic_summ, aes(year, drivers_biotic_group, fill= Volume)) + 
  geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', 
                                               palette = "OrYel", 
                                               begin = 0, end = 1,
                                               limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1986,2020,1), name = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

### biotic group
data_heatmap_FE_biotic_summ = data_heatmap_FE %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% 
  distinct(., year, FE) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(ID = seq(1, 16, 1))

data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% 
  distinct(., year, FE) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(ID = seq(1, 16,1)) %>% 
  dplyr::filter(n > 4)
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ_sel$ID

data_heatmap_FE_biotic <- data_heatmap_FE %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% 
  distinct(., year, FE) %>% 
  group_by(year) %>% 
  group_split()

V = c()
for (i in data_heatmap_FE_biotic_summ_sel[-3]) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_biotic[[i]][,2:5] %>% distinct() %>% 
                                      as.matrix())$volume / VTot) * 100, 2))}

data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE %>% 
  group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  summarise(n = n()) %>% 
  dplyr::select(., -n) %>% 
  dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% 
  distinct(., year, FE) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(ID = seq(1, 16,1)) %>% 
  dplyr::filter(n <= 4) %>% 
  dplyr::filter(n >= 3)
data_heatmap_FE_biotic_summ_sel <- c(data_heatmap_FE_biotic_summ_sel$ID, 15)

for (i in data_heatmap_FE_biotic_summ_sel) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_biotic[[i]][,2:3] %>% distinct() %>% 
                                       as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% 
                                         as.matrix())$volume)^2) * 100, 2))}

V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))

data_heatmap_FE_biotic_summ <- data_heatmap_FE_biotic_summ %>% 
  dplyr::select(year, n) 
data_heatmap_FE_biotic_summ <- cbind(data_heatmap_FE_biotic_summ, c(V$., 0))
colnames(data_heatmap_FE_biotic_summ)[3] <- "Volume"

data_heatmap_FE_biotic_summ <- data_heatmap_FE_biotic_summ %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% 
  complete(year) %>% 
  mutate(drivers_biotic = "Overall biotic pressures")

Figure_4D <- ggplot(data_heatmap_FE_biotic_summ, aes(year, drivers_biotic, fill= Volume)) + 
  geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', 
                                               palette = "OrYel", 
                                               begin = 0, end = 1,
                                               limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1986,2020,1), name = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

## BIOTIC & ABIOTIC
data_heatmap_FE_all_summ_1 = data_heatmap_FE_biotic %>% bind_rows()
data_heatmap_FE_all_summ_2 = data_heatmap_FE_abiotic %>% bind_rows()
data_heatmap_FE_all_summ   = rbind(data_heatmap_FE_all_summ_1[,-1], 
                                   data_heatmap_FE_all_summ_2[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% 
  group_by(year) %>% 
  summarise(n = n()) %>% 
  mutate(ID = seq(1, 29, 1))

data_heatmap_FE_all_summ_sel <- data_heatmap_FE_all_summ %>% 
  dplyr::filter(n > 4)
data_heatmap_FE_all_summ_sel <- data_heatmap_FE_all_summ_sel$ID

data_heatmap_FE_all <- rbind(data_heatmap_FE_all_summ_1[,-1], 
                             data_heatmap_FE_all_summ_2[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% 
  group_by(year) %>%  
  group_split()

V = c()
for (i in data_heatmap_FE_all_summ_sel) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all[[i]][,3:6] %>% distinct() %>% 
                                      as.matrix())$volume / VTot) * 100, 2))}

data_heatmap_FE_all_summ_sel <- data_heatmap_FE_all_summ %>% 
  dplyr::filter(n <= 4) %>% 
  dplyr::filter(n >= 3)
data_heatmap_FE_all_summ_sel <- c(data_heatmap_FE_all_summ_sel$ID)

for (i in data_heatmap_FE_all_summ_sel) {
  V[i] = sqrt(round(((cxhull::cxhull(data_heatmap_FE_all[[i]][,3:4] %>% distinct() %>% 
                                       as.matrix())$volume / 
                        cxhull::cxhull(data_FE_Affected_PA[,12:13] %>% 
                                         as.matrix())$volume)^2) * 100, 2))}

V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))

data_heatmap_FE_all_summ <- data_heatmap_FE_all_summ %>% 
  dplyr::select(year, n) 
data_heatmap_FE_all_summ <- cbind(data_heatmap_FE_all_summ, V$.)
colnames(data_heatmap_FE_all_summ)[3] <- "Volume"

data_heatmap_FE_all_summ <- data_heatmap_FE_all_summ %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% 
  complete(year) %>% 
  mutate(drivers_all = "Overall biotic & \n abiotic pressures")

Figure_4E <- ggplot(data_heatmap_FE_all_summ, aes(year, drivers_all, fill= Volume)) + 
  geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', 
                                               palette = "OrYel", 
                                               begin = 0, end = 1,
                                               limits = c(0,8)) +
  scale_x_continuous(breaks = seq(1986,2020,1), name = "") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold"))

Figure_4 <- Figure_4A / plot_spacer() / Figure_4B / Figure_4C / plot_spacer() / Figure_4D / Figure_4E + 
  plot_layout(heights = c(5, -1.25, 1, 4, -1.25, 1, 1), guides = 'collect') & 
  theme(legend.position = 'bottom') &
  colorspace::scale_fill_continuous_sequential(na.value = 'white', 
                                               palette = "OrYel", 
                                               begin = 0, end = 1,
                                               limits = c(0,8),
                                               name = "Functional volume affected \n(sqrt transformation) (%)")

ggsave(Figure_3, filename = "Figure_3.png", device = "png", width = 18.00, height = 9.50, units = "in", dpi = 300)
