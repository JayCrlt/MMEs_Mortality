## Download datasets from Google Drive
# drive_auth(email = gargle::gargle_oauth_email(), scopes = "https://www.googleapis.com/auth/drive", 
#            cache = gargle::gargle_oauth_cache(), use_oob = gargle::gargle_oob_default())
# drive_download(file = "MED_MME_Review/functional_traits/species_traits", path = "Data/species_traits", overwrite = T, type = "xlsx")
# drive_download(file = "MED_MME_Review/MME-Review data", path = "Data/MME-Review data", overwrite = T, type = "xlsx")

## Load Datasets
### Dataset Functional traits from Nuria Teixido et al.
species_traits <- read_excel("../Data/R/species_traits.xlsx", 
                             sheet = "4_species_traits_clean.vcomplet", 
                             col_types = c("text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", "text",
                                           "text", "text", "text", "text", "text"))

### Dataset Mortality from Massimo Ponti et al.
MME_Merged_data <- read_excel("../Data/R/MME-Review data.xlsx", 
                              sheet = "Merged datasets",
                              col_types = c("numeric", "text"   , "date"   , "date"   , "numeric", "text"   , "numeric", "text"   ,
                                            "text"   , "text"   , "text"   , "text"   , "numeric", "numeric", "numeric", "numeric",
                                            "text"   , "text"   , "text"   , "text"   , "numeric", "text"   , "numeric", "text"   ,
                                            "text"   , "text"   , "text"   , "text"   , "numeric", "numeric", "numeric", "numeric", 
                                            "text"   , "numeric", "numeric", "text"   , "text"   , "text"   , "text"   , "text"   , 
                                            "text"   , "text"   , "text"   , "text"   , "text"   , "numeric", "numeric", "numeric", 
                                            "numeric", "text"   , "text"   , "text"   , "text"   , "text"   , "text"   , "text"   ,
                                            "text"   , "text"   , "text"   , "text"   , "text"   , "text"   , "text"   , "text"   , "text"))

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
tr_cat             <- data.frame(trait_name = colnames(species_traits[3:12]), trait_type = c("N", "N", "N", "N", "N", "N", "N", "N", "N", "N"),
                                 fuzzy_name = rep(NA, 10))
sp_tr              <- species_traits %>% column_to_rownames("species") %>% dplyr::select(., 2:11)
sp_tr              <- sp_tr %>% dplyr::mutate_all(as.factor)
sp_to_fe           <- mFD::sp.to.fe(sp_tr = sp_tr, tr_cat = tr_cat) 
fe_nm              <- unique(sp_to_fe$fe_nm) 

### List of species in each FE
fe_sp              <- list() 
for (k in fe_nm) {
  fe_sp[[k]]       <- names(sp_to_fe$sp_fe[which(sp_to_fe$sp_fe == k)]) }
### Trait values of FE
fe_tr              <- sp_to_fe$fe_tr
data_sp_to_fe      <- sp_to_fe$sp_fe %>% data.frame() %>% rownames_to_column(., var = "Species") %>% rename(., FE = .)
fe_tr              <- fe_tr %>% data.frame() %>% rownames_to_column(., var = "FE") 
colnames(fe_tr)    <- c("FE", "Morphology", "Coloniality", "Longevity", "Height", "Energy", "Feeding", "Growth", "Calcification", "Mobility", "Storage")
table_sp_and_fe    <- inner_join(data_sp_to_fe, fe_tr, by = "FE") %>% 
  mutate(FE = recode(FE, "fe_1" = "fe_01", "fe_2" = "fe_02", "fe_3" = "fe_03", "fe_4" = "fe_04", "fe_5" = "fe_05", 
                     "fe_6" = "fe_06", "fe_7" = "fe_07", "fe_8" = "fe_08", "fe_9" = "fe_09")) %>% arrange(FE)
table_sp_and_fe_up <- table_sp_and_fe %>% dplyr::filter(FE %in% c("fe_100", "fe_101"))
table_sp_and_fe_dn <- table_sp_and_fe %>% dplyr::filter(FE %notin% c("fe_100", "fe_101"))
table_sp_and_fe    <- rbind(table_sp_and_fe_dn, table_sp_and_fe_up)
table_sp_and_fe_sankey = table_sp_and_fe

table_sp_and_fe$Species[table_sp_and_fe$Species == "Haliclona (Halichoclona) fulva"] = "Haliclona fulva"
table_sp_and_fe$Species[table_sp_and_fe$Species == "Hymedesmia (Hymedesmia) paupertas"] = "Hymedesmia paupertas"

## Delineate a grid
grid               <- expand.grid(Lat = seq(31, 45, 1), Long = seq(-6, 36, 1)) %>% data.frame() 

## Add FEs information into the global dataset
function_dataset   <- sp_to_fe$sp_fe %>% data.frame() %>% rownames_to_column(var = "species")
MME_Mortality_grid <- MME_Mortality %>% mutate(lon_rounded_0 = floor(longitude), lon_rounded_1 = ceiling(longitude), 
                                               lat_rounded_0 = floor(latitude), lat_rounded_1 = ceiling(latitude)) %>% 
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
  Poly_Coord_df[[i]]      <- data.frame(lon = c(MME_Mortality_grid$lon_rounded_0[i], MME_Mortality_grid$lon_rounded_1[i]), 
                                        lat = c(MME_Mortality_grid$lat_rounded_0[i], MME_Mortality_grid$lat_rounded_1[i]))
  Polygon[[i]]            <- Poly_Coord_df[[i]] %>% sf::st_as_sf(coords = c("lon", "lat")) %>% st_bbox() %>% st_as_sfc()
  grid_dataset[[i]]       <- data.frame(MME_Mortality_grid[i,], Polygon[[i]]) }

#### Collect all the information into a single vector
for(i in 1:length(MME_Mortality_grid$ID_polygon)) {
  sfc_combined            <- rbind(sfc_combined, st_as_sf(Polygon[[i]])) }

#### Merge dataset with cell (sf object) information into a single document (sf object)
CRS_used = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
grid_dataset <- st_as_sf(x = MME_Mortality_grid, coords = c("longitude", "latitude"), crs = CRS_used)
sfc_combined <- st_as_sf(x = sfc_combined, wkt = "x", crs = CRS_used)
grid_dataset <- cbind(sfc_combined, grid_dataset) %>% st_as_sf(wkt = "x") %>% st_set_crs(CRS_used) %>% st_transform(crs = CRS_used)

## Extract statistics
species_affected  <- grid_dataset %>% group_by(x, species) %>% summarise(occurence = n()) %>% group_by(x) %>% summarise(`number of taxa` = n())
FEs_affected      <- grid_dataset %>% group_by(x, FEs) %>% summarise(occurence = n()) %>% group_by(x) %>% summarise(`number of FEs` = n()) %>% 
  arrange(`number of FEs`) %>% mutate(`FEs impacted` = as.factor(`number of FEs`))
dammaged_cell     <- grid_dataset %>% group_by(x, species) %>%  summarise(dammaged = mean(damaged_percentatge)) %>% group_by(x) %>% 
  summarise(`% of damage` = mean(dammaged))

## Split the dataset into categories
grid_dataset$damaged_qualitative[grid_dataset$damaged_qualitative == "moderate"] = "Moderate"
data_ID_Cell      <- data.frame(x = unique(paste(grid_dataset$lon_rounded_0, grid_dataset$lon_rounded_1, 
                                                  grid_dataset$lat_rounded_0, grid_dataset$lat_rounded_1, sep = "_")),
                                 ID_Cell = paste("Cell_", seq(1, length(unique(grid_dataset$x)), 1), sep = ""))
grid_dataset$rect <- paste(grid_dataset$lon_rounded_0, grid_dataset$lon_rounded_1, grid_dataset$lat_rounded_0, grid_dataset$lat_rounded_1, sep="_")
grid_dataset      <- merge(data_ID_Cell, grid_dataset, by.x = "x", by.y = "rect")
FE_dataset        <- grid_dataset %>% mutate(ID = paste(grid_dataset$ID_Cell, grid_dataset$damaged_qualitative, sep = "_"))

## Play with mortality rates
FE_matrix         <- FE_dataset %>% group_by(ID, FEs) %>% summarise(occurence = n()) %>% reshape2::acast(., ID~FEs, value.var = "occurence")

### Convert in Presence absence
FE_matrix[is.na(FE_matrix)] <- 0 ; FE_matrix[FE_matrix > 0] <- 1

### computing Gower distance between FEs
tr_cat[,1] <- colnames(fe_tr)[-1] ; colnames(sp_tr) <- colnames(fe_tr)[-1]
fe_tr <- fe_tr %>% column_to_rownames("FE")
fe_fe_dist <- funct.dist(fe_tr, tr_cat[,-3], metric = "gower")
fe_fspaces <- mFD::quality.fspaces(sp_dist = fe_fe_dist)

### Looking for how many axes
# print(round(fe_fspaces$quality_fspaces,3)) # > 6 dimensions
fe_6D_coord <- fe_fspaces$details_fspaces$sp_pc_coord[,1:6]

### Define the number of mortality occurences
habcat = FE_matrix %>% data.frame() %>% mutate_if(is.character,as.numeric) %>% mutate(cat = sub("^[^_]*_", "", sub("^[^_]*_", "", rownames(FE_matrix)))) %>% 
  group_by(cat) %>% summarise_all(sum)
FEs_cat_dataset = t(habcat) %>% data.frame() %>% janitor::row_to_names(row_number = 1) %>% rownames_to_column(var = "FEs") %>% dplyr::select(-"NA")
colnames(FEs_cat_dataset) <- c("FEs", "Low_mor_occ", "Mod_mor_occ", "Sev_mor_occ") 

## Define the survival rates
grid_dataset$survival_rate <- 100 - grid_dataset$damaged_percentatge
FE_dataset         <- grid_dataset %>% mutate(ID = paste(grid_dataset$ID_Cell, grid_dataset$damaged_qualitative, sep = "_"))
FE_matrix          <- FE_dataset %>% group_by(ID, FEs) %>% summarise(survival_rate = mean(survival_rate)) %>% 
  reshape2::acast(., ID~FEs, value.var = "survival_rate")

### Convert in survival rate matrix
FE_matrix[FE_matrix > 0] <- 0 ; FE_matrix[is.na(FE_matrix)] <- 1

### Functional statistics
quadrats_multidimFD          <- alpha.fd.multidim(sp_faxes_coord = fe_6D_coord, asb_sp_w = FE_matrix, ind_vect = c("fdis", "fide", "fspe", "fori"), 
                                                  scaling = TRUE, details_returned = TRUE, verbose = FALSE)
quadrats_taxo_hill           <- alpha.fd.hill(asb_sp_w = FE_matrix, sp_dist = fe_fe_dist, q = c(0,1), tau = "min", details_returned = FALSE)
colnames(quadrats_taxo_hill) <- c("FE_richness", "FE_shannon")
quadrats_funct_hill          <- alpha.fd.hill(asb_sp_w = FE_matrix, sp_dist = fe_fe_dist, q = 1, tau = "min", details_returned = FALSE)

### Merging all diversity indices with specific info
quadrats_biodiv     <- data.frame(Nb_sp = quadrats_multidimFD$functional_diversity_indices[,1], 
                                  Impact_damage = sub("^[^_]*_", "", sub("^[^_]*_", "", rownames(quadrats_taxo_hill))),
                                  quadrats_taxo_hill, quadrats_funct_hill, quadrats_multidimFD$functional_diversity_indices[,-1]) 

### Merge PCoA coord with mortality records
pool_coord          <- quadrats_multidimFD$details$sp_faxes_coord %>% data.frame() %>%rownames_to_column(var = "FEs")
FEs_cat_dataset     <- merge(FEs_cat_dataset, pool_coord, by = 'FEs')

### Mortality Presence / Absence
Mortality_PA        <- FE_matrix %>% data.frame() %>% mutate_if(is.character,as.numeric) %>% mutate(cat = quadrats_biodiv$Impact_damage) %>% group_by(cat) %>% 
  summarise_all(min)
Mortality_PA        <- t(Mortality_PA) %>% data.frame() %>% janitor::row_to_names(row_number = 1) %>% rownames_to_column(var = "FEs")

## Final dataset hypervolume
FEs_cat_dataset     <- merge(FEs_cat_dataset, Mortality_PA, by = 'FEs')

## Builds the different datasets layers for the figure
### General convex hull
conv_hull_tot = FEs_cat_dataset %>%  slice(chull(PC1, PC2))

### Low damage
FEs_cat_dataset$Low         <- as.numeric(FEs_cat_dataset$Low)
FEs_cat_dataset$Low_mor_occ <- as.numeric(FEs_cat_dataset$Low_mor_occ)
FEs_cat_dataset$Low_mor_log <- log(FEs_cat_dataset$Low_mor_occ + 1)
FEs_cat_dataset_Low_aliv    <- FEs_cat_dataset %>% filter(Low >  0) %>% dplyr::select(-c(Moderate, Severe, Mod_mor_occ, Sev_mor_occ)) 
FEs_cat_dataset_Low_dead    <- FEs_cat_dataset %>% filter(Low == 0) %>% dplyr::select(-c(Moderate, Severe, Mod_mor_occ, Sev_mor_occ))
conv_hull_low               <- FEs_cat_dataset_Low_aliv %>% slice(chull(PC1, PC2))

### Moderate damage
FEs_cat_dataset$Moderate    <- as.numeric(FEs_cat_dataset$Moderate)
FEs_cat_dataset$Mod_mor_occ <- as.numeric(FEs_cat_dataset$Sev_mor_occ)
FEs_cat_dataset$Mod_mor_log <- log(FEs_cat_dataset$Mod_mor_occ + 1)
FEs_cat_dataset_Mod_aliv    <- FEs_cat_dataset %>% filter(Moderate >  0) %>% dplyr::select(-c(Low, Severe, Low_mor_occ, Sev_mor_occ)) 
FEs_cat_dataset_Mod_dead    <- FEs_cat_dataset %>% filter(Moderate == 0) %>% dplyr::select(-c(Low, Severe, Low_mor_occ, Sev_mor_occ))
conv_hull_mod               <- FEs_cat_dataset_Mod_aliv %>% slice(chull(PC1, PC2))

### Moderate damage
FEs_cat_dataset$Severe      <- as.numeric(FEs_cat_dataset$Severe)
FEs_cat_dataset$Sev_mor_occ <- as.numeric(FEs_cat_dataset$Low_mor_occ)
FEs_cat_dataset$Sev_mor_log <- log(FEs_cat_dataset$Sev_mor_occ + 1)
FEs_cat_dataset_Sev_aliv    <- FEs_cat_dataset %>% filter(Severe >  0) %>% dplyr::select(-c(Low, Moderate, Low_mor_occ, Mod_mor_occ)) 
FEs_cat_dataset_Sev_dead    <- FEs_cat_dataset %>% filter(Severe == 0) %>% dplyr::select(-c(Low, Moderate, Low_mor_occ, Mod_mor_occ))
conv_hull_sev               <- FEs_cat_dataset_Sev_aliv %>% slice(chull(PC1, PC2))