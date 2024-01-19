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
library(reshape2) 
library(ggstream)
library(parallel)
library(cmdstanr)
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

## My color palettes
Fig_1_col  <- c("#f1f292", "#ffdc54", "#ffa654", "#ff8c24", "#ca663a", "#c85250", "#ca3a3a", "#bd0909", "#a20000", "#6f0000")
Fig_2_col  <- c("#ffdc54", "#ff8c24", "#bd0909")
Fig_3a_col <- c("#999999", "#a2cd5a", "#fa8072", "#483d8b", "#cd6839", "#a52a2a", "#daa520", "#cd3278", "#548b54")

## Load models
load("../Models/01_Feeding_model.RData")
load("../Models/02_Longevity_model.RData")
load("../Models/03_Coloniality_model.RData")
load("../Models/04_Morphology_model.RData")
load("../Models/05_Storage_model.RData")
load("../Models/06_Energy_model.RData")
load("../Models/07_Height_model.RData")
load("../Models/08_Growth_model.RData")
load("../Models/09_Calcification_model.RData")
load("../Models/10_Motility_model.RData")

## Loading Scripts – Figure 1 and 2 + 10 BLR 
source("Script_01_Cleaning_data.R")       # 199 lines
source("Script_02_Figure_1.R")            # 008 lines
source("Script_03_Figure_2.R")            # 222 lines
source("Script_04_Figure_3.R")            # 200 lines
source("Script_05_Figure_4.R")            # 170 lines
source("Script_06_Figure_5.R")            # 753 lines
source("Script_07_Bayesian_Models.R")     # 284 lines

### To get the big picture, we added alive species – Figure 3, 4, 5 + Bayesian GLM
source("Script_04_Figure_3_alt.R")        # 306 lines
source("Script_05_Figure_4_alt.R")        # 170 lines
source("Script_06_Figure_5_alt.R")        # 356 lines
source("Script_07_Bayesian_Models_alt.R") # 181 lines

### Total number of lines:                 2849 lines

## Saving Figures
ggsave(Figure_2, filename = "../Figures/raw/Figure_2.png"    , device = "png", width = 18.00, height = 9.50, units = "in", dpi = 300)
ggsave(Figure_3, filename = "../Figures/raw/Figure_3_alt.png", device = "png", width = 18.00, height = 10.0, units = "in", dpi = 300)
ggsave(Figure_4, filename = "../Figures/raw/Figure_4_alt.png", device = "png", width = 18.00, height = 7.00, units = "in", dpi = 300)