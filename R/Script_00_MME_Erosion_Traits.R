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
load("../Models/00_General_model.RData")
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
load("../Models/11_Stressors_model.RData")

## Loading Scripts
source("Script_01_Cleaning_data.R")   # 199 lines
source("Script_02_Figure_1.R")        # 008 lines
source("Script_03_Figure_2.R")        # 222 lines
source("Script_04_Figure_3.R")        # 175 lines
source("Script_05_Figure_4.R")        # 170 lines
source("Script_06_Figure_5.R")        # 743 lines
source("Script_07_Bayesian_Models.R") # 321 lines

## Saving Figures
# ggsave(Figure_2, filename = "../Figures/Figure_2.png", device = "png", width = 18.00, height = 9.50, units = "in", dpi = 300)
# ggsave(Figure_3, filename = "../Figures/Figure_3.png", device = "png", width = 18.00, height = 9.50, units = "in", dpi = 300)
# ggsave(Figure_4, filename = "../Figures/Figure_4.png", device = "png", width = 18.00, height = 7.00, units = "in", dpi = 300)
# ggsave(WT_Panel_0, filename = "../Figures/WESTERN.png", device = "png", width = 10, height = 8, units = "in", dpi = 300)
# ggsave(CT_Panel_0, filename = "../Figures/CENTRAL.png", device = "png", width = 10, height = 8, units = "in", dpi = 300)
# ggsave(ES_Panel_0, filename = "../Figures/Eastern.png", device = "png", width = 10, height = 8, units = "in", dpi = 300)
# ggsave(Figure_5C_WT, filename = "../Figures/Figure_5C_WT.png", device = "png", width = 10, height = 3.5, units = "in", dpi = 300)
# ggsave(Figure_5C_CT, filename = "../Figures/Figure_5C_CT.png", device = "png", width = 10, height = 3.5, units = "in", dpi = 300)
# ggsave(Figure_5C_ES, filename = "../Figures/Figure_5C_ES.png", device = "png", width = 10, height = 3.5, units = "in", dpi = 300)