rm(list = ls()) ; options(warn = -1)

## Packages
library(rnaturalearthdata)  
library(rnaturalearth) 
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
library(ggridges)
library(ggradar)
library(mapdata) 
library(leaflet) 
library(viridis) 
library(plotly)
library(readxl) 
library(scales) 
library(ncdf4) 
library(mFD)
library(sf)

## Functions and shapefiles
`%notin%` <- Negate(`%in%`)
world     <- ne_countries(scale = "medium", returnclass = "sf")
assign_decade <- function(year) {
  if (year >= 1986 & year <= 1991) { return("1986-1990")
  } else if (year > 1991 & year <= 1996) { return("1991-1995")
  } else if (year > 1996 & year <= 2001) { return("1996-2000")
  } else if (year > 2001 & year <= 2006) { return("2001-2005")
  } else if (year > 2006 & year <= 2011) { return("2006-2010")
  } else if (year > 2011 & year <= 2016) { return("2011-2015")
  } else { return("2016-2020")}}

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
source("Script_02_Figure_1.R")            # 161 lines
source("Script_03_Figure_2.R")            # 458 lines
source("Script_04_Figure_3.R")            # 200 lines
source("Script_05_Figure_4.R")            # 170 lines
source("Script_06_Figure_5.R")            # 753 lines
source("Script_07_Bayesian_Models.R")     # 284 lines

### To get the big picture, we added alive species – Figure 3, 4, 5 + Bayesian GLM
source("Script_04_Figure_3_alt.R")        # 306 lines
source("Script_05_Figure_4_alt.R")        # 170 lines
source("Script_06_Figure_5_alt.R")        # 356 lines
source("Script_07_Bayesian_Models_alt.R") # 181 lines

### Total number of lines:                 3238 lines

## Saving Figures
ggsave(Figure_1b, filename = "../Figures/raw/Figure_1b.png", device = "png", width = 18.00, height = 6.50, units = "in", dpi = 300)
ggsave(Figure_2,  filename = "../Figures/raw/Figure_2.png",  device = "png", width = 18.00, height = 9.50, units = "in", dpi = 300)
ggsave(Figure_3,  filename = "../Figures/raw/Figure_3.png",  device = "png", width = 18.00, height = 10.0, units = "in", dpi = 300)
ggsave(Figure_4,  filename = "../Figures/raw/Figure_4.png",  device = "png", width = 18.00, height = 7.00, units = "in", dpi = 300)
ggsave(Figure_S2, filename = "../Figures/raw/Figure_S2.png", device = "png", width = 18.00, height = 8.00, units = "in", dpi = 300)
ggsave(Figure_S4, filename = "../Figures/raw/Figure_S4.png", device = "png", width = 12.00, height = 4.00, units = "in", dpi = 300)