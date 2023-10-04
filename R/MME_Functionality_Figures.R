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

## Loading Scripts
source("Loading_and_cleaning_dataset.R")
source("Figure_1.R")
source("Figure_3.R")
source("Figure_2.R")
source("Figure_4.R")
source("Figure_5.R")

## Saving Figures
