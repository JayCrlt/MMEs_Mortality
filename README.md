# Erosion of trait diversity across the Mediterranean Sea following mortality events

![GitHub](https://img.shields.io/badge/GitHub-39457E?style=for-the-badge&logo=github&logoColor=white)
![Gitlab](https://img.shields.io/badge/GitLab-FFA500?style=for-the-badge&logo=gitlab&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=RStudio&logoColor=white)

#### J. Carlot, C. Galobart, D. Gómez-Gras, J. Santamaria, R. Golo, M. Sini, E. Cebrian, V. Gerovasileiou,  M. Ponti, E. Turicchia, S. Comeau, G. Rilov, L. Tamburello, T. Pulido Mantas, C. Cerrano, J.B. Ledoux, J.P. Gattuso, S. Ramirez-Calero, L. Millán Agudo,  M. Montefalcone, S. Katsanevakis, N. Bensoussan, J. Garrabou and N. Teixidó

### Abstract
Unraveling the functional pathways of marine ecosystems in the face of global change poses a pressing challenge. This is particularly critical in the Mediterranean Sea, which is one of the most impacted marine basins by human activities. Utilizing species traits and extensive mass mortality events (MMEs) datasets spanning from 1986 to 2020, we investigated the global traits of benthic species that suffered from MMEs due to nine different mortality drivers. By analyzing variation in ten ecological traits of 410 benthic species using the largest dataset of Mediterranean benthic species ever compiled, we identified 236 functional entities (FEs, defined as groups of species sharing the same trait values) and assessed that 56 FEs were impaired. Our results unveiled a risk of trait homogenization, with low trait redundancy, and higher vulnerability in specific trait categories. Notably, 54% of mortality records showed severe impacts, including particularly tree-like and massive forms, slow growth rates, calcifying species and large individuals. Specifically, 29 FEs suffered extreme mortality (higher than 90%), impacting in turn 18.4% of the species trait volume. Besides, we also highlight that 10.9% of the trait volume is impaired over the last five years across the Mediterranean, showcasing the risk of a rapid ecological transformation of the Mediterranean Sea.

Scripts and data for Erosion of trait diversity across the Mediterranean Sea following mortality events 
This Github repository is structured as follows:

- :file_folder: [``Data``](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Data) contains the following sub-folders
- :file_folder: [``Figures``](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Figures) which hosts the figures built with this script
- :file_folder: [``Models``](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Models) which hosts the models built with this script
- :file_folder: [``R``](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Data/R) contains the current script and R project related to 

:warning: You might run only the script [``Script_00_MME_Erosion_Traits.R``](https://github.com/JayCrlt/MMEs_Mortality/blob/master/R/Script_00_MME_Erosion_Traits.R) which is the control tower of this study. It will source automatically the following scripts (see R folder).
If you want to look at a specific code, the scripts are numbered according to the figure/analyse you are interested in. 

This analyze has been launched with the following machine parameters

```{Session Info, echo = T}
R version 4.2.1 (2022-06-23)
Platform: x86_64-apple-darwin17.0 (64-bit)
Running under: macOS Ventura 13.6.3

Matrix products: default
LAPACK: /Library/Frameworks/R.framework/Versions/4.2/Resources/lib/libRlapack.dylib

locale:
[1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] sf_1.0-9                mFD_1.0.3               rgdal_1.6-5             rgeos_0.6-2            
 [5] sp_2.0-0                scales_1.2.1            readxl_1.4.2            plotly_4.10.1          
 [9] viridis_0.6.2           viridisLite_0.4.1       leaflet_2.1.2           mapdata_2.3.1          
[13] maps_3.4.1              ggradar_0.2             cmdstanr_0.5.3          ggstream_0.1.0         
[17] reshape2_1.4.4          sfheaders_0.4.2         networkD3_0.4           patchwork_1.1.2        
[21] ggspatial_1.1.7.9000    data.tree_1.0.0         lubridate_1.9.2         forcats_1.0.0          
[25] stringr_1.5.0           dplyr_1.1.0             purrr_1.0.1             readr_2.1.4            
[29] tidyr_1.3.0             tibble_3.2.0            ggplot2_3.4.1           tidyverse_2.0.0        
[33] kableExtra_1.3.4.9000   hrbrthemes_0.8.0        googledrive_2.0.0       circlepackeR_0.0.0.9000
[37] rnaturalearth_0.3.2     rnaturalearthdata_0.1.0
```