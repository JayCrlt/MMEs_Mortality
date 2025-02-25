# Vulnerability of benthic trait diversity across the Mediterranean Sea following Mass Mortality Events

![GitHub](https://img.shields.io/badge/GitHub-39457E?style=for-the-badge&logo=github&logoColor=white)
![Gitlab](https://img.shields.io/badge/GitLab-FFA500?style=for-the-badge&logo=gitlab&logoColor=white)
![RStudio](https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=RStudio&logoColor=white)
[![DOI](https://zenodo.org/badge/573744935.svg)](https://doi.org/10.5281/zenodo.14383069)

#### J. Carlot, C. Galobart, D. Gómez-Gras, J. Santamaria, R. Golo, M. Sini, E. Cebrian, V. Gerovasileiou,  M. Ponti, E. Turicchia, S. Comeau, G. Rilov, L. Tamburello, T. Pulido Mantas, C. Cerrano, J.B. Ledoux, J.P. Gattuso, S. Ramirez-Calero, L. Millán Agudo,  M. Montefalcone, S. Katsanevakis, N. Bensoussan, J. Garrabou and N. Teixidó

### Abstract
Unraveling the functional future of marine ecosystems amid global change poses a pressing challenge. This is particularly critical in the Mediterranean Sea, which is highly impacted by global and local drivers. Utilizing extensive mass mortality events (MMEs) datasets spanning from 1986 to 2020 across the Mediterranean Sea, we investigated the trait vulnerability of benthic species that suffered from MMEs induced by nine distinct mortality drivers. By analyzing changes in ten ecological traits across 389 benthic species – constituting an extensive compendium of Mediterranean ecological traits to date – we identified 228 functional entities (FEs), defined as groups of species sharing the same trait values. Our findings indicate that of these 55 FEs were impacted by MMEs, accentuating a heightened vulnerability within specific trait categories. Notably, more than half of the mortality records showed severe impacts on calcifying and larger species with slower growth which mostly account for tree-like and massive forms. Altogether, we highlight that 29 FEs suffered extreme mortality, leading to a maximum increase of 19.1% of the global trait volume vulnerability over 35 years. We also reveal that 10.8% of the trait volume may have been temporarily lost over the last five years, emphasizing the risk of a rapid ecological transformation in the Mediterranean Sea.

### Repository structure
This Github repository is structured as follows:

- :file_folder: [``Data``](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Data) contains the following sub-folders
- :file_folder: [``Figures``](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Figures) which hosts the figures built with this script
- :file_folder: [``Models``](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Models) which hosts the models built with this script
- :file_folder: [``R``](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Data/R) contains the current script and R project related to 

### Data and Code in brief
#### Data
Mortality data have been acquired from the [T-MEDNet platform](https://t-mednet.org/mass-mortality/mass-mortality-events), from [Garrabou et al., 2023](https://onlinelibrary.wiley.com/doi/full/10.1111/gcb.16301); and a literature review from this article. This data has been compiled in a single document and has been stored [`here`](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Data/R/MME-Review%20data.xlsx). Benthic trait data have been acquired from [Teixido et al., 2024](https://onlinelibrary.wiley.com/doi/10.1111/gcb.17105), [Golo et al., 2024](https://tesisenred.net/handle/10803/692268), [Galobart et al., 2023](https://www.frontiersin.org/journals/marine-science/articles/10.3389/fmars.2023.1176655/full) and [Gomez-Gras et al., 2021](https://onlinelibrary.wiley.com/doi/full/10.1111/ele.13718). This data has been compiled in a single document and has been stored [`here`](https://github.com/JayCrlt/MMEs_Mortality/tree/master/Data/R/Complete_Traits.xlsx).

#### Code
:warning: You might run only the script [``Script_00_MME_Erosion_Traits.R``](https://github.com/JayCrlt/MMEs_Mortality/tree/master/R/Script_00_MME_Erosion_Traits.R) which is the control tower of this study. It will source automatically the following scripts (see R folder). If you want to look at a specific code, the scripts are numbered according to the figure/analyse you are interested in. 

- `Script_01_Cleaning_data.R` is used to prepare the data to be analyzed
- `Script_02_Figure_1.R` is used to map the MMEs across the Mediterranean Sea
- `Script_03_Figure_2.R` is used to look at the affected trait distribution among MMEs
- `Script_04_Figure_3_alt.R` is used to plot the affected trait hypervolume among benthic communities
- `Script_05_Figure_4_alt.R` is used to quantify how stressors impair MMEs over time
- `Script_06_Figure_5_alt.R` is used to quantify how MMEs are impaired spatially
- `Script_07_Bayesian_Models_alt.R` is used to perform all the bayesian models refered in the study.

### Machine parameters
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