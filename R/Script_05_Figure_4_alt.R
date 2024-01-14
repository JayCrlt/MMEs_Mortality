#### Figure_4
fe_6D_coord_df     <- fe_6D_coord %>% data.frame() %>% rownames_to_column("FE") 
data_complete_all  <- data_complete %>% left_join(data_filtered_MME_Merged_data %>% rename(Species = species)) 
data_heatmap_FE    <- data_complete_all %>% 
  dplyr::select(FE, year, `damaged_percentatge`, `damaged_qualitative`, drivers_abiotic, drivers_abiotic_other, drivers_biotic_group,
                drivers_biotic, drivers_biotic_other, PC1, PC2, PC3, PC4, PC5, PC6) %>% 
  mutate(drivers_abiotic = ifelse(drivers_abiotic == "Increase of turbidity / sedimentation", "Increase of turbidity", drivers_abiotic)) %>% 
  dplyr::filter(year > 1985)

## Figure 4A
### Abiotic subgroups
data_heatmap_FE_abiotic_summ     <- data_heatmap_FE %>% group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% group_by(year, drivers_abiotic) %>% 
  summarise(n = n()) %>% mutate(ID = paste(year, drivers_abiotic))
data_heatmap_FE_abiotic_summ_seq <- data_heatmap_FE_abiotic_summ %>% distinct(ID) %>% data.frame() %>% mutate(dataset_ID = seq(1,50,1))
data_heatmap_FE_abiotic          <- data_heatmap_FE %>% group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% mutate(ID = paste(year, drivers_abiotic)) %>% 
  inner_join(data_heatmap_FE_abiotic_summ_seq) %>% group_by(dataset_ID) %>% group_split()
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ %>% inner_join(data_heatmap_FE_abiotic_summ_seq) %>% arrange(n) %>% 
  dplyr::filter(n > 4) %>% arrange(dataset_ID) %>% dplyr::select(dataset_ID) %>% as.vector()
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ_sel$dataset_ID

V = c()
for (i in data_heatmap_FE_abiotic_summ_sel) {V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic[[i]][,4:7] %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ %>% inner_join(data_heatmap_FE_abiotic_summ_seq) %>% arrange(n) %>% dplyr::filter(n >= 3) %>% 
  dplyr::filter(n <= 4) %>% arrange(dataset_ID) %>% dplyr::select(dataset_ID) %>% as.vector()
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ_sel$dataset_ID
for (i in data_heatmap_FE_abiotic_summ_sel) {V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic[[i]][,4:5] %>%  as.matrix())$volume / 
                                                                   cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_to_fill                     <- data.frame(year = seq(1986,2020,1))
data_heatmap_FE_abiotic_summ     <- data_heatmap_FE_abiotic_summ %>% dplyr::select(year, drivers_abiotic, n) %>% cbind(Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year, drivers_abiotic) %>% drop_na(drivers_abiotic)

Figure_4A1 <- ggplot(data_heatmap_FE_abiotic_summ, aes(year, drivers_abiotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "OrYel", begin = 0, end = 1, limits = c(0,6)) +
  scale_x_continuous(breaks = seq(1986, 2020, 1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 15))

### Overall abiotic summary
data_heatmap_FE_abiotic_summ     <- data_heatmap_FE %>% group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 31,1))
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE %>% group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 31,1)) %>% dplyr::filter(n > 4)
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ_sel$ID
data_heatmap_FE_abiotic          <- data_heatmap_FE %>% group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()

V = c() ; for (i in data_heatmap_FE_abiotic_summ_sel) {V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic[[i]][,2:5] %>% distinct() %>% 
                                                                                           as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE %>% group_by(year, drivers_abiotic, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_abiotic %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 31,1)) %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_abiotic_summ_sel <- data_heatmap_FE_abiotic_summ_sel$ID
for (i in data_heatmap_FE_abiotic_summ_sel[-c(7,8)]) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_abiotic[[i]][,2:3] %>% distinct() %>% as.matrix())$volume / 
                       cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_abiotic_summ     <- data_heatmap_FE_abiotic_summ %>% dplyr::select(year, n) %>% cbind(Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_abiotic = "Overall abiotic\ndrivers")

Figure_4A2 <- ggplot(data_heatmap_FE_abiotic_summ, aes(year, drivers_abiotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "OrYel", begin = 0, end = 1, limits = c(0,6)) +
  scale_x_continuous(breaks = seq(1986,2020,1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())

## Figure 4B
### biotic subgroups
data_heatmap_FE_biotic_summ     <- data_heatmap_FE %>% group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% group_by(year, drivers_biotic_group) %>% 
  summarise(n = n()) %>% mutate(ID = paste(year, drivers_biotic_group))
data_heatmap_FE_biotic_summ_seq <- data_heatmap_FE_biotic_summ %>% distinct(ID) %>% data.frame() %>% mutate(dataset_ID = seq(1,29,1))
data_heatmap_FE_biotic          <- data_heatmap_FE %>% group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% mutate(ID = paste(year, drivers_biotic_group)) %>% 
  inner_join(data_heatmap_FE_biotic_summ_seq) %>% group_by(dataset_ID) %>% group_split()
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ %>% inner_join(data_heatmap_FE_biotic_summ_seq) %>% arrange(n) %>% dplyr::filter(n > 4) %>% 
  arrange(dataset_ID) %>% dplyr::select(dataset_ID) %>% as.vector()
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ_sel$dataset_ID

V = c()
for (i in data_heatmap_FE_biotic_summ_sel) {V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_biotic[[i]][,4:7] %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ %>% inner_join(data_heatmap_FE_biotic_summ_seq) %>% arrange(n) %>% dplyr::filter(n >= 3) %>% 
  dplyr::filter(n <= 4) %>% arrange(dataset_ID) %>% dplyr::select(dataset_ID) %>% as.vector()
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ_sel$dataset_ID
for (i in data_heatmap_FE_biotic_summ_sel) {V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_biotic[[i]][,4:5] %>%  as.matrix())$volume / 
                                                                 cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_biotic_summ     <- data_heatmap_FE_biotic_summ %>% dplyr::select(year, drivers_biotic_group, n) %>% cbind(., Volume = c(V$., rep(0, 2))) %>% 
  data.frame() %>% full_join(data_to_fill, by = "year") %>% complete(year, drivers_biotic_group) %>% drop_na(drivers_biotic_group)

Figure_4B1 <- ggplot(data_heatmap_FE_biotic_summ, aes(year, drivers_biotic_group, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "OrYel", begin = 0, end = 1, limits = c(0,6)) +
  scale_x_continuous(breaks = seq(1986,2020,1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 15))

### Overall biotic summary
data_heatmap_FE_biotic_summ     <- data_heatmap_FE %>% group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 19, 1))
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE %>% group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 19, 1)) %>% dplyr::filter(n > 4)
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE_biotic_summ_sel$ID
data_heatmap_FE_biotic          <- data_heatmap_FE %>% group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% group_split()

V = c() ; for (i in data_heatmap_FE_biotic_summ_sel[-c(4,5)]) {
  V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_biotic[[i]][,2:5] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_biotic_summ_sel <- data_heatmap_FE %>% group_by(year, drivers_biotic_group, FE, PC1, PC2, PC3, PC4, PC5, PC6) %>% summarise(n = n()) %>% 
  dplyr::select(., -n) %>% dplyr::filter(drivers_biotic_group %notin% c(NA, "None")) %>% distinct(., year, FE) %>% group_by(year) %>% summarise(n = n()) %>% 
  mutate(ID = seq(1, 19,1)) %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_biotic_summ_sel <- c(data_heatmap_FE_biotic_summ_sel$ID, 17:18)
for (i in data_heatmap_FE_biotic_summ_sel) {V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_biotic[[i]][,2:3] %>% distinct() %>% as.matrix())$volume / 
                                                                  cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))

data_heatmap_FE_biotic_summ <- data_heatmap_FE_biotic_summ %>% dplyr::select(year, n) %>% cbind(., Volume = c(V$., 0)) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_biotic = "Overall biotic\ndrivers")

Figure_4B2 <- ggplot(data_heatmap_FE_biotic_summ, aes(year, drivers_biotic, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "OrYel", begin = 0, end = 1, limits = c(0,6)) +
  scale_x_continuous(breaks = seq(1986,2020,1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(face="bold", size = 15),
        axis.text.x = element_blank(), axis.ticks.x = element_blank())


## Figure 4C
data_heatmap_FE_all_summ     <- rbind((data_heatmap_FE_biotic %>% bind_rows())[,-1], (data_heatmap_FE_abiotic %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% summarise(n = n()) %>% mutate(ID = seq(1, 31, 1))
data_heatmap_FE_all_summ_sel <- data_heatmap_FE_all_summ %>% dplyr::filter(n > 4)
data_heatmap_FE_all_summ_sel <- data_heatmap_FE_all_summ_sel$ID
data_heatmap_FE_all          <- rbind((data_heatmap_FE_biotic %>% bind_rows())[,-1], (data_heatmap_FE_abiotic %>% bind_rows())[,-1]) %>% 
  data.frame() %>% distinct(., year, FE, PC1, PC2, PC3, PC4) %>% group_by(year) %>% group_split()

V = c()
for (i in data_heatmap_FE_all_summ_sel) {V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all[[i]][,3:6] %>% distinct() %>% as.matrix())$volume / VTot) * 100, 2))}
data_heatmap_FE_all_summ_sel <- data_heatmap_FE_all_summ %>% dplyr::filter(n <= 4) %>% dplyr::filter(n >= 3)
data_heatmap_FE_all_summ_sel <- c(data_heatmap_FE_all_summ_sel$ID)
for (i in data_heatmap_FE_all_summ_sel) {V[i] = sqrt(round((cxhull::cxhull(data_heatmap_FE_all[[i]][,3:4] %>% distinct() %>% as.matrix())$volume / 
                                                              cxhull::cxhull(data_complete %>% dplyr::select(PC1, PC2) %>% distinct() %>% as.matrix())$volume) * 100, 2))}
V <- V %>% data.frame() %>% mutate_all(., ~replace_na(.,0))
data_heatmap_FE_all_summ <- data_heatmap_FE_all_summ %>% dplyr::select(year, n) %>% cbind(data_heatmap_FE_all_summ, Volume = V$.) %>% data.frame() %>% 
  full_join(data_to_fill, by = "year") %>% complete(year) %>% mutate(drivers_all = "Overall biotic &\nabiotic drivers")

Figure_4C <- ggplot(data_heatmap_FE_all_summ, aes(year, drivers_all, fill= Volume)) + geom_tile(col = "black") + theme_bw() +
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "OrYel", begin = 0, end = 1, limits = c(0,6)) +
  scale_x_continuous(breaks = seq(1986,2020,1), name = "") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) + scale_y_discrete(name = "") +
  theme(panel.grid.major = element_line(size = 0.5, linewidth = 'solid', colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linewidth = 'solid', colour = "white"),
        axis.text.y = element_text(size = 15, face="bold"),
        axis.text.x = element_text(size = 15))

## Figure 4
Figure_4 <- Figure_4A1 / plot_spacer() / Figure_4A2 / Figure_4B1 / plot_spacer() / Figure_4B2 / Figure_4C + 
  plot_layout(heights = c(5, -1.25, 1, 4, -1.25, 1, 1), guides = 'collect') & theme(legend.position = 'bottom') &
  colorspace::scale_fill_continuous_sequential(na.value = 'white', palette = "OrYel", begin = 0, end = 1, limits = c(0,6),
                                               name = "Trait volume affected \n(sqrt transformation) (%)")