fe_6D_coord_df <- fe_6D_coord %>% data.frame() %>% rownames_to_column("FE") %>% 
  mutate(FE = recode(FE, "fe_1" = "fe_01", "fe_2" = "fe_02", "fe_3" = "fe_03", 
                     "fe_4" = "fe_04", "fe_5" = "fe_05", "fe_6" = "fe_06", 
                     "fe_7" = "fe_07", "fe_8" = "fe_08", "fe_9" = "fe_09"))

data_heatmap_FE <- Global_dataset %>% inner_join(fe_6D_coord_df, by = "FE") %>% 
  dplyr::select(FE, year, `damaged_percentatge`, `damaged_qualitative`,
                drivers_abiotic, drivers_abiotic_other, drivers_biotic_group,
                drivers_biotic, drivers_biotic_other, PC1, PC2, PC3, PC4, PC5, PC6) 

data_heatmap_FE$drivers_abiotic[data_heatmap_FE$drivers_abiotic == "Increase of turbidity / sedimentation"] =
  "Increase of turbidity"

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
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 15))

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
  mutate(drivers_abiotic = "Overall abiotic \npressures")

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
        axis.text.y = element_text(face="bold", size = 15),
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
        axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 15))

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
  mutate(drivers_biotic = "Overall biotic \npressures")

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
        axis.text.y = element_text(face="bold", size = 15),
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
  mutate(drivers_all = "Overall biotic & \nabiotic pressures")

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
        axis.text.y = element_text(size = 15, face="bold"),
        axis.text.x = element_text(size = 15))

Figure_4 <- Figure_4A / plot_spacer() / Figure_4B / Figure_4C / plot_spacer() / Figure_4D / Figure_4E + 
  plot_layout(heights = c(5, -1.25, 1, 4, -1.25, 1, 1), guides = 'collect') & 
  theme(legend.position = 'bottom') &
  colorspace::scale_fill_continuous_sequential(na.value = 'white', 
                                               palette = "OrYel", 
                                               begin = 0, end = 1,
                                               limits = c(0,8),
                                               name = "Functional volume affected \n(sqrt transformation) (%)")
