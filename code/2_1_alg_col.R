library("tidyverse")



dmat <- rbeta(100, 1, 1) %>%
  matrix(ncol = 10) %>%
  as.dist()

clust <- hclust(as.dist(1-abs(mwmt)), method = "complete")

clust <- EFF_clust

start_hue <- 0
max_hue <- 180
max_dist <- 1
distinction_a <- 1.5
distinction_b <- 0
mod <- FALSE
similarities <- 1.2 
level_mod <- function(x,p){2*((exp(p*x)/(1+exp(p*x)))-.5)}




merge_tib <- clust$merge %>%
  as_tibble() %>%
  mutate(hgt = clust$height/max_dist,
         ang_1 = NA,
         ang_2 = NA,
         max_1 = NA,
         max_2 = NA,
         done_1 = 0,
         done_2 = 0) %>%
  mutate(hgt = ifelse(rep(mod, length(hgt)), level_mod(hgt, similarities), hgt))

merges <- c(merge_tib$V1, merge_tib$V2) %>%
  .[.>0] %>%
  sort()

item_list <- list()

for(i in 1:length(merges)){

  temp_row <- unlist(merge_tib[merges[i], c(1,2)])
  temp_vec <- temp_row[temp_row < 0]
  
  while(sum(temp_row>0)>0){
    temp_row <- unlist(merge_tib[temp_row[temp_row > 0], c(1,2)])
    temp_vec <- c(temp_vec, temp_row[temp_row < 0])
  }
  
  item_list[[i]] <- unique(temp_vec)
}

n_items <- c(1, lengths(item_list))
merge_tib$items1 <- n_items[ifelse(merge_tib$V1 > 0, merge_tib$V1 + 1, 1)]
merge_tib$items2 <- n_items[ifelse(merge_tib$V2 > 0, merge_tib$V2 + 1, 1)]
merge_tib$prop <- merge_tib$items1/(merge_tib$items1 + merge_tib$items2)

merge_tib[nrow(merge_tib),4] <- 0
merge_tib[nrow(merge_tib),5] <- 180
merge_tib[nrow(merge_tib),6] <- unlist(merge_tib[nrow(merge_tib),5] - merge_tib[nrow(merge_tib),4])*(merge_tib[nrow(merge_tib),12])
merge_tib[nrow(merge_tib),7] <- unlist(merge_tib[nrow(merge_tib),5] - merge_tib[nrow(merge_tib),4])*(1-merge_tib[nrow(merge_tib),12])


temp_ind_1 <- temp_ind_2 <- nrow(merge_tib)

while(length(is.na(c(merge_tib$ang_1, merge_tib$ang_2)))>0){
  
  if(merge_tib$V1[temp_ind_1] > 0 & merge_tib$done_1[temp_ind_1] != 1){
    ang_1 <- merge_tib$ang_1[temp_ind_1]
    max_1 <- merge_tib$max_1[temp_ind_1] * merge_tib[temp_ind_1,3]
    lim_1 <- merge_tib$prop[temp_ind_1]
    temp_hgt_1 <- merge_tib$hgt[temp_ind_1]
    temp_ind_1 <- merge_tib$V1[temp_ind_1]
    # distinction_1 <- ((temp_hgt_1/(temp_hgt_1 + merge_tib$hgt[temp_ind_1] + .001))+.01)*distinction
    distinction_1 <- distinction_a - (distinction_b * merge_tib$hgt[temp_ind_1])

    
    merge_tib[temp_ind_1,4] <- ang_1 - (max_1/distinction_1) + ((max_1/distinction_1 * lim_1)/2)
    merge_tib[temp_ind_1,5] <- ang_1 + (max_1/distinction_1) - ((max_1/distinction_1 * (1-lim_1))/2)
    merge_tib[temp_ind_1,6] <- max_1 * lim_1
    merge_tib[temp_ind_1,7] <- max_1 * (1-lim_1)

  }else{
    if(merge_tib$V2[temp_ind_1] > 0 & merge_tib$done_2[temp_ind_1] != 1){
      merge_tib[temp_ind_1,8] <- 1 
      ang_1 <- merge_tib$ang_2[temp_ind_1]
      max_1 <- merge_tib$max_2[temp_ind_1] * merge_tib[temp_ind_1,3]
      lim_1 <- merge_tib$prop[temp_ind_1]
      temp_hgt_1 <- merge_tib$hgt[temp_ind_1]
      temp_ind_1 <- merge_tib$V2[temp_ind_1]
      distinction_1 <- distinction_a - (distinction_b * merge_tib$hgt[temp_ind_1])
      
      merge_tib[temp_ind_1,4] <- ang_1 - (max_1/distinction_1) + ((max_1/distinction_1 * lim_1)/2)
      merge_tib[temp_ind_1,5] <- ang_1 + (max_1/distinction_1) - ((max_1/distinction_1 * (1-lim_1))/2)
      merge_tib[temp_ind_1,6] <- max_1 * lim_1
      merge_tib[temp_ind_1,7] <- max_1 * (1-lim_1)

    }else{
      temp_ind_0 <- temp_ind_1
      temp_ind_1 <- which(rowSums(merge_tib[,c(1,2)] == temp_ind_1) == 1)
      if(length(temp_ind_1) == 0){
        break
      }
      merge_tib[temp_ind_1, which(merge_tib[temp_ind_1,c(1,2)] == temp_ind_0)+7] <- 1
    }
    
  }
  
}

start_hue <- 120
max_hue <- 315
col_tib <- tibble(var = c(merge_tib$V1,merge_tib$V2),
       ang = c(merge_tib$ang_1,merge_tib$ang_2)) %>%
  filter(var < 0) %>%
  mutate(var = - var) %>%
  arrange(var) %>%
  mutate(angmod = (max_hue*ang/max(ang))+start_hue) %>%
  mutate(col = grDevices::hcl(angmod, 70, 80))


EFF_graph_comm <- EFF_mat %>%
  `colnames<-`(names(EFF_dat_mod)) %>%
  `rownames<-`(names(EFF_dat_mod)) %>%
  qgraph(layout = "spring", 
         labels = 1:ncol(EFF_mat),
         nodeNames = names(EFF_dat_mod),
         groups = paste0("F",EFF_groups_tree),
         color = col_tib$col,
         vsize = 3,
         GLratio = 1,
         repulsion = .9,
         legend.cex = .35, esize = .3, legend = FALSE)



EFF_fit_tree <- qgraph(EFF_cmat,
                       graph = "glasso",
                       layout = "spring",
                       sampleSize = nrow(EFF_dat_mod),
                       threshold = TRUE,
                       legend = FALSE,
                       groups = paste0("F", EFF_groups_tree),
                       labels = 1:ncol(EFF_cmat),
                       nodeNames = EFF_labs,
                       pie = EFF_props,
                       GLratio = .8,
                       legend.cex = .28,
                       color = col_tib$col,
                       vsize = 3,
                       details = TRUE,
                       pieBorder = .3,
                       pieColor = list(c("darkred",
                                         "pink",
                                         "lightgreen",
                                         "darkgreen"))[rep(1, ncol(EFF_dat_mod))])

plt_EFF_dendro <- ggplot(segment(EFF_ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), lwd = 1.5) + 
  geom_point(data = EFF_ddata$labels %>% 
               as_tibble() %>%
               arrange(as.numeric(label)),
             aes(x = x, 
                 y = y), 
             inherit.aes = FALSE,
             pch = 21,
             fill = col_tib$col,
             cex = 5) +
  geom_text(data = EFF_ddata$labels %>% 
              as_tibble() %>%
              mutate(label = paste0("SQ[0", substr(100 + as.numeric(label), 2, 3) , "]")),
            aes(x = x, 
                y = y, 
                label = label), 
            inherit.aes = FALSE,
            angle = 90, hjust = 1.2) +
  geom_hline(yintercept = .5, 
             lty = 2, 
             lwd = 1,2) +
  theme_classic() +
  theme(legend.position = "bottom", 
        panel.grid.major = element_line(),
        panel.grid.minor = element_line()) +
  scale_y_continuous(breaks = seq(0,1, .2), labels = c(seq(1,0, -.2))) +
  labs(x = "Položka", y = "Pravděpodobnost spoluvýskytu v komunitě") +
  coord_cartesian(ylim = c(-.2,1))

EFF_plt_fac_group <- EFF_lab_tib %>%
  arrange(name) %>%
  mutate(col = col_tib$col,
         ang = col_tib$angmod) %>%
  mutate(Faktor = paste0("Faktor ", EFF_groups_tree),
         group = ordered(group, sort(unique(group), decreasing = TRUE))) %>%
  group_by(Faktor) %>%
  mutate(ang = mean(ang)) %>%
  ungroup() %>%
  arrange(-ang) %>%
  mutate(Faktor = ordered(Faktor, unique(Faktor))) %>%
  group_by(group, Faktor) %>%
  summarise(cnt = n(),
            col = unique(col)) %>%
  group_by(Faktor) %>%
  mutate(col = unique(col)[1]) %>%
  ggplot(aes(y = group, x = Faktor, fill = col, label = cnt)) +
  geom_tile(color = "black") +
  geom_text(color = "black") +
  theme_classic() +
  labs(x = "", y = "") +
  theme(legend.position = "none", 
        panel.grid.major = element_line(), 
        panel.grid.minor = element_line()) +
  coord_fixed() 
