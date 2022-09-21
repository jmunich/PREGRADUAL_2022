hclust_palette <- function(
  clust, ## Output from a hclust() function
  start_hue = 0, ## Starting angle on the hue color scale. Changing the value shifts the resulting color scheme along the hue circle.
  max_hue = 180, ## Maximum angle on th hue circle, set to 180. Higher values may result in an increas in similarity between most disimilar values (comming full circle).
  chroma = 70, ## Set the chroma of the palette (same for all nodes)
  min_luminance = 60, ## Set the luminance of the palette (same for all nodes)
  max_luminance = 80, ## Set the luminance of the palette (same for all nodes)
  alpha = 1, ## Set the alpha of the palette (same for all nodes)
  min_dist = 0, ## Sets a reference value for distance size (largely affects granulation). 
  max_dist = 1, ## Sets a reference value for distance size (largely affects granulation). 
  distinction = .1,## First distinction parameter. Sets the general differentiation between neighboring nodes of hclust. Should be higher than 1.
  mod_hgt = FALSE
  ){
  
 # Set up a tibble from hclust dendrogram 
  merge_tib <- clust$merge %>%
    as_tibble() %>%
    mutate(hgt = clust$height,
           ang_1 = NA,
           ang_2 = NA,
           max_1 = NA,
           max_2 = NA,
           done_1 = 0,
           done_2 = 0)
  
  if(mod_hgt > 0){
    merge_tib$hgt <- log(merge_tib$hgt + 1, mod_hgt)/max(log(merge_tib$hgt + 1, mod_hgt))
  }
  
  merges <- c(merge_tib$V1, merge_tib$V2) %>%
    .[.>0] %>%
    sort()
  
  item_list <- list()
  hgt_list <- list()
  
  for(i in 1:length(merges)){
    
    temp_row <- unlist(merge_tib[merges[i], c(1,2)])
    temp_vec <- temp_row[temp_row < 0]
    temp_hgt <- unlist(merge_tib[unlist(temp_row[temp_row > 0]), 3])
    
    while(sum(temp_row>0)>0){
      temp_hgt <- c(temp_hgt, unlist(merge_tib[unlist(temp_row[temp_row > 0]), 3]))
      temp_row <- unlist(merge_tib[temp_row[temp_row > 0], c(1,2)])
      temp_vec <- c(temp_vec, temp_row[temp_row < 0])
    }
    
    item_list[[i]] <- unique(temp_vec)
    hgt_list[[i]] <- temp_hgt
  }

  
  n_items <- c(1, lengths(item_list))
  hgts <- c(0, unlist(lapply(hgt_list, sum)))
  merge_tib$items1 <- n_items[ifelse(merge_tib$V1 > 0, merge_tib$V1 + 1, 1)]
  merge_tib$items2 <- n_items[ifelse(merge_tib$V2 > 0, merge_tib$V2 + 1, 1)]
  merge_tib$c_hgts_1 <- hgts[ifelse(merge_tib$V1 > 0, merge_tib$V1 + 1, 1)]
  merge_tib$c_hgts_2 <- hgts[ifelse(merge_tib$V2 > 0, merge_tib$V2 + 1, 1)]
  merge_tib$prop <- merge_tib$items1/(merge_tib$items1 + merge_tib$items2)
  
  for(i in 1:nrow(merge_tib)){
    merge_tib[i,c(1,2)] <- merge_tib[i,c(1,2)][order(unlist(merge_tib[i,c(12,13)]), decreasing = FALSE)]
    merge_tib[i,c(10,11)] <- merge_tib[i,c(10,11)][order(unlist(merge_tib[i,c(12,13), decreasing = FALSE]))]
    merge_tib[i,c(12,13)] <- merge_tib[i,c(12,13)][order(unlist(merge_tib[i,c(12,13), decreasing = FALSE]))]
  }
  # Initate positions
  merge_tib <- merge_tib %>%
    mutate(dist = (hgt + (c_hgts_1 + c_hgts_2)))

  merge_tib$dir_1 <- NA
  merge_tib$dir_2 <- NA
  merge_tib[nrow(merge_tib),4] <- - ((distinction * sqrt(merge_tib$dist[nrow(merge_tib)])/2) + (merge_tib$dist[nrow(merge_tib)]/2))
  merge_tib[nrow(merge_tib),5] <- ((distinction * sqrt(merge_tib$dist[nrow(merge_tib)])/2) + (merge_tib$dist[nrow(merge_tib)]/2))
  merge_tib$dir_1[nrow(merge_tib)] <- -1
  merge_tib$dir_2[nrow(merge_tib)] <- 1
  merge_tib[nrow(merge_tib),6] <- unlist(merge_tib[nrow(merge_tib),5] - merge_tib[nrow(merge_tib),4])*(merge_tib[nrow(merge_tib),12])
  merge_tib[nrow(merge_tib),7] <- unlist(merge_tib[nrow(merge_tib),5] - merge_tib[nrow(merge_tib),4])*(1-merge_tib[nrow(merge_tib),12])
  
  
  
  temp_ind_1 <- nrow(merge_tib)

  while(length(is.na(c(merge_tib$ang_1, merge_tib$ang_2)))>0){
    
    if(merge_tib$V1[temp_ind_1] > 0 & merge_tib$done_1[temp_ind_1] != 1){
      parent_ind <- temp_ind_1
      parent_ang <- merge_tib$ang_1[temp_ind_1]
      parent_dir <- merge_tib$dir_1[temp_ind_1]
      temp_ind_1 <- merge_tib$V1[temp_ind_1]
      merge_tib$ang_1[temp_ind_1] <- parent_ang
      merge_tib$ang_2[temp_ind_1] <- parent_ang + (parent_dir * (merge_tib$dist[temp_ind_1] + (distinction * sqrt(merge_tib$dist[temp_ind_1]))))
      merge_tib$dir_1[temp_ind_1] <- parent_dir
      merge_tib$dir_2[temp_ind_1] <- parent_dir
      


    }else{
      if(merge_tib$V2[temp_ind_1] > 0 & merge_tib$done_2[temp_ind_1] != 1){
        parent_ind <- temp_ind_1
        parent_ang <- merge_tib$ang_2[temp_ind_1]
        parent_dir <- merge_tib$dir_2[temp_ind_1]
        temp_ind_1 <- merge_tib$V2[temp_ind_1]
        merge_tib$ang_1[temp_ind_1] <- parent_ang
        merge_tib$ang_2[temp_ind_1] <- parent_ang + (parent_dir * (merge_tib$dist[temp_ind_1] + (distinction * sqrt(merge_tib$dist[temp_ind_1]))))
        merge_tib$dir_1[temp_ind_1] <- parent_dir
        merge_tib$dir_2[temp_ind_1] <- parent_dir
        
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
  
  col_tib <- tibble(variable = c(merge_tib$V1,merge_tib$V2),
                    angle = c(merge_tib$ang_1,merge_tib$ang_2)) %>%
    filter(variable < 0) %>%
    mutate(variable = - variable) %>%
    arrange(variable) %>%
    mutate(angle_modified = angle + min(angle),
           angle_modified = angle_modified/max(angle_modified),
           angle_luminance = ((angle_modified)*(max_luminance - min_luminance)) + min_luminance,
           angle_hue = (angle_modified*max_hue)+start_hue) %>%
    mutate(color = grDevices::hcl(angle_hue, chroma, angle_luminance, alpha = alpha, fixup = TRUE))
  
  return(col_tib)
}
