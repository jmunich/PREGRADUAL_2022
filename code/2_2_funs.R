library("tidyverse")
library("grDevices")

# Create a palette that reflects hierarchical clustering -------------------------------------------------------------------------

## Create a palette based on a hclust object that reflects the similarities between groups of nodes on various levels, assigning different hue angles on the hcl color space

hclust_palette <- function(
  clust, ## Output from a hclust() function
  start_hue = 0, ## Starting angle on the hue color scale. Changing the value shifts the resulting color scheme along the hue circle.
  max_hue = 180, ## Maximum angle on th hue circle, set to 180. Higher values may result in an increas in similarity between most disimilar values (comming full circle).
  chroma = 70, ## Set the chroma of the palette (same for all nodes)
  luminance = 80, ## Set the luminance of the palette (same for all nodes)
  alpha = 1, ## Set the alpha of the palette (same for all nodes)
  ref_dist = .1, ## Sets a reference value for distance size (largely affects granulation). 
  distinction_a = 2, ## First distinction parameter. Sets the general differentiation between neighboring nodes of hclust. Should be higher than 1.
  distinction_b = 0, ## Second differentiation parameter. Sets the effect of height (forks with lower heigh result in smaller range on the hue scale, with nodes ending up closer to each other). Shoul not exceede distinction_a
  mod = FALSE, ## Should heights (similarities) in hclust object be modified? 
  level_mod = function(x, similarities){2*((exp(similarities*x)/(1+exp(similarities*x)))-.5)}, ## Custom function for transforming heights
  level_pars = c(similarities = 2) ## Supply parameters to the function via a named vector
){
  
  if(distinction_b > distinction_a){
    distinction_b <- .99*distinction_a
  }
  
  
  merge_tib <- clust$merge %>%
    as_tibble() %>%
    mutate(hgt = clust$height/ref_dist,
           ang_1 = NA,
           ang_2 = NA,
           max_1 = NA,
           max_2 = NA,
           done_1 = 0,
           done_2 = 0) %>%
    mutate(hgt = ifelse(rep(mod, length(hgt)), 
                        eval(bquote(level_mod(hgt, ..(level_pars)), splice = TRUE)), 
                        hgt))
  
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
  
  col_tib <- tibble(variable = c(merge_tib$V1,merge_tib$V2),
                    angle = c(merge_tib$ang_1,merge_tib$ang_2)) %>%
    filter(variable < 0) %>%
    mutate(variable = - variable) %>%
    arrange(variable) %>%
    mutate(angle_modified = (max_hue*angle/max(angle))+start_hue) %>%
    mutate(color = grDevices::hcl(angle_modified, chroma, luminance, alpha = alpha, fixup = TRUE))
  
  return(col_tib)
}


# Rotate a set of coordinates ---------------------------------------------


pivot_graph <- function(layout, # Original matrix layout 
                        vars, # A numerical vector of length 2, indicating rows 
                              # of the layout matrix that represent nodes that will be 
                              # aligned alongside a horizontal axis
                        axis_coord = NULL, # Alternatively, it is possible to 
                                           # specify the coordinates of two 
                                           # points in a 2x2 matrix 
                                           # to use instead of the axis vars
                        rescale = TRUE){ 
  
  temp_layout <- layout
  
  if(!is.null(axis_coord)){
    axis <- axis_coord
  }else{
  axis <- temp_layout[vars,]
  }
  
  
  
  axis_scaled <- t(t(axis) - colMeans(axis))
  
  ang <- atan(axis_scaled[1,2]/axis_scaled[1,1])
  
  piv_mat <- c(cos(ang), -sin(ang), 
               sin(ang), cos(ang)) %>%
    matrix(ncol = 2, byrow = TRUE)
  
  piv_layout <- (t(t(temp_layout) - colMeans(axis)) %*% piv_mat)
  
  if(rescale){
    piv_layout <- t(t(piv_layout) - colMeans(piv_layout))
    
    piv_layout[,1] <- (piv_layout[,1] - min(piv_layout))/(max(piv_layout) - 
                                                            min(piv_layout)) * 2 - 1
    piv_layout[,2] <- (piv_layout[,2] - min(piv_layout))/(max(piv_layout) - 
                                                            min(piv_layout)) * 2 - 1
    
  }
  
  return(piv_layout)
  
}


# # Examples ----------------------------------------------------------------
# library(igraph)
# library(qgraph)
# 
# ## Simulate sample graph
# cormat <- erdos.renyi.game(n = 30, p.or.m = .1, type = "gnp") %>%
#   as_adjacency_matrix() %>%
#   as.matrix()
# 
# cormat[cormat==1] <- rbeta(sum(cormat), 2, 1)
# 
# cormat[lower.tri(cormat)] <- t(cormat)[lower.tri(cormat)]
# 
# 
# ## Pivot graph to align by nodes 1 and 30
# # get colors for comparison
# gcols <- rep("steelblue", 30)
# gcols[c(1,30)] <- "pink"
# 
# par(mfrow = c(1,2))
# g0 <- qgraph(cormat, layout = "spring", color = gcols)
# grid()
# 
# # Pivot
# layout_piv <- pivot_graph(g0$layout,vars = c(1,30))
# 
# g1 <- qgraph(cormat, layout = layout_piv, color = gcols)
# grid()
# 
# ## Assign colors to the graph
# 
# clust <- as.dist(1-cormat) %>%
#   hclust("average")
# 
# hcols <- hclust_palette(clust = clust,
#                         ref_dist = .1,
#                         distinction_a = 1.5,
#                         distinction_b = 0, mod = TRUE)
# 
# par(mfrow = c(1,2))
# plot(clust)
# g2 <- qgraph(cormat, 
#              layout = "spring",
#              color = hcols$color)

