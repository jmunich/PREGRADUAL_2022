### Convenience functions used in the analysis

# These functions are used throughout the analysis, although some might be useful in more general settings
# A recurring argument in many functions is the *par_list*
# *par_list* is a list used throughout the analysis, containing graphical and other parameters, such as item groups, labels and vignettes
# Most items on the list are character vectors with names corresponding to the names of variables used in the analysis
# Some of these vectors are stored in lists under user-defined names, this can be used for instance to plot variables under differently assigned groups

require(ggpubr)
require(ggforce)
require(RColorBrewer)
require(scales)
require(semTools)
require(semPlot)
require(reshape2)

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

# set palette -------------------------------------------------------------

use_cols <- brewer.pal(7, name = "PuOr")

# modify a table ----------------------------------------------------------

tab_mod <- function(x, skip = 0){x %>%
    mutate_at(-skip, function(x){round(x, 3) %>%
        as.character() %>%
        gsub("^0\\.","\\.",.) %>%
        ifelse(is.na(.), "", .)})}


# frequency plot ----------------------------------------------------------
# creates a frequency (proportion) barchart for a set of grouped ordinal variables

# par_list contains:
# $varlabs[name] - a named vector of labels used for each item with variable *name* as names 
# $groups[[group]][name] - a list containing a named character vector of item groups *name* as names. The vector in the list is names the sames as the *group* argument
# $ref_vec - a vector of response values used to order items (to compute the proportion of responses in the vector eg. c("1", "2"))
# $col_vec - colors used in plotting, corresponding to the levels of variable
# $lab_vec - labels for response levels

freq_plot <- function(dat_tib, # data
                      par_list, # list with plotting parameters
                      group, # name of $group in par_list to be used in organizing items
                      wrap = 40, # string width for item labels
                      varlab = "varlabs", # par_list index for variable label vector
                      prop_geom = "label", # geom used for plotting proportions on the barchart, can be "label", "text" or "none"
                      other.lab = "Ostatní", # What label should be used for a group of items that are only 1 per group?
                      plot.labs = list(fill = "Odpověď", # Labels used in ggplot
                                       x = "",
                                       y = "Podíl"),
                      v.order = NULL, # Optional order for variables
                      f.order = NULL){# Should the facets be ordered? if NULL, default order in the data is used, can be replaced with a character vector of ordered levels
  

  d_0 <- dat_tib %>%
    mutate_all(as.numeric) %>%
    pivot_longer(-0) %>%
    na.omit() %>%
    mutate(name2 = par_list[[varlab]][name] %>%
             as.character() %>%
             enc2utf8() %>%
             str_wrap(wrap),
           group = par_list$groups[[group]][name]) 
  
  if(is.null(v.order)){
   d_0 <- d_0 %>%
     group_by(name2) %>%
     mutate(ord = sum(value %in% par_list$ref_vec)/n()) %>%
     ungroup() %>%
     arrange(-ord) %>%
     mutate(name2 = ordered(name2, unique(name2))) 
  }else{
    d_0 <- d_0 %>%
      mutate(name = ordered(name, f.order)) %>%
      arrange(name) %>%
      mutate(name2 = ordered(name2, unique(name2)))      
  }
  
  d_0 <- d_0 %>%
    group_by(group, name2, value) %>%
    summarise(cnt = n()) %>%
    arrange(-as.numeric(value)) %>%
    group_by(name2) %>%
    mutate(cnt = cnt/sum(cnt),
           cnt2 = cumsum(cnt),
           cnt2 = cnt2 - ((cnt2 - lag(cnt2, 1, 0))/2),
           lab = round(100*cnt, 1)) %>%
    ungroup() %>%
    mutate(value = ordered(value)) %>%
    group_by(group) %>%
    mutate(mark = 1:n()) %>%
    mutate(group = ifelse(max(mark) == length(unique(value)), other.lab, group)) %>%
    select(-mark) %>%
    ungroup()
  
  if(!is.null(f.order)){
    d_0 <- d_0 %>%
      mutate(group = ordered(group, c(f.order, other.lab)))  
  }else{
    d_0 <- d_0 %>%
      mutate(group = ordered(group, c(unique(group)[unique(group) != other.lab], other.lab)))
  }
  
  plt_0 <- d_0 %>%
    ggplot(aes(x = name2, y = cnt, fill = value, label = lab)) +
    geom_bar(stat = "identity", position = "fill", color = "black") +
    coord_flip() +
    facet_col(~group, scales = "free", space = "free") +
    scale_fill_manual(values = par_list$col_vec, labels = par_list$lab_vec) +
    geom_hline(yintercept = .5, lty = 2) +
    labs(fill = plot.labs$fill,
         y = plot.labs$y,
         x = plot.labs$x) +
    scale_y_continuous(labels = percent) +
    theme(legend.position = "bottom")
  
  if(prop_geom == "label"){
    plt_0 <- plt_0 +
      geom_label(aes(y = cnt2), show.legend = FALSE, cex = 3.5)
  }
  if(prop_geom == "text"){
    plt_0 <- plt_0 +
      geom_text(aes(y = cnt2), show.legend = FALSE, cex = 3.5)
  }
  if(prop_geom == "none"){
    plt_0 <- plt_0
  }
  
  return(plt_0)
  
}

# plot qgraph --------------------------------------------------------------
# This function primarily creates a qgraph plot using parameters from the par_list
# The purpose is not to use qgraph for estimation, but for plotting
# therefore, the cmat should be a final edge matrix

# par_list contains:
# $groups[[group]][name] - a list containing a named character vector of item groups with *name* as names. The vector in the list is names the sames as the *group* argument
# $varlabs[name] - a named vector of labels used for each item with variable *name* as names 
# $nodenames[name] - a named vector of item vignettes, used in the legend  with variable *name* as names
# $pies[name] - a named list of vectors of response level proportions, for constructing pie-charts around nodes with variable *name* as names
# $pieCols[name] - a named vector of colors for each response level, for constructing pie-charts around nodes with variable *name* as names
# $color[[color]][name] - a list containing a named character vector of item colors with *name* as names. The vector in the list is names the sames as the *color* argument

qgraph_w_pars <- function(cmat, # matrix of edge weights with named rows and cols
                       par_list, # par_list object
                       color, # par_list color element index
                       group, # par_list group element index
                       legend = TRUE, # parameters passed to qgraph
                       DoNotPlot = TRUE, # do not show the plot in the GUI
                       details = TRUE, # inclde summary plot details
                       layout = NULL, # optional, include a layout matrix
                       ...){ # other arguments, passed to qgraph
  
  if(is.null(layout)){
    layout <- par_list$layout[colnames(cmat),]
  }
  
  cmat %>%
    qgraph(layout = layout,
           groups = par_list$groups[[group]][colnames(.)],
           labels = par_list$varlabs[colnames(.)],
           nodeNames = par_list$nodeNames[colnames(.)],
           pie = par_list$pies[colnames(.)],
           pieColor = par_list$pieCols[colnames(.)],
           color = par_list$color[[color]][colnames(.)],
           GLratio = .8,
           legend = legend,
           legend.cex = .28,
           vsize = 3,
           DoNotPlot = DoNotPlot,
           details = details,
           ...
)
}


# summarise ggm fit -------------------------------------------------------
# Extract selected fit measures from a qgraph::ggmFit() output object

ggmFit_table <- function(ggmFit_obj, # a qgraph::ggmFit() object
                         fit.measures = "chisq|df|^pvalue$|tli|cfi|rmsea") # fit measure names to extract in a regex expression
  {
  ggmFit_obj %>%
  .$fitMeasures %>%
  .[grepl(fit.measures, names(.))] %>%
  .[!grepl("baseline", names(.))] %>%
  `names<-`(gsub("\\.", " ", names(.))) %>%
  `names<-`(gsub("value", "", names(.))) %>%
  `names<-`(toupper(names(.))) %>%
  `names<-`(gsub("RMSEA CI", "CI", names(.))) %>%
  `names<-`(gsub("DF", "df", names(.))) %>%
  `names<-`(gsub("CHISQ", "Chisq", names(.))) %>%
  `names<-`(gsub("UPPER", "Upper", names(.))) %>%
  `names<-`(gsub("LOWER", "Lower", names(.))) %>%
  `names<-`(gsub("^P$|P$", "p", names(.))) %>%
  as_tibble_row()
}


# boot_plot ---------------------------------------------------------------
# using a bootnet() output object, returns bootstrapped interval plots

# par_list contains:
# $varlabs[name] - a named vector of labels used for each item with variable *name* as names 

boot_plot <- function(bootnet_obj, # A bootnet::bootnet() OBJECT
                      par_list, # A par_list object
                      plot.labs = list(x = "Hrany seřazené podle původního odhadu\nČísla indikují uzly spojené hranou", # Labels used in ggplot
                                       y = "Rozpětí indikuje rozpětí 95% středních hodnot bootstrapu",
                                       color = "Hodnota"),
                      boot.labs = list(mean = "Bootstrapovaný průměr", # Labels used in ggplot for mean and sample edges
                                       sample = "Původní odhad")){
  
  dat_boots <- summary(bootnet_obj) %>%
    mutate(id1 = par_list$varlabs[node1],
           id2 = par_list$varlabs[node2]) %>%
    ungroup() %>%
    filter(node2 != "")
  
  # Plot boot results
  plt_CI_1 <- dat_boots %>%  
    filter(q2.5 > 0 | q97.5 < 0) %>%
    arrange(mean) %>%
    arrange(sample) %>%
    ungroup() %>%
    mutate(ord = (1:n())-1) %>%
    ggplot(aes(x = ord, y = mean)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_hline(yintercept = .1, lty = 3) +
    geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "grey", alpha = .3) +
    geom_errorbar(aes(ymin = q2.5, ymax = q97.5)) +
    geom_text(aes(y = q2.5, label = id1, hjust = 1.2)) +
    geom_text(aes(y = q97.5, label = id2, hjust = -.2)) +
    geom_point(aes(color = boot.labs$mean)) +
    geom_point(aes(y = sample, color = boot.labs$sample), 
               show.legend = TRUE) +
    coord_flip() +
    labs(x = plot.labs$x,
         y = plot.labs$y,
         color = plot.labs$color) + 
    scale_color_manual(values = c("black", "darkred") %>% `names<-`(unlist(boot.labs))) +
    theme(legend.position = "bottom") +
    scale_y_continuous(n.breaks = 7, expand = c(.05,.05)) +
    scale_x_continuous(breaks = c(seq(0,nrow(dat_boots),5), nrow(dat_boots)))
  
  plt_CI_2 <- dat_boots %>%  
    filter((!(q2.5 > 0 | q97.5 < 0)) & sample != 0) %>%
    arrange(mean) %>%
    arrange(sample) %>%
    ungroup() %>%
    mutate(ord = (1:n())-1) %>%
    ggplot(aes(x = ord, y = mean)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_hline(yintercept = .1, lty = 3) +
    geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "grey", alpha = .3) +
    geom_errorbar(aes(ymin = q2.5, ymax = q97.5)) +
    geom_text(aes(y = q2.5, label = id1, hjust = 1.2)) +
    geom_text(aes(y = q97.5, label = id2, hjust = -.2)) +
    geom_point(aes(color = boot.labs$mean)) +
    geom_point(aes(y = sample, color = boot.labs$sample), 
               show.legend = TRUE) +
    coord_flip() +
    labs(x = plot.labs$x,
         y = plot.labs$y,
         color = plot.labs$color) + 
    scale_color_manual(values = c("black", "darkred") %>% `names<-`(unlist(boot.labs))) +
    theme(legend.position = "bottom") +
    scale_y_continuous(n.breaks = 7,expand = c(.05,.05)) +
    scale_x_continuous(breaks = c(seq(0,nrow(dat_boots),5), nrow(dat_boots)))
  
  plt_CI_3 <- dat_boots %>%
    arrange(mean) %>%
    arrange(sample) %>%
    ungroup() %>%
    mutate(ord = (1:n())-1) %>%
    ggplot(aes(x = ord, y = mean)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_hline(yintercept = .1, lty = 3) +
    geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "grey", alpha = .5) +
    geom_point(cex = .1, aes(color = boot.labs$mean)) +
    geom_line(aes(y = sample, color = boot.labs$sample), cex = .1) +
    coord_flip() +
    labs(x = plot.labs$x,
         y = plot.labs$y,
         color = plot.labs$color) + 
    scale_color_manual(values = c("black", "darkred") %>% `names<-`(unlist(boot.labs))) +
    scale_y_continuous(n.breaks = 7) +
    scale_x_continuous(breaks = c(seq(0,nrow(dat_boots),100), nrow(dat_boots))) +
    theme(legend.position = "bottom")
  
  return(list(plt_CI_1 = plt_CI_1, # Plot: non-zero sample edges, non-zero intervals 
              plt_CI_2 = plt_CI_2, # Plot: non-zero sample edges, zero intervals
              plt_CI_3 = plt_CI_3))  # Plot: all edges
}



# summaries nct -----------------------------------------------------------

# Create a summary table for a NetworkComparisonTest::NCT() output object

nct_plot <- function(nct_obj, # A NetworkComparisonTest::NCT() output object
                     plot.labs = list(M = "Největší absolutní rozdíly hran v permutacích", # Labels used in ggplot
                                      density = "Hustota",
                                      diff = "Absolutní rozdíl hran")){
  
  nct_test <- tibble(M = nct_obj$nwinv.real, 
                     p = nct_obj$nwinv.pval)
  
  nct_test_print <- tibble(M = nct_obj$nwinv.real, 
                           p = nct_obj$nwinv.pval) %>%
    mutate_all(round, 3)
  
  nct_inv_plt <- nct_obj$nwinv.perm %>% 
    unlist() %>% tibble(x = .) %>% 
    ggplot(aes(x=x)) + 
    geom_density(fill = "steelblue", alpha = .5) +
    geom_vline(xintercept = nct_obj$nwinv.real, lty = 2) +
    labs(x = plot.labs$M, 
         y = plot.labs$density)
  
  nct_plt <- abs(nct_obj$nw1 - nct_obj$nw2) %>% 
    .[lower.tri(., diag = FALSE)] %>%
    unlist() %>% tibble(x = .) %>% 
    ggplot(aes(x=x)) + 
    geom_density(fill = "steelblue", alpha = .5) +
    geom_rug(length = unit(0.045, "npc")) +
    labs(x = plot.labs$diff, 
         y = plot.labs$density)
  
  return(list(nct_test = nct_test,
              nct_test_print = nct_test_print,
              nct_inv_plt = nct_inv_plt,
              nct_plt = nct_plt))
              
}


# NCT nets ----------------------------------------------------------------

# Creates matrices of significant differences detected by NCT test

nct_nets <- function(nct_obj, # A NetworkComparisonTest::NCT() output object
                     alpha = .05){ # Selected significance threshold
  
  if(sum(nct_obj$einv.pvals$`p-value` < alpha) != 0){
    
    difs <- nct_obj$einv.pvals %>%
      filter(`p-value` < .05) %>%
      .[,c(1,2)] %>%
      unlist() %>%
      sort() %>%
      unique() %>%
      as.character()
    
    dif_mat <- nct_obj$einv.pvals %>%
      mutate(`p-value` = ifelse(`p-value` < alpha, 1, 0)) %>%
      qgraph(., 
             directed = FALSE, 
             DoNotPlot = TRUE) %>%
      getWmat() %>%
      .[colnames(nct_obj$nw1),
        colnames(nct_obj$nw1)]
    
  }else{
    dif_mat <- matrix(0, ncol = ncol(nct_obj$nw1), nrow = ncol(nct_obj$nw1)) %>%
      `colnames<-`(colnames(nct_obj$nw1)) %>%
      `rownames<-`(colnames(nct_obj$nw1))
  }
  
  NCT_1 <- (nct_obj$nw2 - nct_obj$nw1)
  
  NCT_2 <- (nct_obj$nw2 - nct_obj$nw1) %>%
    `*`(dif_mat)
  
  return(list(diffs = NCT_1, # All differences
              diffs_sig = NCT_2, # Significant differences only
              sig_mat = dif_mat)) # Binary significance edge matrix
}


# spin_loop ---------------------------------------------------------------

# A loop repeating a spinglass community search

spin_loop <- function(qgraph_obj, # qgraph() fit object
                      nspin = 100, # Number of spins
                      gamma = .75, # Gamma parameter
                      hclust.method = "single", # Method argument passed to hclust()
                      cut = .5, # Cut value used in cutree() on a spinglass co-occurence hclust()
                      seed = 3, # Seed
                      print_progress = TRUE, # Display progress through nspin?
                      ...){ # Other parameters passed to igraph::spinglass.community()
  
  set.seed(seed)
  
  temp_labs_0 <- qgraph_obj$graphAttributes$Nodes$labels
  temp_labs_1 <- names(temp_labs_0) %>%
    `names<-`(temp_labs_0)
  
  wmat <- qgraph_obj %>% 
    getWmat() %>%
    `colnames<-`(temp_labs_1[colnames(.)]) %>%
    `rownames<-`(temp_labs_1[rownames(.)])
  
  hclust_0 <- wmat %>%
    abs() %>%
    `-`() %>%
    as.dist() %>%
    hclust(method = "single")

  grps_0 <- cutree(hclust_0, h = -0.01)
  
  grps_size <- grps_0 %>%
    table()
  
  big <- names(grps_size)[grps_size > 1]
  single <- names(grps_size)[grps_size == 1]

  groups_spin <- lapply(1:nspin,
                        function(x){
                          
                          if(print_progress){
                            print(x)
                          }
                          
                          grps_0_1 <- grps_0
                          for(i in seq_along(big)){
                            
                            temp_sel <- grps_0[grps_0 == big[i]] %>%
                              names()
                            
                            spinmat <- wmat[temp_sel, temp_sel]
                            
                            group_spin <- spinglass.community(graph_from_adjacency_matrix(spinmat,
                                                                                          mode = "undirected",
                                                                                          weighted = TRUE),
                                                              gamma = gamma,
                                                              ...)
                            
                            grps_0_1[temp_sel] <- as.character(group_spin$membership + 100^i)
                            
                          }
                          
                          group_spin <- grps_0_1 %>%
                            ordered() %>%
                            as.numeric()
                          
                          group_spin})

  
  mats <- lapply(groups_spin, function(x){
    
    temp_vec <- x
    
    lapply(temp_vec,
           function(x){x == temp_vec}) %>%
      unlist() %>%
      matrix(ncol = length(temp_vec))})
  
  mat <- Reduce("+", mats) %>%
    `/`(nspin) %>%
    `colnames<-`(names(grps_0)) %>%
    `rownames<-`(names(grps_0))
  
  dmat <- as.dist(1-mat)
  
  clust <- hclust(dmat, method = hclust.method)
  dendro <- as.dendrogram(clust)
  ddata <- dendro_data(dendro, type = "rectangle")
  groups_tree <- cutree(clust,
                        h = 1-cut)
  
  return(list(hclust = clust, # hclust() output from shared community matrix
         data_dendro = ddata, # ggdendro::dendro_data() output
         groups = groups_tree, # Resulting groups
         mat = mat)) # Spinglass shared community matrix
}

# plot dendro -------------------------------------------------------------

# A function producing a dendrogram in ggplot using data from ggdendro::dendro_data() where data_dendro$labels contain variable names used in par_list

# par_list contains:
# $varlabs[name] - a named vector of labels used for each item with variable *name* as names 
# $color[[color]][name] - a list containing a named character vector of item colors with *name* as names. The vector in the list is names the sames as the *color* argument
# $cut - Value used for cutting hierarchical clusters into groups (h parameter in cutree())

plt_dendro <- function(data_dendro,
                       par_list,
                       color,
                       name.labs = list(x = "Položka", 
                                        y = "Pravděpodobnost spoluvýskytu v komunitě")){
  ggplot(segment(data_dendro)) + 
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend),
                 lwd = 1, 
                 lineend = "round") + 
    geom_segment(data = data_dendro$labels %>% 
                 as_tibble(),
               aes(x = x, 
                   y = y-.1,
                   xend = x,
                   yend = y), 
               inherit.aes = FALSE,
               lwd = 1, 
               lineend = "round"
               ) +
    geom_point(data = data_dendro$labels %>% 
                 as_tibble(),
               aes(x = x, 
                   y = y-.1,
                   fill = I(par_list$color[[color]][label])), 
               inherit.aes = FALSE,
               pch = 21,
               cex = 6) +
    geom_text(data = data_dendro$labels %>% 
                as_tibble(),
              aes(x = x, 
                  y = y-.1,
                  label = par_list$varlabs[label]), 
              inherit.aes = FALSE,
              cex = 2) +
    geom_hline(yintercept = 1 - par_list$cut, 
               lty = 2, 
               lwd = .5) +
    theme_classic() +
    theme(legend.position = "bottom", 
          panel.grid.major = element_line(),
          panel.grid.minor = element_line()) +
    scale_y_continuous(breaks = seq(0,1, .2), labels = c(seq(1,0, -.2))) +
    labs(x = name.labs$x, y = name.labs$y) +
    coord_cartesian(ylim = c(-.15,1))
}


# Write lavaan cfa model ------------------------------------------------------

# A function that transforms groups from par_list into a lavaan syntax for a CFA, where each group forms one factor

# par_list contains:
# $groups[[group]][name] - a list containing a named character vector of item groups with *name* as names. The vector in the list is names the sames as the *group* argument


lav_mod <- function(par_list, # A par_list object
                    exclude = "", # List of variable names to exclude from the model
                    groups = 2,  # A group name or index in the par_list
                    add = "", # Character string of lavaan syntax to be added to the model
                    minvar = 2){ # Minimal number of variables required to create a unique factor
  lapply(par_list$factors,
         function(x){
           t1 <- names(par_list$groups[[groups]])[par_list$groups[[groups]] == x] %>%
             .[!. %in% exclude] 
           if(length(t1) > minvar){
             t1 %>%
               paste0(collapse = " + ") %>%
               paste0(gsub("Komunita ", "K", x), " =~ ", .)
           }
         }) %>%
    unlist() %>%
    paste0(collapse = "\n") %>%
    paste0(., "\n", add)
} 


# fit cfa model -----------------------------------------------------------

# A function that fits a lavaan model with cfa on a training and testing data

fit_lav <- function(model, # Lavaan syntax character
                    data, # Data 
                    par_list, # par_list object
                    train = NULL, # Row indices of cases used for training, if NULL, all three models use the full dataset 
                    Coef_vec = c(alpha = "Alpha (Cronbach, 1951)",
                                 alpha.ord = "Ordinální alpha (Zumbo et al., 2007)",
                                 omega = "Omega (Bollen, 1980)",
                                 omega2 = "Omega (Bentler, 1972)",
                                 omega3 = "Omega (McDonald, 1999)",
                                 avevar = "Average variance extracted (Fornell & Larcke, 1981)"),
                    ordered = "", # Names of ordered variables
                    ...){
  # Fit the model on the training data
  
  if(is.null(train)){
    train_f <- 0
    train2 <- 1:nrow(data)
  }else{
    train_f <- 1
    train2 <- train
  }
  
  fit_cfa_0 <- cfa(model,
                   ordered = ordered,
                   data[train2,] %>%
                     mutate_all(as.numeric),
                   std.lv = TRUE,
                   ...)
  
  # Re-fit the model on a testing sample
  
  if(!is.null(train)){
    
  
  fit_cfa <- cfa(fit_cfa_0,
                 ordered = ordered,
                 data[-train,] %>%
                   mutate_all(as.numeric),
                 std.lv = TRUE,
                 ...)
  
  # Re-fit on full sample
  
  fit_cfa_1 <- cfa(fit_cfa_0,
                   ordered = ordered,
                   data %>%
                     mutate_all(as.numeric),
                   std.lv = TRUE,
                   ...)
  }else{
    fit_cfa <- fit_cfa_0
    fit_cfa_1 <- fit_cfa_0
  }
  
  tab_fit <- fitmeasures(fit_cfa) %>%
    .[grepl("chisq|df|^pvalue$|tli|cfi|rmsea|TLI|CFI", names(.))] %>%
    .[!grepl("robust|scaled|scaling|baseline", names(.))] %>%
    `names<-`(gsub("\\.", " ", names(.))) %>%
    `names<-`(gsub("value", "", names(.))) %>%
    `names<-`(toupper(names(.))) %>%
    `names<-`(gsub("RMSEA CI", "CI", names(.))) %>%
    `names<-`(gsub("DF", "df", names(.))) %>%
    `names<-`(gsub("CHISQ", "Chisq", names(.))) %>%
    `names<-`(gsub("UPPER", "Upper", names(.))) %>%
    `names<-`(gsub("LOWER", "Lower", names(.))) %>%
    `names<-`(gsub("^P$|P$", "p", names(.))) %>%
    as_tibble_row()
  
  rels <- reliability(fit_cfa_1) 
  
  tab_rel <- rels %>%
    as_tibble() %>%
    mutate(Koeficient = Coef_vec[rownames(rels)]) %>%
    select(Koeficient, everything()) 
  
  loads <- lavInspect(fit_cfa, "std")$lambda %>% 
    as_tibble() %>% 
    as.matrix() %>% 
    `rownames<-`(par_list$varlabs[rownames(lavInspect(fit_cfa, "std")$lambda)])
  
  load_labs <- rownames(lavInspect(fit_cfa, "std")$lambda)
  
  tab_loads <- loads %>%
    as_tibble(rownames = NA) %>%
    mutate_all(function(x){
      temp_1 <- as.numeric(x) %>%
        round(3) 
      if_else(temp_1 == 0, "", as.character(temp_1))}) %>%
    mutate(Položka = rownames(.)) %>%
    select(Položka, everything()) %>%
    `colnames<-`(gsub("K", "Komunita ", colnames(.)))
  
  list(fits = list(f_train = fit_cfa_0, # Model fit based on training data
                   f_test = fit_cfa, # Refitted trained model on testing data
                   f_all = fit_cfa_1), # Trained model refitted on all data
       tabs = list(rels = rels, # Reliabilities computed from the full data model fit object
                   loads = loads), # Loadings
       tabs_print = list(rels = tab_rel, # Reliabilities for print
                         loads = tab_loads, # Loadings for print
                         fit = tab_fit), # Fit summary for print
       load_labs = load_labs, # variable names for loadings
       train_flag = train_f) # Flag indicating, whether training took place
  
}


# sem plot ----------------------------------------------------------------

# Produce sem_plots from a lavvan fit object

sem_plots <- function(lav_fit, # fit object, e.g. from lavaan::cfa
                      par_list, # par_list object
                      group = 1, # par_list group index
                      color = 1, # par_list color index
                      bifactor = NULL){ # character name of a general factor in a bifactor analysis
  
  graph_sem <- semPaths(lav_fit, 
                                 "std", 
                                 DoNotPlot = TRUE,
                                     intercepts = FALSE,
                                     residuals = FALSE,
                                    bifactor = bifactor,
                                     layout = ifelse(is.null(bifactor), "circle", "tree2"), 
                                     whatLabels = "std",
                                     nodeLabels = par_list$fvarlabs[par_list$fnames[[group]]],
                                     color = par_list$color[[color]][par_list$fnames[[group]]])

  graph_sem_str <- lavInspect(lav_fit, "cor.lv") %>%
    .[!colnames(.)%in%bifactor,!colnames(.)%in%bifactor] %>%
    qgraph(layout = "spring",
           repulsion = .5,
           labels = par_list$fvarlabs[par_list$fnames_s[[group]]][colnames(.)],
           color = par_list$color[[color]][par_list$fnames_s[[group]]][colnames(.)], 
           DoNotPlot = TRUE)
  

  sem_cmat <- lavInspect(lav_fit, "cor.lv") %>%
    melt() %>%
    filter(!Var1 %in% bifactor) %>%
    filter(!Var2 %in% bifactor) %>%
    as_tibble() %>%
    mutate(Var1 = ordered(Var1, unique(Var1)),
           Var2 = ordered(Var2, unique(Var2)),
           value = ifelse(parse_number(as.character(Var1)) >= parse_number(as.character(Var2)), NA, value)) %>%
    na.omit() %>%
    ggplot(aes(x = Var1, y = Var2, fill = value, label = round(value,3))) +
    geom_tile(color = "black", show.legend = FALSE) +
    geom_text(aes(color = value > .7), cex = 3, show.legend = FALSE) +
    scale_fill_gradient2(mid = "white", midpoint = 0, high = "darkgreen") +
    theme_classic() +
    labs(x = "", y = "", fill = "Korelační koeficient") +
    scale_color_manual(values = c("black", "white") %>% `names<-`(c("FALSE", "TRUE"))) +
    coord_fixed()

  list(graph_sem = graph_sem, # semplot
       graph_sem_str = graph_sem_str, # structural semplot
        sem_cmat = sem_cmat) # ggplot with latent covariance matrix
  
} 

# scores ------------------------------------------------------------------

# A function producing predicted scores and summaries for absolute score value interpretation

# par_list contains
# $groups[[group]][name] - a list containing a named character vector of item groups with *name* as names. The vector in the list is names the sames as the *group* argument
# $varlabs[name] - a named vector of labels used for each item with variable *name* as names 
# $lab_vec - a vector of labels for item response levels
# $col_vec - a vector of colors for item response levels


get_scores <- function(data, # Data
                       fit_obj, # lavaan fit object
                       par_list, # par_list object
                       group = c(2,1), # groups of variables, first group should be equivalent to factor structure
                       color = 3, # par_list color index
                       mimp = 5, # number of imputations used in mice::mice() for missing data
                       MaxNWts = 2000, # nnet.MaxNWts parameter in mice::mice()
                       seed = 1, # seed for multiple imputations
                       lab_vec_ord = NULL, # an optional vector for ordered item response labels
                       col_vec_ord = NULL, # an optional vector for ordered item response colors
                       name.labs = list(
                         factor.name.orig = "K",
                         factor.name = "Komunita",
                         sumfac.name = c(sum = "Součtový", 
                                         fac = "Faktorový"),
                         sumfac.gg = list(fill = "Počet pozorování"),
                         thresh.gg = list(x = "Skupina", 
                                          y = "Hodnota faktorového skóru",
                                          color = "Hranice"),
                         thresh.gg2 = list(other.lab = "Ostatní",
                                           plot.labs = list(fill = "Odpověď",
                                                            x = "",
                                                            y = "Podíl")),
                         int.gg = list(x = "Skupina", 
                                       y = "Hodnota faktorového skóru (průměr a standardní odchylka)"),
                         int.gg2 = list(x = "", 
                                        y = 'Predikovaná odpověď'))
){ 
  
  if(is.null(lab_vec_ord)){
    lab_vec_ord <- par_list$lab_vec
  }
  if(is.null(col_vec_ord)){
    col_vec_ord <- par_list$col_vec
  }
  
  impdat <- mice(data,
                 m = mimp, 
                 seed = seed, 
                 nnet.MaxNWts = MaxNWts)
  
  impdat_use <- complete(impdat,1) %>%
    as_tibble() %>%
    mutate_all(base::ordered)
  
  mdpat <- md.pattern(data, plot = FALSE)
  
  scores <- lavPredict(fit_obj, 
                       newdata = impdat_use,
                       method = "EBM")
  
  sumscores <- lapply(gsub(name.labs$factor.name.orig, paste0(name.labs$factor.name, " "), colnames(scores)),
                          function(x){
                            rowMeans(impdat_use[,par_list$groups[[group[1]]] == x] %>%
                                       mutate_all(as.numeric))
                          }) %>%
    as.data.frame() %>%
    `names<-`(paste0("s_", colnames(scores))) %>%
    as_tibble()
  
  sumscores_data <- scores %>%
    as_tibble() %>%
    bind_cols(sumscores) %>%
    mutate_all(as.numeric) 
  
  sumscores_dat <- sumscores_data %>%
    mutate(id = 1:n()) %>%
    pivot_longer(cols = -id) %>%
    mutate(Score = ifelse(grepl("s_", name), name.labs$sumfac.name["sum"], name.labs$sumfac.name["fac"]),
           Factor = gsub("s_", "", name)) %>%
    select(-name) %>%
    pivot_wider(id_cols = c(id, Factor),
                names_from = Score,
                values_from = value) %>%
    mutate(Factor = gsub(name.labs$factor.name.orig, paste0(name.labs$factor.name, " "), Factor)) %>%
    arrange(parse_number(Factor)) %>%
    mutate(Factor = ordered(Factor, unique(Factor)))
  
  sumscores_cors <- sumscores_dat %>%
    group_by(Factor) %>%
    summarise(Correlation = cor(!!sym(name.labs$sumfac.name["sum"]), 
                                !!sym(name.labs$sumfac.name["fac"]))) %>%
    mutate(Correlation = paste0("r = ", round(Correlation, 3)))
  
  sumscores_plt <- sumscores_dat %>%
    ggplot(aes(x = !!sym(name.labs$sumfac.name["sum"]), y = !!sym(name.labs$sumfac.name["fac"]))) +
    geom_hex(binwidth = c(.3,.3)) +
    facet_wrap(~Factor) +
    geom_smooth(color = "red", 
                method = "gam",
                formula = y ~ s(x, bs = "cs")) +
    geom_label(data = sumscores_cors, aes(label = Correlation,
                                          group = 1),
               x = min(unlist(sumscores_dat[,name.labs$sumfac.name["fac"]])), 
               y = median(unlist(sumscores_dat[,name.labs$sumfac.name["sum"]])),
               inherit.aes = FALSE, 
               hjust = 0) +
    labs(fill = name.labs$sumfac.gg$fill) +
    scale_fill_gradientn(colours = par_list$col_vec) +
    theme(legend.position = "bottom")
  
  # Ordinal variables  

  loadings_0_0 <- lavInspect(fit_obj, "coef")$lambda 
  
  loadings_0 <- loadings_0_0 %>%
    as_tibble() %>%
    mutate_all(as.numeric) %>%
    mutate(var = rownames(loadings_0_0)) %>%
    pivot_longer(-var) %>%
    mutate(name = gsub(name.labs$factor.name.orig, paste0(name.labs$factor.name, " "), name)) %>%
    rename(loading = value)

  ords <- lavInspect(fit_obj, "ordered")
  
  threshold_0 <- lavInspect(fit_obj, "th") %>%
    enframe() %>%
    mutate(name3 = gsub(".+\\|(.+)", "\\1", name),
           name2 = gsub("(.+)\\|.+", "\\1", name)) %>%
    filter(name2 %in% ords)
           
  if(nrow(threshold_0) > 0){
    
    thresholds <- threshold_0 %>%
      mutate(group1 = par_list$groups[[group[1]]][name2],
             group2 = par_list$groups[[group[2]]][name2],
             id = par_list$varlabs[name2]) %>%
      left_join(loadings_0, c("group1" = "name", "name2" = "var")) %>%
      mutate(value = value/loading)
    
    t_vec <- c(thresholds$name3 %>% unique()) %>%
      parse_number()
    
    t_labs <- paste0(lab_vec_ord[t_vec], "|", lab_vec_ord[t_vec + 1]) %>%
      `names<-`(c(thresholds$name3 %>% unique()))
    
    plt_thresh_1 <- thresholds %>%
      group_by(group1) %>%
      arrange(value) %>%
      mutate(mark = 1:n(),
             mark = as.numeric(mark %% 2 == 0)) %>%
      ungroup() %>%
      mutate(group1 = group1 %>%
               enc2utf8() %>%
               str_wrap(20)) %>%
      ggplot(aes(x = group1, 
                 y = value, 
                 color = ordered(name3),
                 label = id)) +
      geom_point(pch = "|", stroke = 20) +
    geom_label(aes(vjust = mark), show.legend = FALSE) +
    geom_vline(aes(xintercept = group1), lty = 2) +
    coord_flip() +
    scale_color_manual(values = col_vec_ord[!col_vec_ord %in% c("white", "#ffffff")][t_vec], 
                       labels = t_labs,
                       name = name.labs$thresh.gg$color) +
    theme(legend.position = "bottom") +
    labs(x = name.labs$thresh.gg$x, 
         y = name.labs$thresh.gg$y)
  
  plt_thresh_2 <- thresholds %>%
    group_by(group2) %>%
    arrange(value) %>%
    mutate(mark = 1:n(),
           mark = as.numeric(mark %% 2 == 0)) %>%
    ungroup() %>%
    mutate(group2 = group2 %>%
             enc2utf8() %>%
             str_wrap(20)) %>%
    ggplot(aes(x = group2, 
               y = value, 
               color = ordered(name3),
               label = id)) +
    geom_point(pch = "|", stroke = 20) +
    geom_label(aes(vjust = mark), show.legend = FALSE) +
    geom_vline(aes(xintercept = group2), lty = 2) +
    coord_flip() +
    scale_color_manual(values = col_vec_ord[!col_vec_ord %in% c("white", "#ffffff")][t_vec], 
                       labels = t_labs,
                       name = name.labs$thresh.gg$color) +
    theme(legend.position = "bottom") +
    labs(x = name.labs$thresh.gg$x, 
         y = name.labs$thresh.gg$y)
  
  data_cuts <- sumscores_dat %>%
    inner_join(thresholds %>% 
                 mutate(thresh = "thresh") %>%
                 pivot_wider(id_cols = c(group1, name2),
                             names_from = thresh,
                             values_from = value,
                             values_fn = list),
               c("Factor" = "group1")
    ) %>%
    rowwise() %>%
    mutate(thresh = list(c(-Inf, sort(unlist(thresh, recursive = TRUE)), Inf)),
           cut_val = cut(!!sym(name.labs$sumfac.name["fac"]), unlist(thresh), ordered_result = TRUE, include_lowest = TRUE, labels = lab_vec_ord)) %>%
    select(id, name2, cut_val) %>%
    pivot_wider(id_cols = id, names_from = name2, values_from = cut_val) %>%
    select(-id)

  cuts_var_sel <- thresholds %>%
    filter(name3 == "t1") %>%
    group_by(group1) %>%
    filter(value %in% c(min(value), max(value))) %>%
    arrange(value) %>%
    ungroup() %>%
    select(name2) %>%
    unlist()
  
  par_list2 <- par_list
  par_list2$col_vec <- col_vec_ord
  par_list2$lab_vec <- lab_vec_ord
  
  plt_thresh_rescaled <- freq_plot(data_cuts[,cuts_var_sel],
            par_list = par_list2,
            group = group[1],
            wrap = 40, 
            varlab = "nodeNames",
            prop_geom = "text",
            v.order = cuts_var_sel,
            other.lab = name.labs$thresh.gg2$other.lab,
            plot.labs = name.labs$thresh.gg2$plot.labs)
  
  
  }else{
    plt_thresh_1 <- NULL
    plt_thresh_2 <- NULL
    plt_thresh_rescaled <- NULL
    data_cuts <- NULL
  }
  
  vars_0 <- lavInspect(fit_obj, "cov.ov") %>%
    diag() %>%
    enframe() %>%
    rename(Variance = value)
  
  intercept_0 <- lavInspect(fit_obj, "mu") %>%
    enframe()  %>%
    left_join(vars_0, "name") %>%
    filter(!name %in% ords)
  
  if(nrow(intercept_0) > 0){
    
  intercepts <- intercept_0 %>%
    mutate(group1 = par_list$groups[[group[1]]][name],
           group2 = par_list$groups[[group[2]]][name],
           id = par_list$varlabs[name])

  plt_ints_1 <- intercepts %>%
    mutate(group1 = group1 %>%
             enc2utf8() %>%
             str_wrap(20)) %>%
    group_by(group1) %>%
    arrange(value) %>%
    mutate(mark = 1:n()) %>%
    arrange(mark) %>%
    ggplot(aes(x = group1,
               ymin = value - sqrt(Variance),
               y = value,
               ymax = value + sqrt(Variance),
               color = value,
               label = id)) +
    geom_pointrange(pch = "|", 
                    stroke = 20, 
                    show.legend = FALSE,
                    position = position_dodge2(reverse = TRUE, 
                                               width = .5)) +
    geom_label(show.legend = FALSE,
               position = position_dodge2(reverse = TRUE, 
                                          width = .5)) +
    geom_vline(aes(xintercept = group1), lty = 2) +
    scale_color_gradientn(colors = par_list$col_vec) +
    coord_flip() +
    theme(legend.position = "bottom") +
    labs(x = name.labs$int.gg$x, 
         y = name.labs$int.gg$y,
         colot = name.labs$int.gg$color)

  plt_ints_2 <- intercepts %>%
    mutate(group1 = group2 %>%
             enc2utf8() %>%
             str_wrap(20)) %>%
    group_by(group1) %>%
    arrange(value) %>%
    mutate(mark = 1:n()) %>%
    arrange(mark) %>%
    ggplot(aes(x = group1,
               ymin = value - sqrt(Variance),
               y = value,
               ymax = value + sqrt(Variance),
               color = value,
               label = id)) +
    geom_pointrange(pch = "|", 
                    stroke = 20, 
                    show.legend = FALSE,
                    position = position_dodge2(reverse = TRUE, 
                                               width = .5)) +
    geom_label(show.legend = FALSE,
               position = position_dodge2(reverse = TRUE, 
                                          width = .5)) +
    geom_vline(aes(xintercept = group1), lty = 2) +
    scale_color_gradientn(colors = par_list$col_vec) +
    coord_flip() +
    theme(legend.position = "bottom") +
    labs(x = name.labs$int.gg$x, 
         y = name.labs$int.gg$y,
         colot = name.labs$int.gg$color)
  
  ints_var_sel <- intercepts %>%
    group_by(group1) %>%
    arrange(value) %>%
    mutate(ord = 1:n()) %>%
    filter(ord %in% c(1, n())) %>%
    ungroup() %>%
    select(name) %>%
    unlist()
  
  intercept_loads <- loadings_0 %>%
    left_join(intercepts %>%
                select(name, value),
              c("var" = "name"))
  
  data_interval <- sumscores_dat %>%
    rename(score = !!sym(name.labs$sumfac.name["fac"])) %>%
    select(id, Factor, score) %>%
    inner_join(intercept_loads, 
               c("Factor" = "name")) %>%
    group_by(id, var) %>%
    summarise(score_mod = sum(score*loading),
              intercept = unique(value)) %>%
    ungroup() %>%
    mutate(value = score_mod + intercept)
  
  plt_interval_rescaled <- data_interval %>%
    filter(var %in% ints_var_sel) %>%
    mutate(var2 = par_list$nodeNames[var] %>%
             enc2utf8() %>%
             str_wrap(40),
           var = ordered(var, ints_var_sel)) %>%
    na.omit() %>%
    arrange(var) %>%
    mutate(var2 = ordered(var2, unique(var2)),
           group = par_list$groups[[group[1]]][as.character(var)]) %>%
    ggplot(aes(x = var2, y = value, fill = value)) +
    geom_point(position = position_jitter(height = .5), 
               pch = 21, 
               cex = 2,
               color = "black",
               show.legend = FALSE) +
    geom_boxplot(outlier.color = NA, width = .5, lwd = 1.2) +
    facet_wrap(~group, scales = "free_y", ncol = 1) +
    coord_flip() +
    scale_y_continuous(breaks = parse_number(par_list$lab_vec),
                       labels = par_list$lab_vec) +
    scale_fill_gradientn(colors = par_list$col_vec) +
    theme(legend.position = "none") +
    labs(x = name.labs$int.gg2$x, y = name.labs$int.gg2$y)
  
  
  }else{
    plt_ints_1 <- NULL
    plt_ints_2 <- NULL
    plt_interval_rescaled <- NULL
    data_interval <- NULL
    
  }

  return(list(
    data = list(impdat = impdat_use, # imputed raw data
                data_interval = data_interval, # transformed interval data
                data_cuts = data_cuts, # transformed ordinal data
                sumscores_data = sumscores_data), # factor and sum scores
    mdpat = mdpat, # missing data pattern
    sumscores_plt = sumscores_plt, # sumscores vs factor scores
    plt_ints_1 = plt_ints_1, # observed implied intercepts and variance, groups 1
    plt_ints_2 = plt_ints_2, # observed implied intercepts and variance, groups 2
    plt_interval_rescaled = plt_interval_rescaled, # observed predicted responses, interval
    plt_thresh_1 = plt_thresh_1, # observed thresholds, groups 1
    plt_thresh_2 = plt_thresh_2, # observed thresholds, groups 2
    plt_thresh_rescaled = plt_thresh_rescaled # observed predicted responses, ordinal
  ))

}


# groups match -----------------------------------------------------------------
# Show frequencies of common item membership between two group vectors

groups_match <- function(par_list, # par_list object
                         groups, # par_list groups index, a numeric or character vector of length 2 
                         color){ # par_list color index, a numeric or character of length 1
  
  tibble(group1 = par_list$groups[[groups[1]]][names(par_list$varlabs)],
         group2 = par_list$groups[[groups[2]]][names(par_list$varlabs)]) %>%
    na.omit() %>%
    group_by(group1, group2) %>%
    summarise(cnt = n()) %>%
    ungroup() %>%
    mutate(group1 = ordered(group1, unique(par_list$groups[[groups[1]]]))) %>%
    mutate(group2 = ordered(group2, unique(par_list$groups[[groups[2]]]))) %>%
    ggplot(aes(y = group2, x = group1, fill = group1, label = cnt)) +
    geom_tile(color = "black") +
    geom_text(color = "black") +
    theme_classic() +
    labs(x = "", y = "") +
    theme(legend.position = "none", 
          panel.grid.major = element_line(), 
          panel.grid.minor = element_line()) +
    coord_fixed() +
    scale_fill_manual(values = par_list$color[[color]])
} 


# contrast color ---------------------------------------------------------

## A function, determining the degree of contrastt of white, given a background color

br_fun <- function(x){
  x <- col2rgb(x) %>%
    `row.names<-`(NULL)
  sqrt((.299*(x[1,]^2)) + (.587*(x[2,]^2)) + (.114*(x[3,]^2)))/255
}


# qgraph to ggplot --------------------------------------------------------
# Creates a ggplot out of a qgraph object

q2g <- function(qgraph_object, # insert qgraph
                edge_distribution = TRUE, # show edge weight distribution?
                zero_edge = FALSE, # include zero edges in the distribution?
                directed = FALSE, # is the graph directed?
                edge_max = 1, # highest possible absolute edge value for reference (1 for correlation coefficients)
                vsize = 1, # vertex size
                vlab.size = 1, # vertex label size (relative to vertex size)
                esize = 3, # edge size
                elabs = FALSE, # show edge labels?
                elabs.pos = 2/3, # edge label position 
                threshold = 0, # threshold for reducing edges to zero
                bw = .05, # bandwidth for edge distribution
                splits = NULL, # vector of thresholds for creating graphs separated by edge strenghts
                split_col = 2, # display split graph in n columns
                rm.lav.cov = FALSE,# remove latent covariance edges (semPaths only, currently, latent covariance edges are not curved) 
                ggextra = NULL){# any extra + geoms, themes etc?
  
  require(ggforce)
  vsize <- vsize/20
  vlab.size0 <- vlab.size
  vlab.size <- 65*vlab.size
  delt <- vsize
  
  D_1 <- bind_cols(qgraph_object$layout[qgraph_object$Edgelist$from,] %>% as_tibble() %>% `colnames<-`(c("x", "y")),
                   qgraph_object$layout[qgraph_object$Edgelist$to,] %>% as_tibble() %>% `colnames<-`(c("xend", "yend")),
                   tibble(weight = qgraph_object$Edgelist$weight,
                          curve = qgraph_object$graphAttributes$Edges$curve)) %>%
    mutate(id_to = qgraph_object$Edgelist$to,
           weight = ifelse(abs(weight) < threshold, 0, weight),
           xcent = x + (elabs.pos*(xend - x)), 
           ycent = y + (elabs.pos*(yend - y)),
           x_dir = (xend-x)/sqrt(((xend-x)^2) + ((yend-y)^2)),
           y_dir = (yend-y)/sqrt(((xend-x)^2) + ((yend-y)^2))) %>%
    mutate(rel2 = case_when(x_dir > sin(pi*.25) ~ 1,
                            x_dir < sin(pi*1.75) ~ 3,
                            y_dir > cos(pi*1.75) ~ 2,
                            y_dir < cos(pi*.25) ~ 4),
           x_dir2 = case_when(rel2 == 2 ~ sin(atan(x_dir/y_dir)),
                              rel2 == 1 ~ (1/sqrt(2)),
                              rel2 == 4  ~ sin(atan(-x_dir/y_dir)),
                              rel2 == 3 ~ -(1/sqrt(2))),
           y_dir2 = case_when(rel2 == 2 ~ (1/sqrt(2)), 
                              rel2 == 1 ~ sin(atan(y_dir/x_dir)),
                              rel2 == 4 ~ -(1/sqrt(2)),
                              rel2 == 3 ~ sin(atan(-y_dir/x_dir)))) %>%
    filter(weight != 0)
  
  
  if(rm.lav.cov){
    D_1 <- D_1 %>%
      mutate(filt = !qgraph_object$Edgelist$bidirectional) %>%
      filter(filt) %>%
      select(-filt)
  }

  D_2 <- bind_cols(qgraph_object$layout %>% `colnames<-`(c("x", "y")) %>% as_tibble(),
                   tibble(color = qgraph_object$graphAttributes$Nodes$color,
                          label = qgraph_object$graphAttributes$Nodes$labels,
                          shape = qgraph_object$graphAttributes$Nodes$shape %>% paste0(., " filled"))) %>%
    mutate(label.color = ifelse(br_fun(color)  > .6, "black", "white"))
  

  if(zero_edge){
    D_temp <- D_1[rep(1,((nrow(D_2)^2)-nrow(D_2))/2),] %>%
      mutate_all(function(x){NA}) %>%
      mutate(weight = 0) %>%
      mutate(weight = ifelse(is.na(weight) < threshold, 0, weight))
    
    D_temp[1:nrow(D_1),] <- D_1
    
    D_1 <- D_temp
  }
  
  if(!is.null(splits)){
    D_1 <- D_1 %>%
      mutate(split = cut(abs(weight), 
                         breaks = c(splits,1), 
                         include.lowest = TRUE, 
                         ordered_result = TRUE))
    
    D_2 <- lapply(unique(D_1$split), function(z){
      D_2 %>%
        mutate(split = z)
      
    }) %>%
      bind_rows()
  }
  
  D_1 <- D_1 %>%
    mutate(shape_to = D_2$shape[id_to]) %>%
    mutate(xend_use = case_when(grepl("circle", shape_to) ~ xend - (x_dir * delt),
                                grepl("square", shape_to) ~ xend - (x_dir2 * delt)),
           yend_use = case_when(grepl("circle", shape_to) ~ yend - (y_dir * delt),
                                grepl("square", shape_to) ~ yend - (y_dir2 * delt)))
  
  ggplot_object <- D_1 %>%
    arrange(abs(weight)) %>%
    ggplot(aes(x = x, 
               y = y, 
               xend = xend_use, 
               yend = yend_use))

  if(directed){
  ggplot_object <- ggplot_object +
    geom_segment(aes(size = abs(weight), 
                     color = weight), 
                 lineend = "butt", 
                 linejoin = "mitre", 
                 arrow = arrow(type = "closed",
                               length = unit(esize * 1, "pt"))) 
  }else{
    ggplot_object <- ggplot_object +
      geom_segment(aes(size = abs(weight), 
                       color = weight,
                       xend = xend,
                       yend = yend), 
                   lineend = "round", 
                   linejoin = "mitre") 
  }
  
  if(sum(grepl("circle", D_2$shape))>0){
    
    ggplot_object <- ggplot_object +
      geom_circle(data = D_2 %>%
                    filter(grepl("circle", shape)), 
                  aes(x0 = x, 
                      y0 = y, 
                      r = vsize, 
                      fill = I(color)), 
                  cex = vsize*15*vlab.size0, 
                  inherit.aes = FALSE)
    
  }
  
  if(sum(grepl("square", D_2$shape))>0){
    ggplot_object <- ggplot_object +
      geom_regon(data = D_2 %>%
                 filter(grepl("square", shape)), 
                 aes(x0 = x, 
                     y0 = y, 
                     r = vsize, 
                     sides = 4, 
                     fill = I(color), 
                     angle = 0), 
                 color = "black", 
                 cex = vsize*15*vlab.size0, 
                 inherit.aes = FALSE)
  } 
  
  ggplot_object <- ggplot_object +
    geom_text(data = D_2, 
              aes(x = x, 
                  y = y, 
                  label = label),
              color = D_2$label.color,
              cex = vsize * vlab.size, 
              inherit.aes = FALSE) 
    

  if(edge_distribution){
    ggplot_object <- ggplot_object +
      geom_ribbon(aes(x = weight/edge_max, 
                      ymin = -1.4, 
                      ymax = ((..density../max(..density..))*.3)-1.4), 
                  color = "black", 
                  alpha = .5, 
                  stat = "density", 
                  bw = bw, 
                  inherit.aes = FALSE) +
      geom_text(aes(x = weight/edge_max, 
                    y = -1.4, 
                    color = weight, 
                    label = "I"), 
                cex = 10, 
                vjust = 0) +
      geom_ribbon(aes(x = weight/edge_max, 
                      ymin = -1.4, 
                      ymax = ((..density../max(..density..))*.3)-1.4), 
                  fill = NA, 
                  color = "black", 
                  alpha = .5, 
                  stat = "density", 
                  bw = bw, 
                  inherit.aes = FALSE) +
      geom_segment(
        aes_all(c('x', 'y', 'xend', 'yend')),
        tibble(x = -1, 
               xend = 1, 
               y = -1.4, 
               yend = -1.4), 
        inherit.aes = FALSE) +
      geom_segment(
        aes_all(c('x', 'y', 'xend', 'yend')),
        tibble(x = seq(-1,1,.25), 
               xend = seq(-1,1,.25), 
               y = rep(-1.4, length(seq(-1,1,.25))), 
               yend = rep(-1.425, length(seq(-1,1,.25)))), 
        inherit.aes = FALSE) +
      geom_text(
        aes_all(c('x', 'y', 'label')),
        tibble(x = seq(-1,1,.5), 
               xend = seq(-1,1,.5), 
               y = rep(-1.425, length(seq(-1,1,.5))), 
               label = seq(-1,1,.5)*edge_max), 
        inherit.aes = FALSE, 
        vjust = 1.5, 
        hjust = .5) +
      coord_fixed(ylim = c(-1.5,1.1), 
                  xlim = c(-1.1, 1.1),
                  expand = TRUE) +
      labs(x = ifelse(zero_edge, "Rozdělení vah všech hran", "Rozdělení vah nenulových hran"))
  }else{
    ggplot_object <- ggplot_object +
      coord_fixed(ylim = c(-1.1,1.1), 
                  xlim = c(-1.1, 1.1),
                  expand = TRUE) +
      labs(x = "", y = "")
  }
  
  if(!is.null(splits)){
    ggplot_object <- ggplot_object +
      facet_wrap(~split,
                 drop = TRUE, 
                 ncol = split_col)
  }
  
  if(elabs){
    ggplot_object <- ggplot_object +
      geom_label(data = D_1, aes(x = xcent, 
                                 y = ycent, 
                                 color = ifelse(weight > 0, 1, -1), 
                                 label = round(weight, 2)),
                 size = esize,
                 label.padding = unit(.1, units = "lines"))
  }
  
  
  ggplot_object <- ggplot_object +
    scale_color_gradient2(low = "darkred", 
                          high = "darkgreen", 
                          mid = "white", 
                          midpoint = 0, 
                          guide = "none") +
    theme_void() +
    scale_size_continuous(range = c(0, esize)) +
    theme(plot.background = element_rect(color = "white", fill = "white"),
          legend.position = "none",
          axis.title.x = element_text(vjust = 1))
    
  if(!is.null(ggextra)){
    ggplot_object <- ggplot_object +
      ggextra
  }

  return(ggplot_object)
}


# qlegend -----------------------------------------------------------------

## Create a ggplot legend from a qgraph

qlegend <- function(qgraph_object, # Original qgraph
                    groups = NULL, # Manually define groups
                    names = NULL, # Manually assign node names
                    wrap = 40, # Sting wrap width
                    ncol = 1, # Number of columns
                    text.size = 4, # Text size
                    linmod = .01, # A constant, added to space assigned based on the number of lines 
                    ptmod = 4, # Modification to computed point size of text
                    wdtmod = 3, # Modification to width computed from text width
                    lineheight = .75, # Spacing between lines
                    group_ord = NULL){ # Manually assigned order of item groups

  
  if(is.null(groups)){
    groups <- qgraph_object$graphAttributes$Graph$groups
  }
  
  if(ncol > length(groups)){
    ncol <- length(groups)
  }
  
  if(is.null(names)){
    names <- qgraph_object$graphAttributes$Nodes$names
  }
  
  if(is.null(group_ord)){
    group_ord <- names(groups)
  }
  
  colleft <- length(groups) %% ncol
  colgroupn <- length(groups) - colleft
  cuts_0 <- c(rep(colgroupn/ncol, ncol)) 
  if(colleft > 0){
    cuts_0[1:colleft] <- cuts_0[1:colleft] + 1
  }
  cuts <- cuts_0  %>% cumsum() %>% c(0, .)
  
  group_tib <- lapply(seq_along(groups), function(x){tibble(pos = groups[[x]], group = names(groups)[x])}) %>%
    bind_rows() %>%
    mutate(group = ordered(group, group_ord)) %>%
    arrange(pos)
  
  D_2 <- tibble(color = qgraph_object$graphAttributes$Nodes$color,
                names = names,
                label = qgraph_object$graphAttributes$Nodes$labels,
                shape = qgraph_object$graphAttributes$Nodes$shape %>% paste0(., " filled")) %>%
    bind_cols(group_tib) %>%
    mutate(label.color = ifelse(br_fun(color)  > .6, "black", "white"),
           names = str_wrap(names, wrap),
           nam_br = gsub("[^\n]*", "", names) %>% str_length() %>% `+`(linmod) %>% `+`(1),
           col = cut(as.numeric(group), 
                     cuts, 
                     include.lowest = TRUE, 
                     ordered.results = TRUE) %>%
             as.numeric()) %>%
    group_by(group) %>%
    mutate(pos2 = cumsum(lag(nam_br,n = 1, default = 0))) %>%
    ungroup() %>%
    arrange(group)

  legend_list_0 <- lapply(unique(D_2$group), function(x){
    temp_D <- D_2 %>%
      filter(group == x) 
    
    ymax <- -sum(temp_D$nam_br)*ggplot2:::.pt*text.size
    xmax <- wrap*ggplot2:::.pt*text.size/wdtmod
    ymax2 <- -str_length(x)*ggplot2:::.pt*text.size*1.2
    if(ymax > ymax2){
      ymax <- ymax2
    }
    
    plt <- temp_D %>%
      ggplot(aes(x = 0, y = -(pos2*ggplot2:::.pt*text.size))) +
      geom_point(aes(pch = I(shape), 
                     fill = I(color)), 
                 cex = text.size, 
                 position = position_nudge(y = -(ggplot2:::.pt*text.size/ptmod))) +
      geom_text(aes(label = paste0(label, ": ", names) %>% gsub("\n", "\n    ", .),
                    x = 40/wdtmod), 
                lineheight = lineheight,
                hjust = 0, 
                vjust = 1, 
                size = text.size) +
      geom_line(aes(x = -(40/wdtmod)), position = position_nudge(y = -(ggplot2:::.pt*text.size/ptmod))) +
      facet_wrap(~group, shrink = FALSE, strip.position = "left") +
      theme_void() +
      coord_fixed(clip = FALSE, expand = FALSE) +
      theme(strip.text = element_text(hjust = 1, 
                                      size = 3*text.size,
                                      margin = margin(0,10,0,5, "pt")),
            panel.spacing.x = unit(0, "lines"),
            panel.spacing.y = unit(1, "lines")) +
      scale_x_continuous(limits = c(-(40/wdtmod), xmax)) +
      scale_y_continuous(limits = c(ymax, 0))
    height <- ymax
    return(list(plt, height))
  })
    
  legend_list <- lapply(legend_list_0, function(x){x[[1]]})
  height_vec <- lapply(legend_list_0, function(x){x[[2]]}) %>%
    unlist()
  
  null_plot <- ggplot(tibble(0), aes(x = 0, y = 0)) +
      theme_void()
    
    heights_group <- D_2 %>%
      select(group, col) %>%
      distinct() %>%
      mutate(hgt = abs(height_vec))
      
    heights_col <- heights_group %>%
      group_by(col) %>%
      summarise(hgt = sum(hgt))
    
    temp_arranges <- lapply(unique(heights_group$col), function(x){
      
      temp_list <- legend_list[heights_group$col == x]
      
      temp_heights_0 <- heights_group$hgt[heights_group$col == x]
      
      if(sum(temp_heights_0) == max(heights_col$hgt)){
        temp_heights <- temp_heights_0
      }else{
        temp_list <- temp_list %>%
          c(list(null_plot))
        
        temp_heights <- c(temp_heights_0, max(heights_col$hgt) - sum(temp_heights_0))
        
      }  
      temp_heights <- temp_heights/sum(temp_heights)
      
      ggarrange(plotlist = temp_list, ncol = 1, heights = temp_heights, widths = rep(1, length(temp_list)))
    })
    
    ggarrange(plotlist = temp_arranges, nrow = 1, widths = rep(1, length(temp_arranges))) +
      theme(plot.background = element_rect(fill = "white", color = "white"), plot.margin = unit(c(15,15,0,0), "pt"))
}


# Create a palette that reflects hierarchical clustering -------------------------------------------------------------------------

## Create a palette based on a hclust object that reflects the similarities between groups of nodes on various levels, assigning different hue angles on the hcl color space


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
    mutate(variable = - variable,
           variable = clust$labels[variable]) %>%
    arrange(variable) %>%
    mutate(angle_modified = angle + min(angle),
           angle_modified = angle_modified/max(angle_modified),
           angle_luminance = ((angle_modified)*(max_luminance - min_luminance)) + min_luminance,
           angle_hue = (angle_modified*max_hue)+start_hue) %>%
    mutate(color = grDevices::hcl(angle_hue, chroma, angle_luminance, alpha = alpha, fixup = TRUE))
  
  return(col_tib)
}


# Rotate a set of coordinates ---------------------------------------------

# This function pivots a graph layout, aligning it's position along an axis
# defined either bz two nodes, or two points on the graph surface

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
