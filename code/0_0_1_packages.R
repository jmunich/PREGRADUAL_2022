### Check for, install and load required packages

if(!require(pacman)){
  install.packages("pacman")
  require(pacman)
}

p_load(readr,
       readxl,
       tidyr,
       dplyr,
       tibble,
       stringr,
       stringdist,
       reshape2,
       mice,
       igraph,
       qgraph,
       bootnet,
       lavaan,
       semTools,
       semPlot,
       ggplot2,
       ggforce,
       ggpubr,
       ggdendro,
       ggpubr,
       scales,
       RColorBrewer)