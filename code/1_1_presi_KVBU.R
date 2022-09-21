

tempmat <- qgraph::getWmat(EFF_fit)

varsel <- which(EFF_groups_tree %in% c(1,4))

EFF_fit_sub <- qgraph(tempmat[varsel,varsel], 
#                  graph = "cor",
                  cut = .05,
                  layout = EFF_fit$layout[varsel,],
                  sampleSize = nrow(EFF_dat_mod),
                  groups = paste0("Faktor ", EFF_groups_tree[varsel]),
                  labels = c(1:ncol(EFF_dat))[varsel],
                  nodeNames = EFF_labs[varsel] %>%
  str_wrap(40),
                  pie = EFF_props[varsel],
                  GLratio = .8,
                  legend.cex = .7, 
                  edge.width = .5,
                  palette = "pastel",
                  vsize = 3,
                  details = TRUE,
                  pieColor = 
                    c(
                      list(c("darkred",
                             "pink",
                             "lightgreen",
                             "darkgreen"))[rep(1, length(varsel))])
                  
)


varsel2 <- which(EFF_groups_tree %in% c(1,4))[c(2,10,9)]

EFF_fit_sub2 <- qgraph(tempmat[varsel2,varsel2], 
                      #                  graph = "cor",
                      cut = .05,
                      layout = EFF_fit$layout[varsel2,],
                      sampleSize = nrow(EFF_dat_mod),
                      groups = paste0("Faktor ", EFF_groups_tree[varsel2]),
                      labels = c(1:ncol(EFF_dat))[varsel2],
                      nodeNames = EFF_labs[varsel2] %>%
                        str_wrap(40),
                      pie = EFF_props[varsel2],
                      GLratio = .8,
                      legend.cex = .75, 
                      edge.width = .5,
                      palette = "pastel",
                      vsize = 3,
                      details = TRUE,
                      pieColor = 
                        c(
                          list(c("darkred",
                                 "pink",
                                 "lightgreen",
                                 "darkgreen"))[rep(1, length(varsel2))])
                      
)





png("outputs/reports/2022/plots/17_EXGRAF.png",
    height = 14,
    width = 14,
    res = 360,
    units = "in")
plot(EFF_fit_sub2)
dev.off()

png("outputs/reports/2022/plots/18_EXGRAF2.png",
    height = 14,
    width = 14,
    res = 360,
    units = "in")
plot(EFF_fit_sub)
dev.off()