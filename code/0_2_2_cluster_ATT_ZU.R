source("code/0_1_1_clean_ZU.R", encoding = "UTF-8")
source("code/2_3_hcols.R", encoding = "UTF-8")
library("bootnet")
library("psychonetrics")
library("qgraph")
library("lavaan")
library("igraph")
library('ggdendro')
library("mice")
library("png")
library("svglite")

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

# Read data and prepare labels --------------------------------------------

# Reading data
## Indicate reverse-coded items; NB: ind 9 non
ATT_revs <- c(1,3,6,7,9,11,13,15,17,19,21,23,26,27,30,32,34,35,37)
ATT_revs_2 <- c(1,4)

ATT_dat_1 <- data_use %>%
  filter(filt_RESPs) %>%
  select(starts_with("TEACHINGATT")) %>%
  mutate_all(parse_number) %>%
  mutate_at(ATT_revs,
             function(x){8 - x})

ATT_lev <- c("Rozhodně nesouhlasím", 
             "Nesouhlasím",          
             "Spíše nesouhlasím",    
             "Spíše souhlasím",      
             "Souhlasím",           
             "Rozhodně souhlasím")

ATT_lev_us <- ATT_lev %>% 
  `names<-`(ATT_lev[6:1])

ATT_dat_2 <- data_use %>%
  filter(filt_RESPs) %>%
  select(starts_with("PUPILMIND")) %>%
  mutate_at(ATT_revs_2, function(x){
    ATT_lev_us[x]
  }) %>%
  mutate_all(ordered, levels = ATT_lev)

ATT_dat_mod <- ATT_dat_1 %>%
  mutate_all(ordered) %>%
  bind_cols(ATT_dat_2)

ATT_dat_group <- data_use %>%
  filter(filt_RESPs) %>%
  select(TIMEEMPLOYEDSCHOOL)

# Creating labels

ATT_labs_1 <- varlabs_2[grepl("TEACHINGATT", varnames)] %>%
  enc2utf8()

ATT_labs_1[ATT_revs] <- paste0(ATT_labs_1[ATT_revs], " (-)") %>% 
  enc2utf8()

ATT_labs_2 <- varlabs_2[grepl("PUPILMIND", varnames)] %>%
  enc2utf8()

ATT_labs_2[ATT_revs_2] <- paste0(ATT_labs_2[ATT_revs_2], " (-)") %>% 
  enc2utf8()

ATT_labs <- c(ATT_labs_1, ATT_labs_2)

ATT_groups <- c(
  rep("1. PLÁNOVÁNÍ VÝUKY", 6),
  rep("2. PROCESY UČENÍ", 12),
  rep("3. PROSTŘEDÍ PRO UČENÍ", 10),
  rep("4. HODNOCENÍ", 10),
  rep("5. SCHOPNOSTI ŽÁKŮ", 4)
) %>%
  enc2utf8()

ATT_lab_tib <- tibble(name = names(ATT_dat_mod),
                      label = ATT_labs,
                      group = ATT_groups) %>%
  mutate(reverse = grepl("\\(-\\)", label))


ATT_lab_tib_print <- ATT_lab_tib %>%
  group_by(group) %>%
  mutate(mark = 1:n()) %>%
  mutate(group = ifelse(mark == 1, group, "")) %>%
  ungroup() %>%
  select(-mark) %>%
  `names<-`(c("Kód", "Položka", "Okruh", "Reverzní")) %>%
  mutate(Kód = 1:n()) %>%
  select(Okruh, Kód, Položka, Reverzní)

# Create descriptive summaries --------------------------------------------------------

ATT_freq_plot <- ATT_dat_mod %>%
  mutate_all(as.numeric) %>%
  pivot_longer(-0) %>%
  na.omit() %>%
  left_join(ATT_lab_tib) %>%
  mutate(name2 = paste0(name, "\n", label) %>%
           enc2utf8() %>%
           str_wrap(40)) %>%
  group_by(name2) %>%
  mutate(ord = sum(value %in% c("1", "2", "3", "4"))/n()) %>%
  ungroup() %>%
  arrange(-ord) %>%
  mutate(name2 = ordered(name2, unique(name2)),
         value = ordered(value)) %>%
  ggplot(aes(x = name2, fill = value)) +
  geom_bar(stat = "count", position = "fill", color = "black") +
  coord_flip() +
  facet_wrap(~group, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("darkred", "red2", "pink", "white", "lightgreen", "green2", "darkgreen")) +
  geom_hline(yintercept = .5, lty = 2) +
  labs(fill = "Odpověď",
       y = "Podíl",
       x = "")

# Proportions of responses per item

ATT_props <- ATT_dat_mod %>%
  mutate_all(as.numeric) %>%
  pivot_longer(-0) %>%
  group_by(name, value) %>%
  summarise(cnt = n()) %>%
  na.omit() %>%
  group_by(name) %>%
  mutate(cnt = cnt/sum(cnt)) %>%
  ungroup() %>%
  pivot_wider(names_from = name,
              values_from = cnt,
              id_cols = value) %>%
  arrange(value) %>%
  select(-value) %>%
  as.list() %>%
  lapply(na.omit) %>%
  .[colnames(ATT_dat_mod)]


# Fit network ------------------------
# Correlation matrix and first model
ATT_cmat <- cor_auto(ATT_dat_mod)

ATT_fit <- qgraph(ATT_cmat, 
                  graph = "glasso",
                 threshold = TRUE,
                  layout = "spring",
                  sampleSize = nrow(ATT_dat_mod),
                   groups = ATT_groups,
                  labels = 1:ncol(ATT_cmat),
                  nodeNames = ATT_labs,
                  pie = ATT_props,
                  GLratio = .8,
                  legend.cex = .28,
                  palette = "pastel",
                  vsize = 3,
                  DoNotPlot = TRUE,
                  details = TRUE,
                  pieColor = 
                    c(
                      list(c("darkred",
                             "red2",
                             "pink",
                             "white",
                             "lightgreen",
                             "green2",
                             "darkgreen"))[rep(1, length(ATT_labs_1))],
                      list(c("darkred",
                             "red2",
                             "pink",
                             "lightgreen",
                             "green2",
                             "darkgreen"))[rep(1, length(ATT_labs_2))])
                  
)

ATT_fit_nl <- qgraph(ATT_cmat, 
                  graph = "glasso",
                  threshold = TRUE,
                  layout = "spring",
                  sampleSize = nrow(ATT_dat_mod),
                   groups = ATT_groups,
                  labels = 1:ncol(ATT_cmat),
                  nodeNames = ATT_labs,
                  pie = ATT_props,
                  legend = FALSE,
                  GLratio = .8,
                  legend.cex = .28,
                  palette = "pastel",
                  vsize = 3,
                  DoNotPlot = TRUE,
                  details = TRUE,
                  pieColor = 
                    c(
                      list(c("darkred",
                             "red2",
                             "pink",
                             "white",
                             "lightgreen",
                             "green2",
                             "darkgreen"))[rep(1, length(ATT_labs_1))],
                      list(c("darkred",
                             "red2",
                             "pink",
                             "lightgreen",
                             "green2",
                             "darkgreen"))[rep(1, length(ATT_labs_2))])
                  
)

# First model fit

ATT_fit_m <- ggmFit(ATT_fit, 
                    covMat = ATT_cmat,
                    sampleSize = nrow(ATT_dat_mod)) %>%
  .$fitMeasures %>%
  .[grepl("chisq|df|^pvalue$|tli|cfi|rmsea", names(.))] %>%
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

# Bootstrap network stability ---------------------------------------------
# Perform bootstrapping

ATT_nboots <- 1000

ATT_boots <- bootnet(ATT_dat_mod %>%
                       mutate_all(as.numeric), 
                     nBoots = ATT_nboots, 
                     default = "glasso", 
                     nCores = parallel::detectCores(),
                     statistics = "edge",
                     corMethod = "cor_auto",
                     threshold = TRUE)

ATT_dat_boots <- summary(ATT_boots) %>%
  left_join(ATT_lab_tib %>%
              mutate(id1 = 1:n()),
            c("node1" = "name")) %>%
  left_join(ATT_lab_tib %>%
              mutate(id2 = 1:n()),
            c("node2" = "name")) %>%
  filter(q2.5 > 0 | q97.5 < 0)



# Plot boot results
ATT_plt_CI_1 <- ATT_dat_boots %>%  
  arrange(mean) %>%
  ungroup() %>%
  mutate(ord = (1:n())-1) %>%
  ggplot(aes(x = ord, y = mean)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(yintercept = .1, lty = 3) +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "grey", alpha = .3) +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5)) +
  geom_text(aes(y = q2.5, label = id1, hjust = 1.2)) +
  geom_text(aes(y = q97.5, label = id2, hjust = -.2)) +
  geom_point(aes(color = "Bootstrapovaný průměr")) +
  geom_point(aes(y = sample, color = "Původní odhad"), 
             show.legend = TRUE) +
  coord_flip() +
  labs(x = "Hrany seřazené podle původního odhadu\nČísla indikují uzly spojené hranou",
       y = "Rozpětí indikuje rozpětí 95% středních hodnot bootstrapu",
       color = "Hodnota") + 
  scale_color_manual(values = c("black", "darkred")) +
  theme(legend.position = "bottom") +
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(breaks = c(seq(0,nrow(ATT_dat_boots),5), nrow(ATT_dat_boots)-1))

ATT_CI_cmat <- summary(ATT_boots) %>%
  ungroup() %>%
  mutate(weight = ifelse(q2.5 > 0 | q97.5 < 0, mean, 0)) %>%
  select(node1, node2, weight) %>%
  qgraph(directed = FALSE, 
         DoNotPlot = TRUE) %>%
  getWmat() %>%
  .[colnames(ATT_cmat),colnames(ATT_cmat)]

ATT_plt_CI_2 <- summary(ATT_boots) %>%
  left_join(ATT_lab_tib %>%
              mutate(id1 = 1:n()),
            c("node1" = "name")) %>%
  left_join(ATT_lab_tib %>%
              mutate(id2 = 1:n()),
            c("node2" = "name")) %>%
  arrange(mean) %>%
  arrange(sample) %>%
  ungroup() %>%
  mutate(ord = (1:n())-1) %>%
  ggplot(aes(x = ord, y = mean)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_hline(yintercept = .1, lty = 3) +
  geom_ribbon(aes(ymin = q2.5, ymax = q97.5), fill = "grey", alpha = .5) +
  geom_point(cex = .1) +
  geom_line(aes(y = sample), color = "darkred", cex = .1) +
  coord_flip() +
  labs(x = "Hrany seřazené podle původního odhadu\nČísla indikují uzly spojené hranou",
       y = "Rozpětí indikuje rozpětí 95% středních hodnot bootstrapu",
       color = "Hodnota") + 
  scale_color_manual(values = c("black", "darkred")) +
  scale_y_continuous(n.breaks = 7) +
  scale_x_continuous(breaks = c(seq(0,nrow(summary(ATT_boots))-1,100), nrow(summary(ATT_boots))-1)) +
  theme(legend.position = "bottom")

# Plot conservative network estimates

ATT_fit_cons <- qgraph(ATT_CI_cmat, 
                       layout = ATT_fit$layout,
                       sampleSize = nrow(ATT_dat_mod),
                       groups = ATT_groups,
                       labels = 1:ncol(ATT_cmat),
                       nodeNames = ATT_labs,
                       pie = ATT_props,
                       GLratio = .8,
                       legend.cex = .28,
                       palette = "pastel",
                       vsize = 3,
                       details = TRUE,
                       DoNotPlot = TRUE,
                       pieBorder = .3,
                       pieColor = list(c("darkred",
                                         "pink",
                                         "lightgreen",
                                         "darkgreen"))[rep(1, ncol(ATT_dat_mod))])

ATT_fit_cons_nl <- qgraph(ATT_CI_cmat, 
                          layout = ATT_fit$layout,
                          sampleSize = nrow(ATT_dat_mod),
                          legend = FALSE,
                          groups = ATT_groups,
                          labels = 1:ncol(ATT_cmat),
                          nodeNames = ATT_labs,
                          pie = ATT_props,
                          GLratio = .8,
                          legend.cex = .28,
                          palette = "pastel",
                          vsize = 3,
                          details = TRUE,
                          DoNotPlot = TRUE,
                          pieBorder = .3,
                          pieColor = list(c("darkred",
                                            "pink",
                                            "lightgreen",
                                            "darkgreen"))[rep(1, ncol(ATT_dat_mod))])


# Compare teacher groups --------------------------------------------------

set.seed(993)
ATT_nct <- NetworkComparisonTest::NCT(data1 = ATT_dat_mod %>%
                                        filter(ATT_dat_group == "1."),
                                      data2 = ATT_dat_mod %>%
                                        filter(ATT_dat_group == "2."),
                                      use = "pariwise.complete.obs",
                                      make.positive.definite = TRUE,
                                      estimator = function(x){qgraph::EBICglasso(x %>%
                                                                                   mutate_all(as.numeric) %>%
                                                                                   cor(method = "spearman", use = "pairwise.complete.obs"), 
                                                                                 n = nrow(x), 
                                                                                 gamma = 1)},
                                      test.edges = TRUE,
                                      p.adjust.methods = "BH",
                                      paired = FALSE,
                                      it = 1000)

ATT_nct_test <- tibble(M = ATT_nct$nwinv.real, p = ATT_nct$nwinv.pval)

ATT_n1 <- qgraph::EBICglasso(ATT_dat_mod %>%
                               filter(ATT_dat_group == "1.") %>%
                               mutate_all(as.numeric) %>%
                               cor(method = "spearman", use = "pairwise.complete.obs"), 
                             n = nrow(ATT_dat_mod %>%
                                        filter(ATT_dat_group == "1.")),gamma = 1)

ATT_n2 <- qgraph::EBICglasso(ATT_dat_mod %>%
                               filter(ATT_dat_group == "2.") %>%
                               mutate_all(as.numeric) %>%
                               cor(method = "spearman", use = "pairwise.complete.obs"), 
                             n = nrow(ATT_dat_mod %>%
                                        filter(ATT_dat_group == "2.")),gamma = 1)

if(sum(ATT_nct$einv.pvals$`p-value` < .05) != 0){
  
  ATT_difs <- ATT_nct$einv.pvals %>%
    filter(`p-value` < .05) %>%
    .[,c(1,2)] %>%
    unlist() %>%
    sort() %>%
    unique() %>%
    as.character()
  
  ATT_dif_mat <- ATT_nct$einv.pvals %>%
    mutate(`p-value` = ifelse(`p-value` < .05, 1, 0)) %>%
    qgraph(., 
           directed = FALSE, 
           DoNotPlot = TRUE) %>%
    getWmat() %>%
    .[colnames(ATT_cmat),colnames(ATT_cmat)]
  
}else{
  ATT_dif_mat <- matrix(0, ncol = ncol(ATT_cmat), nrow = ncol(ATT_cmat))
}

ATT_NCT_1 <- (ATT_n2 - ATT_n1) %>%
  qgraph(layout = ATT_fit$layout,
         sampleSize = nrow(ATT_dat_mod),
         groups = ATT_groups,
         labels = 1:ncol(ATT_cmat),
         nodeNames = ATT_labs,
         pie = ATT_props,
         GLratio = .8,
         legend.cex = .28,
         palette = "pastel",
         vsize = 3,
         details = TRUE,
         legend = FALSE,
         cut = .1,
         DoNotPlot = TRUE,
         pieBorder = .3,
         pieColor = list(c("darkred",
                           "pink",
                           "lightgreen",
                           "darkgreen"))[rep(1, ncol(ATT_dat_mod))])

ATT_NCT_2 <- (ATT_n2 - ATT_n1) %>%
  `*`(ATT_dif_mat) %>%
  qgraph(layout = ATT_fit$layout,
         sampleSize = nrow(ATT_dat_mod),
         groups = ATT_groups,
         labels = 1:ncol(ATT_cmat),
         nodeNames = ATT_labs,
         pie = ATT_props,
         GLratio = .8,
         legend.cex = .28,
         palette = "pastel",
         vsize = 3,
         details = TRUE,
         legend = FALSE,
         DoNotPlot = TRUE,
         pieBorder = .3,
         pieColor = list(c("darkred",
                           "pink",
                           "lightgreen",
                           "darkgreen"))[rep(1, ncol(ATT_dat_mod))])



# Spinglass community detection -------------------------------------------

ATT_wmat <- ATT_fit %>% 
  getWmat()

ATT_hclust_0 <- ATT_wmat %>%
  abs() %>%
  `-`() %>%
  as.dist() %>%
  hclust(method = "single")

ATT_grps_0 <- cutree(ATT_hclust_0, h = -0.01)
ATT_grps_size <- ATT_grps_0 %>%
  table()
ATT_big <- names(ATT_grps_size)[ATT_grps_size > 6]
ATT_small <- names(ATT_grps_size)[ATT_grps_size <= 6 &
                       ATT_grps_size > 1]
ATT_single <- names(ATT_grps_size)[ATT_grps_size == 1]
ATT_spinmat <- ATT_wmat[ATT_grps_0 == ATT_big, ATT_grps_0 == ATT_big]

set.seed(3)

ATT_group_spin <- spinglass.community(graph_from_adjacency_matrix(ATT_spinmat %>%
                                                                    abs(),
                                                                  mode = "undirected",
                                                                  weighted = TRUE))

ATT_grps_0[ATT_grps_0 == ATT_big] <- as.character(ATT_group_spin$membership + 100)

ATT_group_spin <- ATT_grps_0 %>%
  ordered() %>%
  as.numeric()

ATT_fit_spin <- qgraph(ATT_cmat, 
                       graph = "glasso", 
                       layout = "spring",
                       sampleSize = nrow(ATT_dat_mod),
                       threshold = TRUE,
                       groups = LETTERS[ATT_group_spin],
                       labels = 1:ncol(ATT_cmat),
                       nodeNames = ATT_labs,
                       pie = ATT_props,
                       GLratio = .8,
                       legend.cex = .28,
                       palette = "pastel",
                       vsize = 3,
                       details = TRUE,
                       DoNotPlot = TRUE,
                       pieColor = 
                         c(
                           list(c("darkred",
                                  "red2",
                                  "pink",
                                  "white",
                                  "lightgreen",
                                  "green2",
                                  "darkgreen"))[rep(1, length(ATT_labs_1))],
                           list(c("darkred",
                                  "red2",
                                  "pink",
                                  "lightgreen",
                                  "green2",
                                  "darkgreen"))[rep(1, length(ATT_labs_2))])
                       )



nspin <- 100

ATT_grps_0_1 <- cutree(ATT_hclust_0, h = -0.01)
ATT_grps_size_1 <- ATT_grps_0_1 %>%
  table()
ATT_big_1 <- names(ATT_grps_size_1)[ATT_grps_size_1 > 6]
ATT_small_1 <- names(ATT_grps_size_1)[ATT_grps_size_1 <= 6 &
                                    ATT_grps_size_1 > 1]
ATT_single_1 <- names(ATT_grps_size_1)[ATT_grps_size_1 == 1]
ATT_spinmat_1 <- ATT_wmat[ATT_grps_0_1 == ATT_big_1, ATT_grps_0_1 == ATT_big_1]

set.seed(3)

ATT_groups_spin <- lapply(1:nspin,
                          function(x){
                            print(x)
                            ATT_group_spin <- spinglass.community(graph_from_adjacency_matrix(ATT_spinmat_1 %>%
                                                                                                abs(),
                                                                                              mode = "undirected",
                                                                                              weighted = TRUE))
                            
                            ATT_grps_0_1[ATT_grps_0_1 == ATT_big_1] <- as.character(ATT_group_spin$membership + 100)
                            
                            ATT_group_spin <- ATT_grps_0_1 %>%
                              ordered() %>%
                              as.numeric()
                            
                            ATT_group_spin})




ATT_mats <- lapply(ATT_groups_spin, function(x){
  
  temp_vec <- x
  
  lapply(temp_vec,
         function(x){x == temp_vec}) %>%
    unlist() %>%
    matrix(ncol = length(temp_vec))})

ATT_mat <- Reduce("+", ATT_mats) %>%
  `/`(nspin)

ATT_dmat <- as.dist(1-ATT_mat)

ATT_clust <- hclust(ATT_dmat, method = "single")
ATT_dendro <- as.dendrogram(ATT_clust)
ATT_ddata <- dendro_data(ATT_dendro, type = "rectangle")

ATT_groups_tree <- cutree(ATT_clust,
                          h = .5)

ATT_groups_cols_tib <- hclust_palette(ATT_clust, 
                                      start_hue = 50, 
                                      max_hue = 330, 
                                      distinction = 0,
                                      min_luminance = 70,
                                      max_luminance = 90,
                                      mod_hgt = FALSE)


ATT_graph_comm <- ATT_mat %>%
  `colnames<-`(names(ATT_dat_mod)) %>%
  `rownames<-`(names(ATT_dat_mod)) %>%
  qgraph(layout = "spring", 
         labels = 1:ncol(ATT_mat),
         nodeNames = names(ATT_dat_mod),
         groups = paste0("Komunita ",ATT_groups_tree),
         color = ATT_groups_cols_tib$color,
         vsize = 3,
         GLratio = 1,
         repulsion = .9,
         legend.cex = .35, esize = .3, legend = FALSE)

ATT_group_cols_d <- ATT_graph_comm$graphAttributes$Nodes$color[as.numeric(ATT_ddata$labels$label)]
ATT_group_cols <- ATT_graph_comm$graphAttributes$Nodes$color

plt_ATT_dendro <- ggplot(segment(ATT_ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), lwd = 1.5) + 
  geom_point(data = ATT_ddata$labels %>% 
               as_tibble(),
             aes(x = x, 
                 y = y), 
             inherit.aes = FALSE,
             pch = 21,
             fill = ATT_group_cols_d,
             cex = 5) +
  geom_text(data = ATT_ddata$labels %>% 
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


ATT_fit_tree <- qgraph(ATT_cmat, 
                       graph = "glasso", 
                       layout = "spring",
                       sampleSize = nrow(ATT_dat_mod),
                       threshold = TRUE,
                       groups = paste0("F",ATT_groups_tree),
                       labels = 1:ncol(ATT_cmat),
                       nodeNames = ATT_labs,
                       pie = ATT_props,
                       GLratio = .8,
                       legend.cex = .28,
                       color = ATT_group_cols,
                       vsize = 3,
                       details = TRUE,
                       DoNotPlot = TRUE,
                       pieColor = list(c("darkred",
                                         "pink",
                                         "lightgreen",
                                         "darkgreen"))[rep(1, ncol(ATT_dat_mod))])

ATT_fit_tree_nl <- qgraph(ATT_cmat, 
                       graph = "glasso", 
                       layout = "spring",
                       sampleSize = nrow(ATT_dat_mod),
                       threshold = TRUE,
                       groups = paste0("F",ATT_groups_tree),
                       labels = 1:ncol(ATT_cmat),
                       nodeNames = ATT_labs,
                       pie = ATT_props,
                       GLratio = .8,
                       legend.cex = .28,
                       color = ATT_group_cols,
                       vsize = 3,
                       details = TRUE,
                       DoNotPlot = TRUE,
                       legend = FALSE,
                       pieColor = list(c("darkred",
                                         "pink",
                                         "lightgreen",
                                         "darkgreen"))[rep(1, ncol(ATT_dat_mod))])


ATT_lab_tib_print_tree <- ATT_lab_tib %>%
  mutate(group = paste0("Komunita ", ATT_groups_tree),
         ag = as.numeric(ATT_groups_tree)) %>%
  arrange(ag) %>%
  select(-ag) %>%
  mutate(group = ordered(group, unique(group))) %>%
  mutate(name = 1:n()) %>%
  group_by(group) %>%
  arrange(group) %>%
  mutate(mark = 1:n()) %>%
  ungroup() %>%
  mutate(group = ifelse(mark == 1, group, "")) %>%
  select(-mark) %>%
  `names<-`(c("Kód", "Položka", "Komunita", "Reverzní")) %>%
  select(Komunita, Kód, Položka, Reverzní)

ATT_freq_plot_tree <- ATT_dat_mod %>%
  mutate_all(as.numeric) %>%
  pivot_longer(-0) %>%
  na.omit() %>%
  left_join(ATT_lab_tib %>%
              mutate(group = paste0("Komunita ", ATT_groups_tree),
                     ag = as.numeric(ATT_groups_tree)) %>%
              arrange(ag) %>%
              select(-ag) %>%
              mutate(group = ordered(group, unique(group)))) %>%
  mutate(name2 = paste0(name, "\n", label) %>%
           enc2utf8() %>%
           str_wrap(40)) %>%
  group_by(name2) %>%
  mutate(ord = sum(value %in% c(1, 2))/n()) %>%
  ungroup() %>%
  arrange(-ord) %>%
  mutate(name2 = ordered(name2, unique(name2))) %>%
  ggplot(aes(x = name2, fill = as.character(value))) +
  geom_bar(stat = "count", position = "fill", color = "black") +
  coord_flip() +
  facet_wrap(~group, scales = "free_y", ncol = 2,dir = "v") +
  scale_fill_manual(values = c("darkred", "red2", "pink", "white", "lightgreen", "green2", "darkgreen")) +
  geom_hline(yintercept = .5, lty = 2) +
  labs(fill = "Odpověď",
       y = "Podíl",
       x = "") +
  theme(legend.position = "bottom")

# Create a CFA model based on the detected communities --------------------

ATT_disconnected <- ATT_fit_tree %>%
  getWmat() %>%
  abs() %>%
  colSums() %>%
  `==`(0)

ATT_lav_facts <- names(table(ATT_groups_tree)[table(ATT_groups_tree)>1])

ATT_lab_vec <- ATT_lab_tib %>%
  select(name) %>%
  unlist()

ATT_lav_mod <- lapply(ATT_lav_facts,
                      function(x){colnames(ATT_dat_mod)[ATT_groups_tree==x & !ATT_disconnected] %>%
                          paste0(collapse = " + ") %>%
                          paste0("F", x, " =~ ", .)}) %>%
  unlist() %>%
  paste0(collapse = "\n")  

# Split the data for a training and testing sample

set.seed(1)
ATT_tprop <- .75

ATT_train <- sample(1:nrow(ATT_dat_mod), size = round(nrow(ATT_dat_mod)*ATT_tprop, 0), replace = FALSE)

ATT_dat_mod_mod <- ATT_dat_mod %>%
  mutate_at(which(grepl("TEACHING", names(ATT_dat_mod))), 
            function(x){
              if(table(x)["1"]<30){
                case_when(x == "1" ~ "2",
                          TRUE ~ as.character(x)) %>%
                  as.numeric()
              }else{
                if(table(x)["7"]<30){
                  case_when(x == "7" ~ "6",
                            TRUE ~ as.character(x)) %>%
                    as.numeric()
                }else{
                  x
                }
              }
            }) %>%
  mutate_all(ordered)

ATT_dat_mod_use <- ATT_dat_mod_mod %>%
  `names<-`(gsub("\\[|\\]", "", names(.)))

ATT_dat_mod_train <- ATT_dat_mod_use %>%
  .[ATT_train,]

ATT_dat_mod_test <- ATT_dat_mod_use %>%
  .[-ATT_train,]

# Fit the model on the training data

ATT_lav_mod_use <- ATT_lav_mod %>%
  gsub("\\[|\\]", "", .)

ATT_fit_cfa_0 <- cfa(ATT_lav_mod_use,
                     ordered = TRUE,
                     ATT_dat_mod_train)

# Re-fit the model on a testing sample

ATT_fit_cfa <- cfa(model = ATT_fit_cfa_0,                     
                   ordered = TRUE,
                   ATT_dat_mod_test)



ATT_tab_fit <- fitmeasures(ATT_fit_cfa) %>%
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

ATT_rels <- semTools::reliability(ATT_fit_cfa) 

ATT_Coef_vec <- c(alpha = "Alpha (Cronbach, 1951)",
              alpha.ord = "Ordinální alpha (Zumbo et al., 2007)",
              omega = "Omega (Bollen, 1980)",
              omega2 = "Omega (Bentler, 1972)",
              omega3 = "Omega (McDonald, 1999)",
              avevar = "Average variance extracted (Fornell & Larcke, 1981)")

ATT_tab_rel <- ATT_rels %>%
  as_tibble() %>%
  mutate(Koeficient = Coef_vec[rownames(ATT_rels)]) %>%
  select(Koeficient, everything()) 

ATT_loads <- lavInspect(ATT_fit_cfa, "std")$lambda %>% 
  as_tibble() %>% 
  as.matrix() %>% 
  `rownames<-`(rownames(lavInspect(ATT_fit_cfa, "std")$lambda))

ATT_tab_loads <- ATT_loads %>%
  as_tibble() %>%
  mutate_all(function(x){
    temp_1 <- as.numeric(x) %>%
      round(3) 
    if_else(temp_1 == 0, "", as.character(temp_1))}) %>%
  mutate(Položka = rownames(ATT_loads) %>%
           gsub("(.{2})(.{3})", "\\1[\\2]", .)) %>%
  select(Položka, everything())

ATT_fac_keeps <- colnames(ATT_tab_rel)[-1][ATT_tab_rel[2,-1] > .65] %>%
  gsub("F", "", .)

ATT_var_rems <- colnames(ATT_dat_mod) %>%
  gsub("\\[", "\\\\[", .) %>%
  gsub("\\]", "\\\\]", .) %>%
  .[!ATT_groups_tree %in% ATT_fac_keeps] %>%
  paste0(collapse = "|")

ATT_lav_mod_2_0 <- ATT_lav_mod %>%
  str_split("\n",
            simplify =  TRUE) %>%
  .[1,] %>%
  .[!grepl(ATT_var_rems, .)] %>%
  paste0(collapse = "\n")

ATT_fit_cfa_2_0 <- cfa(ATT_lav_mod_2 %>%
                     gsub("SELFEFF|\\[|\\]", "", .),
                   ordered = TRUE,
                   ATT_dat_mod_train)

ATT_lav_mod_2 <- ATT_lav_mod_2_0 %>%
  paste0(. , "\nTEACHINGATTSQ001 ~~ TEACHINGATTSQ015\nTEACHINGATTSQ001 ~~ TEACHINGATTSQ006")

ATT_fit_cfa_2 <- cfa(ATT_lav_mod_2 %>%
                     gsub("SELFEFF|\\[|\\]", "", .),
                   ordered = TRUE,
                   ATT_dat_mod_test)

ATT_tab_fit_2 <- fitmeasures(ATT_fit_cfa_2) %>%
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

ATT_rels_2 <- semTools::reliability(ATT_fit_cfa_2) 

ATT_tab_rel_2 <- ATT_rels_2 %>%
  as_tibble() %>%
  mutate(Koeficient = Coef_vec[rownames(ATT_rels)]) %>%
  select(Koeficient, everything()) 

ATT_loads_2 <- lavInspect(ATT_fit_cfa_2, "std")$lambda %>% 
  as_tibble() %>% 
  as.matrix() %>% 
  `rownames<-`(rownames(lavInspect(ATT_fit_cfa_2, "std")$lambda))

ATT_tab_loads_2 <- ATT_loads_2 %>%
  as_tibble() %>%
  mutate_all(function(x){
    temp_1 <- as.numeric(x) %>%
      round(3) 
    if_else(temp_1 == 0, "", as.character(temp_1))}) %>%
  mutate(Položka = rownames(ATT_loads_2) %>%
           gsub("(.+)(SQ.+)", "\\1[\\2]", .)) %>%
  select(Položka, everything())

ATT_gcol_0 <- ATT_fit_tree$graphAttributes$Nodes$color %>%
  `names<-`(names(ATT_dat_mod) %>%
              gsub("\\[|\\]", "", .))

ATT_frefs <- ATT_lav_mod %>%
  strsplit("\n") %>%
  unlist() %>%
  gsub(".+ \\=\\~ ([^\\+]+) \\+.+", "\\1", .)
ATT_fs <- ATT_lav_mod %>%
  strsplit("\n") %>%
  unlist() %>%
  gsub("(.+) \\=\\~ ([^\\+]+) \\+.+", "\\1", .)

ATT_gcol_1 <- ATT_gcol_0[lavNames(ATT_fit_cfa)]

ATT_gcol_2 <- ATT_gcol_0[ATT_frefs %>%
                         gsub("\\[|\\]", "", .)] %>%
  `names<-`(ATT_fs)

ATT_gcol <- c(ATT_gcol_1, ATT_gcol_2)

ATT_hold <- lavNames(ATT_fit_cfa) %>%
  gsub("PUP", "Z", .) %>%
  order()

ATT_graph_sem <- semPlot::semPaths(ATT_fit_cfa, 
                                   "std", 
                                   intercepts = FALSE,
                                   residuals = FALSE,
                                   layout = "circle", 
                                   whatLabels = "std",
                                   nodeLabels = c(ATT_hold, names(ATT_gcol_2)),
                                   color = ATT_gcol)

ATT_lvmat <- lavInspect(ATT_fit_cfa, "cor.lv") %>% 
  as_tibble() %>% 
  as.matrix() %>% 
  `rownames<-`(colnames(.))

ATT_graph_lat <- qgraph(ATT_lvmat, 
                          layout = "spring",
                          graph = "cor",
                          threshold = .2,
                          color = ATT_gcol_2,
                          labels = names(ATT_gcol_2))

# Get factor scores -------------------------------------------------------

ATT_impdat <- mice(ATT_dat_mod_use)

ATT_impdat_use <- complete(ATT_impdat,1) %>%
  as_tibble() %>%
  mutate_all(ordered)

ATT_mdpat <- md.pattern(ATT_dat_mod_use, plot = FALSE)

ATT_scores <- lavPredict(ATT_fit_cfa_2, 
                         newdata = ATT_impdat_use,
                         method = "EBM")

ATT_sumscores <- lapply(unique(ATT_groups_tree),
                        function(x){
                          rowMeans(ATT_impdat_use[,ATT_groups_tree == x] %>%
                                     mutate_all(as.numeric))
                        }) %>%
  as.data.frame() %>%
  `names<-`(paste0("s_F", unique(ATT_groups_tree))) %>%
  as_tibble() %>%
  select(colnames(ATT_scores) %>% gsub("F", "s_F", .))


ATT_sumscores_dat <- ATT_scores %>%
  as_tibble() %>%
  bind_cols(ATT_sumscores) %>%
  mutate_all(as.numeric) %>%
  mutate(id = 1:n()) %>%
  pivot_longer(cols = -id) %>%
  mutate(Skór = ifelse(grepl("s_", name), "Součtový", "Faktorový"),
         Faktor = gsub("s_", "", name)) %>%
  select(-name) %>%
  pivot_wider(id_cols = c(id, Faktor),
              names_from = Skór,
              values_from = value)

ATT_sumscores_cors <-  ATT_sumscores_dat %>%
  group_by(Faktor) %>%
  summarise(Korelace = cor(Faktorový, Součtový)) %>%
  mutate(Korelace = paste0("r = ", round(Korelace, 3)))


ATT_sumscores_plt <- ATT_sumscores_dat %>%
  ggplot(aes(x = Faktorový, y = Součtový)) +
  geom_hex(binwidth = c(.3,.3)) +
  facet_wrap(~Faktor, scales = "free_y") +
  geom_smooth(color = "red") +
  geom_label(data = ATT_sumscores_cors, aes(x = -1.5, y = 3.5, label = Korelace), inherit.aes = FALSE) +
  labs(fill = "Počet pozorování") +
  theme(legend.position = "bottom")

ATT_thresholds <- lavInspect(ATT_fit_cfa_2, "th") %>%
  enframe() %>%
  mutate(name3 = gsub(".+\\|(.+)", "\\1", name),
         name2 = gsub("(.+)\\|.+", "\\1", name)) %>%
  left_join(ATT_lab_tib %>%
              mutate(name2 = gsub("\\[|\\]", "", name)) %>%
              mutate(id = 1:n()), "name2")

ATT_thresholds_2 <- lavInspect(ATT_fit_cfa, "th") %>%
  enframe() %>%
  mutate(name3 = gsub(".+\\|(.+)", "\\1", name),
         name2 = gsub("(.+)\\|.+", "\\1", name)) %>%
  left_join(ATT_lab_tib %>%
              mutate(group = paste0("Faktor ", ATT_groups_tree)) %>%
              mutate(name2 = gsub("\\[|\\]", "", name)) %>%
              mutate(id = 1:n()), "name2")

ATT_plt_thresh_1 <- ATT_thresholds %>%
  group_by(group) %>%
  arrange(value) %>%
  mutate(mark = 1:n(),
         mark = as.numeric(mark %% 2 == 0)) %>%
  ungroup() %>%
  ggplot(aes(x = group, 
             y = value, 
             color = ordered(name3),
             label = id)) +
  geom_point(pch = "|", stroke = 20) +
  geom_label(aes(vjust = mark), show.legend = FALSE) +
  coord_flip() +
  scale_color_manual(values = c("darkred", 
                               "red2", 
                               "pink", 
                               "lightgreen", 
                               "green2",
                               "darkgreen"),
                    labels = c("1|2","2|3","3|4","4|5","5|6","6|7"),
                    name = "Hranice") +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Hodnota faktoru")

ATT_plt_thresh_2 <- ATT_thresholds_2 %>%
  group_by(group) %>%
  arrange(value) %>%
  mutate(mark = 1:n(),
         mark = as.numeric(mark %% 2 == 0)) %>%
  ungroup() %>%
  filter(group %in% gsub("F", "Faktor ", colnames(ATT_scores))) %>%
  mutate(group = ordered(group, gsub("F", "Faktor ", colnames(ATT_scores)))) %>%
  ggplot(aes(x = group, 
             y = value, 
             color = ordered(name3),
             label = id)) +
  geom_vline(aes(xintercept = group), lty = 2) +
  geom_point(pch = "|", stroke = 20) +
  geom_label(aes(vjust = mark), show.legend = FALSE) +
  coord_flip() +
  scale_color_manual(values = c("darkred", 
                               "red2", 
                               "pink", 
                               "lightgreen", 
                               "green2",
                               "darkgreen"),
                    labels = c("1|2","2|3","3|4","4|5","5|6","6|7"),
                    name = "Hranice") +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Hodnota faktoru")

ATT_loads_thr <- ATT_loads %>%
  rowSums()

ATT_thresh_2_2 <- ATT_thresholds_2 %>%
  filter(group %in% gsub("F", "Faktor ", colnames(ATT_scores))) %>%
  mutate(wgt = ATT_loads_thr[name2]) %>%
  select(group, name3, value, wgt) %>%
  group_by(group, name3) %>%
  mutate(wgt = wgt/sum(wgt)) %>%
  summarise(value_m = stats::weighted.mean(x = value, n()*wgt),
            value_sd = sqrt(Hmisc::wtd.var(x = value, weights =  n()*wgt)),
            size = n()) %>%
  ungroup() %>%
  mutate(value_se = value_sd/sqrt(size),
         value_ci = value_se*1.96)

ATT_plt_thresh_2_2 <- ATT_thresh_2_2 %>%
  ggplot(aes(x = group, 
             y = value_m, 
             color = ordered(name3))) +
  geom_vline(aes(xintercept = group), lty = 2) +
  geom_point(pch = "|", stroke = 20) +
  geom_point(aes(y = value_m - value_ci), pch = "|", stroke = 5) +
  geom_point(aes(y = value_m + value_ci), pch = "|", stroke = 5) +
  geom_segment(aes(y = value_m - value_ci, yend = value_m + value_ci, xend = group), cex = 1.2) +
  coord_flip() +
  scale_color_manual(values = c("darkred", 
                                "red2", 
                                "pink", 
                                "lightgreen", 
                                "green2",
                                "darkgreen"),
                     labels = c("1|2","2|3","3|4","4|5","5|6","6|7"),
                     name = "Hranice (s 95% CI)") +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Hodnota faktorového skóru")

ATT_scores_scaled <- lapply(1:ncol(ATT_scores),
                            function(x){
                              temp_dat <- ATT_scores[,x]
                              
                              temp_thresh <- ATT_thresh_2_2 %>%
                                select(group, name3, value_m) %>%
                                mutate_all(as.character) %>%
                                mutate_all(parse_number) %>%
                                filter(group == parse_number(colnames(ATT_scores))[x]) %>%
                                arrange(name3) %>%
                                select(value_m) %>%
                                unlist() %>%
                                c(-Inf, ., Inf)
                              
                              temp_vals <- cut(temp_dat, 
                                               temp_thresh, 
                                               include.lowest = TRUE)
                            }) %>%
  `names<-`(colnames(ATT_scores)) %>%
  as_tibble() %>%
  mutate_all(ordered) %>%
  mutate_all(as.numeric) %>%
  mutate_all(ordered)

ATT_scores_scaled_plt <- ATT_scores_scaled %>%
  mutate_all(as.numeric) %>%
  pivot_longer(-0) %>%
  group_by(name) %>%
  mutate(pos = sum(value %in% c("6", "7"))) %>%
  ungroup() %>%
  arrange(pos) %>%
  mutate(name = gsub("F", "Faktor ", name)) %>%
  mutate(name = ordered(name, gsub("F", "Faktor ", colnames(ATT_scores)))) %>%
  ggplot(aes(x = name, fill = as.character(value))) +
  geom_bar(stat = "count", position = "fill", color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("darkred", "red2", "pink", "white", "lightgreen", "green2", "darkgreen")) +
  geom_hline(yintercept = .5, lty = 2) +
  labs(fill = "Odpověď",
       y = "Podíl",
       x = "") +
  theme(legend.position = "bottom")

ATT_fcols <- ATT_gcol_0[lapply(unique(ATT_groups_tree), function(x){
  which(ATT_groups_tree == x)[1]
}) %>% unlist()]

ATT_plt_fac_group <- ATT_lab_tib %>%
  mutate(Faktor = paste0("Faktor ", ATT_groups_tree),
         Faktor = ordered(Faktor, paste0("Faktor ", 1:max(ATT_groups_tree))),
         group = ordered(group, sort(unique(group), decreasing = TRUE))) %>%
  group_by(group, Faktor) %>%
  summarise(cnt = n()) %>%
  ggplot(aes(y = group, x = Faktor, fill = Faktor, label = cnt)) +
  geom_tile(color = "black") +
  geom_text(color = "black") +
  theme_classic() +
  labs(x = "", y = "") +
  theme(legend.position = "none", 
        panel.grid.major = element_line(), 
        panel.grid.minor = element_line()) +
  coord_fixed() +
  scale_fill_manual(values = `names<-`(ATT_fcols, NULL))


# Create outputs ----------------------------------------------------------

W1 <- 14 
H1 <- 14
W2 <- 9 
H2 <- 9
W3 <- 6 
H3 <- 6

# ggsave

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/1_1_ATT_freq_plot.png",
       ATT_freq_plot,
       width = W1 + 4, 
       height = H1 + 4)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/2_1_ATT_plt_CI_1.png",
       ATT_plt_CI_1,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/2_2_ATT_plt_CI_2.png",
       ATT_plt_CI_2,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/3_1_plt_ATT_dendro.png",
       plt_ATT_dendro,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/3_5_ATT_freq_plot_tree.png",
       ATT_freq_plot_tree,
       width = W1, 
       height = H1)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/4_5_ATT_sumscores_plt.png",
       ATT_sumscores_plt,
       width = W1, 
       height = H1)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/4_6_ATT_plt_thresh_1.png",
       ATT_plt_thresh_1,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/4_7_ATT_plt_thresh_2.png",
       ATT_plt_thresh_2,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/4_8_ATT_plt_thresh_2_2.png",
       ATT_plt_thresh_2_2,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/4_9_ATT_scores_scaled_plt.png",
       ATT_scores_scaled_plt,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_2_cluster_ATT_ZU/png/5_1_ATT_plt_fac_group.png",
       ATT_plt_fac_group,
       width = W2, 
       height = H3)


# png

png("outputs/code/0_2_2_cluster_ATT_ZU/png/2_4_ATT_fit.png",
    width = W1+3, height = H3, units = "in", res = 360)
plot(ATT_fit)
dev.off()

png("outputs/code/0_2_2_cluster_ATT_ZU/png/2_5_ATT_fit_nl.png",
    width = W3, height = H3, units = "in", res = 360)
plot(ATT_fit_nl)
dev.off()

png("outputs/code/0_2_2_cluster_ATT_ZU/png/2_6_ATT_fit_cons.png",
    width = W1 + 3, height = H3, units = "in", res = 360)
plot(ATT_fit_cons)
dev.off()

png("outputs/code/0_2_2_cluster_ATT_ZU/png/2_7_ATT_fit_cons_nl.png",
    width = W3, height = H3, units = "in", res = 360)
plot(ATT_fit_cons_nl)
dev.off()

png("outputs/code/0_2_2_cluster_ATT_ZU/png/3_2_ATT_graph_comm.png",
    width = W3, height = H3, units = "in", res = 360)
plot(ATT_graph_comm)
dev.off()

png("outputs/code/0_2_2_cluster_ATT_ZU/png/3_3_ATT_fit_tree.png",
    width = W1+3, height = H2, units = "in", res = 360)
plot(ATT_fit_tree)
dev.off()

png("outputs/code/0_2_2_cluster_ATT_ZU/png/3_4_ATT_fit_tree_nl.png",
    width = W3, height = H3, units = "in", res = 360)
plot(ATT_fit_tree_nl)
dev.off()

png("outputs/code/0_2_2_cluster_ATT_ZU/png/4_1_ATT_graph_sem.png",
    width = W2, height = H2, units = "in", res = 360)
plot(ATT_graph_sem)
dev.off()

png("outputs/code/0_2_2_cluster_ATT_ZU/png/4_2_ATT_graph_lat.png",
    width = W2, height = H2, units = "in", res = 360)
plot(ATT_graph_lat)
dev.off()

# Save tables

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/1_ATT_lab_tib_print.txt",
            ATT_lab_tib_print,
            fileEncoding = "UTF-8")

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/2_ATT_fit_m.txt",
            ATT_fit_m,
            fileEncoding = "UTF-8")

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/3_ATT_nct_test.txt",
            ATT_fit_m,
            fileEncoding = "UTF-8")

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/4_ATT_lab_tib_print_tree.txt",
            ATT_lab_tib_print_tree,
            fileEncoding = "UTF-8")

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/5_ATT_tab_fit.txt",
            ATT_tab_fit,
            fileEncoding = "UTF-8")

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/6_ATT_tab_rel.txt",
            ATT_tab_rel,
            fileEncoding = "UTF-8")

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/7_ATT_tab_loads.txt",
            ATT_tab_loads,
            fileEncoding = "UTF-8")

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/8_ATT_tab_fit_2.txt",
            ATT_tab_fit_2,
            fileEncoding = "UTF-8")

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/9_ATT_tab_rel_2.txt",
            ATT_tab_rel_2,
            fileEncoding = "UTF-8")

write.table(file = "outputs/code/0_2_2_cluster_ATT_ZU/tab/10_ATT_tab_loads_2.txt",
            ATT_tab_loads_2,
            fileEncoding = "UTF-8")


#### Save data


ATT_D0 <- data_use %>%
  filter(filt_RESPs) %>%
  bind_cols()

ATT_D1 <- ATT_impdat_use %>%
  `colnames<-`(paste0("ATT_imputed_", names(.)))

ATT_D2 <- ATT_scores %>%
  as_tibble() %>%
  `colnames<-`(paste0("ATT_", names(.))) %>%
  mutate_all(as.numeric)

ATT_D3 <- ATT_sumscores %>%
  `colnames<-`(paste0("ATT_sumscore", names(.)))

ATT_D_scores <- bind_cols(ATT_D1, ATT_D2, ATT_D3)

write_csv(ATT_D0, "outputs/code/0_2_2_cluster_ATT_ZU/dat/1_ATT_data_0.csv")
write_csv(ATT_D_scores, "outputs/code/0_2_2_cluster_ATT_ZU/dat/2_ATT_D_scores.csv")
