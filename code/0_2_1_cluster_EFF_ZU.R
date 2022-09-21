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

EFF_dat <- data_use %>%
  filter(filt_RESPs) %>%
  select(starts_with("SELFEFF")) %>%
  mutate_all(parse_number)

EFF_dat_group <- data_use %>%
  filter(filt_RESPs) %>%
  select(TIMEEMPLOYEDSCHOOL)

EFF_dat_mod <- EFF_dat %>%
  mutate_all(function(x)ifelse(x < 5, 1, x) %>% 
               ordered() %>% 
               as.numeric() %>% 
               ordered())

# Creating labels 

EFF_labs <- varlabs_2[grepl("SELFEFF", varnames)] %>%
  enc2utf8()

EFF_groups <- c(
  rep("1. PLÁNOVÁNÍ VÝUKY", 4),
  rep("2. PROCESY UČENÍ", 7),
  rep("3. PROSTŘEDÍ PRO UČENÍ", 6),
  rep("4. HODNOCENÍ", 5),
  rep("5. REFLEXE VÝUKY", 4),
  rep("6. ROZVOJ ŠKOLY, SPOLUPRÁCE", 6)
) %>%
  enc2utf8()

EFF_lab_tib <- tibble(name = names(EFF_dat),
       label = EFF_labs,
       group = EFF_groups) %>%
  mutate(label = gsub("\U202F", " ", label))

EFF_lab_tib_print <- EFF_lab_tib %>%
  group_by(group) %>%
  mutate(mark = 1:n()) %>%
  mutate(group = ifelse(mark == 1, group, "")) %>%
  ungroup() %>%
  select(-mark) %>%
  `names<-`(c("Kód", "Položka", "Okruh")) %>%
  mutate(Kód = 1:n()) %>%
  select(Okruh, Kód, Položka)

# Create descriptive summaries --------------------------------------------

EFF_freq_plot <- EFF_dat_mod %>%
  pivot_longer(-0) %>%
  na.omit() %>%
  left_join(EFF_lab_tib) %>%
  mutate(name2 = paste0(name, "\n", label) %>%
           enc2utf8() %>%
           str_wrap(40)) %>%
  group_by(name2) %>%
  mutate(ord = sum(value %in% c("1", "2"))/n()) %>%
  ungroup() %>%
  arrange(-ord) %>%
  mutate(name2 = ordered(name2, unique(name2))) %>%
  ggplot(aes(x = name2, fill = value)) +
  geom_bar(stat = "count", position = "fill", color = "black") +
  coord_flip() +
  facet_wrap(~group, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = c("darkred", "pink", "lightgreen", "darkgreen"),
                    labels = c("1-4", "5", "6", "7")) +
  geom_hline(yintercept = .5, lty = 2) +
  labs(fill = "Odpověď",
       y = "Podíl",
       x = "") +
  theme(legend.position = "bottom")

# Proportions of responses per item

EFF_props <- EFF_dat_mod %>%
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
  as.list()


# Fit network -----------------------------------------------------------
# Correlation matrix and first model
EFF_cmat <- cor_auto(EFF_dat_mod)

EFF_fit <- qgraph(EFF_cmat, 
                  graph = "glasso", 
                  layout = "spring",
                  sampleSize = nrow(EFF_dat_mod),
                  threshold = TRUE,
                  groups = EFF_groups,
                  labels = 1:ncol(EFF_cmat),
                  nodeNames = EFF_labs,
                  pie = EFF_props,
                  GLratio = .8,
                  legend.cex = .28,
                  palette = "pastel",
                  vsize = 3,
                  details = TRUE,
                  pieBorder = .3,
                  DoNotPlot = TRUE,
                  pieColor = list(c("darkred",
                                    "pink",
                                    "lightgreen",
                                    "darkgreen"))[rep(1, ncol(EFF_dat_mod))])

EFF_fit_nl <- qgraph(EFF_cmat, 
                     graph = "glasso", 
                     layout = "spring",
                     sampleSize = nrow(EFF_dat_mod),
                     legend = FALSE,
                     threshold = TRUE,
                     groups = EFF_groups,
                     labels = 1:ncol(EFF_cmat),
                     nodeNames = EFF_labs,
                     pie = EFF_props,
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
                                       "darkgreen"))[rep(1, ncol(EFF_dat_mod))])

# First model fit

EFF_fit_m <- ggmFit(EFF_fit, 
                    covMat = EFF_cmat,
                    sampleSize = nrow(EFF_dat_mod)) %>%
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

EFF_nboots <- 1000

EFF_boots <- bootnet(EFF_dat_mod %>%
                       mutate_all(as.numeric), 
                     nBoots = EFF_nboots, 
                     default = "glasso", 
                     nCores = parallel::detectCores(),
                     statistics = "edge",
                     corMethod = "cor_auto",
                     threshold = TRUE)

EFF_dat_boots <- summary(EFF_boots) %>%
  left_join(EFF_lab_tib %>%
              mutate(id1 = 1:n()),
            c("node1" = "name")) %>%
  left_join(EFF_lab_tib %>%
              mutate(id2 = 1:n()),
            c("node2" = "name")) %>%
  filter(q2.5 > 0 | q97.5 < 0)

# Plot boot results
EFF_plt_CI_1 <- EFF_dat_boots %>%  
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
  scale_x_continuous(breaks = c(seq(0,nrow(EFF_dat_boots),5), nrow(EFF_dat_boots)-1))

EFF_CI_cmat <- summary(EFF_boots) %>%
  ungroup() %>%
  mutate(weight = ifelse(q2.5 > 0 | q97.5 < 0, mean, 0)) %>%
  select(node1, node2, weight) %>%
  qgraph(directed = FALSE, DoNotPlot = TRUE) %>%
  getWmat() %>%
  .[colnames(EFF_cmat),colnames(EFF_cmat)]

EFF_plt_CI_2 <- summary(EFF_boots) %>%
  left_join(EFF_lab_tib %>%
              mutate(id1 = 1:n()),
            c("node1" = "name")) %>%
  left_join(EFF_lab_tib %>%
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
  scale_x_continuous(breaks = c(seq(0,nrow(summary(EFF_boots))-1,100), nrow(summary(EFF_boots))-1)) +
  theme(legend.position = "bottom")

# Plot conservative network estimates

EFF_fit_cons <- qgraph(EFF_CI_cmat, 
                  layout = EFF_fit$layout,
                  sampleSize = nrow(EFF_dat_mod),
                  groups = EFF_groups,
                  labels = 1:ncol(EFF_cmat),
                  nodeNames = EFF_labs,
                  pie = EFF_props,
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
                                    "darkgreen"))[rep(1, ncol(EFF_dat_mod))])

EFF_fit_cons_nl <- qgraph(EFF_CI_cmat, 
                     layout = EFF_fit$layout,
                     sampleSize = nrow(EFF_dat_mod),
                     legend = FALSE,
                     groups = EFF_groups,
                     labels = 1:ncol(EFF_cmat),
                     nodeNames = EFF_labs,
                     pie = EFF_props,
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
                                       "darkgreen"))[rep(1, ncol(EFF_dat_mod))])


# Compare teacher groups --------------------------------------------------

set.seed(993)
EFF_nct <- NetworkComparisonTest::NCT(data1 = EFF_dat %>%
                                    filter(EFF_dat_group == "1."),
                                  data2 = EFF_dat %>%
                                    filter(EFF_dat_group == "2."),
                                  use = "pariwise.complete.obs",
                                  make.positive.definite = TRUE,
                                  estimator = function(x){qgraph::EBICglasso(x %>%
                                                                               cor_auto(detectOrdinal = FALSE), 
                                                                             n = nrow(x), 
                                                                             threshold = TRUE)},
                                  test.edges = TRUE,
                                  p.adjust.methods = "BH",
                                  paired = FALSE,
                                  it = 1000)

EFF_nct_test <- tibble(M = EFF_nct$nwinv.real, p = EFF_nct$nwinv.pval)

EFF_n1 <- qgraph::EBICglasso(EFF_dat %>%
                     filter(EFF_dat_group == "1.") %>%
                     cor_auto(detectOrdinal = FALSE), 
                   n = nrow(EFF_dat %>%
                              filter(EFF_dat_group == "1.")), 
                   threshold = TRUE)

EFF_n2 <- qgraph::EBICglasso(EFF_dat %>%
                     filter(EFF_dat_group == "2.") %>%
                     cor_auto(detectOrdinal = FALSE), 
                   n = nrow(EFF_dat %>%
                              filter(EFF_dat_group == "2.")), 
                   threshold = TRUE) 

EFF_nct_plt <- abs(EFF_n1 - EFF_n2) %>% 
  .[lower.tri(., diag = FALSE)] %>%
  unlist() %>% tibble(x = .) %>% 
  ggplot(aes(x=x)) + 
  geom_density(fill = "steelblue", alpha = .5) +
  geom_rug(length = unit(0.045, "npc")) +
  labs(x = "Absolutní rozdíl hran", y = "Hustota")



if(sum(EFF_nct$einv.pvals$`p-value` < .05) != 0){
  
  EFF_difs <- EFF_nct$einv.pvals %>%
    filter(`p-value` < .05) %>%
    .[,c(1,2)] %>%
    unlist() %>%
    sort() %>%
    unique() %>%
    as.character()
  
  EFF_dif_mat <- EFF_nct$einv.pvals %>%
    mutate(`p-value` = ifelse(`p-value` < .05, 1, 0)) %>%
    qgraph(., 
           directed = FALSE, 
           DoNotPlot = TRUE) %>%
    getWmat() %>%
    .[colnames(EFF_cmat),colnames(EFF_cmat)]
  
}else{
  EFF_dif_mat <- matrix(0, ncol = ncol(EFF_cmat), nrow = ncol(EFF_cmat))
}

EFF_NCT_1 <- (EFF_n2 - EFF_n1) %>%
  qgraph(layout = EFF_fit$layout,
         sampleSize = nrow(EFF_dat_mod),
         groups = EFF_groups,
         labels = 1:ncol(EFF_cmat),
         nodeNames = EFF_labs,
         pie = EFF_props,
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
                           "darkgreen"))[rep(1, ncol(EFF_dat_mod))])

EFF_NCT_2 <- (EFF_n2 - EFF_n1) %>%
  `*`(EFF_dif_mat) %>%
  qgraph(layout = EFF_fit$layout,
         sampleSize = nrow(EFF_dat_mod),
         groups = EFF_groups,
         labels = 1:ncol(EFF_cmat),
         nodeNames = EFF_labs,
         pie = EFF_props,
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
                           "darkgreen"))[rep(1, ncol(EFF_dat_mod))])

# Spinglass community detection -------------------------------------------

# Initial communities

set.seed(3)
EFF_group_spin <- igraph::spinglass.community( 
  igraph::as.igraph(EFF_fit))

EFF_fit_spin <- qgraph(EFF_cmat, 
                  graph = "glasso", 
                  layout = "spring",
                  sampleSize = nrow(EFF_dat_mod),
                  threshold = TRUE,
                  groups = LETTERS[EFF_group_spin$membership],
                  labels = 1:ncol(EFF_cmat),
                  nodeNames = EFF_labs,
                  pie = EFF_props,
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
                                    "darkgreen"))[rep(1, ncol(EFF_dat_mod))])

EFF_fit_spin_nl <- qgraph(EFF_cmat, 
                  graph = "glasso", 
                  layout = "spring",
                  sampleSize = nrow(EFF_dat_mod),
                  legend = FALSE,
                  threshold = TRUE,
                  groups = LETTERS[EFF_group_spin$membership],
                  labels = 1:ncol(EFF_cmat),
                  nodeNames = EFF_labs,
                  pie = EFF_props,
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
                                    "darkgreen"))[rep(1, ncol(EFF_dat_mod))])


# Robust spinglass communities 

# Perform n community searches

nspin <- 100
set.seed(994)
EFF_groups_spin <- lapply(1:nspin,
                          function(x){
                            print(x)
                            igraph::spinglass.community(igraph::as.igraph(EFF_fit))$membership})


# Find the frequency of pairwise co-ocurences of nodes in detected communities

EFF_mats <- lapply(EFF_groups_spin, function(x){
  
  temp_vec <- x
  
  lapply(temp_vec,
         function(x){x == temp_vec}) %>%
    unlist() %>%
    matrix(ncol = length(temp_vec))})

EFF_mat <- Reduce("+", EFF_mats) %>%
  `/`(nspin)

EFF_dmat <- as.dist(1-EFF_mat)

EFF_clust <- hclust(EFF_dmat, method = "single")
EFF_dendro <- as.dendrogram(EFF_clust)
EFF_ddata <- dendro_data(EFF_dendro, type = "rectangle")

# Create groups based on more than .5 probability of sharing a community

EFF_groups_tree <- cutree(EFF_clust,
                      h = .5)

EFF_groups_cols_tib <- EFF_cols <- hclust_palette(EFF_clust, 
                                                  start_hue = 90, 
                                                  max_hue = 300, 
                                                  distinction = 10,
                                                  min_luminance = 70,
                                                  max_luminance = 90,
                                                  mod_hgt = FALSE)

EFF_graph_comm <- EFF_mat %>%
  `colnames<-`(names(EFF_dat_mod)) %>%
  `rownames<-`(names(EFF_dat_mod)) %>%
  qgraph(layout = "spring", 
         labels = 1:ncol(EFF_mat),
         nodeNames = names(EFF_dat_mod),
         groups = paste0("F",EFF_groups_tree),
         color = EFF_groups_cols_tib$color,
         vsize = 3,
         GLratio = 1,
         repulsion = .9,
         legend.cex = .35, esize = .3, legend = FALSE)

# Color hclust

EFF_group_cols_d <- EFF_graph_comm$graphAttributes$Nodes$color[as.numeric(EFF_ddata$labels$label)]
EFF_group_cols <- EFF_graph_comm$graphAttributes$Nodes$color

plt_EFF_dendro <- ggplot(segment(EFF_ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), lwd = 1.5) + 
  geom_point(data = EFF_ddata$labels %>% 
              as_tibble(),
            aes(x = x, 
                y = y), 
            inherit.aes = FALSE,
            pch = 21,
            fill = EFF_group_cols_d,
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

EFF_fit_tree <- qgraph(EFF_cmat, 
                       graph = "glasso", 
                       layout = "spring",
                       sampleSize = nrow(EFF_dat_mod),
                       threshold = TRUE,
                       groups = paste0("F", EFF_groups_tree),
                       color = EFF_group_cols,
                       labels = 1:ncol(EFF_cmat),
                       nodeNames = EFF_labs,
                       pie = EFF_props,
                       GLratio = .8,
                       legend.cex = .28,
                       vsize = 3,
                       details = TRUE,
                       pieBorder = .3,
                       DoNotPlot = TRUE,
                       pieColor = list(c("darkred",
                                         "pink",
                                         "lightgreen",
                                         "darkgreen"))[rep(1, ncol(EFF_dat_mod))])

EFF_fit_tree_nl <- qgraph(EFF_cmat, 
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
                       color = EFF_group_cols,
                       vsize = 3,
                       details = TRUE,
                       DoNotPlot = TRUE,
                       pieBorder = .3,
                       pieColor = list(c("darkred",
                                         "pink",
                                         "lightgreen",
                                         "darkgreen"))[rep(1, ncol(EFF_dat_mod))])


EFF_lab_tib_print_tree <- EFF_lab_tib %>%
  mutate(group = paste0("Komunita ", EFF_groups_tree),
         ag = as.numeric(EFF_groups_tree)) %>%
  arrange(ag) %>%
  select(-ag) %>%
  mutate(group = ordered(group, unique(group))) %>%
  mutate(name = 1:n()) %>%
  group_by(group) %>%
  arrange(group) %>%
  mutate(mark = 1:n()) %>%
  ungroup() %>%
  mutate(group = ifelse(mark == 1, as.character(group), "")) %>%
  select(-mark) %>%
  `names<-`(c("Kód", "Položka", "Komunita")) %>%
  select(Komunita, Kód, Položka)


EFF_freq_plot_tree <- EFF_dat_mod %>%
  pivot_longer(-0) %>%
  na.omit() %>%
  left_join(EFF_lab_tib %>%
              mutate(group = paste0("Komunita ", EFF_groups_tree),
                     ag = as.numeric(EFF_groups_tree)) %>%
              arrange(ag) %>%
              select(-ag) %>%
              mutate(group = ordered(group, unique(group)))) %>%
  mutate(name2 = paste0(name, "\n", label) %>%
           enc2utf8() %>%
           str_wrap(40)) %>%
  group_by(name2) %>%
  mutate(ord = sum(value %in% c("1", "2"))/n()) %>%
  ungroup() %>%
  arrange(-ord) %>%
  mutate(name2 = ordered(name2, unique(name2))) %>%
  ggplot(aes(x = name2, fill = value)) +
  geom_bar(stat = "count", position = "fill", color = "black") +
  coord_flip() +
  facet_wrap(~group, scales = "free_y", ncol = 2, dir = "v") +
  scale_fill_manual(values = c("darkred", "pink", "lightgreen", "darkgreen"),
                    labels = c("1-4", "5", "6", "7")) +
  geom_hline(yintercept = .5, lty = 2) +
  labs(fill = "Odpověď",
       y = "Podíl",
       x = "") +
  theme(legend.position = "bottom")

# Create a CFA model based on the detected communities --------------------

EFF_lav_mod <- lapply(unique(EFF_groups_tree),
                      function(x){colnames(EFF_dat_mod)[EFF_groups_tree==x] %>%
                        paste0(collapse = " + ") %>%
                        paste0("F", x, " =~ ", .)}) %>%
  unlist() %>%
  sort() %>%
  paste0(collapse = "\n")

# Split the data for a training and testing sample

EFF_tprop <- .5

EFF_train <- sample(1:nrow(EFF_dat_mod), size = round(nrow(EFF_dat_mod)*EFF_tprop, 0), replace = FALSE)

EFF_dat_mod_use <- EFF_dat_mod %>%
  `names<-`(gsub("SELFEFF|\\[|\\]", "", names(.)))

EFF_dat_mod_train <- EFF_dat_mod_use %>%
  .[EFF_train,]

EFF_dat_mod_test <- EFF_dat_mod_use %>%
  .[-EFF_train,]

# Fit the model on the training data

EFF_lav_mod_use <- EFF_lav_mod %>%
  gsub("SELFEFF|\\[|\\]", "", .)

EFF_fit_cfa_0 <- cfa(EFF_lav_mod_use,
                     ordered = TRUE,
                   EFF_dat_mod_train)

# Re-fit the model on a testing sample

EFF_fit_cfa <- cfa(model = EFF_fit_cfa_0,
                   ordered = TRUE,
                   EFF_dat_mod_test)

EFF_tab_fit <- fitmeasures(EFF_fit_cfa) %>%
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

EFF_rels <- semTools::reliability(EFF_fit_cfa) 

Coef_vec <- c(alpha = "Alpha (Cronbach, 1951)",
  alpha.ord = "Ordinální alpha (Zumbo et al., 2007)",
  omega = "Omega (Bollen, 1980)",
  omega2 = "Omega (Bentler, 1972)",
  omega3 = "Omega (McDonald, 1999)",
  avevar = "Average variance extracted (Fornell & Larcke, 1981)")

EFF_tab_rel <- EFF_rels %>%
  as_tibble() %>%
  mutate(Koeficient = Coef_vec[rownames(EFF_rels)]) %>%
  select(Koeficient, everything()) 

EFF_loads <- lavInspect(EFF_fit_cfa_0, "std")$lambda %>% 
  as_tibble() %>% 
  as.matrix() %>% 
  `rownames<-`(rownames(lavInspect(EFF_fit_cfa_0, "std")$lambda))

EFF_tab_loads <- EFF_loads %>%
  as_tibble() %>%
  mutate_all(function(x){
    temp_1 <- as.numeric(x) %>%
               round(3) 
    if_else(temp_1 == 0, "", as.character(temp_1))}) %>%
  mutate(Položka = rownames(EFF_loads) %>%
           gsub("(.{2})(.{3})", "\\1[\\2]", .)) %>%
  select(Položka, everything())

EFF_gcol_0 <- EFF_fit_tree$graph$Nodes$color %>%
  `names<-`(names(EFF_dat_mod) %>%
              gsub("\\[|\\]", "", .))

EFF_frefs <- EFF_lav_mod %>%
  strsplit("\n") %>%
  unlist() %>%
  gsub(".+ \\=\\~ ([^\\+]+) \\+.+", "\\1", .)
EFF_fs <- EFF_lav_mod %>%
  strsplit("\n") %>%
  unlist() %>%
  gsub("(.+) \\=\\~ ([^\\+]+) \\+.+", "\\1", .)

EFF_gcol_1 <- EFF_gcol_0[paste0("SELFEFF", lavNames(EFF_fit_cfa))]

EFF_gcol_2 <- EFF_gcol_0[EFF_frefs %>%
                           gsub("\\[|\\]", "", .)] %>%
  `names<-`(EFF_fs)

EFF_gcol <- c(EFF_gcol_1, EFF_gcol_2)

EFF_graph_sem <- semPlot::semPaths(EFF_fit_cfa, 
                                   "std", 
                                   intercepts = FALSE,
                                   residuals = FALSE,
                                   layout = "circle", 
                                   whatLabels = "std",
                                   nodeLabels = c(lavNames(EFF_fit_cfa_BF) %>% order(), names(EFF_gcol_2)),
                                   color = EFF_gcol)

EFF_lvmat <- lavInspect(EFF_fit_cfa, "cor.lv") %>% 
  as_tibble() %>% 
  as.matrix() %>% 
  `rownames<-`(colnames(.))

EFF_graph_lat <- qgraph(EFF_lvmat, 
                        layout = "spring",
                        graph = "cor",
                        threshold = .2,
                        color = EFF_gcol_2,
                        labels = names(EFF_gcol_2))

# Bi-factor model -----------------------------------------------------

BFadd <- paste0("G =~ ", paste0(colnames(EFF_dat_mod), collapse = " + "))
BFadd2 <- paste0("G ~~ 0*F", paste0(1:8, collapse = " + 0*F"))

EFF_lav_mod_BF <- EFF_lav_mod %>%
  paste0(., "\n", BFadd, "\n", BFadd2) %>%
  gsub("SELFEFF|\\[|\\]", "", .)

EFF_fit_cfa_BF <- cfa(EFF_lav_mod_BF,
                      ordered = TRUE,
                      EFF_dat_mod_train, 
                      std.lv = TRUE)

EFF_graph_BF_cor <- lavInspect(EFF_fit_cfa_BF, "cor.lv") %>%
  .[-9,-9] %>%
  qgraph(layout = "spring", graph = "cor", cut = .3, color = EFF_gcol_2, details = TRUE, vsize = 5)

EFF_graph_BF <- semPlot::semPaths(EFF_fit_cfa_BF,
                  whatLabels = "hide", 
                  layout = "tree3",
                  bifactor = "G",
                  sizeLat = 5, curvature = 2,
                  sizeMan = 3,edge.label.cex = .5,
                  residuals = FALSE, 
                  intercepts = FALSE, 
                  what = "std",
                  nodeLabels = c(lavNames(EFF_fit_cfa_BF) %>% order(), paste0("F", 1:8), "G"),
                  color = c(EFF_gcol, 
                            "#D99999"))

EFF_tab_fit_BF <- fitmeasures(EFF_fit_cfa_BF) %>%
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

EFF_rels_BF <- semTools::reliability(EFF_fit_cfa_BF) 

EFF_tab_rel_BF <- EFF_rels_BF %>%
  as_tibble() %>%
  mutate(Koeficient = Coef_vec[rownames(EFF_rels)]) %>%
  select(Koeficient, everything()) 

EFF_loads_BF <- lavInspect(EFF_fit_cfa_BF, "std")$lambda %>% 
  as_tibble() %>% 
  as.matrix() %>% 
  `rownames<-`(rownames(lavInspect(EFF_fit_cfa_BF, "std")$lambda))


EFF_tab_loads_BF <- EFF_loads_BF %>%
  as_tibble() %>%
  mutate_all(function(x){
    temp_1 <- as.numeric(x) %>%
      round(3) 
    if_else(temp_1 == 0, "", as.character(temp_1))}) %>%
  mutate(Položka = rownames(EFF_loads_BF) %>%
           gsub("(.{2})(.{3})", "\\1[\\2]", .)) %>%
  select(Položka, everything())

# Get factor scores -------------------------------------------------------


EFF_impdat <- mice(EFF_dat_mod_use)

EFF_impdat_use <- complete(EFF_impdat,1) %>%
  as_tibble() %>%
  mutate_all(ordered)

EFF_mdpat <- md.pattern(EFF_dat_mod_use, plot = FALSE)

EFF_scores <- lavPredict(EFF_fit_cfa, 
                         newdata = EFF_impdat_use,
                         method = "EBM")

EFF_sumscores <- lapply(unique(EFF_groups_tree),
       function(x){
         rowMeans(EFF_impdat_use[,EFF_groups_tree == x] %>%
                    mutate_all(as.numeric))
       }) %>%
  as.data.frame() %>%
  `names<-`(paste0("s_", colnames(EFF_scores))) %>%
  as_tibble()
  

EFF_sumscores_dat <- EFF_scores %>%
  as_tibble() %>%
  bind_cols(EFF_sumscores) %>%
  mutate_all(as.numeric) %>%
  mutate(id = 1:n()) %>%
  pivot_longer(cols = -id) %>%
  mutate(Skór = ifelse(grepl("s_", name), "Součtový", "Faktorový"),
         Faktor = gsub("s_", "", name)) %>%
  select(-name) %>%
  pivot_wider(id_cols = c(id, Faktor),
              names_from = Skór,
              values_from = value)

EFF_sumscores_cors <-  EFF_sumscores_dat %>%
  group_by(Faktor) %>%
  summarise(Korelace = cor(Faktorový, Součtový)) %>%
  mutate(Korelace = paste0("r = ", round(Korelace, 3)))


EFF_sumscores_plt <- EFF_sumscores_dat %>%
  ggplot(aes(x = Faktorový, y = Součtový)) +
  geom_hex(binwidth = c(.3,.3)) +
  facet_wrap(~Faktor, scales = "free_y") +
  geom_smooth(color = "red") +
  geom_label(data = EFF_sumscores_cors, aes(x = -1.5, y = 3.5, label = Korelace), inherit.aes = FALSE) +
  labs(fill = "Počet pozorování") +
  theme(legend.position = "bottom")

EFF_thresholds <- lavInspect(EFF_fit_cfa, "th") %>%
  enframe() %>%
  mutate(name3 = gsub(".+\\|(.+)", "\\1", name),
         name2 = gsub("(.+)\\|.+", "\\1", name)) %>%
  left_join(EFF_lab_tib %>%
              mutate(name2 = gsub(".+\\[(.+)\\]", "\\1", name)) %>%
              mutate(id = 1:n()), "name2")

EFF_thresholds_2 <- lavInspect(EFF_fit_cfa, "th") %>%
  enframe() %>%
  mutate(name3 = gsub(".+\\|(.+)", "\\1", name),
         name2 = gsub("(.+)\\|.+", "\\1", name)) %>%
  left_join(EFF_lab_tib %>%
              mutate(group = paste0("Faktor ", EFF_groups_tree)) %>%
              mutate(name2 = gsub(".+\\[(.+)\\]", "\\1", name)) %>%
              mutate(id = 1:n()), "name2")

EFF_plt_thresh_1 <- EFF_thresholds %>%
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
                                "lightgreen", 
                                "darkgreen"), 
                     labels = c("1-4|5", "5|6", "6|7"),
                     name = "Hranice") +
  theme(legend.position = "bottom")

EFF_plt_thresh_2 <- EFF_thresholds_2 %>%
  group_by(group) %>%
  arrange(value) %>%
  mutate(mark = 1:n(),
         mark = as.numeric(mark %% 2 == 0)) %>%
  ungroup() %>%
  ggplot(aes(x = group, 
             y = value, 
             color = ordered(name3),
             label = id)) +
  geom_vline(aes(xintercept = group), lty = 2) +
  geom_point(pch = "|", stroke = 20) +
  geom_label(aes(vjust = mark), show.legend = FALSE) +
  coord_flip() +
  scale_color_manual(values = c("darkred", 
                                "lightgreen", 
                                "darkgreen"), 
                     labels = c("1-4|5", "5|6", "6|7"),
                     name = "Hranice") +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Hodnota faktoru")

EFF_loads_thr <- EFF_loads %>%
  rowSums()

EFF_thresh_2_2 <- EFF_thresholds_2 %>%
  mutate(wgt = EFF_loads_thr[name2]) %>%
  select(group, name3, value, wgt) %>%
  group_by(group, name3) %>%
  mutate(wgt = wgt/sum(wgt)) %>%
  summarise(value_m = stats::weighted.mean(x = value, n()*wgt),
            value_sd = sqrt(Hmisc::wtd.var(x = value, weights =  n()*wgt)),
            size = n()) %>%
  ungroup() %>%
  mutate(value_se = value_sd/sqrt(size),
         value_ci = value_se*1.96)

EFF_plt_thresh_2_2 <- EFF_thresh_2_2 %>%
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
                                "lightgreen", 
                                "darkgreen"), 
                     labels = c("1-4|5", "5|6", "6|7"),
                     name = "Hranice (s 95% CI)") +
  theme(legend.position = "bottom") +
  labs(x = "", y = "Hodnota faktorového skóru")

EFF_scores_scaled <- lapply(1:ncol(EFF_scores),
                            function(x){
                              temp_dat <- EFF_scores[,x]
                              
                              temp_thresh <- EFF_thresh_2_2 %>%
                                select(group, name3, value_m) %>%
                                mutate_all(as.character) %>%
                                mutate_all(parse_number) %>%
                                filter(group == x) %>%
                                arrange(name3) %>%
                                select(value_m) %>%
                                unlist() %>%
                                c(-Inf, ., Inf)
                              
                              temp_vals <- cut(temp_dat, 
                                               temp_thresh, 
                                               include.lowest = TRUE, labels = c("1-4", "5", "6", "7"))
                            }) %>%
  `names<-`(colnames(EFF_scores)) %>%
  as_tibble() %>%
  mutate_all(ordered)

EFF_scores_scaled_plt <- EFF_scores_scaled %>%
  pivot_longer(-0) %>%
  group_by(name) %>%
  mutate(pos = sum(value %in% c("6", "7"))) %>%
  ungroup() %>%
  arrange(pos) %>%
  mutate(name = gsub("F", "Faktor ", name)) %>%
  mutate(name = ordered(name, unique(name))) %>%
  ggplot(aes(x = name, fill = value)) +
  geom_bar(stat = "count", position = "fill", color = "black") +
  coord_flip() +
  scale_fill_manual(values = c("darkred", "pink", "lightgreen", "darkgreen"),
                    labels = c("1-4", "5", "6", "7")) +
  geom_hline(yintercept = .5, lty = 2) +
  labs(fill = "Odpověď",
       y = "Podíl",
       x = "") +
  theme(legend.position = "bottom")



EFF_fcols <- EFF_gcol_0[lapply(unique(EFF_groups_tree), function(x){
  which(EFF_groups_tree == x)[1]
}) %>% unlist()]


EFF_plt_fac_group <- EFF_lab_tib %>%
  mutate(Faktor = paste0("Komunita ", EFF_groups_tree),
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
  scale_fill_manual(values = `names<-`(EFF_fcols, NULL))
  
# Create outputs ----------------------------------------------------------

W1 <- 14 
H1 <- 14
W2 <- 9 
H2 <- 9
W3 <- 6 
H3 <- 6

# ggsave

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/1_1_EFF_freq_plot.png",
       EFF_freq_plot,
       width = W1, 
       height = H1)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/2_1_EFF_plt_CI_1.png",
       EFF_plt_CI_1,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/2_2_EFF_plt_CI_2.png",
       EFF_plt_CI_2,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/2_8_EFF_nct_plt.png",
       EFF_nct_plt,
       width = W3, 
       height = H3)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/3_1_plt_EFF_dendro.png",
       plt_EFF_dendro,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/3_5_EFF_freq_plot_tree.png",
       EFF_freq_plot_tree,
       width = W1, 
       height = H1)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/4_5_EFF_sumscores_plt.png",
       EFF_sumscores_plt,
       width = W1, 
       height = H1)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/4_6_EFF_plt_thresh_1.png",
       EFF_plt_thresh_1,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/4_7_EFF_plt_thresh_2.png",
       EFF_plt_thresh_2,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/4_8_EFF_plt_thresh_2_2.png",
       EFF_plt_thresh_2_2,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/4_9_EFF_scores_scaled_plt.png",
       EFF_scores_scaled_plt,
       width = W2, 
       height = H2)

ggsave("outputs/code/0_2_1_cluster_EFF_ZU/png/5_1_EFF_plt_fac_group.png",
       EFF_plt_fac_group,
       width = W2, 
       height = H3)


# png

png("outputs/code/0_2_1_cluster_EFF_ZU/png/2_4_EFF_fit.png",
    width = W1, height = H3, units = "in", res = 360)
plot(EFF_fit)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/2_5_EFF_fit_nl.png",
    width = W3, height = H3, units = "in", res = 360)
plot(EFF_fit_nl)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/2_6_EFF_fit_cons.png",
    width = W1, height = H3, units = "in", res = 360)
plot(EFF_fit_cons)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/2_7_EFF_fit_cons_nl.png",
    width = W3, height = H3, units = "in", res = 360)
plot(EFF_fit_cons_nl)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/3_2_EFF_graph_comm.png",
    width = W3, height = H3, units = "in", res = 360)
plot(EFF_graph_comm)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/3_3_EFF_fit_tree.png",
    width = W1, height = H3, units = "in", res = 360)
plot(EFF_fit_tree)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/3_4_EFF_fit_tree_nl.png",
    width = W3, height = H3, units = "in", res = 360)
plot(EFF_fit_tree_nl)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/4_1_EFF_graph_sem.png",
    width = W2, height = H2, units = "in", res = 360)
plot(EFF_graph_sem)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/4_2_EFF_graph_lat.png",
    width = W2, height = H2, units = "in", res = 360)
plot(EFF_graph_lat)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/4_3_EFF_graph_BF.png",
    width = W1, height = H2, units = "in", res = 360)
plot(EFF_graph_BF)
dev.off()

png("outputs/code/0_2_1_cluster_EFF_ZU/png/4_4_EFF_graph_BF_cor.png",
    width = W1, height = H2, units = "in", res = 360)
plot(EFF_graph_BF_cor)
dev.off()

# Save tables

tab_mod <- function(x){x %>%
    mutate_all(function(x){round(x, 3) %>%
        as.character() %>%
        gsub("^0\\.","\\.",.)})}

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/1_EFF_lab_tib_print.txt",
            EFF_lab_tib_print,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/2_EFF_fit_m.txt",
            tab_mod(EFF_fit_m),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/3_EFF_nct_test.txt",
            tab_mod(EFF_nct_test),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/4_EFF_lab_tib_print_tree.txt",
            EFF_lab_tib_print_tree,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/5_EFF_tab_fit.txt",
            tab_mod(EFF_tab_fit),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/6_EFF_tab_rel.txt",
            EFF_tab_rel %>%
              mutate_at(-1,function(x){gsub("^0", "", as.character(round(x,3)))}),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/7_EFF_tab_loads.txt",
            EFF_tab_loads %>%
              mutate_at(-1,function(x){gsub("^0", "", x)}),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/8_EFF_tab_fit_BF.txt",
            tab_mod(EFF_tab_fit_BF),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/9_EFF_tab_rel_BF.txt",
            EFF_tab_rel_BF %>%
              mutate_at(-1,function(x){gsub("^0", "", as.character(round(x,3)))}),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_2_1_cluster_EFF_ZU/tab/10_EFF_tab_loads_BF.txt",
            EFF_tab_loads_BF %>%
              mutate_at(-1,function(x){gsub("^0", "", x)}),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

#### Save data


EFF_D0 <- data_use %>%
  filter(filt_RESPs) %>%
  bind_cols()

EFF_D1 <- EFF_impdat_use %>%
  `colnames<-`(paste0("EFF_imputed_", names(.)))

EFF_D2 <- EFF_scores %>%
  as_tibble() %>%
  `colnames<-`(paste0("EFF_", names(.))) %>%
  mutate_all(as.numeric)

EFF_D3 <- EFF_sumscores %>%
  `colnames<-`(paste0("EFF_sumscore", names(.)))

EFF_D_scores <- bind_cols(EFF_D1, EFF_D2, EFF_D3)

write_csv(EFF_D0, "outputs/code/0_2_1_cluster_EFF_ZU/dat/1_EFF_data_0.csv")
write_csv(EFF_D_scores, "outputs/code/0_2_1_cluster_EFF_ZU/dat/2_EFF_D_scores.csv")