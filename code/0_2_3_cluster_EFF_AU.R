source("code/0_0_1_packages.R", encoding = "UTF-8")
source("code/0_0_2_funs.R", encoding = "UTF-8")
source("code/0_1_2_clean_AU.R", encoding = "UTF-8")

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

# Read data and prepare labels --------------------------------------------
EFFAU_outputs <- list()

# Read ZU

EFF0_par_list <- read_rds("outputs/code/0_2_1_cluster_EFF_ZU/rds/EFF_par_list.rds")
EFF0_outputs <- read_rds("outputs/code/0_2_1_cluster_EFF_ZU/rds/EFF_outputs.rds")

# Reading data

EFFAU_outputs$data$all <- AU_data_use %>%
  filter(AU_filt_RESPs)

EFFAU_dat <- AU_data_use %>%
  filter(AU_filt_RESPs) %>%
  select(match_tib$l1) %>%
  `names<-`(match_tib$ml1_mod) %>%
  select(starts_with("SELFEFF")) %>%
  mutate_all(parse_number)

EFFAU_outputs$data$raw <- EFFAU_dat

EFFAU_dat_mod <- EFFAU_dat

EFFAU_dat_mod2 <- EFFAU_dat_mod %>%
  mutate_all(function(x){ifelse(x < 5, 1, x) %>%
      ordered() %>%
      as.numeric() %>%
      ordered()})

EFFAU_outputs$data$mod <- EFFAU_dat_mod2

# Creating labels 

EFFAU_varcodes <- EFF0_par_list$varlabs 

EFFAU_labs <- AU_varlabs_2 %>%
  `names<-`(AU_varnames) %>%
  .[match_tib$l1] %>%
  `names<-`(match_tib$ml1_mod) %>%
  .[colnames(EFFAU_dat_mod)] %>%
  enc2utf8()

EFFAU_groups <- EFF0_par_list$groups$init

EFFAU_lab_tib <- tibble(name = names(EFFAU_dat),
                      code = EFFAU_varcodes[name],
                      label = EFFAU_labs[name],
                      group = EFFAU_groups[name]) %>%
  mutate(label = gsub("\U202F", " ", label))

EFFAU_lab_tib_print <- EFFAU_lab_tib %>%
  arrange(code) %>%
  arrange(group) %>%
  group_by(group) %>%
  mutate(mark = 1:n()) %>%
  mutate(group = ifelse(mark == 1, group, "")) %>%
  ungroup() %>%
  select(-mark, -name) %>%
  `names<-`(c("Kód", "Položka", "Okruh")) %>%
  mutate(Kód = 1:n()) %>%
  select(Okruh, Kód, Položka)

# Proportions of responses per item

EFFAU_props <- EFFAU_dat_mod %>%
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

EFFAU_piecols <- list(use_cols[7:1])[rep(1, ncol(EFFAU_dat_mod))] %>%
  `names<-`(names(EFFAU_props))

EFFAU_par_list <- list(
  sampleSize = nrow(EFFAU_dat_mod),
  varlabs = EFFAU_varcodes,
  nodeNames = EFFAU_labs,
  pies = EFFAU_props,
  pieCols = EFFAU_piecols,
  lab_vec = as.character(c(1:7)),
  ref_vec = c("1", "2", "3", "4"),
  col_vec = use_cols[7:1])  

EFFAU_par_list$groups$init <- EFFAU_groups

EFFAU_outputs$tabs$lab_init <- EFFAU_lab_tib_print

# Create descriptive summaries --------------------------------------------

EFFAU_outputs$freq$compare <- bind_rows(
  tibble(
    sada = "Začínající učitelé",
    Odpověď = EFF0_outputs$data$all %>%
      `colnames<-`(gsub("\\[", "_", colnames(.)) %>%
                     gsub("\\]", "", .)) %>%
      .[,colnames(EFFAU_dat)] %>% 
      unlist() %>%
      parse_number()),
  tibble(
    sada = "Absolventi",
    Odpověď = EFFAU_dat %>% unlist())
) %>%
  na.omit() %>%
  group_by(sada, Odpověď) %>%
  summarise(Podíl = n()) %>%
  group_by(sada) %>%
  mutate(Podíl = Podíl/sum(Podíl)) %>%
  ungroup() %>%
  ggplot(aes(x = ordered(Odpověď), y = Podíl, fill = sada)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = unique(AU_data_use$`SELFEFF[SQ001]`) %>%
                     sort() %>%
                     na.omit()) +
  labs(x = "Odpověď", fill = "Datová sada") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = use_cols[c(1, length(use_cols))])



EFFAU_outputs$freq$init <- freq_plot(dat_tib = EFFAU_dat_mod,
                                     par_list = EFFAU_par_list,
                                     group = "init")  

EFFAU_par_list$groups$spin0 <- EFF0_par_list$groups$spin

EFFAU_outputs$freq$compare_AU <- freq_plot(dat_tib = EFFAU_dat_mod,
                                   par_list = EFFAU_par_list,
                                   group = "spin0")  

EFFAU_outputs$freq$compare_ZU <- freq_plot(dat_tib = EFF0_outputs$data$raw,
                                   par_list = EFFAU_par_list,
                                   group = "spin0")  

# Fit network -----------------------------------------------------------
# Correlation matrix and first model
EFFAU_par_list$wmats$cormat <- cor_auto(EFFAU_dat_mod)

EFFAU_fit_0 <- qgraph(EFFAU_par_list$wmats$cormat, 
                    graph = "glasso",
                    sampleSize = EFFAU_par_list$sampleSize,
                    groups = EFFAU_par_list$groups[[1]][colnames(EFFAU_par_list$wmats$cormat)],
                    threshold = TRUE,
                    layout = "spring",
                    color = EFF0_par_list$color$init[colnames(EFFAU_par_list$wmats$cormat)],
                    DoNotPlot = TRUE)

EFFAU_par_list$wmats$gmat <- getWmat(EFFAU_fit_0)

EFFAU_par_list$color$init <- EFFAU_fit_0$graphAttributes$Nodes$color %>%
  `names<-`(colnames(EFFAU_dat_mod))

EFFAU_par_list$layout <- EFFAU_fit_0$layout %>%
  `rownames<-`(colnames(EFFAU_dat_mod))

EFFAU_outputs$qgraphs$init <- qgraph_w_pars(cmat = EFFAU_par_list$wmats$gmat,
                                          par_list = EFFAU_par_list,
                                          group = 1,
                                          color = "init",
                                          legend = FALSE)


EFFAU_outputs$qgraphs_nl$init <- qgraph_w_pars(cmat = EFFAU_par_list$wmats$gmat,
                                             par_list = EFFAU_par_list,
                                             group = 1,
                                             color = "init",
                                             legend = TRUE)

# First model fit

EFFAU_outputs$tabs$fit_init <- ggmFit(EFFAU_outputs$qgraphs$init, 
                                    covMat = EFFAU_par_list$wmats$cormat,
                                    sampleSize = nrow(EFFAU_dat_mod))  %>%
  ggmFit_table()

EFFAU_outputs$tabs$fit_EFF <- ggmFit(EFF0_par_list$wmats$gmat[colnames(EFFAU_par_list$wmats$cormat),
                                                              colnames(EFFAU_par_list$wmats$cormat)],
                                     EFFAU_par_list$wmats$cormat, 
                                     sampleSize = nrow(EFFAU_dat_mod)) %>%
  ggmFit_table()


# Bootstrap network stability ---------------------------------------------
# Perform bootstrapping

EFFAU_nboots <- 1000

EFFAU_boot0 <- estimateNetwork(EFFAU_outputs$data$mod,
                             fun = function(x){
                               cmt <- cor_auto(x,
                                               forcePD = TRUE,
                                               detectOrdinal = FALSE)
                               qgraph::EBICglasso(cmt,
                                                  nrow(x),
                                                  threshold = TRUE)
                             })

EFFAU_boots <- bootnet(EFFAU_boot0,
                     nBoots = EFFAU_nboots,
                     statistics = "edge")

EFFAU_outputs$ggplot$boot <- boot_plot(EFFAU_boots,
                                     EFFAU_par_list)


# Plot conservative network estimates

EFFAU_par_list$wmats$gmat_cons <- summary(EFFAU_boots) %>%
  ungroup() %>%
  mutate(weight = ifelse(q2.5 > 0 | q97.5 < 0, mean, 0)) %>%
  select(node1, node2, weight) %>%
  qgraph(directed = FALSE, 
         DoNotPlot = TRUE) %>%
  getWmat()

EFFAU_outputs$qgraphs$cons_fit <- qgraph_w_pars(cmat = EFFAU_par_list$wmats$gmat_cons,
                                              par_list = EFFAU_par_list,
                                              color = "init",
                                              group = "init",
                                              legend = TRUE)

EFFAU_outputs$qgraphs_nl$cons_fit <- qgraph_w_pars(cmat = EFFAU_par_list$wmats$gmat_cons,
                                                 par_list = EFFAU_par_list,
                                                 color = "init",
                                                 group = "init",
                                                 legend = FALSE)


# Compare teacher groups --------------------------------------------------

set.seed(993)
EFFAU_nct <- NetworkComparisonTest::NCT(data1 = EFF0_outputs$data$raw %>%
                                          .[,colnames(EFFAU_dat)],
                                      data2 = EFFAU_dat_mod,
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

EFFAU_outputs$nct <- nct_plot(EFFAU_nct)

EFFAU_nct_wmats <- nct_nets(EFFAU_nct)

EFFAU_outputs$qgraphs$nct <- qgraph_w_pars(cmat = EFFAU_nct_wmats$diffs,
                                         par_list = EFFAU_par_list,
                                         color = "init",
                                         group = "init",
                                         legend = FALSE)

EFFAU_outputs$qgraphs$nct_sig <- qgraph_w_pars(cmat = EFFAU_nct_wmats$diffs_sig,
                                             par_list = EFFAU_par_list,
                                             color = "init",
                                             group = "init",
                                             legend = FALSE)

# Spinglass community detection -------------------------------------------

# Initial communities

EFFAU_par_list$cut <- .5
EFFAU_spins <- spin_loop(EFFAU_outputs$qgraphs$init,
                       gamma = 1,
                       cut = EFFAU_par_list$cut)

EFFAU_groups_cols_tib <- hclust_palette(EFFAU_spins$hclust, 
                                      start_hue = 100, 
                                      max_hue = 330, 
                                      distinction = 15,
                                      min_luminance = 60,
                                      max_luminance = 90,
                                      mod_hgt = FALSE)

EFFAU_par_list$color$spin <- EFFAU_groups_cols_tib$color %>%
  `names<-`(EFFAU_groups_cols_tib$variable)

EFFAU_par_list$groups$spin <- EFFAU_spins$hclust %>%
  cutree(., h = EFFAU_par_list$cut) %>%
  paste0("Komunita ", .) %>%
  `names<-`(names(EFFAU_spins$groups))

EFFAU_par_list$wmats$spin <- EFFAU_spins$mat

EFFAU_outputs$qgraphs_nl$spin_coms <- qgraph_w_pars(EFFAU_par_list$wmats$spin,
                                                  par_list = EFFAU_par_list,
                                                  color = "spin",
                                                  group = "spin",
                                                  layout = "spring",
                                                  legend = FALSE)

EFFAU_outputs$ggplot$dendro <- plt_dendro(EFFAU_spins$data_dendro,
                                        par_list = EFFAU_par_list,
                                        color = "spin")

EFFAU_outputs$qgraphs$spin <- qgraph_w_pars(EFFAU_par_list$wmats$gmat,
                                          par_list = EFFAU_par_list,
                                          color = "spin",
                                          group = "spin",
                                          legend = TRUE)

EFFAU_outputs$qgraphs_nl$spin <- qgraph_w_pars(EFFAU_par_list$wmats$gmat,
                                             par_list = EFFAU_par_list,
                                             color = "spin",
                                             group = "spin",
                                             legend = FALSE)

EFFAU_mat <- tibble(ZU = EFF0_par_list$groups$spin[names(EFFAU_par_list$groups$spin)],
       AU = EFFAU_par_list$groups$spin) %>%
  group_by(ZU, AU) %>%
  summarise(cnt = n()) %>%
  ungroup() %>%
  reshape2::dcast(AU ~ ZU, value.var = "cnt", fill = 0) %>%
  `rownames<-`(., .[,1]) %>%
  .[,-1] %>%
  mutate_all(as.numeric) %>%
  as.matrix()

Rowv <- as.dendrogram(hclust(dist(-EFFAU_mat)))
rowInd <- order.dendrogram(Rowv)

Colv <- as.dendrogram(hclust(dist(t(-EFFAU_mat))))
colInd <- order.dendrogram(Colv)

EFFAU_outputs$ggplot$compare_coms <- tibble(ZU = EFF0_par_list$groups$spin[names(EFFAU_par_list$groups$spin)],
       AU = EFFAU_par_list$groups$spin) %>%
  mutate(AU = ordered(AU, rownames(EFFAU_mat)[rowInd]),
         ZU = ordered(ZU, colnames(EFFAU_mat)[colInd])) %>%
  group_by(ZU, AU) %>%
  summarise(cnt = n()) %>%
  group_by(ZU) %>%
  mutate(cnt2 = cnt/sum(cnt)) %>%
  ungroup() %>%
  ggplot(aes(x = ZU, y = AU, fill = cnt2, label = cnt)) +
  geom_tile(color = "black") +
  geom_text() +
  coord_fixed() +
  theme_classic() +
  labs(x = "Začínající učitelé", y = "Absolventi učitelství") +
  scale_fill_gradient2(low = use_cols[7], high = use_cols[1], mid = use_cols[4], midpoint = .5) +
  theme(legend.position = "none", panel.grid.major = element_line(color = "grey"))

EFFAU_outputs$tabs$lab_spin <- EFFAU_lab_tib %>%
  mutate(Komunita = EFFAU_par_list$groups$spin[name],
         Komunita_2 = parse_number(Komunita)) %>%
  arrange(Komunita_2) %>%
  mutate(Komunita = ordered(Komunita, unique(Komunita))) %>%
  group_by(Komunita) %>%
  mutate(ag = 1:n()) %>%
  ungroup() %>%
  mutate(Komunita = ifelse(ag == 1, as.character(Komunita), "")) %>%
  rename(Okruh = group,
         Kód = code,
         Položka = label) %>%
  select(Komunita, Okruh, Kód, Položka)

EFFAU_outputs$freq$spin <- freq_plot(dat_tib = EFFAU_dat_mod,
                                   par_list = EFFAU_par_list, 
                                   group = "spin",
                                   f.order = unique(EFFAU_par_list$groups$spin))  

# Create a CFA model based on the detected communities --------------------

EFFAU_firsts <- EFFAU_par_list$groups$spin %>%
  unique() %>%
  lapply(function(x){EFFAU_par_list$groups$spin[EFFAU_par_list$groups$spin == x][1]}) %>%
  unlist()

# Color per community, based on first ov from every community

EFFAU_par_list$color$communities <- EFFAU_par_list$color$spin[names(EFFAU_firsts)] %>%
  `names<-`(EFFAU_firsts)

# First ov belonging to each lv (for color selection)

EFFAU_par_list$factors <- names(table(EFFAU_par_list$groups$spin)[table(EFFAU_par_list$groups$spin)>2]) %>%
  .[order(parse_number(.))]

names(EFFAU_par_list$factors) <- EFFAU_par_list$factors %>%
  lapply(function(x){
    names(EFFAU_par_list$groups$spin[EFFAU_par_list$groups$spin==x])[1]}) %>%
  unlist()

# Latent variable colors

EFFAU_par_list$color$cfa_lv_1 <- EFFAU_par_list$color$spin[names(EFFAU_par_list$factors)] %>%
  `names<-`(gsub("Komunita ", "K", EFFAU_par_list$factors))

# Node colors for sem plot

EFFAU_par_list$color$cfa_1 <- c(EFFAU_par_list$color$spin, EFFAU_par_list$color$cfa_lv_1)

# Not labels for sem plot

EFFAU_par_list$fvarlabs <- names(EFFAU_par_list$color$cfa_lv_1) %>% 
  `names<-`(names(EFFAU_par_list$color$cfa_lv_1)) %>%
  c(EFFAU_par_list$varlabs, G = "G", .)

# First cfa model

EFFAU_par_list$cfa_mod$init <- lav_mod(EFFAU_par_list, groups = "spin", minvar = 2)

# Split the data for a training and testing sample

EFFAU_outputs$fits$init <- fit_lav(model = EFFAU_par_list$cfa_mod$init,
                                 ordered = names(EFFAU_par_list$varlabs),
                                 par_list = EFFAU_par_list,
                                 data = EFFAU_dat_mod2)

EFFAU_gload <- EFF0_outputs$fits$bifactor$tabs$loads[,"G"][1]
BFadd <- paste0("G =~ ", paste0(EFFAU_gload, "*", colnames(EFFAU_dat_mod), collapse = " + "))
BFadd2 <- paste0("G ~~ 0*", lavNames(EFFAU_outputs$fits$init$fits$f_train, "lv"), collapse = "\n")


EFFAU_par_list$cfa_mod$bifactor <- lav_mod(EFFAU_par_list, 
                                         groups = "spin", 
                                         minvar = 2, 
                                         add = paste0(BFadd, "\n", BFadd2, "\n"))

EFFAU_outputs$fits$bifactor <- fit_lav(model = EFFAU_par_list$cfa_mod$bifactor,
                                     ordered = names(EFFAU_par_list$varlabs),
                                     par_list = EFFAU_par_list,
                                     data = EFFAU_dat_mod2)

EFFAU_par_list$fnames$init <- lavNames(EFFAU_outputs$fits$init$fits$f_train, c("ov", "lv")) %>%
  unlist(recursive = TRUE)

EFFAU_par_list$fnames_s$init <- lavNames(EFFAU_outputs$fits$init$fits$f_train, c("lv")) %>%
  unlist(recursive = TRUE)

EFFAU_par_list$fnames$bifactor <- lavNames(EFFAU_outputs$fits$bifactor$fits$f_train, c("ov", "lv")) %>%
  unlist(recursive = TRUE)

EFFAU_par_list$fnames_s$bifactor <- lavNames(EFFAU_outputs$fits$bifactor$fits$f_train, c("lv")) %>%
  unlist(recursive = TRUE)

EFFAU_par_list$color$cfa_2 <- c(EFFAU_par_list$color$cfa_1, G = "steelblue")
EFFAU_par_list$fnames$cfa_2 <- c(EFFAU_par_list$color$cfa_1, G = "steelblue")

EFFAU_outputs$semplots$init <- sem_plots(lav_fit = EFFAU_outputs$fits$init$fits$f_test,
                                       par_list = EFFAU_par_list,
                                       group = "init",
                                       color = "cfa_1")

EFFAU_outputs$semplots$bifactor <- sem_plots(lav_fit = EFFAU_outputs$fits$bifactor$fits$f_test,
                                           par_list = EFFAU_par_list,
                                           group = "bifactor",
                                           color = "cfa_2",
                                           bifactor = "G")

EFFAU_outputs$fits$EFFinit$fit <- cfa(model = EFF0_outputs$fits$init$fits$f_train,
                                   data = EFFAU_dat_mod %>%
                                    mutate_all(function(x){ifelse(x < 5, 4, x) %>%
                                        ordered() %>%
                                        as.numeric() %>%
                                        ordered()}))

EFFAU_outputs$fits$compare <- fit_lav(model = EFF0_outputs$fits$init$fits$f_train,
                                       ordered = names(EFFAU_par_list$varlabs),
                                       par_list = EFFAU_par_list,
                                       data = EFFAU_dat_mod2)


# Get factor scores -------------------------------------------------------

EFF0_par_list2 <- EFF0_par_list
EFF0_par_list2$groups$spin2 <- EFF0_par_list2$groups$spin[names(EFF0_par_list2$groups$spin) %in% colnames(EFFAU_dat_mod2)]
EFF0_par_list2$groups$init2 <- EFF0_par_list2$groups$init[names(EFF0_par_list2$groups$init) %in% colnames(EFFAU_dat_mod2)]

EFFAU_outputs$scores <- get_scores(data = EFFAU_dat_mod2,
                                 fit_obj = EFF0_outputs$fits$init$fits$f_train,
                                 par_list = EFF0_par_list2,
                                 lab_vec_ord = EFF0_par_list2$lab_vec, 
                                 group = c("spin2","init2"),
                                 mimp = 1,
                                 color = "spin")

EFFAU_outputs$ggplot$groups <- groups_match(par_list = EFFAU_par_list,
                                          groups = c("spin", "init"), 
                                          color = "communities")

write_rds(EFFAU_par_list, "outputs/code/0_2_3_cluster_EFFAU_ZU/rds/EFFAU_par_list.rds")
write_rds(EFFAU_outputs, "outputs/code/0_2_3_cluster_EFFAU_ZU/rds/EFFAU_outputs.rds")