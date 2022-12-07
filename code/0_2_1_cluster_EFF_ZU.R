source("code/0_0_1_packages.R", encoding = "UTF-8")
source("code/0_0_2_funs.R", encoding = "UTF-8")
source("code/0_1_1_clean_ZU.R", encoding = "UTF-8")

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

# Set up outputs container ------------------------------------------------

EFF_outputs <- list()

EFF_outputs$data$all <- data_use %>%
  filter(filt_RESPs)

# Read data and prepare labels --------------------------------------------

# Reading data

EFF_dat <- data_use %>%
  filter(filt_RESPs) %>%
  select(starts_with("SELFEFF")) %>%
  mutate_all(parse_number) %>%
  `colnames<-`(gsub("\\[", "_", colnames(.)) %>%
                 gsub("\\]", "", .))

EFF_outputs$data$raw <- EFF_dat

EFF_dat_group <- data_use %>%
  filter(filt_RESPs) %>%
  select(TIMEEMPLOYEDSCHOOL)

EFF_dat_mod <- EFF_dat %>%
  mutate_all(function(x)ifelse(x < 5, 1, x) %>% 
               ordered() %>% 
               as.numeric() %>% 
               ordered())

EFF_outputs$data$mod <- EFF_dat_mod

# Creating labels 

EFF_varcodes <- c(1:ncol(EFF_dat)) %>%
  `names<-`(colnames(EFF_dat))

EFF_labs <- varlabs_2[grepl("SELFEFF", varnames)] %>%
  enc2utf8() %>%
  `names<-`(colnames(EFF_dat))

EFF_groups <- c(
  rep("1. PLÁNOVÁNÍ VÝUKY", 4),
  rep("2. PROCESY UČENÍ", 7),
  rep("3. PROSTŘEDÍ PRO UČENÍ", 6),
  rep("4. HODNOCENÍ", 5),
  rep("5. REFLEXE VÝUKY", 4),
  rep("6. ROZVOJ ŠKOLY, SPOLUPRÁCE", 6)
) %>%
  enc2utf8() %>%
  `names<-`(colnames(EFF_dat))

EFF_lab_tib <- tibble(name = names(EFF_dat),
                      code = EFF_varcodes[name],
                      label = EFF_labs,
                      group = EFF_groups) %>%
  mutate(label = gsub("\U202F", " ", label))

EFF_lab_tib_print <- EFF_lab_tib %>%
  group_by(group) %>%
  mutate(mark = 1:n()) %>%
  mutate(group = ifelse(mark == 1, group, "")) %>%
  ungroup() %>%
  select(-mark, -name) %>%
  `names<-`(c("Kód", "Položka", "Okruh")) %>%
  mutate(Kód = 1:n()) %>%
  select(Okruh, Kód, Položka)

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

EFF_piecols <- list(use_cols[c(6,3,2,1)])[rep(1, ncol(EFF_dat_mod))] %>%
  `names<-`(names(EFF_props))

EFF_par_list <- list(
  sampleSize = nrow(EFF_dat_mod),
  varlabs = EFF_varcodes,
  nodeNames = EFF_labs,
  pies = EFF_props,
  pieCols = EFF_piecols,
  lab_vec = c("1-4", "5", "6", "7"),
  ref_vec = c("1", "2"),
  col_vec = use_cols[c(6,3,2,1)])  

EFF_par_list$groups$init <- EFF_groups

EFF_outputs$tabs$lab_init <- EFF_lab_tib_print

# Create descriptive summaries --------------------------------------------

EFF_outputs$freq$init <- freq_plot(dat_tib = EFF_dat_mod,
                           par_list = EFF_par_list,
                           group = "init")  


# Fit network -----------------------------------------------------------
# Correlation matrix and first model
EFF_par_list$wmats$cormat <- cor_auto(EFF_dat_mod)

EFF_fit_0 <- qgraph(EFF_par_list$wmats$cormat, 
                    graph = "glasso",
                    sampleSize = EFF_par_list$sampleSize,
                    groups = EFF_par_list$groups[[1]][colnames(EFF_par_list$wmats$cormat)],
                    threshold = TRUE,
                    layout = "spring",
                    palette = "pastel",
                    DoNotPlot = TRUE)

EFF_par_list$wmats$gmat <- getWmat(EFF_fit_0)

EFF_par_list$color$init <- EFF_fit_0$graphAttributes$Nodes$color %>%
  `names<-`(colnames(EFF_dat_mod))

EFF_par_list$layout <- EFF_fit_0$layout %>%
  `rownames<-`(colnames(EFF_dat_mod))

EFF_outputs$qgraphs$init <- qgraph_w_pars(cmat = EFF_par_list$wmats$gmat,
                         par_list = EFF_par_list,
                         group = 1,
                         color = "init",
                         legend = FALSE)

EFF_outputs$qgraphs_nl$init <- qgraph_w_pars(cmat = EFF_par_list$wmats$gmat,
                         par_list = EFF_par_list,
                         group = 1,
                         color = "init",
                         legend = TRUE)

# First model fit

EFF_outputs$tabs$fit_init <- ggmFit(EFF_outputs$qgraphs$init, 
                    covMat = EFF_par_list$wmats$cormat,
                    sampleSize = nrow(EFF_dat_mod))  %>%
  ggmFit_table()


# Bootstrap network stability ---------------------------------------------
# Perform bootstrapping

EFF_nboots <- 1000

EFF_boot0 <- estimateNetwork(EFF_outputs$data$mod,
                             fun = function(x){
                               
                               cmt <- cor_auto(x,
                                               forcePD = TRUE,
                                               detectOrdinal = FALSE)
                               
                               qgraph::EBICglasso(cmt,
                                                  nrow(x),
                                                  threshold = TRUE)
                             })

EFF_boots <- bootnet(EFF_boot0,
                     nBoots = EFF_nboots,
                     statistics = "edge")


EFF_outputs$ggplot$boot <- boot_plot(EFF_boots,
                           EFF_par_list)


# Plot conservative network estimates

EFF_par_list$wmats$gmat_cons <- summary(EFF_boots) %>%
  ungroup() %>%
  mutate(weight = ifelse(q2.5 > 0 | q97.5 < 0, mean, 0)) %>%
  select(node1, node2, weight) %>%
  qgraph(directed = FALSE, 
         DoNotPlot = TRUE) %>%
  getWmat()

EFF_outputs$qgraphs$cons_fit <- qgraph_w_pars(cmat = EFF_par_list$wmats$gmat_cons,
                              par_list = EFF_par_list,
                              color = "init",
                              group = "init",
                              legend = TRUE)

EFF_outputs$qgraphs_nl$cons_fit <- qgraph_w_pars(cmat = EFF_par_list$wmats$gmat_cons,
                                 par_list = EFF_par_list,
                                 color = "init",
                                 group = "init",
                                 legend = FALSE)


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

EFF_outputs$nct <- nct_plot(EFF_nct)

EFF_nct_wmats <- nct_nets(EFF_nct)

EFF_outputs$qgraphs$nct <- qgraph_w_pars(cmat = EFF_nct_wmats$diffs,
                               par_list = EFF_par_list,
                               color = "init",
                               group = "init",
                               legend = FALSE)

EFF_outputs$qgraphs$nct_sig <- qgraph_w_pars(cmat = EFF_nct_wmats$diffs_sig,
                               par_list = EFF_par_list,
                               color = "init",
                               group = "init",
                               legend = FALSE)

# Spinglass community detection -------------------------------------------

# Initial communities

EFF_par_list$cut <- .5
EFF_spins <- spin_loop(EFF_outputs$qgraphs$init,
                       gamma = 1,
                       cut = EFF_par_list$cut)

EFF_groups_cols_tib <- hclust_palette(EFF_spins$hclust, 
                                      start_hue = 100, 
                                      max_hue = 330, 
                                      distinction = 15,
                                      min_luminance = 60,
                                      max_luminance = 90,
                                      mod_hgt = FALSE)

EFF_par_list$color$spin <- EFF_groups_cols_tib$color %>%
  `names<-`(EFF_groups_cols_tib$variable)

EFF_par_list$groups$spin <- EFF_spins$hclust %>%
  cutree(., h = EFF_par_list$cut) %>%
  paste0("Komunita ", .) %>%
  `names<-`(names(EFF_spins$groups))

EFF_par_list$wmats$spin <- EFF_spins$mat

EFF_outputs$qgraphs_nl$spin_coms <- qgraph_w_pars(EFF_par_list$wmats$spin,
                                par_list = EFF_par_list,
                                color = "spin",
                                group = "spin",
                                layout = "spring",
                                legend = FALSE)

EFF_outputs$ggplot$dendro <- plt_dendro(EFF_spins$data_dendro,
                             par_list = EFF_par_list,
                             color = "spin")

EFF_outputs$qgraphs$spin <- qgraph_w_pars(EFF_par_list$wmats$gmat,
                              par_list = EFF_par_list,
                              color = "spin",
                              group = "spin",
                              legend = TRUE)

EFF_outputs$qgraphs_nl$spin <- qgraph_w_pars(EFF_par_list$wmats$gmat,
                              par_list = EFF_par_list,
                              color = "spin",
                              group = "spin",
                              legend = FALSE)

EFF_outputs$tabs$lab_spin <- EFF_lab_tib %>%
  mutate(Komunita = EFF_par_list$groups$spin[name],
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

EFF_outputs$freq$spin <- freq_plot(dat_tib = EFF_dat_mod,
                                par_list = EFF_par_list, 
                                group = "spin",
                                f.order = unique(EFF_par_list$groups$spin))  

# Create a CFA model based on the detected communities --------------------

EFF_firsts <- EFF_par_list$groups$spin %>%
  unique() %>%
  lapply(function(x){EFF_par_list$groups$spin[EFF_par_list$groups$spin == x][1]}) %>%
  unlist()

# Color per community, based on first ov from every community

EFF_par_list$color$communities <- EFF_par_list$color$spin[names(EFF_firsts)] %>%
  `names<-`(EFF_firsts)

# First ov belonging to each lv (for color selection)

EFF_par_list$factors <- names(table(EFF_par_list$groups$spin)[table(EFF_par_list$groups$spin)>2]) %>%
  .[order(parse_number(.))]

names(EFF_par_list$factors) <- EFF_par_list$factors %>%
  lapply(function(x){
    names(EFF_par_list$groups$spin[EFF_par_list$groups$spin==x])[1]}) %>%
  unlist()

# Latent variable colors

EFF_par_list$color$cfa_lv_1 <- EFF_par_list$color$spin[names(EFF_par_list$factors)] %>%
  `names<-`(gsub("Komunita ", "K", EFF_par_list$factors))

# Node colors for sem plot

EFF_par_list$color$cfa_1 <- c(EFF_par_list$color$spin, EFF_par_list$color$cfa_lv_1)

# Not labels for sem plot

EFF_par_list$fvarlabs <- names(EFF_par_list$color$cfa_lv_1) %>% 
  `names<-`(names(EFF_par_list$color$cfa_lv_1)) %>%
  c(EFF_par_list$varlabs, G = "G", .)

# First cfa model

EFF_par_list$cfa_mod$init <- lav_mod(EFF_par_list, groups = "spin", minvar = 2)

# Split the data for a training and testing sample

set.seed(1)
EFF_tprop <- .5
EFF_par_list$train <- sample(1:nrow(EFF_dat_mod), size = round(nrow(EFF_dat_mod)*EFF_tprop, 0), replace = FALSE)

EFF_outputs$fits$init <- fit_lav(model = EFF_par_list$cfa_mod$init,
                                 ordered = names(EFF_par_list$varlabs),
                                 par_list = EFF_par_list,
                                 data = EFF_dat_mod,
                                 train = EFF_par_list$train)

BFadd <- paste0("G =~ ", paste0("a*", colnames(EFF_dat_mod), collapse = " + "))
BFadd2 <- paste0("G ~~ 0*", lavNames(EFF_outputs$fits$init$fits$f_train, "lv"), collapse = "\n")

EFF_par_list$cfa_mod$bifactor <- lav_mod(EFF_par_list, 
                                         groups = "spin", 
                                         minvar = 2, 
                                         add = paste0(BFadd, "\n", BFadd2))

EFF_outputs$fits$bifactor <- fit_lav(model = EFF_par_list$cfa_mod$bifactor,
                                 ordered = names(EFF_par_list$varlabs),
                                 par_list = EFF_par_list,
                                 data = EFF_dat_mod,
                                 train = EFF_par_list$train)


EFF_par_list$fnames$init <- c(lavNames(EFF_outputs$fits$init$fits$f_train, c("ov")),
                              lavNames(EFF_outputs$fits$init$fits$f_train, c("lv")))

EFF_par_list$fnames_s$init <- lavNames(EFF_outputs$fits$init$fits$f_train, c("lv")) %>%
  unlist(recursive = TRUE)

EFF_par_list$fnames$bifactor <- c(lavNames(EFF_outputs$fits$bifactor$fits$f_train, c("ov")),
                                  lavNames(EFF_outputs$fits$bifactor$fits$f_train, c("lv")))

EFF_par_list$fnames_s$bifactor <- lavNames(EFF_outputs$fits$bifactor$fits$f_train, c("lv")) %>%
  unlist(recursive = TRUE)

EFF_par_list$color$cfa_2 <- c(EFF_par_list$color$cfa_1, G = "steelblue")
EFF_par_list$fnames$cfa_2 <- c(EFF_par_list$color$cfa_1, G = "steelblue")

EFF_outputs$semplots$init <- sem_plots(lav_fit = EFF_outputs$fits$init$fits$f_test,
                           par_list = EFF_par_list,
                           group = "init",
                           color = "cfa_1")

EFF_outputs$semplots$bifactor <- sem_plots(lav_fit = EFF_outputs$fits$bifactor$fits$f_test,
                           par_list = EFF_par_list,
                           group = "bifactor",
                           color = "cfa_2",
                           bifactor = "G")

# Get factor scores -------------------------------------------------------

EFF_outputs$scores <- get_scores(data = EFF_dat_mod,
                         fit_obj = EFF_outputs$fits$init$fits$f_all,
                         par_list = EFF_par_list,
                         group = c("spin","init"),
                         mimp = 5,
                         color = "spin")

EFF_par_list$com_labs_spec <- c(
  `Komunita 1` = "1. Práce se vzdělávacími cíli",
  `Komunita 2` = "2. Relevance a podnětnost výuky",
  `Komunita 3` = "3. Digitální kompetence",
  `Komunita 4` = "4. Práce s chybou a zpětnou vazbou",
  `Komunita 5` = "5. Vytváření prostředí pro učení",
  `Komunita 6` = "6. Partnerské zapojování žáků",
  `Komunita 7` = "7. Komunikace a spolupráce s rodiči",
  `Komunita 8` = "8. spolupráce s kolegy")

EFF_outputs$scores$plt_thresh_rescaled_2 <- EFF_outputs$scores$plt_thresh_rescaled
EFF_outputs$scores$plt_thresh_rescaled_2$data$group <- EFF_par_list$com_labs_spec[as.character(EFF_outputs$scores$plt_thresh_rescaled_2$data$group)]

EFF_outputs$scores$plt_thresh_rescaled_2_1 <- freq_plot(dat_tib = EFF_dat_mod[,c("SELFEFF_SQ010", "SELFEFF_SQ018")],
                                                       par_list = EFF_par_list,
                                                       group = "spin")

EFF_outputs$scores$plt_thresh_rescaled2_2 <- EFF_outputs$scores$plt_thresh_rescaled_2_1
EFF_outputs$scores$plt_thresh_rescaled2_3 <- EFF_outputs$scores$plt_thresh_rescaled_2_1

EFF_outputs$scores$plt_thresh_rescaled2_2$data <- EFF_outputs$scores$plt_thresh_rescaled2_2$data %>%
  mutate(name3 = ifelse(name2 == "8", "SELFEFF_SQ010", "SELFEFF_SQ018"),
         name3 = EFF_par_list$nodeNames[name3] %>%
           enc2utf8() %>%
           str_wrap(40)) %>%
  arrange(-as.numeric(name2)) %>%
  mutate(name2 = ordered(name3, unique(name3)))

EFF_outputs$scores$plt_thresh_rescaled2_3$data <- EFF_outputs$scores$plt_thresh_rescaled2_3$data %>%
  mutate(name3 = ifelse(name2 == "8", "SELFEFF_SQ010", "SELFEFF_SQ018"),
         name3 = EFF_par_list$nodeNames[name3] %>%
           enc2utf8() %>%
           str_wrap(40)) %>%
  arrange(-as.numeric(name2)) %>%
  mutate(name2 = ordered(name3, unique(name3)),
         group = EFF_par_list$com_labs_spec[3])

EFF_outputs$ggplot$groups <- groups_match(par_list = EFF_par_list,
                                          groups = c("spin", "init"), 
                                          color = "communities")

EFF_outputs$ggplot$groups2 <- EFF_outputs$ggplot$groups +
  scale_x_discrete(labels = EFF_par_list$com_labs_spec %>%
                     enc2utf8() %>%
                     str_wrap(15))

write_rds(EFF_par_list, "outputs/code/0_2_1_cluster_EFF_ZU/rds/EFF_par_list.rds")
write_rds(EFF_outputs, "outputs/code/0_2_1_cluster_EFF_ZU/rds/EFF_outputs.rds")