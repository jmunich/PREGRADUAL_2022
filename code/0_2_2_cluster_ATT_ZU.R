source("code/0_0_1_packages.R", encoding = "UTF-8")
source("code/0_0_2_funs.R", encoding = "UTF-8")
source("code/0_1_1_clean_ZU.R", encoding = "UTF-8")

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

ATT_outputs <- list()

# Read data and prepare labels --------------------------------------------

# Reading data
## Indicate reverse-coded items; NB: ind 9 non
ATT_revs <- c(1,3,6,7,9,11,13,15,17,19,21,23,26,27,30,32,34,35,37)
ATT_revs_2 <- c(1,4)
ATT_revs_all <- c(ATT_revs,39, 42)

ATT_outputs$data$all <- data_use %>%
  filter(filt_RESPs)


ATT_outputs$data$raw <- data_use %>%
  filter(filt_RESPs) %>%
  select(starts_with("TEACHINGATT"), starts_with("PUPILMIND"))


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
  bind_cols(ATT_dat_2) %>%
  `colnames<-`(gsub("\\[", "_", colnames(.)) %>%
                 gsub("\\]", "", .))

ATT_outputs$data$mod <- ATT_dat_mod

ATT_dat_group <- data_use %>%
  filter(filt_RESPs) %>%
  select(TIMEEMPLOYEDSCHOOL)

# Creating labels

ATT_labs_1 <- varlabs_2[grepl("TEACHINGATT", varnames)] %>%
  enc2utf8() %>%
  `names<-`(varnames[grepl("TEACHINGATT", varnames)] %>%
              gsub("\\[", "_", .) %>%
                 gsub("\\]", "", .))

ATT_labs_1[ATT_revs] <- paste0(ATT_labs_1[ATT_revs], " (-)") %>% 
  enc2utf8()

ATT_labs_2 <- varlabs_2[grepl("PUPILMIND", varnames)] %>%
  enc2utf8() %>%
  `names<-`(varnames[grepl("PUPILMIND", varnames)] %>%
              gsub("\\[", "_", .) %>%
              gsub("\\]", "", .))

ATT_labs_2[ATT_revs_2] <- paste0(ATT_labs_2[ATT_revs_2], " (-)") %>% 
  enc2utf8()

ATT_labs <- c(ATT_labs_1, ATT_labs_2)

ATT_pairs_1 <- rep(1:(ncol(ATT_dat_1)/2), each = 2)

ATT_pairs_2 <- c(1,2,1,2) + max(ATT_pairs_1)

ATT_pairs <- c(ATT_pairs_1, ATT_pairs_2)

ATT_groups <- c(
  rep("1. PLÁNOVÁNÍ VÝUKY", 6),
  rep("2. PROCESY UČENÍ", 12),
  rep("3. PROSTŘEDÍ PRO UČENÍ", 10),
  rep("4. HODNOCENÍ", 10),
  rep("5. SCHOPNOSTI ŽÁKŮ", 4)
) %>%
  enc2utf8() %>%
  `names<-`(names(ATT_labs))

ATT_lab_tib <- tibble(name = names(ATT_dat_mod),
                      label = ATT_labs,
                      group = ATT_groups,
                      pairs = ATT_pairs) %>%
  mutate(reverse = grepl("\\(-\\)", label),
         label = gsub("\U202F", " ", label),
         code = paste0(pairs, ifelse(reverse, " -", "")))

ATT_outputs$tabs$lab_init <- ATT_lab_tib %>%
  group_by(group) %>%
  mutate(mark = 1:n()) %>%
  mutate(group = ifelse(mark == 1, group, "")) %>%
  ungroup() %>%
  select(c(code, label, group, reverse)) %>%
  `names<-`(c("Kód", "Položka", "Okruh", "Reverzní")) %>%
  select(Okruh, Kód, Položka, Reverzní)

ATT_varcodes <- ATT_lab_tib$code %>%
  `names<-`(ATT_lab_tib$name)

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

ATT_piecols <- c(
  list(use_cols[7:1])[rep(1, length(ATT_labs_1))],
  list(use_cols[6:2])[rep(1, length(ATT_labs_2))]) %>%
  `names<-`(names(ATT_props))

ATT_par_list <- list(
  sampleSize = nrow(ATT_dat_mod),
  groups = list(init = ATT_groups),
  varlabs = ATT_varcodes,
  nodeNames = ATT_labs,
  pies = ATT_props,
  pieCols = ATT_piecols,
  lab_vec = c("1 – Zcela nesouhlasím", as.character(2:6) ,"7 – Zcela souhlasím"),
  ref_vec = c("1", "2", "3", "4"),
  col_vec = use_cols[7:1])  

# Create descriptive summaries --------------------------------------------------------
ATT_outputs$freq$init <- freq_plot(dat_tib = ATT_dat_mod,
                           par_list = ATT_par_list,
                           group = "init")

# Fit network ------------------------
# Correlation matrix and first model
ATT_par_list$wmats$cormat <- cor_auto(ATT_dat_mod)

ATT_fit_0 <- qgraph(ATT_par_list$wmats$cormat, 
                    graph = "glasso",
                    sampleSize = ATT_par_list$sampleSize,
                    threshold = TRUE,
                    layout = "spring",
                    groups = ATT_par_list$groups$init,
                    palette = "pastel",
                    DoNotPlot = TRUE)

ATT_par_list$wmats$gmat <- getWmat(ATT_fit_0)

ATT_par_list$color$init <- ATT_fit_0$graphAttributes$Nodes$color %>%
  `names<-`(colnames(ATT_dat_mod))

ATT_par_list$layout <- ATT_fit_0$layout %>%
  `rownames<-`(colnames(ATT_dat_mod))


ATT_outputs$qgraphs$pos_neg$pos <- ATT_par_list$wmats$cormat[colnames(ATT_dat_mod)[ATT_revs_all],
                                                            colnames(ATT_dat_mod)[ATT_revs_all]] %>%
  qgraph(graph = "glasso",
         sampleSize = ATT_par_list$sampleSize,
         threshold = TRUE,
         layout = "spring",
         labels = ATT_par_list$varlabs[colnames(.)],
         groups = ATT_par_list$groups[colnames(.)],
         pie = ATT_par_list$pies[colnames(.)],
         color = ATT_par_list$color$init[colnames(.)],
         DoNotPlot = TRUE)

ATT_outputs$qgraphs$pos_neg$neg <- ATT_par_list$wmats$cormat[colnames(ATT_dat_mod)[-ATT_revs_all],
                                         colnames(ATT_dat_mod)[-ATT_revs_all]] %>%
  qgraph(graph = "glasso",
         sampleSize = ATT_par_list$sampleSize,
         threshold = TRUE,
         layout = "spring",
         labels = ATT_par_list$varlabs[colnames(.)],
         groups = ATT_par_list$groups[colnames(.)],
         pie = ATT_par_list$pies[colnames(.)],
         color = ATT_par_list$color$init[colnames(.)],
         DoNotPlot = TRUE)

ATT_pos_names <- names(sort(ATT_par_list$varlabs[grepl("-", ATT_par_list$varlabs)]))
ATT_neg_names <- names(sort(ATT_par_list$varlabs[!grepl("-", ATT_par_list$varlabs)]))

ATT_selmat <- matrix(c(ATT_pos_names, ATT_neg_names),
                     ncol = 2, byrow = FALSE)
ATT_temp_mat <- ATT_par_list$wmats$cormat
ATT_temp_mat[,] <- 0
ATT_temp_mat[ATT_selmat] <- ATT_par_list$wmats$cormat[ATT_selmat]

ATT_outputs$qgraphs$pos_neg$pairs <- ATT_temp_mat %>%
  qgraph(
    directed = FALSE,
    layout = matrix(c(rep(c(1,4), length(ATT_revs_all)),
                      rep(1:length(ATT_revs_all), each = 2)),
                     byrow = FALSE,
                     ncol = 2)[c(1:38,c(39,41,40,42)),],
    groups = ATT_par_list$groups$init[colnames(.)],
    color = ATT_par_list$color$init[colnames(.)],
    labels = ATT_par_list$varlabs[colnames(.)],
    pie = ATT_par_list$pies[colnames(.)],
    palette = "pastel",
    DoNotPlot = TRUE)

ATT_outputs$qgraphs$pos_neg$pos$layout <- averageLayout(ATT_outputs$qgraphs$pos_neg[c("pos", "neg")])
ATT_outputs$qgraphs$pos_neg$neg$layout <- averageLayout(ATT_outputs$qgraphs$pos_neg[c("pos", "neg")])


ATT_par_list$wmats$init <- getWmat(ATT_fit_0)

ATT_par_list$color$init <- ATT_fit_0$graphAttributes$Nodes$color %>%
  `names<-`(colnames(ATT_dat_mod))

ATT_par_list$layout <- ATT_fit_0$layout %>%
  `rownames<-`(colnames(ATT_dat_mod))

ATT_outputs$qgraphs$init <- qgraph_w_pars(cmat = ATT_par_list$wmats$gmat,
                 par_list = ATT_par_list,
                 group = "init",
                 color = "init",
                 legend = TRUE)

ATT_outputs$qgraphs_nl$init <- qgraph_w_pars(cmat = ATT_par_list$wmats$gmat,
                            par_list = ATT_par_list,
                            group = "init",
                            color = "init",
                            legend = TRUE)

# First model fit

ATT_outputs$tabs$fit_init <- ggmFit(ATT_outputs$qgraphs$init, 
                    covMat = ATT_par_list$wmats$cormat,
                    sampleSize = nrow(ATT_dat_mod)) %>%
  ggmFit_table()


# Bootstrap network stability ---------------------------------------------
# Perform bootstrapping

ATT_nboots <- 1000

ATT_boot0 <- estimateNetwork(ATT_outputs$data$mod,
                      fun = function(x){

                        cmt <- cor_auto(x,
                                        forcePD = TRUE,
                                        detectOrdinal = FALSE)
                        
                        qgraph::EBICglasso(cmt,
                                           nrow(x),
                                           threshold = TRUE)
                      })

ATT_boots <- bootnet(ATT_boot0,
                     nBoots = ATT_nboots,
                     statistics = "edge")

ATT_outputs$ggplot$boot <- boot_plot(ATT_boots,
                           ATT_par_list)

# Plot conservative network estimates

ATT_par_list$wmats$gmat_cons <- summary(ATT_boots) %>%
  ungroup() %>%
  mutate(weight = ifelse(q2.5 > 0 | q97.5 < 0, mean, 0)) %>%
  filter(node2 != "") %>%
  select(node1, node2, weight) %>%
  qgraph(directed = FALSE, 
         DoNotPlot = TRUE) %>%
  getWmat()

ATT_outputs$qgraphs$cons_fit <- qgraph_w_pars(cmat = ATT_par_list$wmats$gmat_cons,
                                              par_list = ATT_par_list,
                                              color = "init",
                                              group = "init",
                                              legend = TRUE)

ATT_outputs$qgraphs_nl$cons_fit <- qgraph_w_pars(cmat = ATT_par_list$wmats$gmat_cons,
                         par_list = ATT_par_list,
                         color = "init",
                         group = "init",
                         legend = FALSE)


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

ATT_outputs$nct <- nct_plot(ATT_nct)

ATT_nct_wmats <- nct_nets(ATT_nct)

ATT_outputs$qgraphs$nct <- qgraph_w_pars(cmat = ATT_nct_wmats$diffs,
                              par_list = ATT_par_list,
                              color = 1,
                              group = 1,
                              legend = FALSE)

ATT_outputs$qgraphs$nct_sig <- qgraph_w_pars(cmat = ATT_nct_wmats$diffs_sig,
                              par_list = ATT_par_list,
                              color = 1,
                              group = 1,
                              legend = FALSE)


# Spinglass community detection -------------------------------------------
ATT_par_list$cut <- .6
ATT_spins <- spin_loop(ATT_outputs$qgraphs$init,
                       gamma = .5,
                       cut = ATT_par_list$cut)

ATT_groups_cols_tib <- hclust_palette(ATT_spins$hclust, 
                                      start_hue = 100, 
                                      max_hue = 350, 
                                      distinction = 15,
                                      min_luminance = 20,
                                      max_luminance = 90,
                                      mod_hgt = FALSE)

ATT_par_list$color$spin <- ATT_groups_cols_tib$color %>%
  `names<-`(ATT_groups_cols_tib$variable)

ATT_par_list$groups$spin <- ATT_spins$hclust %>%
  cutree(., h = ATT_par_list$cut) %>%
  paste0("Komunita ", .) %>%
  `names<-`(names(ATT_spins$groups))

ATT_par_list$wmats$spin <- ATT_spins$mat

ATT_outputs$qgraphs_nl$spin_coms <- qgraph_w_pars(ATT_par_list$wmats$spin,
                                par_list = ATT_par_list,
                                color = "spin",
                                group = "spin",
                                layout = "spring",
                                repulsion = .8,
                                legend = FALSE)

ATT_outputs$ggplot$dendro <- plt_dendro(ATT_spins$data_dendro,
                             par_list = ATT_par_list,
                             color = "spin")

ATT_outputs$qgraphs$spin <- qgraph_w_pars(ATT_par_list$wmats$gmat,
                              par_list = ATT_par_list,
                              color = "spin",
                              group = "spin",
                              legend = TRUE)

ATT_outputs$qgraphs_nl$spin <- qgraph_w_pars(ATT_par_list$wmats$gmat,
                              par_list = ATT_par_list,
                              color = "spin",
                              group = "spin",
                              legend = FALSE)

ATT_outputs$tabs$lab_spin <- ATT_lab_tib %>%
  mutate(Komunita = ATT_par_list$groups$spin[name],
         Komunita_2 = parse_number(Komunita)) %>%
  arrange(Komunita_2) %>%
  mutate(Komunita = ordered(Komunita, unique(Komunita))) %>%
  group_by(Komunita) %>%
  mutate(ag = 1:n()) %>%
  ungroup() %>%
  mutate(Komunita = ifelse(ag == 1, as.character(Komunita), "")) %>%
  rename(Okruh = group,
         Kód = code,
         Reverzní = reverse,
         Položka = label) %>%
  select(Komunita, Okruh, Kód, Položka, Reverzní)

ATT_outputs$freq$spin <- freq_plot(dat_tib = ATT_dat_mod,
                                par_list = ATT_par_list, 
                                group = "spin",
                                f.order = unique(ATT_par_list$groups$spin))  

# Create a CFA model based on the detected communities --------------------

ATT_firsts <- ATT_par_list$groups$spin %>%
  unique() %>%
  lapply(function(x){ATT_par_list$groups$spin[ATT_par_list$groups$spin == x][1]}) %>%
  unlist()

# Color per community, based on first ov from every community

ATT_par_list$color$communities <- ATT_par_list$color$spin[names(ATT_firsts)] %>%
  `names<-`(ATT_firsts)

# First ov belonging to each lv (for color selection)

ATT_par_list$factors <- names(table(ATT_par_list$groups$spin)[table(ATT_par_list$groups$spin)>2]) %>%
  .[order(parse_number(.))]

names(ATT_par_list$factors) <- ATT_par_list$factors %>%
  lapply(function(x){
    names(ATT_par_list$groups$spin[ATT_par_list$groups$spin==x])[1]}) %>%
  unlist()

# Latent variable colors

ATT_par_list$color$cfa_lv_1 <- ATT_par_list$color$spin[names(ATT_par_list$factors)] %>%
  `names<-`(gsub("Komunita ", "K", ATT_par_list$factors))

# Node colors for sem plot

ATT_par_list$color$cfa_1 <- c(ATT_par_list$color$spin, ATT_par_list$color$cfa_lv_1)

# Not labels for sem plot

ATT_par_list$fvarlabs <- names(ATT_par_list$color$cfa_lv_1) %>% 
  `names<-`(names(ATT_par_list$color$cfa_lv_1)) %>%
  c(ATT_par_list$varlabs, G = "G", .)

# First cfa model

ATT_par_list$cfa_mod$init <- lav_mod(ATT_par_list, groups = "spin", minvar = 2)

# Split the data for a training and testing sample

set.seed(1)
ATT_tprop <- .5
ATT_par_list$train <- sample(1:nrow(ATT_dat_mod), size = round(nrow(ATT_dat_mod)*ATT_tprop, 0), replace = FALSE)

ATT_outputs$fits$init <- fit_lav(model = ATT_par_list$cfa_mod$init,
                       ordered = names(ATT_par_list$varlabs) %>%
                         .[grepl("PUPIL", .)],
                       par_list = ATT_par_list,
                       data = ATT_dat_mod,
                       train = ATT_par_list$train)

ATT_exvar <- lapply(unique(ATT_par_list$groups$spin), function(x){
  
  n_vec <- names(ATT_par_list$groups$spin[ATT_par_list$groups$spin == x])
  
  ATT_par_list$wmats$cormat[n_vec, n_vec, drop = FALSE] %>%
    `diag<-`(0) %>%
    abs() %>%
    apply(MARGIN = 1, 
          median)
  
}) %>%
  unlist() %>%
  .[.<.15] %>%
  names()

ATT_par_list$cfa_mod$adapt <- lav_mod(ATT_par_list,
                                      groups = "spin",
                                     exclude = ATT_exvar)

ATT_outputs$fits$adapt <- fit_lav(model = ATT_par_list$cfa_mod$adapt,
                       ordered = names(ATT_par_list$varlabs) %>%
                         .[grepl("PUPIL", .)],
                       par_list = ATT_par_list,
                       data = ATT_dat_mod,
                       train = ATT_par_list$train)

ATT_par_list$fnames$init <- lavNames(ATT_outputs$fits$init$fits$f_train, c("ov", "lv")) %>%
  unlist(recursive = TRUE)

ATT_par_list$fnames_s$init <- lavNames(ATT_outputs$fits$init$fits$f_train, c("lv")) %>%
  unlist(recursive = TRUE)

ATT_par_list$fnames$adapt <- lavNames(ATT_outputs$fits$adapt$fits$f_train, c("ov", "lv")) %>%
  unlist(recursive = TRUE)

ATT_par_list$fnames_s$adapt <- lavNames(ATT_outputs$fits$adapt$fits$f_train, c("lv")) %>%
  unlist(recursive = TRUE)

ATT_outputs$semplots$init <- sem_plots(lav_fit = ATT_outputs$fits$init$fits$f_train,
                           par_list = ATT_par_list,
                           group = "init",
                           color = "cfa_1")

ATT_outputs$semplots$adapt <- sem_plots(lav_fit = ATT_outputs$fits$adapt$fits$f_train,
                               par_list = ATT_par_list,
                               group = "adapt",
                               color = "cfa_1")


# Get factor scores -------------------------------------------------------

ATT_outputs$scores <- get_scores(ATT_dat_mod,
                         ATT_outputs$fits$init$fits$f_all,
                         ATT_par_list,
                         group = c("spin","init"),
                         mimp = 1,
                         color = "spin",
                         lab_vec_ord = ATT_lev)

ATT_par_list$com_labs_spec <- c(
  `Komunita 1` = "1. Diferenciace výuky",
  `Komunita 2` = "2. Práce s nejistotou a flexibilita",
  `Komunita 6` = "3. Rovné zapojení všech žáků",
  `Komunita 8` = "4. Aktivní zapojení žáků",
  `Komunita 9` = "5. Prostor pro zkušenost žáka",
  `Komunita 13` = "6. Schopnosti žáků")

ATT_outputs$scores$plt_thresh_rescaled_2 <- ATT_outputs$scores$plt_thresh_rescaled
ATT_outputs$scores$plt_interval_rescaled_2 <- ATT_outputs$scores$plt_interval_rescaled

ATT_outputs$scores$plt_thresh_rescaled_2$data$group <- ATT_par_list$com_labs_spec[as.character(ATT_outputs$scores$plt_thresh_rescaled_2$data$group)]
ATT_outputs$scores$plt_interval_rescaled_2$data$group <- ATT_par_list$com_labs_spec[as.character(ATT_outputs$scores$plt_interval_rescaled_2$data$group)]

ATT_singles <- ATT_par_list$groups$spin %>%
  table() %>%
  .[.==1] %>%
  names()

ATT_par_list$groups$spin2 <- ATT_par_list$groups$spin %>% ifelse(. %in% ATT_singles,
                                                                     "Ostatní",
                                                                     .) %>%
  `names<-`(names(ATT_par_list$groups$spin)) %>%
  .[order(parse_number(.))]

ATT_outputs$ggplot$groups <- groups_match(par_list = ATT_par_list,
                                          groups = c("spin2", "init"), 
                                          color = "communities")

write_rds(ATT_par_list, "outputs/code/0_2_2_cluster_ATT_ZU/rds/ATT_par_list.rds")
write_rds(ATT_outputs, "outputs/code/0_2_2_cluster_ATT_ZU/rds/ATT_outputs.rds")