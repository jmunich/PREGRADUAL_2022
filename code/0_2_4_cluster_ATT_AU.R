source("code/0_0_1_packages.R", encoding = "UTF-8")
source("code/0_0_2_funs.R", encoding = "UTF-8")
source("code/0_1_2_clean_AU.R", encoding = "UTF-8")

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

ATTAU_outputs <- list()

# Read data and prepare labels --------------------------------------------

ATT0_par_list <- read_rds("outputs/code/0_2_2_cluster_ATT_ZU/rds/ATT_par_list.rds")
ATT0_outputs <- read_rds("outputs/code/0_2_2_cluster_ATT_ZU/rds/ATT_outputs.rds")

ATTAU_outputs <- list()
# Reading data
## Indicate reverse-coded items; NB: ind 9 non
ATTAU_revs <- c(1,3,5,8,9,11,13,15,17,19,21,23,25,27,30,31,34,36,38,39,41)
ATTAU_revs_2 <- c(1,4)
ATTAU_revs_all <- c(ATTAU_revs, length(AU_varlabs_2[grepl("TEACHINGATT\\[", AU_varnames)]) + ATTAU_revs_2)

ATTAU_outputs$data$all <- AU_data_use %>%
  filter(AU_filt_RESPs)

ATTAU_outputs$data$raw <- AU_data_use %>%
  filter(AU_filt_RESPs) %>%
  select(matches("TEACHINGATT\\[|PUPILMIND"))


ATTAU_dat_1 <- AU_data_use %>%
  filter(AU_filt_RESPs) %>%
  select(matches("TEACHINGATT\\[")) %>%
  mutate_all(parse_number) %>%
  mutate_at(ATTAU_revs,
            function(x){8 - x})

ATTAU_lev <- c("Rozhodně nesouhlasím", 
             "Nesouhlasím",          
             "Spíše nesouhlasím",    
             "Spíše souhlasím",      
             "Souhlasím",           
             "Rozhodně souhlasím")

ATTAU_lev_us <- ATTAU_lev %>% 
  `names<-`(ATTAU_lev[6:1])

ATTAU_dat_2 <- AU_data_use %>%
  filter(AU_filt_RESPs) %>%
  select(starts_with("PUPILMIND")) %>%
  mutate_at(ATTAU_revs_2, function(x){
    ATTAU_lev_us[x]
  }) %>%
  mutate_all(ordered, levels = ATTAU_lev)

ATTAU_dat_mod <- ATTAU_dat_1 %>%
  bind_cols(ATTAU_dat_2) %>%
  mutate_all(ordered) %>%
  `colnames<-`(gsub("\\[", "_", colnames(.)) %>%
                 gsub("\\]", "", .))

ATTAU_outputs$data$mod <- ATTAU_dat_mod
  

# Creating labels

ATTAU_labs_1 <- AU_varlabs_2[grepl("TEACHINGATT\\[", AU_varnames)] %>%
  enc2utf8() %>%
  `names<-`(AU_varnames[grepl("TEACHINGATT\\[", AU_varnames)] %>%
              gsub("\\[", "_", .) %>%
              gsub("\\]", "", .))

ATTAU_labs_1[ATTAU_revs] <- paste0(ATTAU_labs_1[ATTAU_revs], " (-)") %>% 
  enc2utf8()

ATTAU_labs_2 <- AU_varlabs_2[grepl("PUPILMIND", AU_varnames)] %>%
  enc2utf8() %>%
  `names<-`(AU_varnames[grepl("PUPILMIND", AU_varnames)] %>%
              gsub("\\[", "_", .) %>%
              gsub("\\]", "", .))

ATTAU_labs_2[ATTAU_revs_2] <- paste0(ATTAU_labs_2[ATTAU_revs_2], " (-)") %>% 
  enc2utf8()

ATTAU_labs <- c(ATTAU_labs_1, 
                ATTAU_labs_2)

ATTAU_pairs_1 <- rep(1:(ncol(ATTAU_dat_1)/2), each = 2)

ATTAU_pairs_2 <- c(1,2,1,2) + max(ATTAU_pairs_1)

ATTAU_pairs <- c(ATTAU_pairs_1, ATTAU_pairs_2) %>%
  `names<-`(colnames(ATTAU_dat_mod))

ATTAU_varcodes <- ATTAU_pairs %>%
  `names<-`(colnames(ATTAU_dat_mod))

ATTAU_varcodes[ATTAU_revs_all] <- ATTAU_varcodes[ATTAU_revs_all] %>%
  paste0(., "-")

ATTAU_groups <- c(rep("1. PLÁNOVÁNÍ VÝUKY", 8), 
                  rep("2. PROCESY UČENÍ", 12),
                  rep("3. PROSTŘEDÍ PRO UČENÍ", 12),
                  rep("4. HODNOCENÍ", 10),
                  rep("5. SCHOPNOSTI ŽÁKŮ", 4)) %>%
  enc2utf8() %>%
  `names<-`(colnames(ATTAU_dat_mod))

ATTAU_lab_tib <- tibble(name = names(ATTAU_dat_mod),
                      label = ATTAU_labs,
                      group = ATTAU_groups,
                      pairs = ATTAU_pairs) %>%
  mutate(reverse = grepl("\\(-\\)", label),
         label = gsub("\U202F", " ", label),
         code = paste0(pairs, ifelse(reverse, " -", "")))

ATTAU_outputs$tabs$lab_init <- ATTAU_lab_tib %>%
  group_by(group) %>%
  mutate(mark = 1:n()) %>%
  mutate(group = ifelse(mark == 1, group, "")) %>%
  ungroup() %>%
  select(c(code, label, group, reverse)) %>%
  `names<-`(c("Kód", "Položka", "Okruh", "Reverzní")) %>%
  select(Okruh, Kód, Položka, Reverzní)


# Proportions of responses per item

ATTAU_props <- ATTAU_outputs$data$mod %>%
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
  .[colnames(ATTAU_dat_mod)]

ATTAU_piecols <- c(
  list(use_cols[7:1])[rep(1, length(ATTAU_labs_1))],
  list(use_cols[6:2])[rep(1, length(ATTAU_labs_2))]) %>%
  `names<-`(names(ATTAU_props))

ATTAU_par_list <- list(
  sampleSize = nrow(ATTAU_dat_mod),
  groups = list(init = ATTAU_groups),
  varlabs = ATTAU_varcodes,
  nodeNames = ATTAU_labs,
  pies = ATTAU_props,
  pieCols = ATTAU_piecols,
  lab_vec = as.character(c(1:7)),
  ref_vec = c("1", "2", "3", "4"),
  col_vec = use_cols[7:1])  

# Create descriptive summaries --------------------------------------------------------
ATTAU_outputs$freq$init <- freq_plot(dat_tib = ATTAU_outputs$data$mod,
                                   par_list = ATTAU_par_list,
                                   group = "init",
                                   f.order = ATTAU_par_list$groups$init %>% unique() %>% sort())  

# Fit network ------------------------
# Correlation matrix and first model
ATTAU_par_list$wmats$cormat <- cor_auto(ATTAU_outputs$data$mod)

ATTAU_fit_0 <- qgraph(ATTAU_par_list$wmats$cormat, 
                    graph = "glasso",
                    sampleSize = ATTAU_par_list$sampleSize,
                    threshold = TRUE,
                    layout = "spring",
                    groups = ATTAU_par_list$groups$init,
                    palette = "pastel",
                    DoNotPlot = TRUE)

ATTAU_par_list$wmats$gmat <- getWmat(ATTAU_fit_0)

ATTAU_par_list$color$init <- ATTAU_fit_0$graphAttributes$Nodes$color %>%
  `names<-`(colnames(ATTAU_dat_mod))

ATTAU_par_list$layout <- ATTAU_fit_0$layout %>%
  `rownames<-`(colnames(ATTAU_dat_mod))

ATTAU_rev_names <- ATTAU_pairs[ATTAU_revs_all] %>% names()

ATTAU_nrev_names <- ATTAU_pairs[-ATTAU_revs_all] %>% names()

ATTAU_outputs$qgraphs$pos_neg$pos <- ATTAU_par_list$wmats$cormat[ATTAU_rev_names,
                                                                 ATTAU_rev_names] %>%
  qgraph(graph = "glasso",
         sampleSize = ATTAU_par_list$sampleSize,
         threshold = TRUE,
         layout = "spring",
         labels = ATTAU_par_list$varlabs[colnames(.)],
         groups = ATTAU_par_list$groups[colnames(.)],
         pie = ATTAU_par_list$pies[colnames(.)],
         color = ATTAU_par_list$color$init[colnames(.)],
         DoNotPlot = TRUE)

ATTAU_outputs$qgraphs$pos_neg$neg <- ATTAU_par_list$wmats$cormat[ATTAU_nrev_names,
                                                                 ATTAU_nrev_names] %>%
  qgraph(graph = "glasso",
         sampleSize = ATTAU_par_list$sampleSize,
         threshold = TRUE,
         layout = "spring",
         labels = ATTAU_par_list$varlabs[colnames(.)],
         groups = ATTAU_par_list$groups[colnames(.)],
         pie = ATTAU_par_list$pies[colnames(.)],
         color = ATTAU_par_list$color$init[colnames(.)],
         DoNotPlot = TRUE)

ATTAU_pos_names <- ATTAU_nrev_names
ATTAU_neg_names <- ATTAU_rev_names

ATTAU_selmat <- matrix(c(ATTAU_pos_names, ATTAU_neg_names),
                     ncol = 2, byrow = FALSE)

ATTAU_temp_mat <- ATTAU_par_list$wmats$cormat
ATTAU_temp_mat[,] <- 0
ATTAU_temp_mat[ATTAU_selmat] <- ATTAU_par_list$wmats$cormat[ATTAU_selmat]
ATTAU_temp_mat <- ATTAU_temp_mat[c(ATTAU_pos_names, ATTAU_neg_names),c(ATTAU_pos_names, ATTAU_neg_names)]

ATTAU_layout <- matrix(c(rep(1, length(ATTAU_pos_names)), rep(4, length(ATTAU_pos_names))),
                       ncol = 2, byrow = FALSE) %>%
  as.numeric() %>%
  c(., rep(1:length(ATTAU_pos_names), 2)) %>%
  matrix(ncol = 2, byrow = FALSE) %>%
  `rownames<-`(as.character(ATTAU_selmat))

ATTAU_outputs$qgraphs$pos_neg$pairs <- ATTAU_temp_mat %>%
  qgraph(
    directed = FALSE,
    layout = ATTAU_layout,
    groups = ATTAU_par_list$groups$init[colnames(.)],
    color = ATTAU_par_list$color$init[colnames(.)],
    labels = ATTAU_par_list$varlabs[colnames(.)],
    pie = ATTAU_par_list$pies[colnames(.)],
    palette = "pastel",
    DoNotPlot = TRUE)

ATTAU_outputs$qgraphs$pos_neg$pos$layout <- averageLayout(ATTAU_outputs$qgraphs$pos_neg[c("pos", "neg")])
ATTAU_outputs$qgraphs$pos_neg$neg$layout <- averageLayout(ATTAU_outputs$qgraphs$pos_neg[c("pos", "neg")])

ATTAU_par_list$wmats$init <- getWmat(ATTAU_fit_0)

ATTAU_par_list$color$init <- ATTAU_fit_0$graphAttributes$Nodes$color %>%
  `names<-`(colnames(ATTAU_dat_mod))

ATTAU_par_list$layout <- ATTAU_fit_0$layout %>%
  `rownames<-`(colnames(ATTAU_dat_mod))

ATTAU_outputs$qgraphs$init <- qgraph_w_pars(cmat = ATTAU_par_list$wmats$gmat,
                                          par_list = ATTAU_par_list,
                                          group = "init",
                                          color = "init",
                                          legend = TRUE)

ATTAU_outputs$qgraphs_nl$init <- qgraph_w_pars(cmat = ATTAU_par_list$wmats$gmat,
                                             par_list = ATTAU_par_list,
                                             group = "init",
                                             color = "init",
                                             legend = TRUE)
# First model fit

ATTAU_outputs$tabs$fit_init <- ggmFit(ATTAU_outputs$qgraphs$init, 
                                    covMat = ATTAU_par_list$wmats$cormat,
                                    sampleSize = nrow(ATTAU_dat_mod)) %>%
  ggmFit_table()


# Bootstrap network stability ---------------------------------------------
# Perform bootstrapping

ATTAU_nboots <- 1000

ATTAU_boot0 <- estimateNetwork(ATTAU_outputs$data$mod,
                             fun = function(x){
                               
                               cmt <- cor(mutate_all(x, as.numeric),use = "pairwise.complete.obs",method = "spearman")
                               
                               qgraph::EBICglasso(cmt,
                                                  nrow(x),
                                                  threshold = TRUE)
                             })

ATTAU_boots <- bootnet(ATTAU_boot0, 
                     nBoots = ATTAU_nboots,
                     statistics = "edge")


ATTAU_outputs$ggplot$boot <- boot_plot(ATTAU_boots,
                                     ATTAU_par_list)

# Plot conservative network estimates

ATTAU_par_list$wmats$gmat_cons <- summary(ATTAU_boots) %>%
  ungroup() %>%
  mutate(weight = ifelse(q2.5 > 0 | q97.5 < 0, mean, 0)) %>%
  select(node1, node2, weight) %>%
  qgraph(directed = FALSE, 
         DoNotPlot = TRUE) %>%
  getWmat()

ATTAU_outputs$qgraphs$cons_fit <- qgraph_w_pars(cmat = ATTAU_par_list$wmats$gmat_cons,
                                              par_list = ATTAU_par_list,
                                              color = "init",
                                              group = "init",
                                              legend = TRUE)

ATTAU_outputs$qgraphs_nl$cons_fit <- qgraph_w_pars(cmat = ATTAU_par_list$wmats$gmat_cons,
                                                 par_list = ATTAU_par_list,
                                                 color = "init",
                                                 group = "init",
                                                 legend = FALSE)



# Spinglass community detection -------------------------------------------
ATTAU_par_list$cut <- .6
ATTAU_spins <- spin_loop(ATTAU_outputs$qgraphs$init,
                       gamma = .75,
                       cut = ATTAU_par_list$cut)

ATTAU_groups_cols_tib <- hclust_palette(ATTAU_spins$hclust, 
                                      start_hue = 100, 
                                      max_hue = 350, 
                                      distinction = 15,
                                      min_luminance = 20,
                                      max_luminance = 90,
                                      mod_hgt = FALSE)

ATTAU_par_list$color$spin <- ATTAU_groups_cols_tib$color %>%
  `names<-`(ATTAU_groups_cols_tib$variable)

ATTAU_par_list$groups$spin <- ATTAU_spins$hclust %>%
  cutree(., h = ATTAU_par_list$cut) %>%
  paste0("Komunita ", .) %>%
  `names<-`(names(ATTAU_spins$groups))

ATTAU_par_list$wmats$spin <- ATTAU_spins$mat

ATTAU_outputs$qgraphs_nl$spin_coms <- qgraph_w_pars(ATTAU_par_list$wmats$spin,
                                                  par_list = ATTAU_par_list,
                                                  color = "spin",
                                                  group = "spin",
                                                  layout = "spring",
                                                  legend = FALSE)

ATTAU_outputs$ggplot$dendro <- plt_dendro(ATTAU_spins$data_dendro,
                                        par_list = ATTAU_par_list,
                                        color = "spin")

ATTAU_outputs$qgraphs$spin <- qgraph_w_pars(ATTAU_par_list$wmats$gmat,
                                          par_list = ATTAU_par_list,
                                          color = "spin",
                                          group = "spin",
                                          legend = TRUE)

ATTAU_outputs$qgraphs_nl$spin <- qgraph_w_pars(ATTAU_par_list$wmats$gmat,
                                             par_list = ATTAU_par_list,
                                             color = "spin",
                                             group = "spin",
                                             legend = FALSE)

ATTAU_outputs$tabs$lab_spin <- ATTAU_lab_tib %>%
  mutate(Komunita = ATTAU_par_list$groups$spin[name],
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

ATTAU_outputs$freq$spin <- freq_plot(dat_tib = ATTAU_dat_mod,
                                   par_list = ATTAU_par_list, 
                                   group = "spin",
                                   f.order = unique(ATTAU_par_list$groups$spin))  

# Create a CFA model based on the detected communities --------------------

ATTAU_firsts <- ATTAU_par_list$groups$spin %>%
  unique() %>%
  lapply(function(x){ATTAU_par_list$groups$spin[ATTAU_par_list$groups$spin == x][1]}) %>%
  unlist()

# Color per community, based on first ov from every community

ATTAU_par_list$color$communities <- ATTAU_par_list$color$spin[names(ATTAU_firsts)] %>%
  `names<-`(ATTAU_firsts)

# First ov belonging to each lv (for color selection)

ATTAU_par_list$factors <- names(table(ATTAU_par_list$groups$spin)[table(ATTAU_par_list$groups$spin)>2]) %>%
  .[order(parse_number(.))]

names(ATTAU_par_list$factors) <- ATTAU_par_list$factors %>%
  lapply(function(x){
    names(ATTAU_par_list$groups$spin[ATTAU_par_list$groups$spin==x])[1]}) %>%
  unlist()

# Latent variable colors

ATTAU_par_list$color$cfa_lv_1 <- ATTAU_par_list$color$spin[names(ATTAU_par_list$factors)] %>%
  `names<-`(gsub("Komunita ", "K", ATTAU_par_list$factors))

# Node colors for sem plot

ATTAU_par_list$color$cfa_1 <- c(ATTAU_par_list$color$spin, ATTAU_par_list$color$cfa_lv_1)

# Not labels for sem plot

ATTAU_par_list$fvarlabs <- names(ATTAU_par_list$color$cfa_lv_1) %>% 
  `names<-`(names(ATTAU_par_list$color$cfa_lv_1)) %>%
  c(ATTAU_par_list$varlabs, G = "G", .)

# First cfa model

ATTAU_par_list$cfa_mod$init <- lav_mod(ATTAU_par_list, groups = "spin", minvar = 2)

# Split the data for a training and testing sample

ATTAU_outputs$fits$init <- fit_lav(model = ATTAU_par_list$cfa_mod$init,
                                 ordered = names(ATTAU_par_list$varlabs) %>%
                                   .[grepl("PUPIL", .)],
                                 par_list = ATTAU_par_list,
                                 data = ATTAU_dat_mod)

ATTAU_exvar <- lapply(unique(ATTAU_par_list$groups$spin), function(x){
  
  n_vec <- names(ATTAU_par_list$groups$spin[ATTAU_par_list$groups$spin == x])
  
  ATTAU_par_list$wmats$cormat[n_vec, n_vec, drop = FALSE] %>%
    `diag<-`(0) %>%
    abs() %>%
    apply(MARGIN = 1, 
          median)
  
}) %>%
  unlist() %>%
  .[.<.15] %>%
  names()

ATTAU_par_list$cfa_mod$adapt <- lav_mod(ATTAU_par_list,
                                      groups = "spin",
                                      exclude = ATTAU_exvar)

ATTAU_outputs$fits$adapt <- fit_lav(model = ATTAU_par_list$cfa_mod$adapt,
                                  ordered = names(ATTAU_par_list$varlabs) %>%
                                    .[grepl("PUPIL", .)],
                                  par_list = ATTAU_par_list,
                                  data = ATTAU_dat_mod)

ATTAU_par_list$fnames$init <- c(lavNames(ATTAU_outputs$fits$init$fits$f_train, c("ov")),
                              lavNames(ATTAU_outputs$fits$init$fits$f_train, c("lv")))

ATTAU_par_list$fnames_s$init <- lavNames(ATTAU_outputs$fits$init$fits$f_train, c("lv")) %>%
  unlist(recursive = TRUE)

ATTAU_par_list$fnames$adapt <- c(lavNames(ATTAU_outputs$fits$adapt$fits$f_train, c("ov")),
                                  lavNames(ATTAU_outputs$fits$adapt$fits$f_train, c("lv")))

ATTAU_par_list$fnames_s$adapt <- lavNames(ATTAU_outputs$fits$adapt$fits$f_train, c("lv")) %>%
  unlist(recursive = TRUE)

ATTAU_outputs$semplots$init <- sem_plots(lav_fit = ATTAU_outputs$fits$init$fits$f_train,
                                       par_list = ATTAU_par_list,
                                       group = "init",
                                       color = "cfa_1")

ATTAU_outputs$semplots$adapt <- sem_plots(lav_fit = ATTAU_outputs$fits$adapt$fits$f_train,
                                        par_list = ATTAU_par_list,
                                        group = "adapt",
                                        color = "cfa_1")


# Get factor scores -------------------------------------------------------

ATTAU_outputs$scores <- get_scores(ATTAU_outputs$data$mod,
                                 ATTAU_outputs$fits$init$fits$f_all,
                                 ATTAU_par_list,
                                 group = c("spin","init"),
                                 mimp = 5,
                                 color = "spin",
                                 MaxNWts = 3000,
                                 lab_vec_ord = ATTAU_lev)

ATTAU_singles <- ATTAU_par_list$groups$spin %>%
  table() %>%
  .[.==1] %>%
  names()

ATTAU_par_list$groups$spin2 <- ATTAU_par_list$groups$spin %>% ifelse(. %in% ATTAU_singles,
                                                                 "Ostatní",
                                                                 .) %>%
  `names<-`(names(ATTAU_par_list$groups$spin)) %>%
  .[order(parse_number(.))]


ATTAU_outputs$ggplot$groups <- groups_match(par_list = ATTAU_par_list,
                                          groups = c("spin2", "init"), 
                                          color = "communities")


write_rds(ATTAU_par_list, "outputs/code/0_2_4_cluster_ATTAU_ZU/rds/ATTAU_par_list.rds")
write_rds(ATTAU_outputs, "outputs/code/0_2_4_cluster_ATTAU_ZU/rds/ATTAU_outputs.rds")