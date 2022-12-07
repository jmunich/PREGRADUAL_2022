source("code/0_0_0_set_dirs.R", encoding = "UTF-8")
source("code/0_0_1_packages.R", encoding = "UTF-8")
source("code/0_0_2_funs.R", encoding = "UTF-8")
source("code/0_1_1_clean_ZU.R", encoding = "UTF-8")
source("code/0_1_2_clean_AU.R", encoding = "UTF-8")
source("code/0_1_3_weights.R", encoding = "UTF-8")


if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

EFF_tosave <- read_rds("outputs/code/0_2_1_cluster_EFF_ZU/rds/EFF_outputs.rds")
ATT_tosave <- read_rds("outputs/code/0_2_2_cluster_ATT_ZU/rds/ATT_outputs.rds")
EFFAU_tosave <- read_rds("outputs/code/0_2_3_cluster_EFFAU_ZU/rds/EFFAU_outputs.rds")
ATTAU_tosave <- read_rds("outputs/code/0_2_4_cluster_ATTAU_ZU/rds/ATTAU_outputs.rds")

EFF_par_list <- read_rds("outputs/code/0_2_1_cluster_EFF_ZU/rds/EFF_par_list.rds")
ATT_par_list <- read_rds("outputs/code/0_2_2_cluster_ATT_ZU/rds/ATT_par_list.rds")
EFFAU_par_list <- read_rds("outputs/code/0_2_3_cluster_EFFAU_ZU/rds/EFFAU_par_list.rds")
ATTAU_par_list <- read_rds("outputs/code/0_2_4_cluster_ATTAU_ZU/rds/ATTAU_par_list.rds")

# Size parameters ---------------------------------------------------------

W1 <- 14 
H1 <- 14
W2 <- 9 
H2 <- 9
W3 <- 6 
H3 <- 6

vsize1 <- 1.2
vsize2 <- 1.2
vsize3 <- 1.2
vsize4 <- 1.1
splits <- c(0,.1,.2,.3)
esize1 <- 3
esize2 <- 4
esize3 <- 1.75
vlab.size2 <- .5
elabs.pos <- 8/10


# Ad hoc saves ------------------------------------------------------------

EFF_tosave$scores$plt_thresh_rescaled_2
EFFAU_tosave$scores$plt_thresh_rescaled_2 <- EFFAU_tosave$scores$plt_thresh_rescaled
EFFAU_tosave$scores$plt_thresh_rescaled_2$data$group <- EFF_tosave$scores$plt_thresh_rescaled_2$data$group

# summary table -----------------------------------------------------------
dat_list_raw <- list(
  EFF_tosave$data$raw,
  ATT_tosave$data$raw,
  EFFAU_tosave$data$raw,
  ATTAU_tosave$data$raw
)

dat_list_id1 <- list(
  EFF_tosave$data$all$IDfac,
  ATT_tosave$data$all$IDfac,
  EFFAU_tosave$data$all$IDfac.,
  ATTAU_tosave$data$all$IDfac.
)

dat_list_all <- list(
  EFF_tosave$data$all,
  ATT_tosave$data$all,
  EFFAU_tosave$data$all,
  ATTAU_tosave$data$all
)

tab_list <- list()
  
tab_list[[1]] <- lapply(dat_list_raw, nrow) %>% unlist() %>% tibble(N = .)

tab_list[[2]] <- lapply(dat_list_all, function(x){table(x$SEX, useNA = "always")}) %>% 
  bind_rows() %>% 
  rename(`Bez odpovědi` = ...4) %>% 
  select(-...1) %>% 
  select(Žena, Muž, Jiné, `Bez odpovědi`) %>%
  rename(`Z toho: Žena` = Žena)

tab_list[[3]] <- lapply(dat_list_all, function(x){paste0(
  round(mean(x$AGE, na.rm = TRUE), 2), 
  " (", 
  round(sd(x$AGE, na.rm = TRUE), 2),
  ")")}) %>% 
  unlist() %>% 
  ifelse(grepl("NA", .), "", .) %>%
  tibble(`Věk (průměr a SD)` = .)

tab_list[[4]] <- lapply(dat_list_id1, function(x){mean(is.na(x))}) %>% unlist() %>% round(3) %>% tibble(`Neuvedená fakulta (podíl)` = .)
tab_list[[5]] <- lapply(dat_list_raw, function(x){mean(unlist(is.na(x)))}) %>% unlist() %>% round(3) %>% tibble(`Podíl NA` = .)

tab_N_0 <- bind_cols(tab_list) %>%
  mutate_all(function(x){ifelse(is.na(x), "", as.character(x))}) %>%
  mutate(Sada = c("Začínající učitelé", "Začínající učitelé", "Absolventi", "Absolventi"),
         Dotazník = c("Sebehodnocení", "Postoje", "Sebehodnocení", "Postoje")) %>%
  mutate_at(1:7, function(x){ifelse(c(T,F,T,F), x, "")}) %>%
  select(Sada, Dotazník, everything())

tab_N_1 <- tab_N_0[c(1,3),-c(2,10)]

tab_N_2 <- tab_N_0 %>% 
  select(Sada, Dotazník, `Podíl NA`) %>%
  pivot_longer(-c(1,2)) %>%
  mutate(name = paste0(name, ": ", Dotazník)) %>%
  select(-Dotazník) %>%
  pivot_wider(id_cols = Sada) %>%
  .[,c(3,2)]

tab_N <- bind_cols(tab_N_1, tab_N_2)


(ggarrange(ggarrange(W_plt_0 +
                       scale_y_continuous(labels = scales::percent), 
                     W_plt_1 + labs(x = "") + scale_y_continuous(labels = scales::percent), 
                     ncol = 2, 
                     widths = c(2,4),
                     labels = "AUTO",
                     common.legend = TRUE, legend = "bottom"), 
           W_plt_2,
           labels = c("", "C"), 
           ncol = 1, 
           heights = c(2,2.5)) +
    theme(plot.background = element_rect(color = "white", fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/0_N.png",
         .,
         width = W1, 
         height = H2+2)

# base plots --------------------------------------------------------------


g000_ATT <- (ggarrange(ATT_tosave$scores$plt_interval_rescaled_2 +
                         theme_minimal() +
                         theme(legend.position = "none"), 
                       ncol = 1,
                       ATT_tosave$scores$plt_thresh_rescaled_2 +
                         theme_minimal() +
                         theme(legend.position = "bottom") +
                         guides(fill = guide_legend(nrow = 1)), heights = c(9,3)) +
               theme(plot.background = element_rect(color = "white", fill = "white")))

g000_ATTAU <- (ggarrange(ATTAU_tosave$scores$plt_interval_rescaled +
                         theme_minimal() +
                         theme(legend.position = "none"), 
                       ncol = 1,
                       ATTAU_tosave$scores$plt_thresh_rescaled +
                         theme_minimal() +
                         theme(legend.position = "bottom") +
                         guides(fill = guide_legend(nrow = 1)), heights = c(9,3)) +
               theme(plot.background = element_rect(color = "white", fill = "white")))

g000_ATT %>%
  ggsave("outputs/code/0_3_save/report/000_ATT_FSC.png",
         .,
         width = W2, 
         height = H2+1)

(ggarrange(EFF_tosave$scores$plt_thresh_rescaled_2 +
            theme_minimal() +
            theme(legend.position = "none"), 
          ncol = 1,
          EFF_tosave$scores$plt_thresh_rescaled2_3 +
            theme_minimal() +
            theme(legend.position = "bottom"), 
          heights = c(10,1.75), 
          common.legend = TRUE, legend = "bottom") +
  theme(plot.background = element_rect(color = "white", fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/000_EFF_FSC.png",
         .,
         width = W2, 
         height = H2+2)


g10 <- q2g(EFF_tosave$qgraphs$spin, 
    vsize = vsize1, 
    esize = esize1, 
    vlab.size = vlab.size2,
    zero_edge = FALSE)
g11 <- q2g(EFF_tosave$qgraphs$spin, 
    vsize = vsize1, 
    esize = esize1, 
    vlab.size = vlab.size2,
    zero_edge = FALSE,
    split_col = 3,
    splits = c(0,.1,.3))

g13 <- qlegend(EFF_tosave$qgraphs$spin, 
               ncol = 3,
               wdtmod = 1.1,
               linmod = 3,
               text.size = 3.1,
               ptmod = 1, 
               lineheight = .9,
               wrap = 50)

(ggarrange(ggarrange(g10, 
                     g11, 
                     ncol = 2, 
                     widths = c(2,4),
                     labels = "AUTO"), 
           g13, 
           ncol = 1, 
           heights = c(2,2.5)) +
    theme(plot.background = element_rect(color = "white", fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/00_EFF.png",
         .,
         width = W1, 
         height = H2)

g20 <- q2g(ATT_tosave$qgraphs$spin, 
    vsize = vsize1, 
    esize = esize1, 
    vlab.size = vlab.size2,
    zero_edge = FALSE)
g21 <- q2g(ATT_tosave$qgraphs$spin, 
    vsize = vsize1, 
    esize = esize1, 
    vlab.size = vlab.size2,
    zero_edge = FALSE,
    split_col = 3,
    splits = c(0,.1,.3))

tmpgrp <- ATT_tosave$qgraphs$spin$graphAttributes$Graph$groups
tmpgrp2 <- tmpgrp[lengths(tmpgrp) != 1]
tmpgrp2$Ostatní <- unlist(tmpgrp[lengths(tmpgrp) == 1]) %>%
  `names<-`(NULL)

cord <- names(tmpgrp2)[order(parse_number(names(tmpgrp2)))]

g23 <- qlegend(ATT_tosave$qgraphs$spin, 
               group_ord = cord,
               groups = tmpgrp2, 
               ncol = 3,
               wdtmod = 1,
               linmod = 3,
               text.size = 3.1,
               ptmod = 1, 
               lineheight = .9,
               wrap = 55)

(ggarrange(ggarrange(g20, 
                    g21, 
                    ncol = 2, 
                    widths = c(2,4),
                    labels = "AUTO"), 
          g23, 
          ncol = 1, 
          heights = c(2,2.75)) +
  theme(plot.background = element_rect(color = "white", fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/00_ATT.png",
         .,
         width = W1, 
         height = H1-2)


g30 <- q2g(ATTAU_tosave$qgraphs$spin, 
           vsize = vsize1, 
           esize = esize1, 
           vlab.size = vlab.size2,
           zero_edge = FALSE)
g31 <- q2g(ATTAU_tosave$qgraphs$spin, 
           vsize = vsize1, 
           esize = esize1, 
           vlab.size = vlab.size2,
           zero_edge = FALSE,
           split_col = 3,
           splits = c(0,.1,.3))

AUtmpgrp <- ATTAU_tosave$qgraphs$spin$graphAttributes$Graph$groups
AUtmpgrp2 <- AUtmpgrp[lengths(AUtmpgrp) != 1]
AUtmpgrp2$Ostatní <- unlist(AUtmpgrp[lengths(AUtmpgrp) == 1]) %>%
  `names<-`(NULL)

AUcord <- names(AUtmpgrp2)[order(parse_number(names(AUtmpgrp2)))]

g33 <- qlegend(ATTAU_tosave$qgraphs$spin, 
               group_ord = AUcord,
               groups = AUtmpgrp2, 
               ncol = 3,
               wdtmod = 1,
               linmod = 3,
               text.size = 3.1,
               ptmod = 1, 
               lineheight = .9,
               wrap = 55)

(ggarrange(ggarrange(g30, 
                     g31, 
                     ncol = 2, 
                     widths = c(2,4),
                     labels = "AUTO"), 
           g33, 
           ncol = 1, 
           heights = c(2,2.75)) +
    theme(plot.background = element_rect(color = "white", fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/00_ATTAU.png",
         .,
         width = W1 + 4, 
         height = H1)


g40 <- q2g(EFFAU_tosave$qgraphs$spin, 
           vsize = vsize1, 
           esize = esize1, 
           vlab.size = vlab.size2,
           zero_edge = FALSE)
g41 <- q2g(EFFAU_tosave$qgraphs$spin, 
           vsize = vsize1, 
           esize = esize1, 
           vlab.size = vlab.size2,
           zero_edge = FALSE,
           split_col = 3,
           splits = c(0,.1,.3))

g43 <- qlegend(EFFAU_tosave$qgraphs$spin, 
               ncol = 3,
               wdtmod = 1.1,
               linmod = 3,
               text.size = 3.1,
               ptmod = 1, 
               lineheight = .9,
               wrap = 50)

(ggarrange(ggarrange(g40, 
                     g41, 
                     ncol = 2, 
                     widths = c(2,4),
                     labels = "AUTO"), 
           g43, 
           ncol = 1, 
           heights = c(2,2.5)) +
    theme(plot.background = element_rect(color = "white", fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/00_EFFAU.png",
         .,
         width = W1, 
         height = H2)


# Create outputs ----------------------------------------------------------

# Key plots ---------------------------------------------------------------
(ggarrange(labels = "AUTO",
          EFF_tosave$freq$init + theme_minimal() + theme(legend.position = "bottom"),
          ATT_tosave$freq$init +
            guides(fill = guide_legend(nrow = 1)) + theme_minimal() + theme(legend.position = "bottom")
) + theme(plot.background = element_rect(fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/1_1_freq_init_ZU.png",
         .,
         width = W1, 
         height = H1)

(ggarrange(labels = "AUTO",
          EFF_tosave$freq$spin + theme_minimal() + theme(legend.position = "bottom"),
          ATT_tosave$freq$spin +
            guides(fill = guide_legend(nrow = 1)) + theme_minimal() + theme(legend.position = "bottom")
) + theme(plot.background = element_rect(fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/1_2_freq_spin_ZU.png",
         .,
         width = W1, 
         height = H1)

(ggarrange(labels = "AUTO",
          EFFAU_tosave$freq$init +
            guides(fill = guide_legend(nrow = 1)) + theme_minimal() + theme(legend.position = "bottom"),
          ATTAU_tosave$freq$init +
            guides(fill = guide_legend(nrow = 1)) + theme_minimal() + theme(legend.position = "bottom")
) + theme(plot.background = element_rect(fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/2_1_freq_init_AU.png",
         .,
         width = W1, 
         height = H1)

(ggarrange(labels = "AUTO",
          EFFAU_tosave$freq$spin +
            guides(fill = guide_legend(nrow = 1)) + theme_minimal() + theme(legend.position = "bottom"),
          ATTAU_tosave$freq$spin +
            scale_fill_manual(values = use_cols[7:1]) +
            guides(fill = guide_legend(nrow = 1)) + theme_minimal() + theme(legend.position = "bottom")
) + theme(plot.background = element_rect(fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/2_2_freq_spin_AU.png",
         .,
         width = W1, 
         height = H1)

(ggarrange(labels = c("A", ""), 
          ncol = 1,
          heights = c(H3-1, H1),
          EFFAU_tosave$freq$compare +
            scale_fill_manual(values = use_cols[c(7,1)]) +
            theme_minimal() + theme(legend.position = "bottom"),
          
          ggarrange(labels = c("B", "C"),
                    EFFAU_tosave$freq$compare_ZU + theme_minimal() + theme(legend.position = "bottom") +
                      guides(fill = guide_legend(nrow = 1)),
                    EFFAU_tosave$freq$compare_AU + theme_minimal() + theme(legend.position = "bottom") +
                      guides(fill = guide_legend(nrow = 1))
          )) + theme(plot.background = element_rect(fill = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/2_3_freq_spin_COMP.png",
         .,
         width = W1, 
         height = H1 + H3 - 1)


# Bootstrap
(ggarrange(labels = "AUTO",
  ggarrange(labels = "auto",
           EFF_tosave$ggplot$boot$plt_CI_1 +
             labs(y = "") +
             theme_minimal(),
           EFF_tosave$ggplot$boot$plt_CI_2 +
             labs(x = "", y = "") +
             theme_minimal(),
           EFF_tosave$ggplot$boot$plt_CI_3 +
             labs(y = "", x = "") +
             theme_minimal(),
           ncol = 3,
           legend = "none",
           common.legend = TRUE,
           label.x = c(.05,0,0)
           ),
  ggarrange(labels = "auto",
            ATT_tosave$ggplot$boot$plt_CI_1 +
              labs(y = "") +
              theme_minimal(),
            ATT_tosave$ggplot$boot$plt_CI_2 +
              labs(x = "") +
              scale_size(range = c(5,100)) +
              theme_minimal(),
            ATT_tosave$ggplot$boot$plt_CI_3 +
              labs(y = "", x = "") +
              theme_minimal(),
            ncol = 3,
            legend = "bottom",
            common.legend = TRUE,
            label.x = c(.05,0,0)
            
  ),
  ncol = 1
) +
  theme(plot.background = element_rect(fill = "white", 
                                       color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/3_1_plt_CI_ZU.png",
         .,
         width = W1, 
         height = H1)

(ggarrange(labels = "AUTO",
  ggarrange(labels = "auto",
           EFFAU_tosave$ggplot$boot$plt_CI_1 +
             labs(y = "") +
             theme_minimal(),
           EFFAU_tosave$ggplot$boot$plt_CI_2 +
             labs(x = "", y = "") +
             theme_minimal(),
           EFFAU_tosave$ggplot$boot$plt_CI_3 +
             labs(y = "", x = "") +
             theme_minimal(),
           ncol = 3,
           legend = "none",
           common.legend = TRUE,
           label.x = c(.05,0,0)
           ),
  ggarrange(labels = "auto",
            ATTAU_tosave$ggplot$boot$plt_CI_1 +
              labs(y = "") +
              theme_minimal(),
            ATTAU_tosave$ggplot$boot$plt_CI_2 +
              labs(x = "") +
              scale_size(range = c(5,100)) +
              theme_minimal(),
            ATTAU_tosave$ggplot$boot$plt_CI_3 +
              labs(y = "", x = "") +
              theme_minimal(),
            ncol = 3,
            legend = "bottom",
            common.legend = TRUE,
            label.x = c(.05,0,0)
            
  ),
  ncol = 1
) +
  theme(plot.background = element_rect(fill = "white", 
                                       color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/4_1_plt_CI_AU.png",
         .,
         width = W1, 
         height = H1)

# NCT
(ggarrange(labels = "AUTO",
  ggarrange(labels = c("", "c"),
            ggarrange(labels = "auto",
                      ncol = 1,
                      EFF_tosave$nct$nct_inv_plt +
                        theme_minimal(),
                      EFF_tosave$nct$nct_plt +
                        theme_minimal(),
                      legend = "bottom",
                      common.legend = TRUE,
                      label.x = c(.05,.01) 
            ),
            q2g(EFF_tosave$qgraphs$nct, 
                vsize = vsize1, 
                esize = esize1, 
                zero_edge = FALSE),
            ncol = 2),
  ggarrange(labels = c("", "c"),
            ggarrange(labels = "auto",
                      ncol = 1,
                      ATT_tosave$nct$nct_inv_plt +
                        theme_minimal(),
                      ATT_tosave$nct$nct_plt +
                        theme_minimal(),
                      legend = "bottom",
                      common.legend = TRUE,
                      label.x = c(.025,.01)
            ),
            q2g(ATT_tosave$qgraphs$nct, 
                vsize = vsize1, 
                esize = esize1,
                threshold = .1,
                zero_edge = FALSE),
            ncol = 2), ncol = 1)+
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/5_1_plt_nct_ZU.png",
         .,
         width = W1, 
         height = H1)

(ggarrange(labels = c("", "c"),
           ggarrange(labels = "auto",
                     ncol = 1,
                     EFFAU_tosave$nct$nct_inv_plt +
                       theme_minimal(),
                     EFFAU_tosave$nct$nct_plt +
                       theme_minimal(),
                     legend = "bottom",
                     common.legend = TRUE,
                     label.x = c(.05,.01) 
           ),
           q2g(EFFAU_tosave$qgraphs$nct, 
               vsize = vsize1, 
               esize = esize1, 
               zero_edge = FALSE),
           ncol = 2) + 
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/6_1_plt_nct_AU.png",
         .,
         width = W1, 
         height = H1/2)

# init fit

(ggarrange(labels = "AUTO",
           ncol = 1,
           ggarrange(labels = "auto",
            q2g(EFF_tosave$qgraphs$init, 
                vsize = vsize1, 
                esize = esize1, 
                zero_edge = FALSE),
            q2g(EFF_tosave$qgraphs$init, 
                splits = splits,
                vsize = vsize2,
                esize = esize2,
                vlab.size = vlab.size2,
                edge_distribution = FALSE),
            label.x = c(.05, 0)),
  ggarrange(labels = "auto",
           q2g(ATT_tosave$qgraphs$init, 
               vsize = vsize1, 
               esize = esize1, 
               zero_edge = FALSE),
           q2g(ATT_tosave$qgraphs$init, 
               splits = splits,
               vsize = vsize2,
               esize = esize2,
               vlab.size = vlab.size2,
               edge_distribution = FALSE),
           label.x = c(.05, 0))) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/7_fit_ZU.png",
         .,
         width = W1 - 2, 
         height = H2 + 3)

(ggarrange(labels = "AUTO",
           ncol = 1,
           ggarrange(labels = "auto",
            q2g(EFFAU_tosave$qgraphs$init, 
                vsize = vsize1, 
                esize = esize1, 
                zero_edge = FALSE),
            q2g(EFFAU_tosave$qgraphs$init, 
                splits = splits,
                vsize = vsize2,
                esize = esize2,
                vlab.size = vlab.size2,
                edge_distribution = FALSE),
            label.x = c(.05, 0)),
  ggarrange(labels = "auto",
           q2g(ATTAU_tosave$qgraphs$init, 
               vsize = vsize1, 
               esize = esize1, 
               zero_edge = FALSE),
           q2g(ATTAU_tosave$qgraphs$init, 
               splits = splits,
               vsize = vsize2,
               esize = esize2,
               vlab.size = vlab.size2,
               edge_distribution = FALSE),
           label.x = c(.05, 0))) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/8_fit_AU.png",
         .,
         width = W1 - 2, 
         height = H2 + 3)


# cons fit

(ggarrange(labels = "AUTO",
           ncol = 1,
           ggarrange(labels = "auto",
                     q2g(EFF_tosave$qgraphs$cons_fit, 
                         vsize = vsize1, 
                         esize = esize1, 
                         zero_edge = FALSE),
                     q2g(EFF_tosave$qgraphs$cons_fit, 
                         splits = splits,
                         vsize = vsize2,
                         esize = esize2,
                         vlab.size = vlab.size2,
                         edge_distribution = FALSE),
                     label.x = c(.05, 0)),
           ggarrange(labels = "auto",
                     q2g(ATT_tosave$qgraphs$cons_fit, 
                         vsize = vsize1, 
                         esize = esize1, 
                         zero_edge = FALSE),
                     q2g(ATT_tosave$qgraphs$cons_fit, 
                         splits = splits,
                         vsize = vsize2,
                         esize = esize2,
                         vlab.size = vlab.size2,
                         edge_distribution = FALSE),
                     label.x = c(.05, 0))) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/9_cons_fit_ZU.png",
         .,
         width = W1 - 2, 
         height = H2 + 3)

(ggarrange(labels = "AUTO",
           ncol = 1,
           ggarrange(labels = "auto",
                     q2g(EFFAU_tosave$qgraphs$cons_fit, 
                         vsize = vsize1, 
                         esize = esize1, 
                         zero_edge = FALSE),
                     q2g(EFFAU_tosave$qgraphs$cons_fit, 
                         splits = splits,
                         vsize = vsize2,
                         esize = esize2,
                         vlab.size = vlab.size2,
                         edge_distribution = FALSE),
                     label.x = c(.05, 0)),
           ggarrange(labels = "auto",
                     q2g(ATTAU_tosave$qgraphs$cons_fit, 
                         vsize = vsize1, 
                         esize = esize1, 
                         zero_edge = FALSE),
                     q2g(ATTAU_tosave$qgraphs$cons_fit, 
                         splits = splits,
                         vsize = vsize2,
                         esize = esize2,
                         vlab.size = vlab.size2,
                         edge_distribution = FALSE),
                     label.x = c(.05, 0))) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/10_cons_fit_AU.png",
         .,
         width = W1 - 2, 
         height = H2 + 3)

# community fit

(ggarrange(labels = "AUTO",
           ncol = 1,
  ggarrange(labels = "auto",
            widths = c(1,1.2),
            q2g(EFF_tosave$qgraphs_nl$spin_coms, 
                vsize = vsize3,
                vlab.size = .7, 
                esize = esize3, 
                zero_edge = FALSE),
            EFF_tosave$ggplot$dendro,
            label.x = c(.05, 0)),
  ggarrange(labels = "auto",
            widths = c(1,1.2),
            q2g(ATT_tosave$qgraphs_nl$spin_coms, 
                vsize = vsize3, 
                vlab.size = .7, 
                esize = esize3, 
                zero_edge = FALSE),
            ATT_tosave$ggplot$dendro,
            label.x = c(.05, 0))) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/11_graph_comm_ZU.png",
         .,
         width = W1, 
         height = H2+3)

(ggarrange(labels = "AUTO",
           ncol = 1,
  ggarrange(labels = "auto",
            widths = c(1,1.2),
            q2g(EFFAU_tosave$qgraphs_nl$spin_coms, 
                vsize = vsize3,
                vlab.size = .7, 
                esize = esize3, 
                zero_edge = FALSE),
            EFFAU_tosave$ggplot$dendro,
            label.x = c(.05, 0)),
  ggarrange(labels = "auto",
            widths = c(1,1.2),
            q2g(ATTAU_tosave$qgraphs_nl$spin_coms, 
                vsize = vsize3, 
                vlab.size = .7, 
                esize = esize3, 
                zero_edge = FALSE),
            ATTAU_tosave$ggplot$dendro,
            label.x = c(.05, 0))) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/12_graph_comm_AU.png",
         .,
         width = W1, 
         height = H2+3)


# spins

(ggarrange(labels = "AUTO",
           ncol = 1,
           ggarrange(labels = "auto",
                     q2g(EFF_tosave$qgraphs$spin, 
                         vsize = vsize1, 
                         esize = esize1, 
                         zero_edge = FALSE),
                     q2g(EFF_tosave$qgraphs$spin, 
                         splits = splits,
                         vsize = vsize2,
                         esize = esize2,
                         vlab.size = vlab.size2,
                         edge_distribution = FALSE),
                     label.x = c(.05, 0)),
           ggarrange(labels = "auto",
                     q2g(ATT_tosave$qgraphs$spin, 
                         vsize = vsize1, 
                         esize = esize1, 
                         zero_edge = FALSE),
                     q2g(ATT_tosave$qgraphs$spin, 
                         splits = splits,
                         vsize = vsize2,
                         esize = esize2,
                         vlab.size = vlab.size2,
                         edge_distribution = FALSE),
                     label.x = c(.05, 0))) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/13_fit_tree_ZU.png",
         .,
         width = W1 - 2, 
         height = H2 + 3)

(ggarrange(labels = "AUTO",
           ncol = 1,
           ggarrange(labels = "auto",
                     q2g(EFFAU_tosave$qgraphs$spin, 
                         vsize = vsize1, 
                         esize = esize1, 
                         zero_edge = FALSE),
                     q2g(EFFAU_tosave$qgraphs$spin, 
                         splits = splits,
                         vsize = vsize2,
                         esize = esize2,
                         vlab.size = vlab.size2,
                         edge_distribution = FALSE),
                     label.x = c(.05, 0)),
           ggarrange(labels = "auto",
                     q2g(ATTAU_tosave$qgraphs$spin, 
                         vsize = vsize1, 
                         esize = esize1, 
                         zero_edge = FALSE),
                     q2g(ATTAU_tosave$qgraphs$spin, 
                         splits = splits,
                         vsize = vsize2,
                         esize = esize2,
                         vlab.size = vlab.size2,
                         edge_distribution = FALSE),
                     label.x = c(.05, 0))) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/14_fit_tree_AU.png",
         .,
         width = W1 - 2, 
         height = H2 + 3)


# sem init
g_sem_1 <- ggarrange(
  labels = c("a", ""),
  label.x = c(.05, 0),
  widths = c(1.75,1),
  q2g(EFF_tosave$semplots$init$graph_sem,
      rm.lav.cov = TRUE,
      vsize = vsize4,
      elabs = TRUE,
      vlab.size = vlab.size2,
      esize = esize3,
      elabs.pos = elabs.pos, 
      directed = TRUE),
  ggarrange(ncol = 1, nrow = 2,
            labels = c("b", "c"),
            EFF_tosave$semplots$init$sem_cmat,
            q2g(EFF_tosave$semplots$init$graph_sem_str,
                vsize = vsize3,
                esize = esize3,
                vlab.size = vlab.size2,
                rm.lav.cov = TRUE)))

g_sem_2 <- ggarrange(
  labels = c("a", ""),
  label.x = c(.05, 0),
  widths = c(1.75,1),
  q2g(ATT_tosave$semplots$init$graph_sem,
      rm.lav.cov = TRUE,
      vsize = vsize4,
      elabs = TRUE,
      vlab.size = vlab.size2,
      esize = esize3,
      elabs.pos = elabs.pos, 
      directed = TRUE),
  ggarrange(ncol = 1, nrow = 2,
            labels = c("b", "c"),
            ATT_tosave$semplots$init$sem_cmat,
            q2g(ATT_tosave$semplots$init$graph_sem_str,
                vsize = vsize3,
                esize = esize3,
                vlab.size = vlab.size2,
                rm.lav.cov = TRUE)))

  
(ggarrange(labels = "AUTO",
           ncol = 1,
           g_sem_1,
           g_sem_2) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/15_graph_sem_ZU.png",
         .,
         width = W2, 
         height = 2*H3)

g_sem_1AU <- ggarrange(
  labels = c("a", ""),
  label.x = c(.05, 0),
  widths = c(1.75,1),
  q2g(EFFAU_tosave$semplots$init$graph_sem,
      rm.lav.cov = TRUE,
      vsize = vsize4,
      elabs = TRUE,
      vlab.size = vlab.size2,
      esize = esize3,
      elabs.pos = elabs.pos, 
      directed = TRUE),
  ggarrange(ncol = 1, nrow = 2,
            labels = c("b", "c"),
            EFFAU_tosave$semplots$init$sem_cmat,
            q2g(EFFAU_tosave$semplots$init$graph_sem_str,
                vsize = vsize3,
                esize = esize3,
                vlab.size = vlab.size2,
                rm.lav.cov = TRUE)))

g_sem_2AU <- ggarrange(
  labels = c("a", ""),
  label.x = c(.05, 0),
  widths = c(1.75,1),
  q2g(ATTAU_tosave$semplots$init$graph_sem,
      rm.lav.cov = TRUE,
      vsize = vsize4,
      elabs = TRUE,
      vlab.size = vlab.size2,
      esize = esize3,
      elabs.pos = elabs.pos, 
      directed = TRUE),
  ggarrange(ncol = 1, nrow = 2,
            labels = c("b", "c"),
            ATTAU_tosave$semplots$init$sem_cmat,
            q2g(ATTAU_tosave$semplots$init$graph_sem_str,
                vsize = vsize3,
                esize = esize3,
                vlab.size = vlab.size2,
                rm.lav.cov = TRUE)))

  
(ggarrange(labels = "AUTO",
           ncol = 1,
           g_sem_1AU,
           g_sem_2AU) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/16_graph_sem_AU.png",
         .,
         width = W2, 
         height = 2*H3)


# adjusted SEM ------------------------------------------------------------

g_asem_1 <- ggarrange(
  labels = c("a", ""),
  label.x = c(.05, 0),
  widths = c(1.75,1),
  q2g(EFF_tosave$semplots$bifactor$graph_sem,
      rm.lav.cov = TRUE,
      vsize = vsize4,
      elabs = TRUE,
      vlab.size = vlab.size2,
      esize = esize3,
      elabs.pos = elabs.pos, 
      directed = TRUE),
  ggarrange(ncol = 1, nrow = 2,
            labels = c("b", "c"),
            EFF_tosave$semplots$bifactor$sem_cmat,
            q2g(EFF_tosave$semplots$bifactor$graph_sem_str,
                vsize = vsize3,
                esize = esize3,
                vlab.size = vlab.size2,
                rm.lav.cov = TRUE)))

g_asem_2 <- ggarrange(
  labels = c("a", ""),
  label.x = c(.05, 0),
  widths = c(1.75,1),
  q2g(ATT_tosave$semplots$adapt$graph_sem,
      rm.lav.cov = TRUE,
      vsize = vsize4,
      elabs = TRUE,
      vlab.size = vlab.size2,
      esize = esize3,
      elabs.pos = elabs.pos, 
      directed = TRUE),
  ggarrange(ncol = 1, nrow = 2,
            labels = c("b", "c"),
            ATT_tosave$semplots$adapt$sem_cmat,
            q2g(ATT_tosave$semplots$adapt$graph_sem_str,
                vsize = vsize3,
                esize = esize3,
                vlab.size = vlab.size2,
                rm.lav.cov = TRUE)))


(ggarrange(labels = "AUTO",
           ncol = 1,
           g_asem_1,
           g_asem_2) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/17_graph_BF_ZU.png",
         .,
         width = W2, 
         height = 2*H3)

g_asem_1AU <- ggarrange(
  labels = c("a", ""),
  label.x = c(.05, 0),
  widths = c(1.75,1),
  q2g(EFFAU_tosave$semplots$bifactor$graph_sem,
      rm.lav.cov = TRUE,
      vsize = vsize4,
      elabs = TRUE,
      vlab.size = vlab.size2,
      esize = esize3,
      elabs.pos = elabs.pos, 
      directed = TRUE),
  ggarrange(ncol = 1, nrow = 2,
            labels = c("b", "c"),
            EFFAU_tosave$semplots$bifactor$sem_cmat,
            q2g(EFFAU_tosave$semplots$bifactor$graph_sem_str,
                vsize = vsize3,
                esize = esize3,
                vlab.size = vlab.size2,
                rm.lav.cov = TRUE)))

g_asem_2AU <- ggarrange(
  labels = c("a", ""),
  label.x = c(.05, 0),
  widths = c(1.75,1),
  q2g(ATTAU_tosave$semplots$adapt$graph_sem,
      rm.lav.cov = TRUE,
      vsize = vsize4,
      elabs = TRUE,
      vlab.size = vlab.size2,
      esize = esize3,
      elabs.pos = elabs.pos, 
      directed = TRUE),
  ggarrange(ncol = 1, nrow = 2,
            labels = c("b", "c"),
            ATTAU_tosave$semplots$adapt$sem_cmat,
            q2g(ATTAU_tosave$semplots$adapt$graph_sem_str,
                vsize = vsize3,
                esize = esize3,
                vlab.size = vlab.size2,
                rm.lav.cov = TRUE)))


(ggarrange(labels = "AUTO",
           ncol = 1,
           g_asem_1AU,
           g_asem_2AU) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/18_graph_BF_AU.png",
         .,
         width = W2, 
         height = 2*H3)

# thresholds

g_thresh_1 <- ggarrange(ncol  = 2,
                        labels = c("", "c"),
                        ggarrange(labels = c("a", "b"),
                                  label.x = c(.05, 0),
                                  ncol = 1,
                                  EFF_tosave$scores$plt_thresh_1 +
                                    theme_minimal(),
                                  EFF_tosave$scores$plt_thresh_2 +
                                    theme_minimal(),
                                  legend = "bottom",
                                  common.legend = TRUE
                        ),
                        EFF_tosave$scores$plt_thresh_rescaled_2 + 
                          theme_minimal() +
                          theme(legend.position = "bottom") +
                          scale_fill_manual(values = use_cols[c(6,3,2,1)]))

g_thresh_2_0 <- ggarrange(ncol  = 1,heights = c(3,2.25),
                        labels = c("a", "b"),
                        label.x = c(.025, 0),
                        EFF_tosave$scores$sumscores_plt +
                          labs(x = "") +
                          theme_minimal(),
                        ATT_tosave$scores$sumscores_plt +
                          theme_minimal(),
                        common.legend = TRUE,
                        legend = "bottom")

g_thresh_2 <- ggarrange(ncol  = 2,
                        labels = c("", "C"),
                        label.x = c(.05, 0),
                        g000_ATT,
                        g_thresh_2_0)
  
(ggarrange(labels = c("A", "B"),
           ncol = 1,
           g_thresh_1,
           g_thresh_2) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/19_plt_thresh_ZU.png",
         .,
         width = W1, 
         height = H1+5)


g_thresh_1AU <- ggarrange(ncol  = 2,
                        labels = c("", "c"),
                        ggarrange(labels = c("a", "b"),
                                  label.x = c(.05, 0),
                                  ncol = 1,
                                  EFFAU_tosave$scores$plt_thresh_1 +
                                    theme_minimal(),
                                  EFFAU_tosave$scores$plt_thresh_2 +
                                    theme_minimal(),
                                  legend = "bottom",
                                  common.legend = TRUE
                        ),
                        EFFAU_tosave$scores$plt_thresh_rescaled + 
                          theme_minimal() +
                          theme(legend.position = "bottom") +
                          scale_fill_manual(values = use_cols[c(6,3,2,1)]))

g_thresh_2_0AU <- ggarrange(ncol  = 1,heights = c(2,3),
                        labels = c("a", "b"),
                        label.x = c(.025, 0),
                        EFFAU_tosave$scores$sumscores_plt +
                          labs(x = "") +
                          theme_minimal(),
                        ATTAU_tosave$scores$sumscores_plt +
                          theme_minimal(),
                        common.legend = TRUE,
                        legend = "bottom")


g_thresh_2AU <- ggarrange(ncol  = 2,
                          labels = c("", "C"),
                          label.x = c(.05, 0),
                          g000_ATTAU,
                          g_thresh_2_0AU)

(ggarrange(labels = c("A", "B"),
           ncol = 1,
           g_thresh_1AU,
           g_thresh_2AU) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/20_plt_thresh_AU.png",
         .,
         width = W1, 
         height = H1+4)




(ggarrange(ncol = 3, labels = "AUTO",
          q2g(ATT_tosave$qgraphs$pos_neg$pos, vsize = vsize1, vlab.size = vlab.size2),
          q2g(ATT_tosave$qgraphs$pos_neg$pairs, vsize = vsize1, vlab.size = vlab.size2,
              elabs = TRUE, elabs.pos = .5),
          q2g(ATT_tosave$qgraphs$pos_neg$neg, vsize = vsize1, vlab.size = vlab.size2)) +
  theme(plot.background = element_rect(fill = "white", 
                                       color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/21_plt_ATT_pairs_ZU.png",
         .,
         width = W1, 
         height = H3)

(ggarrange(ncol = 3, labels = "AUTO",
          q2g(ATTAU_tosave$qgraphs$pos_neg$pos, vsize = vsize1, vlab.size = vlab.size2),
          q2g(ATTAU_tosave$qgraphs$pos_neg$pairs, vsize = vsize1, vlab.size = vlab.size2,
              elabs = TRUE, elabs.pos = .5),
          q2g(ATTAU_tosave$qgraphs$pos_neg$neg, vsize = vsize1, vlab.size = vlab.size2)) +
  theme(plot.background = element_rect(fill = "white", 
                                       color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/22_plt_ATT_pairs_AU.png",
         .,
         width = W1, 
         height = H3)


(ggarrange(ncol = 1, labels = "AUTO", heights = c(6, 5),
           EFF_tosave$ggplot$groups,
           ATT_tosave$ggplot$groups) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/23_groups_ZU.png",
         .,
         width = W2, 
         height = H2+1, dpi = 720)

(ggarrange(ncol = 1, labels = "AUTO",
           EFFAU_tosave$ggplot$groups,
           ATTAU_tosave$ggplot$groups) +
    theme(plot.background = element_rect(fill = "white", 
                                         color = "white"))) %>%
  ggsave("outputs/code/0_3_save/report/24_groups_AU.png",
         .,
         width = W2+3, 
         height = H2+1, dpi = 720)

EFFAU_tosave$ggplot$compare_coms %>%
  ggsave("outputs/code/0_3_save/report/25_AU_groups_comp.png",
         .,
         width = W3, 
         height = H3-1, dpi = 720)

# Save tables


write.table(file = "outputs/code/0_3_save/report/0_N.txt",
            tab_N,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_3_save/report/1_1_EFF_lab_init.txt",
            EFF_tosave$tabs$lab_init,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_3_save/report/1_2_ATT_lab_init.txt",
            ATT_tosave$tabs$lab_init %>%
              mutate(Reverzní = ifelse(Reverzní, "Ano", "Ne")),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")
write.table(file = "outputs/code/0_3_save/report/2_1_AU_EFF_lab_comp.txt",
            EFFAU_tosave$tabs$fit_EFF,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_3_save/report/2_2_AU_ATT_labs.txt",
            ATTAU_tosave$tabs$lab_spin,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

tab_3 <- EFF_tosave$tabs$fit_init %>%
  mutate(Model = "Sebehodnocení") %>%
  bind_rows(ATT_tosave$tabs$fit_init %>%
              mutate(Model = "Postoje")) %>%
  select(Model, everything())

write.table(file = "outputs/code/0_3_save/report/1_3_fit_init.txt",
            tab_mod(tab_3, skip = 1),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

tab_3_AU <- EFFAU_tosave$tabs$fit_init %>%
  mutate(Model = "Sebehodnocení") %>%
  bind_rows(ATTAU_tosave$tabs$fit_init %>%
              mutate(Model = "Postoje")) %>%
  select(Model, everything())

write.table(file = "outputs/code/0_3_save/report/2_3_AU_fit_init.txt",
            tab_mod(tab_3, skip = 1),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_3_save/report/2_4_AU_EFF_fit_init.txt",
            tab_mod(EFFAU_tosave$tabs$fit_EFF, skip = 1),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

tab_4 <- EFF_tosave$nct$nct_test_print %>%
  mutate(Model = "Sebehodnocení") %>%
  bind_rows(ATT_tosave$nct$nct_test_print %>%
              mutate(Model = "Postoje")) %>%
  select(Model, everything())

write.table(file = "outputs/code/0_3_save/report/1_4_nct_test.txt",
            tab_mod(tab_4, skip = 1),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_3_save/report/2_5_EFFAU_nct_test.txt",
            tab_mod(EFFAU_tosave$nct$nct_test_print, skip = 1),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")


write.table(file = "outputs/code/0_3_save/report/1_5_EFF_lab_spin.txt",
            EFF_tosave$tabs$lab_spin,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_3_save/report/2_6_EFFAU_lab_spin.txt",
            EFFAU_tosave$tabs$lab_spin,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_3_save/report/1_6_ATT_lab_spin.txt",
            ATT_tosave$tabs$lab_spin %>%
              mutate(Reverzní = ifelse(Reverzní, "Ano", "Ne")),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

write.table(file = "outputs/code/0_3_save/report/2_7_ATTAU_lab_spin.txt",
            ATTAU_tosave$tabs$lab_spin %>%
              mutate(Reverzní = ifelse(Reverzní, "Ano", "Ne")),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

tab_7 <- EFF_tosave$fits$init$tabs_print$fit %>%
  mutate(Model = "Sebehodnocení") %>%
  bind_rows(EFF_tosave$fits$bifactor$tabs_print$fit %>%
              mutate(Model = "Sebehodnocení - bifactor")) %>%
  bind_rows(ATT_tosave$fits$init$tabs_print$fit %>%
              mutate(Model = "Postoje")) %>%
  bind_rows(ATT_tosave$fits$adapt$tabs_print$fit %>%
              mutate(Model = "Postoje - upravené")) %>%
  select(Model, everything())

write.table(file = "outputs/code/0_3_save/report/1_7_tab_fit.txt",
            tab_mod(tab_7, skip = 1),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

tab_7AU <- EFFAU_tosave$fits$init$tabs_print$fit %>%
  mutate(Model = "Sebehodnocení") %>%
  bind_rows(EFFAU_tosave$fits$bifactor$tabs_print$fit %>%
              mutate(Model = "Sebehodnocení - bifactor")) %>%
  bind_rows(ATTAU_tosave$fits$init$tabs_print$fit %>%
              mutate(Model = "Postoje")) %>%
  bind_rows(ATTAU_tosave$fits$adapt$tabs_print$fit %>%
              mutate(Model = "Postoje - upravené")) %>%
  select(Model, everything())

write.table(file = "outputs/code/0_3_save/report/2_8_AU_tab_fit.txt",
            tab_mod(tab_7AU, skip = 1),
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

tab_8 <- EFF_tosave$fits$init$tabs_print$rels %>%
  mutate(Model = "Sebehodnocení") %>%
  bind_rows(EFF_tosave$fits$bifactor$tabs_print$rels %>%
              mutate(Model = "Sebehodnocení - bifactor")) %>%
  bind_rows(ATT_tosave$fits$init$tabs_print$rels %>%
              mutate(Model = "Postoje")) %>%
  bind_rows(ATT_tosave$fits$adapt$tabs_print$rels %>%
              mutate(Model = "Postoje - upravené")) %>%
  select(Model, starts_with("K"), G) %>%
  group_by(Model) %>%
  mutate(Model = ifelse((1:n()) == 1, Model, "")) %>%
  ungroup() %>%
  tab_mod(skip = c(1,2)) %>%
  `names<-`(gsub("K([0-9]+)", "Komunita \\1", names(.)))

write.table(file = "outputs/code/0_3_save/report/1_8_rel.txt",
            tab_8,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")

tab_8AU <- EFFAU_tosave$fits$init$tabs_print$rels %>%
  mutate(Model = "Sebehodnocení") %>%
  bind_rows(EFFAU_tosave$fits$bifactor$tabs_print$rels %>%
              mutate(Model = "Sebehodnocení - bifactor")) %>%
  bind_rows(ATTAU_tosave$fits$init$tabs_print$rels %>%
              mutate(Model = "Postoje")) %>%
  bind_rows(ATTAU_tosave$fits$adapt$tabs_print$rels %>%
              mutate(Model = "Postoje - upravené")) %>%
  select(Model, starts_with("K"), G) %>%
  group_by(Model) %>%
  mutate(Model = ifelse((1:n()) == 1, Model, "")) %>%
  ungroup() %>%
  tab_mod(skip = c(1,2)) %>%
  `names<-`(gsub("K([0-9]+)", "Komunita \\1", names(.)))

write.table(file = "outputs/code/0_3_save/report/2_8_rel.txt",
            tab_8AU,
            fileEncoding = "UTF-8",
            row.names = FALSE,
            sep = "\t",
            na = " ")