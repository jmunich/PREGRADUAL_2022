source("code/0_0_2_funs.R", encoding = "UTF-8")
source("code/0_1_1_clean_ZU.R", encoding = "UTF-8")
library("tidyverse")

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}


EFF_outputs <- read_rds("outputs/code/0_2_1_cluster_EFF_ZU/rds/EFF_outputs.rds")
ATT_outputs <- read_rds("outputs/code/0_2_2_cluster_ATT_ZU/rds/ATT_outputs.rds")
EFFAU_outputs <- read_rds("outputs/code/0_2_3_cluster_EFFAU_ZU/rds/EFFAU_outputs.rds")
ATTAU_outputs <- read_rds("outputs/code/0_2_4_cluster_ATTAU_ZU/rds/ATTAU_outputs.rds")

data_EFF <- read_rds("outputs/code/0_2_1_cluster_EFF_ZU/rds/EFF_outputs.rds")$scores$data$sumscores_data
data_ATT <- read_rds("outputs/code/0_2_2_cluster_ATT_ZU/rds/ATT_outputs.rds")$scores$data$sumscores_data
data_EFFAU <- read_rds("outputs/code/0_2_3_cluster_EFFAU_ZU/rds/EFFAU_outputs.rds")$scores$data$sumscores_data
data_ATTAU <- read_rds("outputs/code/0_2_4_cluster_ATTAU_ZU/rds/ATTAU_outputs.rds")$scores$data$sumscores_data

qual_ped <- c("Vysokoškolské vzdělání (Mgr.) se zaměřením na učitelství pro 1. stupeň nebo 2. stupeň a/nebo střední školy")                                                                                             
qual_nped <- c("Vysokoškolské vzdělání (Mgr./Ing./MgA.) jiného zaměření")
qual_dps <- c("Vysokoškolské vzdělání (Mgr./Ing./MgA.) jiného zaměření + dokončené Doplňující pedagogické studium (DPS, tzv. „pedagogické minimum“)")

EFF_rel <- EFF_outputs$fits$init$tabs$rels %>%
  .[,.[3,] > .6] %>%
  colnames()
ATT_rel <- ATT_outputs$fits$init$tabs$rels %>%
  .[,.[3,] > .6] %>%
  colnames()

data_scores <- data_use %>%
  filter(filt_RESPs) %>%
  bind_cols(data_EFF %>%
              select(EFF_rel) %>%
              `colnames<-`(paste0("EFF_", colnames(.)))) %>%
  bind_cols(data_ATT %>%
              select(ATT_rel) %>%
              `colnames<-`(paste0("ATT_", colnames(.)))) %>%
  mutate(Fakulta = case_when(IDfac == "Fakulta informatiky" ~ "Jiné",
                             IDfac == "Matematicko-fyzikální fakulta" ~ "Jiné",
                             TRUE ~ IDfac),
         Kvalifikace = case_when(QUAL %in% c(qual_ped, qual_nped, qual_dps) ~ "2. Magisterské",
                                 !is.na(QUAL) ~ "1. Nemagisterské",
                                 TRUE ~ NA_character_)
  ) %>%
  mutate(QUAL2 = ifelse(grepl("1\\.", LEVEL) & grepl("1\\.", QUAL), 
                        paste0(QUAL,": 1. Stupeň"),
                        ifelse((!grepl("1\\.", LEVEL)) & grepl("1\\.", QUAL), paste0(QUAL,": 2. Stupeň a SŠ"), 
                               QUAL))
  )



score_vars <- data_scores %>%
  select(starts_with("EFF_K")) %>%
  colnames() %>%
  c(data_scores %>%
      select(starts_with("ATT_K")) %>%
      colnames(),.)

# 0. Jak se lisi kvalifikovani a nekvalifikovani ucitele + Kubovo deleni

tab_QUAL2 <- data_scores$QUAL2 %>%
  table(useNA = "always")

fit_lm_QUAL2 <- list()

data_use_QUAL2 <- data_scores %>%
  filter(QUAL2 != "Jiné") %>%
  filter(!grepl("maturita|Bc\\.) jin", QUAL2))

data_pred_QUAL2 <- data_use_QUAL2 %>%
  select(QUAL2, AGE) %>%
  mutate(AGE = mean(AGE, na.rm = TRUE)) %>%
  distinct()

for(i in seq_along(score_vars)){
  temp_fit <- lm(paste0(score_vars[i] ," ~ QUAL2 + AGE"), data = data_use_QUAL2)
  
  temp_sum <- summary(temp_fit)
  
  temp_sigs <- temp_sum$coefficients[,4] %>% round(4)
  
  fit_lm_QUAL2[[i]] <- temp_fit %>%
    predict.lm(newdata = data_pred_QUAL2, se.fit = TRUE ) %>%
    .[c("fit", "se.fit")] %>%
    as_tibble() %>%
    bind_cols(data_pred_QUAL2) %>%
    mutate(n = tab_QUAL2[QUAL2],
           CIl = fit - (1.96*se.fit),
           CIu = fit + (1.96*se.fit),
           f = score_vars[i],
           p = temp_sigs[paste0("QUAL2", QUAL2)])
}


plt_QUAL <- fit_lm_QUAL2 %>%
  bind_rows() %>%
  mutate(f = gsub("ATT", "Postoje:\n", f) %>%
           gsub("EFF", "Sebehodnocení:\n", .) %>%
           gsub("_K", "Komunita ", .),
         f = ordered(f, unique(f))) %>%
  mutate(QUAL2 = str_wrap(QUAL2, 50),
         Signifikance = case_when(is.na(p) ~ "Referenční hodnota",
                       p > .05 ~ "p > .05",
                       p > .01 ~ "p < .05",
                       p > .001 ~ "p < .01",
                       p < .001 ~ "p < .001"
                       ) %>%
           ordered(c("Referenční hodnota", 
                     "p > .05",
                     "p < .05",
                     "p < .01",
                     "p < .001"))) %>%
  ggplot(aes(x = QUAL2, y = fit, ymin = CIl, ymax = CIu, color = Signifikance)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_pointrange() +
  facet_wrap(~f) +
  coord_flip() +
  scale_color_manual(values = c("grey1", 
                                  "darkblue", 
                                  "orange1", 
                                  "orange3", 
                                  "red4")) +
  labs(y = "Predikovaná hodnota skóru (M = 0, SD = 1), očištěná od efektu věku\nPredikce a 95% interval spolehlivosti", 
       x = "Kvalifikace",
       color = "Statistická významnost\npříslušného koeficientu") +
  theme(legend.position = "bottom")

ggsave("outputs/code/1_1_scores_groups/plt_QUAL.png", plt_QUAL, height = 12, width = 12)
ggsave("outputs/code/0_3_save/report/99_plt_QUAL.png", plt_QUAL, height = 12, width = 12)


# 1. Jak se lisi kvalifikovani a nekvalifikovani ucitele + Kubovo deleni


tab_UNIS <- data_scores$IDuni %>%
  table(useNA = "always")

UNIS <- tab_UNIS %>%
  sort(decreasing = TRUE) %>%
  names() %>%
  na.omit()

uni_list <- list()

for(j in 1:5){

  fit_lm_UNIS1 <- list()
  
  data_use_UNIS1 <- data_scores %>%
    filter(IDuni %in% UNIS) %>%
    mutate(UNIS1 = ifelse(IDuni == UNIS[j], paste0("2. ", UNIS[j]), "1. Ostatní univerzity"))
  
  data_pred_UNIS1 <- data_use_UNIS1 %>%
    select(UNIS1, AGE, QUAL2) %>%
    mutate(AGE = mean(AGE, na.rm = TRUE)) %>%
    distinct()
  
  for(i in seq_along(score_vars)){
    temp_fit <- lm(paste0(score_vars[i] ," ~ UNIS1 + QUAL2 + AGE"), data = data_use_UNIS1)
    
    temp_sum <- summary(temp_fit)
    
    temp_sigs <- temp_sum$coefficients[,4] %>% round(4)
    
    fit_lm_UNIS1[[i]] <- temp_fit %>%
      predict.lm(newdata = data_pred_UNIS1, se.fit = TRUE ) %>%
      .[c("fit", "se.fit")] %>%
      as_tibble() %>%
      bind_cols(data_pred_UNIS1) %>%
      mutate(n = tab_UNIS[UNIS1],
             CIl = fit - (1.96*se.fit),
             CIu = fit + (1.96*se.fit),
             f = score_vars[i],
             p = temp_sigs[paste0("UNIS1", UNIS1)])
  }
  
  
  uni_list[[j]] <- fit_lm_UNIS1 %>%
    bind_rows() %>%
    mutate(f = gsub("ATT", "Postoje:\n", f) %>%
             gsub("EFF", "Sebehodnocení:\n", .) %>%
             gsub("_K", "Komunita ", .),
           f = ordered(f, unique(f))) %>%
    mutate(UNIS1 = str_wrap(UNIS1, 50),
           Signifikance = case_when(is.na(p) ~ "Referenční hodnota",
                                    p > .05 ~ "p > .05",
                                    p > .01 ~ "p < .05",
                                    p > .001 ~ "p < .01",
                                    p < .001 ~ "p < .001"
           ) %>%
             ordered(c("Referenční hodnota", 
                       "p > .05",
                       "p < .05",
                       "p < .01",
                       "p < .001"))) %>%
    ggplot(aes(x = UNIS1, y = fit, ymin = CIl, ymax = CIu, color = Signifikance, pch = QUAL2, group = QUAL2)) +
    geom_hline(yintercept = 0, lty = 2) +
    geom_pointrange(position = position_dodge(width = .5)) +
    facet_wrap(~f) +
    coord_flip() +
    scale_color_manual(values = c("grey1", 
                                  "darkblue", 
                                  "orange1", 
                                  "orange3", 
                                  "red4")) +
    labs(y = "Predikovaná hodnota skóru (M = 0, SD = 1), očištěná od efektu věku\nPredikce a 95% interval spolehlivosti", 
         x = "Pracoviště",
         pch = "Kvalifikace",
         color = "Statistická významnost\npříslušného koeficientu") +
    theme(legend.position = "bottom", legend.direction = "vertical")
}

ggsave("outputs/code/1_1_scores_groups/plt_UPOL.png", uni_list[[1]], height = 12, width = 12)
ggsave("outputs/code/1_1_scores_groups/plt_MUNI.png", uni_list[[2]], height = 12, width = 12)
