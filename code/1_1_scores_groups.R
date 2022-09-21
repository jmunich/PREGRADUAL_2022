library("tidyverse")
readMSMT::set_cz()

data_0 <- read_csv("outputs/code/0_2_1_cluster_EFF_ZU/dat/1_EFF_data_0.csv")
data_EFF <- read_csv("outputs/code/0_2_1_cluster_EFF_ZU/dat/2_EFF_D_scores.csv")
data_ATT <- read_csv("outputs/code/0_2_2_cluster_ATT_ZU/dat/2_ATT_D_scores.csv")

qual_ped <- c("Vysokoškolské vzdělání (Mgr.) se zaměřením na učitelství pro 1. stupeň nebo 2. stupeň a/nebo střední školy")                                                                                             

qual_nped <- c("Vysokoškolské vzdělání (Mgr./Ing./MgA.) jiného zaměření")

qual_dps <- c("Vysokoškolské vzdělání (Mgr./Ing./MgA.) jiného zaměření + dokončené Doplňující pedagogické studium (DPS, tzv. „pedagogické minimum“)")

data_use <- data_0 %>%
  bind_cols(data_EFF) %>%
  bind_cols(data_ATT) %>%
  mutate(Fakulta = case_when(IDfac == "Fakulta informatiky" ~ "Jiné",
                             IDfac == "Matematicko-fyzikální fakulta" ~ "Jiné",
                             TRUE ~ IDfac),
         Kvalifikace = case_when(QUAL %in% c(qual_ped, qual_nped, qual_dps) ~ "2. Magisterské",
                                 !is.na(QUAL) ~ "1. Nemagisterské",
                                 TRUE ~ NA_character_)
  )

data_use$TIMEEMPLOYED %>% table()

data_use %>%
  select(TIMEEMPLOYED, matches("EFF_F"), matches("ATT_F")) %>%
  pivot_longer(-TIMEEMPLOYED) %>%
  na.omit() %>%
  ggplot(aes(x = TIMEEMPLOYED, y = value)) +
  geom_boxplot(notch = TRUE) +
  facet_wrap(~name) +
  coord_cartesian(ylim = c(-1,1))


data_use %>%
  select(AGE, matches("EFF_F"), matches("ATT_F")) %>%
  pivot_longer(-AGE) %>%
  group_by(name) %>%
  summarise(cor.p = cor.test(AGE, value, use = "pairwise.complete.obs")$p.value < (12*.05),
            cor = cor.test(AGE, value, use = "pairwise.complete.obs")$estimate,
            R2 = cor^2)
  

plot(uu$AGE, predict(ee))

uu <- data_use %>%
  select(AGE, matches("EFF_F"), matches("ATT_F")) %>%
  na.omit()

ee <- lm(AGE ~ .,
   uu)

  ggplot(aes(x = AGE, y = value)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~name)

data_use %>%
  select(TIMEEMPLOYED, AGE) %>%
  ggplot(aes(y = AGE, x = TIMEEMPLOYED)) +
  geom_boxplot(notch = TRUE) +
  coord_flip()
  
data_medians_fac <- data_use %>%
  select(Fakulta, matches("EFF_F"), matches("ATT_F")) %>%
  filter(Fakulta != "Jiné") %>%
  pivot_longer(-1) %>%
  na.omit() %>%
  group_by(Fakulta, name) %>%
  summarise(value = median(value)) %>%
  ungroup()

data_medians_qual <- data_use %>%
  select(Kvalifikace, matches("EFF_F"), matches("ATT_F")) %>%
  filter(!is.na(Kvalifikace)) %>%
  pivot_longer(-1) %>%
  na.omit() %>%
  group_by(Kvalifikace, name) %>%
  summarise(value = median(value)) %>%
  ungroup()

plt_comp_fac <- data_use %>%
  select(Fakulta, matches("EFF_F"), matches("ATT_F")) %>%
  filter(Fakulta != "Jiné") %>%
  pivot_longer(-1) %>%
  na.omit() %>%
  ggplot(aes(x = Fakulta, fill = Fakulta, y = value)) +
  geom_violin(alpha = .5, width = .75, bw = .2) +
  geom_boxplot(notch = TRUE, outlier.colour = NA, alpha = .5, width = .25, fill = "white") +
  geom_hline(data = data_medians_fac,
             aes(yintercept = value, color = Fakulta), lty = 2) +
  facet_wrap(~name) +
  coord_cartesian(ylim = c(-1.25,1.25)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")


plt_comp_qual <- data_use %>%
  select(Kvalifikace, matches("EFF_F"), matches("ATT_F")) %>%
  pivot_longer(-1) %>%
  na.omit() %>%
  ggplot(aes(x = Kvalifikace, fill = Kvalifikace, y = value)) +
  geom_violin(alpha = .5, width = .75, bw = .2) +
  geom_boxplot(notch = TRUE, outlier.colour = NA, alpha = .5, width = .25, fill = "white") +
  geom_hline(data = data_medians_qual,
             aes(yintercept = value, color = Kvalifikace), lty = 2) +
  facet_wrap(~name) +
  coord_cartesian(ylim = c(-1.25,1.25)) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom")


  