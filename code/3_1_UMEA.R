library("tidyverse")
library("readxl")
library("lubridate")
library("mice")
library("lavaan")


# Setup -------------------------------------------------------------------


if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")} else {
    Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")}


# Function to pivot graph layout

pivot_graph <- function(layout, # Original matrix layout 
                        vars, # A numerical vector of length 2, indicating rows of the layout matrix representing nodes that will be aligned to a horizontal axis
                        rescale = TRUE){ 
  
  temp_layout <- layout
  
  axis <- temp_layout[vars,]
  
  axis_scaled <- t(t(axis) - colMeans(axis))
  
  ang <- atan(axis_scaled[1,2]/axis_scaled[1,1])
  
  piv_mat <- c(cos(ang), -sin(ang), 
               sin(ang), cos(ang)) %>%
    matrix(ncol = 2, byrow = TRUE)
  
  piv_layout <- (t(t(temp_layout) - colMeans(axis)) %*% piv_mat)
  
  if(rescale){
    piv_layout <- t(t(piv_layout) - colMeans(piv_layout))
    
    piv_layout[,1] <- (piv_layout[,1] - min(piv_layout))/(max(piv_layout) - 
                                                            min(piv_layout)) * 2 - 1
    piv_layout[,2] <- (piv_layout[,2] - min(piv_layout))/(max(piv_layout) - 
                                                            min(piv_layout)) * 2 - 1
    
  }
  
  return(piv_layout)
  
}


# Analysis ----------------------------------------------------------------

U_kontrola_p <- readxl::read_excel("data/2022-09-08/Kontrola_podzim.xlsx", sheet = 1)
U_kontrola_j <- readxl::read_excel("data/2022-09-08/Kontrola_jaro.xlsx", sheet = 2)

U_data_podzim <- read_csv("data/2022-08-18/Projekt/Pre_post/ucitelsky_dotaznik_podzim_projekt_433129.csv") %>%
  mutate(sada = "podzim")
U_data_jaro <- read_csv("data/2022-08-18/Projekt/Pre_post/ucitelsky_dotaznik_jaro_projekt_669419.csv") %>%
  mutate(sada = "jaro")
U_data_podzim_2 <- read_csv("data/2022-08-18/Projekt/Pre_post/ucitelsky_dotaznik_jaro_kontrolni_593313.csv") %>%
  mutate(sada = "podzim")


U_data_rican <- read_excel("data/2022-08-18/Rican/BHQ.xlsx")

U_data_list <- list(U_data_podzim,
                    U_data_jaro)

U_labs_1 <- U_data_list %>%
  lapply(function(x){
    names(x) %>%
      gsub("^([^\\.]+)\\. (.+)", "\\2", .) %>%
      `names<-`(names(x) %>%
                  gsub("^([^\\.]+)\\. (.+)", "\\1", .) %>%
                  gsub("\\[", "_", .) %>%
                  gsub("\\]", "", .))
  })

U_labs_2_1 <- U_labs_1 %>%
  lapply(function(x){
    x %>%
      gsub("(.+) \\[(.+)\\]", "\\1", .) %>%
      `names<-`(names(x))
  })

U_labs_2_2 <- U_labs_1 %>%
  lapply(function(x){
    x %>%
      gsub(".+\\[(.+)\\]", "\\1", .) %>%
      `names<-`(names(x))
  })

U_data_use <- U_data_list %>%
  lapply(function(x){
    x %>%
      `names<-`(names(x) %>%
                  gsub("^([^\\.]+)\\. (.+)", "\\1", .) %>%
                  gsub("\\[", "_", .) %>%
                  gsub("\\]", "", .)) %>%
      mutate(rok = year(startdate)) %>%
      filter((rok == "2021" & sada == "podzim")|(rok == "2022" & sada == "jaro")) %>%
      #      filter(!is.na(IDteach)) %>%
      mutate(IDteach = gsub("O|o", "0", IDteach),
             IDcom = substr(IDteach, 1, 5)) %>%
      group_by(IDteach) %>%
      arrange(-as.numeric(startdate)) %>%
      filter(1:n() == 1) %>%
      ungroup()
  })

U_levels_bhq <- c("Silně nesouhlasím",         
                  "Nesouhlasím",               
                  "Spíše nesouhlasím",       
                  "Ani nesouhlas ani souhlas",
                  "Spíše souhlasím",           
                  "Souhlasím",                 
                  "Silně souhlasím")           

U_levels_learninggoals <- c("Zcela nedůležité",
                            "Spíše nedůležité", 
                            "Důležité",         
                            "Velmi důležité")   

Ukp <- U_kontrola_p %>%
  filter(grepl("nový", stav)) %>%
  select(`ID podle archu`)

Ukj <- U_kontrola_j %>%
  filter(grepl("nový", stav)) %>%
  select(`ID podle archu`)

U_data_use[[1]] <- U_data_use[[1]] %>%
  filter(!IDteach %in% Ukp$`ID podle archu`)

U_data_use[[2]] <- U_data_use[[2]] %>%
  filter(!IDteach %in% Ukj$`ID podle archu`)


data_bhq <- U_data_use %>%
  lapply(function(x){
    x %>%
      select(IDcom, sada, starts_with("bhq"), starts_with("learninggoals"))
  }) %>%
  bind_rows() %>%
  bind_rows(U_data_podzim_2 %>%
              filter(!is.na(`IDteach. Zadejte identifikační číslo učitele`)) %>%
              select(`IDteach. Zadejte identifikační číslo učitele`, sada, starts_with("bhq"), starts_with("learning")) %>%
              `names<-`(colnames(data_bhq))) %>%
  filter(rowSums(select(., starts_with("bhq")) %>% is.na()) < 5) %>%
  mutate_at(vars(starts_with("bhq")), ordered, U_levels_bhq) %>%
  mutate_at(vars(starts_with("learninggoals")), ordered, U_levels_learninggoals) %>%
  select(-starts_with("learninggoals"))





U_cmt_bhq_1 <- data_bhq %>%
  filter(sada == "podzim") %>%
  select(-c(IDcom, sada)) %>%
  mutate_all(as.numeric)

U_cmt_bhq_2 <- data_bhq %>%
  filter(sada == "jaro") %>%
  select(-c(IDcom, sada)) %>%
  mutate_all(as.numeric)

model_ng <- ggm(data_bhq %>% mutate_at(-c(1,2), as.numeric), 
                vars = colnames(data_bhq)[-c(1,2)], 
                omega = "full")

model_g <- ggm(data_bhq %>% mutate_at(-c(1,2), as.numeric), 
               vars = colnames(data_bhq)[-c(1,2)], 
               omega = "full",
               groups = "sada")

model_ng_fit <- model_ng %>%
  runmodel() %>% 
  stepup() %>%
  prune(alpha = 0.05, recursive = TRUE)

model_g_fit <- model_ng %>%
  runmodel() %>% 
  stepup() %>%
  prune(alpha = 0.05, recursive = TRUE)

adjacency <- 1*(getmatrix(model_ng_fit, "omega")!=0)


groupmodel <- ggm(data_bhq %>% mutate_at(-c(1,2), as.numeric), 
                  vars = colnames(data_bhq)[-c(1,2)], 
                  omega = adjacency, 
                  groups = "sada") %>% runmodel

groupmodel_2 <- groupmodel %>% groupequal("omega") %>% runmodel

compare(groupmodel, groupmodel_2)
fit(groupmodel)
group_list <- list(Kriterialismus = c(1, 3, 7, 11, 13, 15, 18, 21),
                   Subjektivismus = c(2, 4, 6, 8, 10, 12, 14, 17, 22),
                   Objektivismus = c(5, 9, 16, 19, 20),
                   `Cíle výuky` = c(23:30))

Mat_p <- getmatrix(groupmodel, "omega")[[1]]
Mat_j <- getmatrix(groupmodel, "omega")[[2]]

umax <- getmatrix(groupmodel, "omega")[[1]] %>% max()
umin <- getmatrix(groupmodel, "omega")[[1]] %>% abs() %>% min()

Avlo <- qgraph::averageLayout(Mat_p, Mat_j)


png("outputs/BHQ.png", units = "in", width = 16, height = 12, res = 500)

par(mfrow = c(2,1))
Mat_p %>% 
  qgraph::qgraph(groups = group_list, repulsion = 1.2, layout = Avlo, palette = "pastel", vsize = 3.5, rescale = FALSE,
                 nodeNames = U_labs_2_2[[1]][colnames(data_bhq)[-c(1,2)]], 
                 legend.cex = .35,
                 GLratio = 1,
                 maximum = umax, minimum = umin, title = "Podzim", details = TRUE)

Mat_j %>% 
  qgraph::qgraph(groups = group_list, repulsion = 1.2, layout = Avlo, palette = "pastel", vsize = 3.5, rescale = FALSE,
                 nodeNames = U_labs_2_2[[1]][colnames(data_bhq)[-c(1,2)]],
                 legend.cex = .35,
                 GLratio = 1,
                 maximum = umax, minimum = umin, title = "Jaro", details = TRUE)

dev.off()








U_g_1 <- U_cmt_bhq_1 %>% 
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  qgraph::qgraph(layout = "spring", 
                 graph = "glasso",
                 sampleSize = nrow(U_cmt_bhq_1),
                 gamma = .2,
                 nodeNames = U_labs_2_2[[1]][colnames(.)],
                 legend.cex = .35,
                 groups = group_list[-4],
                 palette = "pastel",
                 GLratio = 1,
                 vsize = 3,
                 labels = 1:ncol(.),
                 shape = c("square", "circle")[grepl("bhq", colnames(.)) + 1])

U_g_2 <- U_cmt_bhq_2 %>% 
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  qgraph::qgraph(layout = "spring", 
                 graph = "glasso",
                 sampleSize = nrow(U_cmt_bhq_2),
                 gamma = .2,
                 nodeNames = U_labs_2_2[[1]][colnames(.)],
                 legend.cex = .35,
                 groups = group_list[-4],
                 palette = "pastel",
                 GLratio = 1,
                 vsize = 3,
                 labels = 1:ncol(.),
                 shape = c("square", "circle")[grepl("bhq", colnames(.)) + 1])


U_avl <- qgraph::averageLayout(U_g_1, U_g_2,repulsion = 1.05) %>% 
  pivot_graph(c(10,5), rescale = TRUE)


svg("outputs/plots/BHQ_tri.svg", width = 18, height = 25)
par(mfrow = c(3,1))
U_g_ri <- U_data_rican %>%
  select(starts_with("Otázka 9")) %>%
  mutate_all(parse_number) %>%
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  qgraph::qgraph(layout = U_avl[-c(23:30),], 
                 graph = "glasso",
                 sampleSize = nrow(U_data_rican),
                 gamma = .2,
                 nodeNames = U_labs_2_2[[1]][grepl("bhq", names(U_labs_2_2[[1]]))],
                 esize = 10,
                 legend.cex = .5,
                 groups = group_list[-4],
                 color = U_g_1$graphAttributes$Nodes$color[-c(23:30)] %>% unique(),
                 GLratio = 1,
                 vsize = 3,
                 labels = 1:ncol(.),
                 title = "Říčan (under review)",
                 rescale = FALSE,
                 maximum = 1)

U_g_po <- U_cmt_bhq_1 %>% 
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  qgraph::qgraph(layout = U_avl, 
                 graph = "glasso",
                 sampleSize = nrow(U_cmt_bhq_1),
                 gamma = .2,
                 nodeNames = U_labs_2_2[[1]][colnames(.)],
                 esize = 10,
                 legend.cex = .5,
                 groups = group_list,
                 palette = "pastel",
                 GLratio = 1,
                 vsize = 3,
                 labels = 1:ncol(.),
                 shape = c("square", "circle")[grepl("bhq", colnames(.)) + 1],
                 title = "Podzim",
                 rescale = FALSE,
                 maximum = 1)

U_g_ja <- U_cmt_bhq_2 %>% 
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  qgraph::qgraph(layout = U_avl, 
                 graph = "glasso",
                 sampleSize = nrow(U_cmt_bhq_2),
                 gamma = .2,
                 nodeNames = U_labs_2_2[[1]][colnames(.)],
                 esize = 10,
                 legend.cex = .5,
                 groups = group_list,
                 palette = "pastel",
                 GLratio = 1,
                 vsize = 3,
                 labels = 1:ncol(.),
                 shape = c("square", "circle")[grepl("bhq", colnames(.)) + 1],
                 title = "Jaro",
                 rescale = FALSE,
                 maximum = 1)



dev.off()

png("outputs/plots/BHQ_tri.png", width = 19, height = 25, units = "in", res = 360)
par(mfrow = c(3,1))
U_g_ri <- U_data_rican %>%
  select(starts_with("Otázka 9")) %>%
  mutate_all(parse_number) %>%
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  qgraph::qgraph(layout = U_avl[-c(23:30),], 
                 graph = "glasso",
                 sampleSize = nrow(U_data_rican),
                 gamma = .2,
                 nodeNames = U_labs_2_2[[1]][grepl("bhq", names(U_labs_2_2[[1]]))],
                 esize = 10,
                 legend.cex = .5,
                 groups = group_list[-4],
                 color = U_g_1$graphAttributes$Nodes$color[-c(23:30)] %>% unique(),
                 GLratio = 1,
                 vsize = 3,
                 labels = 1:ncol(.),
                 title = "Říčan (under review)",
                 rescale = FALSE,
                 maximum = 1)

U_g_po <- U_cmt_bhq_1 %>% 
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  qgraph::qgraph(layout = U_avl, 
                 graph = "glasso",
                 sampleSize = nrow(U_cmt_bhq_1),
                 gamma = .2,
                 nodeNames = U_labs_2_2[[1]][colnames(.)],
                 esize = 10,
                 legend.cex = .5,
                 groups = group_list[-4],
                 palette = "pastel",
                 GLratio = 1,
                 vsize = 3,
                 labels = 1:ncol(.),
                 shape = c("square", "circle")[grepl("bhq", colnames(.)) + 1],
                 title = "Podzim",
                 rescale = FALSE,
                 maximum = 1)

U_g_ja <- U_cmt_bhq_2 %>% 
  cor(method = "spearman", use = "pairwise.complete.obs") %>%
  qgraph::qgraph(layout = U_avl, 
                 graph = "glasso",
                 sampleSize = nrow(U_cmt_bhq_2),
                 gamma = .2,
                 nodeNames = U_labs_2_2[[1]][colnames(.)],
                 esize = 10,
                 legend.cex = .5,
                 groups = group_list[-4],
                 palette = "pastel",
                 GLratio = 1,
                 vsize = 3,
                 labels = 1:ncol(.),
                 shape = c("square", "circle")[grepl("bhq", colnames(.)) + 1],
                 title = "Jaro",
                 rescale = FALSE,
                 maximum = 1)



dev.off()
library("qgraph")


U_g_po %>%
  getWmat() %>%
  qgraph::ggmFit(pcor = ., 
                 covMat = U_cmt_bhq_1 %>% 
                   cor(method = "spearman", 
                       use = "pairwise.complete.obs"), 
                 sampleSize = sum(data_bhq$sada == "podzim")) %>%
  filter(measure %in% c("df", "chisq", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "tli", "cfi"))

U_g_ja %>%
  getWmat() %>%
  qgraph::ggmFit(pcor = ., 
                 covMat = U_cmt_bhq_2 %>% 
                   cor(method = "spearman", 
                       use = "pairwise.complete.obs"), 
                 sampleSize = sum(data_bhq$sada == "jaro"))

(getWmat(U_g_ja) - getWmat(U_g_po))[1,] %>% sort()

U_nct <- NetworkComparisonTest::NCT(data1 = data_bhq %>%
                                        filter(sada == "podzim") %>%
                                        select(-c(IDcom, sada)) %>%
                                        mutate_all(as.numeric),
                                      data2 = data_bhq %>%
                                        filter(sada == "jaro") %>%
                                        select(-c(IDcom, sada)) %>%
                                        mutate_all(as.numeric),
                                      use = "pariwise.complete.obs",
                                      make.positive.definite = TRUE,
                                      estimator = function(x){x %>%
                                          cor_auto(detectOrdinal = FALSE) %>%
                                          corpcor::cor2pcor()},
                                      test.edges = TRUE,
                                      p.adjust.methods = "BH",
                                      paired = FALSE,
                                      it = 1000)




U_impute <- mice::mice(data_bhq %>% select(starts_with("bhq")))
U_complete <- mice::complete(U_impute) %>%
  as_tibble() %>%
  `names<-`(paste0("BHQ", 1:ncol(.)))

U_data_cfa <- data_bhq %>% 
  select(starts_with("bhq")) %>%
  `names<-`(paste0("BHQ", 1:ncol(.))) %>%
  mutate_all(as.numeric) %>%
  mutate(sada = data_bhq$sada,
         IDcom = data_bhq$IDcom)

U_model <- "Kriterialismus =~ BHQ1 + BHQ3 + BHQ7 + BHQ11 + BHQ13 + BHQ15 + BHQ18 + BHQ21
Subjektivismus =~ BHQ2 + BHQ4 + BHQ6 + BHQ8 + BHQ10 + BHQ12 + BHQ14 + BHQ17 + BHQ22
Objektivismus =~ BHQ5 + BHQ9 + BHQ16 + BHQ19 + BHQ20"

U_model_1 <- "Kriterialismus =~ BHQ1 + BHQ3 + BHQ7 + BHQ11 + BHQ13 + BHQ15 + BHQ18 + BHQ21
              Subjektivismus =~ BHQ2 + BHQ4 + BHQ6 + BHQ8 + BHQ10 + BHQ12 + BHQ14 + BHQ17 + BHQ22
              Kriterialismus ~ 0
              Kriterialismus ~~ 1*Kriterialismus
              Subjektivismus ~ 0
              Subjektivismus ~~ 1*Subjektivismus
               
"


U_fit_0 <- cfa(model = U_model_1,
               data = U_data_cfa, 
               meanstructure = TRUE,
               group.equal = c("loadings", 
                               "composite.loadings", 
                               "intercepts", 
                               "means", 
                               "thresholds", 
                               "regressions", 
                               "residuals",
                               "residual.covariances",
                               "lv.variances",
                               "lv.covariances"),
               group = "sada")

U_fit_1 <- cfa(model = U_model_1,
               data = U_data_cfa, 
               meanstructure = TRUE,
               group.equal = c("loadings", 
                               "composite.loadings", 
                               "intercepts", 
                               "means", 
                               "thresholds", 
                               "regressions", 
                               "residuals",
                               "residual.covariances",
                               "lv.variances"),
               group = "sada")

anova(U_fit_0, U_fit_1)

fitMeasures(U_fit_0)

lavInspect(U_fit_0_1, "cov.ov")
lavInspect(U_fit_1_1, "cov.ov")






U_fit_2 <- cfa(model =paste0(U_model_1, "\nKriterialismus ~ 1\nSubjektivismus ~ 1"),
               data = U_data_cfa, 
               meanstructure = TRUE,
               group.equal = c("loadings", "composite.loadings", "intercepts",
                               "thresholds", "residuals", "residual.covariances"),
               group = "sada")


U_model_cov <- "
B3 =~ 1*BHQ3
B21 =~ 1*BHQ21
B22 =~ 1*BHQ22

BHQ3 ~~ 0*BHQ3
BHQ21 ~~ 0*BHQ21
BHQ22 ~~ 0*BHQ22
"

lavInspect(U_fit_covs_ng, "cov.lv")
lavInspect(U_fit_covs_g, "cov.lv")
lavInspect(U_fit_1, "cor.lv")

fitMeasures(U_fit_covs_ng)
U_fit_covs_ng <- cfa(model = U_model_cov,
               data = U_data_cfa, 
               meanstructure = TRUE,
               group.equal = c("loadings", 
                               "composite.loadings", 
                               "intercepts", "means", 
                               "thresholds", 
                               "regressions", 
                               "residuals", 
                               "residual.covariances", 
                               "lv.variances",
                               "lv.covariances"),
               group = "sada")

U_fit_covs_g <- cfa(model = U_model_cov,
               data = U_data_cfa, 
               meanstructure = TRUE,
               group.equal = c("loadings", 
                               "composite.loadings", 
                               "intercepts", "means", 
                               "thresholds", 
                               "regressions", 
                               "residuals", 
                               "residual.covariances"),
               group = "sada")



covs <- lavInspect(U_fit_covs_g, "cor.lv")

covs$jaro - covs$podzim %>% qgraph()

anova(U_fit_covs_g, U_fit_covs_ng)

fitMeasures(U_fit_0_1, "std")

png("outputs/BHQ_2.png",width = 6.77, units = "in", res = 360, height = 5)
par(mfrow = c(1,2))

semPlot::semPaths(U_fit_covs_g,
                  whatLabels =  "std",intStyle = "single", 
                  layout = "tree",ask = FALSE, title = FALSE, curve = 2, node.height = .1)

dev.off()









U_scores %>%
  as_tibble() %>%
  mutate_all(as.numeric) %>%
  bind_cols(U_data_cfa) %>%
  mutate(sada = ordered(sada, c("podzim", "jaro"))) %>%
  group_by(IDcom) %>%
  filter(n()>1) %>%
  group_by(IDcom, sada) %>%
  summarise(val = mean(Subjektivismus), size = n()) %>%
  arrange(sada) %>%
  mutate(slope = val[2] - val[1]) %>%
  ungroup() %>%
  mutate(size = ordered(size)) %>%
  select(IDcom,
         slope,
         sada,
         val,
         size) %>%
  ggplot(aes(x = sada, 
             y = val,
             group = IDcom)) +
  geom_point(size = 0) +
  geom_smooth(aes(x = as.numeric(sada), color = slope),
              method = "lm", se = FALSE, lwd = 2) +
  geom_point(aes(fill = val, size = size), pch = 21) +
  scale_color_gradient2(mid = "grey") +
  scale_fill_gradient2(mid = "grey")

U_scores %>%
  as_tibble() %>%
  mutate_all(as.numeric) %>%
  bind_cols(U_data_cfa) %>%
  mutate(sada = ordered(sada, c("podzim", "jaro"))) %>%
  group_by(IDcom) %>%
  filter(n()>1) %>%
  group_by(IDcom, sada) %>%
  summarise(val = mean(Kriterialismus), size = n()) %>%
  arrange(sada) %>%
  mutate(slope = val[2] - val[1]) %>%
  ungroup() %>%
  mutate(size = ordered(size)) %>%
  select(IDcom,
         slope,
         sada,
         val,
         size) %>%
  ggplot(aes(x = sada, 
             y = val,
             group = IDcom)) +
  geom_point(size = 0) +
  geom_smooth(aes(x = as.numeric(sada), color = slope),
              method = "lm", se = FALSE, lwd = 2) +
  geom_point(aes(fill = val, size = size), pch = 21) +
  scale_color_gradient2(mid = "grey") +
  scale_fill_gradient2(mid = "grey")


dat_paired <- U_scores %>%
  as_tibble() %>%
  mutate_all(as.numeric) %>%
  bind_cols(U_data_cfa) %>%
  mutate(sada = ordered(sada, c("podzim", "jaro"))) %>%
  group_by(IDcom) %>%
  filter(n()>1) %>%
  ungroup()

lmefit <- lme4::lmer(Kriterialismus ~ (1 + sada|IDcom), dat_paired, REML = TRUE)

summary(lmefit)
