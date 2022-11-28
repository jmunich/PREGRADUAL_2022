source("code/0_0_1_packages.R")
source("code/0_1_1_clean_ZU.R", encoding = "UTF-8")

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

AU_data_raw <- read_csv("data/AU_2022_09_12.csv", lazy = FALSE)

AU_varnames <- names(AU_data_raw) %>%
  gsub("([^\\.]+)\\.(.+)", "\\1", .)

AU_varlabs_1 <- names(AU_data_raw) %>%
  gsub("([^\\.]+)\\. (.+)", "\\2", .)

AU_varlabs_2 <- AU_varlabs_1 %>%
  gsub("(.+)\\[(.+)\\]", "\\2", .)

AU_data_use <- AU_data_raw %>%
  `names<-`(AU_varnames)

# nechat lidi, co to vyplnili k subjektivní připravenosti (strana 1 dotazníku)

AU_filt_1_selfeff <- AU_data_use$lastpage >= 2

# podívat se na intraindividuální rozptyl – v rámci lidí

AU_vargroups <- c("SELFEFF",
               "TEACHINGATT",
               "PUPILMIND")

AU_var_vars <- list()

for(i in seq_along(AU_vargroups)){
  
  AU_var_vars[[i]] <- AU_data_use %>%
    select(starts_with(AU_vargroups[i])) %>%
    apply(MARGIN = 1, function(x){length(na.omit(unique(x)))})
  
}

AU_filt_2_var <- AU_var_vars[[1]] > 1 | AU_var_vars[[2]] > 1 | AU_var_vars[[3]] > 1

# míň než 50% - celý oddíl NA’s

AU_na_vars <- list()
AU_length_vars <- list()

for(i in 1:3){
  
  AU_na_vars[[i]] <- AU_data_use %>%
    select(starts_with(AU_vargroups[i])) %>%
    mutate_all(is.na) %>%
    apply(MARGIN = 1, function(x){sum(x)})
  
  AU_length_vars[[i]] <- AU_data_use %>%
    select(starts_with(AU_vargroups[i])) %>%
    apply(MARGIN = 1, function(x){length(x)})
  
}

AU_filt_3_na <- (((rowSums(as.data.frame(AU_na_vars))/rowSums(as.data.frame(AU_length_vars))) < .5) & AU_data_use$lastpage >= 4)|(AU_data_use$lastpage < 4)

# čas na vyplnění dotazníku per sekce – udělat pro každou sekci, vykopnout lidi, kterým to trvalo nějaký meaningful time na položku per sekce dotazníku, 1s na položku

AU_times <- AU_data_use %>%
  mutate(t_diff = difftime(datestamp, startdate, units = "min") %>% as.numeric()) %>%
  select(t_diff) %>%
  unlist()

AU_filt_4_time <- ((AU_times > 5) & AU_data_use$lastpage >= 4)|(AU_data_use$lastpage < 4)

# identifikátory fakulty – necháváme všechno za lomítkem, Jirka udělá kontingenční tabulku na vyplněnost, necháváme všechny, co mohou být meaningful, čísla s jedním výskytem apod. hážeme pryč_ vyhazujeme Robonosovu univerzitu

AU_filt_5_fac <- AU_data_use$IDfac. %>% is.na() %>% `!`()

# rok: jsou tam nějací lidi, co zahájili studium v roce 2010, NA nad rámec nevyplnění první strany letí taky

AU_filt_6_year <- AU_data_use$YEAR %>% is.na() %>% `!`()

# stupeň studia – vyřadit NA taky

AU_filt_7_level <- AU_data_use$LEVEL %>% is.na() %>% `!`()

AU_filt_RESPs <- AU_filt_1_selfeff & AU_filt_2_var & AU_filt_3_na & AU_filt_4_time
AU_filt_IDs <- AU_filt_5_fac  & AU_filt_6_year & AU_filt_7_level
AU_filt_IDs_fac <- AU_filt_5_fac



# rematch varnames --------------------------------------------------------

pattern <- "ATT\\[|SELFEFF\\[|PUPILMIND\\["

EFF_vlab_ref <- varlabs_2 %>%
  `names<-`(varnames) %>%
  gsub("\\/ka|\\/a", "", .)

EFFAU_vlab_0 <- AU_varlabs_2 %>%
  `names<-`(AU_varnames)

vec_AU <- EFFAU_vlab_0[grepl(pattern, names(EFFAU_vlab_0))]
vec_ZU <- EFF_vlab_ref[grepl(pattern, names(EFF_vlab_ref))]

dmat <- stringdist::stringdistmatrix(vec_AU, vec_ZU)

match_val <- dmat %>% 
  apply(MARGIN = 1, FUN = min)

matches <- dmat %>% 
  apply(MARGIN = 1, FUN = which.min)

match_tib <- tibble(v1 = vec_AU, 
       l1 = names(vec_AU),
       matches,
       match_val,
       m1 = vec_ZU[matches], 
       ml1 = names(vec_ZU)[matches]) %>%
  arrange(-match_val) %>%
  filter(substr(l1, 1, 4) == substr(ml1, 1, 4)) %>%
  filter(match_val < 22) %>%
  select(l1, ml1) %>%
  mutate(ml1_mod = gsub("\\[", "_", ml1) %>%
           gsub("\\]", "", .))

unmatched_tib <- tibble(v1 = vec_AU, 
       l1 = names(vec_AU),
       matches,
       match_val,
       m1 = vec_ZU[matches], 
       ml1 = names(vec_ZU)[matches]) %>%
  arrange(-match_val) %>%
  filter(substr(l1, 1, 4) == substr(ml1, 1, 4)) %>%
  filter(match_val > 21) %>%
  select(l1, ml1)
