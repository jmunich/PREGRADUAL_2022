library("tidyverse")

data_raw <- read_csv("data/ZU_2022_07_04.csv", lazy = FALSE)

varnames <- names(data_raw) %>%
  gsub("([^\\.]+)\\.(.+)", "\\1", .)

varlabs_1 <- names(data_raw) %>%
  gsub("([^\\.]+)\\. (.+)", "\\2", .)

varlabs_2 <- varlabs_1 %>%
  gsub("(.+)\\[(.+)\\]", "\\2", .)

data_use <- data_raw %>%
  `names<-`(varnames)

# nechat lidi, co to vyplnili k subjektivní připravenosti (strana 1 dotazníku)

filt_1_selfeff <- data_use$lastpage >= 2

# podívat se na intraindividuální rozptyl – v rámci lidí

vargroups <- c("SELFEFF",
               "TEACHINGATT",
               "PUPILMIND")

var_vars <- list()

for(i in seq_along(vargroups)){
  
var_vars[[i]] <- data_use %>%
  select(starts_with(vargroups[i])) %>%
  apply(MARGIN = 1, function(x){length(na.omit(unique(x)))})

}

filt_2_var <- var_vars[[1]] > 1 & var_vars[[2]] > 1 & var_vars[[3]] > 1

# míň než 50% - celý oddíl NA’s

na_vars <- list()
length_vars <- list()

for(i in 1:3){
  
  na_vars[[i]] <- data_use %>%
    select(starts_with(vargroups[i])) %>%
    mutate_all(is.na) %>%
    apply(MARGIN = 1, function(x){sum(x)})
  
  length_vars[[i]] <- data_use %>%
    select(starts_with(vargroups[i])) %>%
    apply(MARGIN = 1, function(x){length(x)})
  
}

filt_3_na <- (((rowSums(as.data.frame(na_vars))/rowSums(as.data.frame(length_vars))) < .5) & data_use$lastpage >= 4)|(data_use$lastpage < 4)

# čas na vyplnění dotazníku per sekce – udělat pro každou sekci, vykopnout lidi, kterým to trvalo nějaký meaningful time na položku per sekce dotazníku, 1s na položku

times <-  data_use %>%
  mutate(t_diff = difftime(datestamp, startdate, units = "min") %>% as.numeric()) %>%
  select(t_diff) %>%
  unlist()

filt_4_time <- ((times > 5) & data_use$lastpage >= 4)|(data_use$lastpage < 4)

# identifikátory fakulty – necháváme všechno za lomítkem, Jirka udělá kontingenční tabulku na vyplněnost, necháváme všechny, co mohou být meaningful, čísla s jedním výskytem apod. hážeme pryč_ vyhazujeme Robonosovu univerzitu

filt_5_fac <- data_use$IDfac %>% is.na() %>% `!`()

filt_5_uni <- data_use$IDuni %>% is.na() %>% `!`()

# rok: jsou tam nějací lidi, co zahájili studium v roce 2010, NA nad rámec nevyplnění první strany letí taky

filt_6_year <- data_use$YEAR %>% is.na() %>% `!`()

# stupeň studia – vyřadit NA taky

filt_7_level <- data_use$LEVEL %>% is.na() %>% `!`()

filt_RESPs <- filt_1_selfeff & filt_2_var & filt_3_na & filt_4_time
filt_IDs <- filt_5_fac  & filt_6_year & filt_7_level
filt_IDs_fac <- filt_5_fac
filt_IDs_uni <- filt_5_uni
