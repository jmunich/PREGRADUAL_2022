source("code/0_0_1_packages.R", encoding = "UTF-8")
source("code/0_0_2_funs.R", encoding = "UTF-8")
source("code/0_1_1_clean_ZU.R", encoding = "UTF-8")

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

W_sample <- data_use %>%
  select(IDuni, IDfac) %>%
  group_by(IDuni, IDfac) %>%
  summarise(cnt_samp = n()) %>%
  na.omit() %>%
  ungroup() %>%
  arrange(-cnt_samp) %>%
  mutate(IDfac = gsub("věděcká", "vědecká", IDfac))

W_data <- read_excel("data/SIMS.xlsx", sheet = "2020", skip = 6)

W_vec_1_ped <- c("B7501", 
                 "B7507", 
                 "F0003", 
                 "F0019", 
                 "F0062", 
                 "N7501", 
                 "N7503",
                 "N7504", 
                 "N7507", 
                 "P7501", 
                 "P7507", 
                 "P7536", 
                 "P0111D190006")

W_vec_2_spec <- c(
  "B7506", 
  "M7506", 
  "N7506", 
  "P7506", 
  "P7511", 
  "P7540")

W_vec_3_physical <- c("B7401")

W_vec_4_misc <- c("B0114A300017",
                  "B7531", 
                  "N0188A280001")

W_list <- list(nonspec = c(W_vec_1_ped, W_vec_3_physical, W_vec_4_misc),
               ped = W_vec_1_ped,
               spec = W_vec_2_spec,
               phys = W_vec_3_physical,
               misc = W_vec_4_misc)

W_cnts <- lapply(W_list,
       function(x){
         W_data %>% 
           filter(grepl(paste0(x, collapse = "|"), `Studijní program`)) %>%
           select(`Student ID`, `Vysoká škola`, Fakulta) %>%
           arrange(Fakulta) %>%
           group_by(`Student ID`, `Vysoká škola`) %>%
           mutate(filt = 1:n()) %>%
           filter(filt == sample(filt, 1)) %>%
           group_by(`Vysoká škola`, Fakulta) %>%
           summarise(cnt = n(), rep = max(filt)) %>%
           ungroup()
         
       })

W_cnts$use <- W_data %>% 
  filter(grepl("^75", `Studijní obor`)) %>%
  filter(!grepl("Speciál|speciál|Andragogika|andragogika|dospěl|Dospěl", `Studijní obor`)) %>%
  select(`Student ID`, `Vysoká škola`, Fakulta) %>%
  arrange(Fakulta) %>%
  group_by(`Student ID`, `Vysoká škola`) %>%
  mutate(filt = 1:n()) %>%
  filter(filt == sample(filt, 1)) %>%
  group_by(`Vysoká škola`, Fakulta) %>%
  summarise(cnt = n(), rep = max(filt)) %>%
  ungroup()

W_key_uni <- c(
  `3100-VŠE` = "Vysoká škola ekonomická v Praze",
  `4100-ČZU` = "Česká zemědělská univerzita v Praze",                                       
  `2100-ČVUT` = "České vysoké učení technické v Praze",                                      
  `1200-JU` = "Jihočeská univerzita v Českých Budějovicích",                               
  `1400-MU` = "Masarykova univerzita",                                                     
  `4300-MENDELU` = "Mendelova univerzita v Brně",                                               
  `1700-OU` = "Ostravská univerzita",                                                      
  `1900-SU` = "Slezská univerzita v Opavě",                                                
  `2400-TUL` = "Technická univerzita v Liberci",                                            
  `1800-UHK` = "Univerzita Hradec Králové",                                                 
  `7600-Univerzita J. A. Komenského Praha` = "Univerzita Jana Amose Komenského Praha s.r.o.",                             
  `1300-UJEP` = "Univerzita Jana Evangelisty Purkyně v Ústí nad Labem",                      
  `1100-UK` = "Univerzita Karlova",                                                        
  `1500-UP` = "Univerzita Palackého v Olomouci",                                           
  `2500-UPa` = "Univerzita Pardubice",                                                      
  `2800-UTB` = "Univerzita Tomáše Bati ve Zlíně",                                           
  `2200-VŠCHT Praha` = "Vysoká škola chemicko-technologická v Praze",                               
  `2300-ZČU` = "Západočeská univerzita v Plzni")

W_key_fac  <- c(
  `11320-Matematicko-fyzikální fakulta` = "Matematicko-fyzikální fakulta",
  `28140-Fakulta aplikované informatiky` = "Fakulta informatiky",  
  `11210-Filozofická fakulta` = "Filozofická fakulta",                                                  
  `13410-Filozofická fakulta` = "Filozofická fakulta",              
  `14210-Filozofická fakulta` = "Filozofická fakulta",                                                  
  `25210-Fakulta filozofická` = "Filozofická fakulta",                                                  
  `12210-Filozofická fakulta` = "Filozofická fakulta",                                                  
  `15210-Filozofická fakulta` = "Filozofická fakulta",                                                  
  `17250-Filozofická fakulta` = "Filozofická fakulta",                                                  
  `28150-Fakulta humanitních studií` = "Filozofická fakulta",                                           
  `11410-Pedagogická fakulta` = "Pedagogická fakulta",                                                  
  `12410-Pedagogická fakulta` = "Pedagogická fakulta",                                                  
  `13430-Pedagogická fakulta` = "Pedagogická fakulta",                                                  
  `14410-Pedagogická fakulta` = "Pedagogická fakulta",                                                  
  `15410-Pedagogická fakulta` = "Pedagogická fakulta",                                                  
  `17450-Pedagogická fakulta` = "Pedagogická fakulta",                                                  
  `18440-Pedagogická fakulta` = "Pedagogická fakulta",                                                  
  `23420-Fakulta pedagogická` = "Pedagogická fakulta",                                                  
  `24510-Fakulta přírodovědně-humanitní a pedagogická` = "Pedagogická fakulta",                        
  `12310-Přírodovědecká fakulta` = "Přírodověděcká fakulta",                                               
  `19240-Filozoficko-přírodovědecká fakulta` = "Přírodověděcká fakulta",                                   
  `11310-Přírodovědecká fakulta` = "Přírodovědecká fakulta",
  `14310-Přírodovědecká fakulta` = "Přírodovědecká fakulta",
  `13440-Přírodovědecká fakulta` = "Přírodovědecká fakulta",          
  `15310-Přírodovědecká fakulta` = "Přírodovědecká fakulta",
  `17310-Přírodovědecká fakulta` = "Přírodovědecká fakulta",          
  `18470-Přírodovědecká fakulta` = "Přírodovědecká fakulta",
  `15260-Cyrilometodějská teologická fakulta` = "Jiné",                                  
  `11280-Husitská teologická fakulta` = "Jiné",                                          
  `12260-Teologická fakulta` = "Jiné",                                                   
  `19510-Fakulta veřejných politik v Opavě` = "Jiné",                                    
  `21900-Celoškolská pracoviště (studium mimo fakulty)` = "Jiné",                        
  `22900-Celoškolská pracoviště (studium mimo fakulty)` = "Jiné",                        
  `41900-Celoškolská pracoviště (studium mimo fakulty)` = "Jiné",                        
  `43900-Celoškolská pracoviště (studium mimo fakulty)` = "Jiné",                        
  `76900-Celoškolská pracoviště (studium mimo fakulty)` = "Jiné",                        
  `7L900-Celoškolská pracoviště (studium mimo fakulty)` = "Jiné",                        
  `F00290-Celoškolská pracoviště (studium mimo fakulty)` = "Jiné",                       
  `F00590-Celoškolská pracoviště (studium mimo fakulty)` = "Jiné",                       
  `11510-Fakulta tělesné výchovy a sportu` = "Jiné",
  `14510-Fakulta sportovních studií` = "Jiné",
  `15510-Fakulta tělesné kultury` = "Jiné",
  `14230-Fakulta sociálních studií` = "Jiné",
  `23520-Fakulta aplikovaných věd` = "Jiné", 
  `31110-Fakulta financí a účetnictví` = "Jiné",  
  `12110-Zdravotně sociální fakulta` = "Jiné",
  `F02010-Collegium Humanum – Varšavská univerzita managementu - Frýdek-Místek` = "Jiné")

data_weights <- W_cnts$use %>%
  mutate(IDfac = W_key_fac[Fakulta],
         IDuni = W_key_uni[`Vysoká škola`]) %>%
  na.omit() %>%
  group_by(IDuni, IDfac) %>%
  summarise(cnt_pop = sum(cnt)) %>%
  arrange(-cnt_pop) %>%
  ungroup() %>%
  mutate(wgt_pop = cnt_pop/sum(cnt_pop)) %>%
  right_join(W_sample) %>%
  na.omit() %>%
  mutate(wgt_samp = cnt_samp/sum(cnt_samp))

W_plt_0 <- data_weights %>%
  mutate(org = IDfac) %>%
  mutate(org = ifelse(grepl("Mate|infor", org), "Jiné", org)) %>%
  group_by(org) %>%
  summarise(cnt_pop = sum(cnt_pop),
            cnt_samp = sum(cnt_samp)) %>%
  mutate_at(-1, function(x){x/sum(x)}) %>%
  arrange(cnt_pop) %>%
  mutate(org = ordered(org, org)) %>%
  select(org, starts_with("cnt")) %>%
  `names<-`(c("Pracoviště", "SIMS", "Vzorek")) %>%
  pivot_longer(-1) %>%
  mutate(name = ordered(name, c("SIMS", "Vzorek"))) %>%
  ggplot(aes(x = Pracoviště, y = value, fill = name)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", width = .8) +
    coord_flip() +
  labs(fill = "Data", y = "Podíl v dané sadě") +
  theme_minimal() +
  scale_fill_manual(values = use_cols[c(7,1)]) +
  theme(legend.position = "bottom")

W_plt_1 <- data_weights %>%
  mutate(org = paste0(IDuni, ": ", IDfac)) %>%
  arrange(cnt_pop) %>%
  mutate(org = ordered(org, org)) %>%
  select(org, starts_with("wgt")) %>%
  `names<-`(c("Pracoviště", "SIMS", "Vzorek")) %>%
  pivot_longer(-1) %>%
  mutate(name = ordered(name, c("SIMS", "Vzorek"))) %>%
  ggplot(aes(x = Pracoviště, y = value, fill = name)) +
    geom_bar(stat = "identity", position = "dodge", color = "black", width = .8) +
    coord_flip() +
  labs(fill = "Data", y = "Podíl v dané sadě") +
  theme_minimal() +
  scale_fill_manual(values = use_cols[c(7,1)]) +
  theme(legend.position = "bottom")

W_plt_2 <- data_weights %>%
  mutate(org = paste0(IDuni, ": ", IDfac)) %>%
  mutate(`Rozdíl podílů` = wgt_pop - wgt_samp) %>%
  arrange(`Rozdíl podílů`) %>%
  mutate(Pracoviště = ordered(org, org)) %>%
  mutate(Fakulta = case_when(grepl("Pedagogická", Pracoviště) ~ "Pedagogická",
                         grepl("Přírodovědecká", Pracoviště) ~ "Přírodovědecká",
                         grepl("informatiky", Pracoviště) ~ "Jiné",
                         grepl("Matematicko", Pracoviště) ~ "Jiné",
                         grepl("Filozofická", Pracoviště) ~ "Filozofická",
                         grepl("Jiné", Pracoviště) ~ "Jiné")) %>%
  ggplot(aes(x = Pracoviště, 
             y = `Rozdíl podílů`, 
             fill = wgt_pop,
             pch = Fakulta)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_point(cex = 5, stroke = 2) +
  coord_flip(clip = "off") +
  scale_fill_gradient2(low = use_cols[7], 
                       mid = use_cols[4],
                       high = use_cols[1], 
                       labels = scales::percent,
                       midpoint = mean(data_weights$wgt_pop)) +
  scale_y_continuous(labels = scales::label_percent(suffix = " %")) +
  labs(y = "Rozdíl podílů na celku (SIMS - vzorek)") +
  scale_shape_manual(values = c(21:24)) +
  labs(fill = "Podíl v SIMS") +
  theme_minimal() +
  theme(legend.position = "bottom")

    
ZU_weights <- data_weights %>%
  mutate(W = cnt_pop/cnt_samp,
         W_s = (W*sum(cnt_samp))/sum(cnt_pop))


write_csv(ZU_weights,
          "outputs/code/0_1_2_weights/data/ZU_weights.csv")


ggsave("outputs/code/0_1_3_weights/png/0_W_plt_1.png",
       W_plt_0, 
       height = 8,
       width = 10)

ggsave("outputs/code/0_1_3_weights/png/1_W_plt_1.png",
       W_plt_1, 
       height = 8,
       width = 10)

ggsave("outputs/code/0_1_3_weights/png/2_W_plt_1.png",
       W_plt_2, 
       height = 8,
       width = 10)