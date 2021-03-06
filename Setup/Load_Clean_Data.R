# LOAD and CLEAN DATASETS

source("Setup/Packages.R")
source("Setup/basicfunctions.R")


# Load All Datasets

# ParlGov
cabinets = fread("Datasets/view_cabinet.csv")
party = fread("Datasets/view_party.csv")
election = fread("Datasets/view_election.csv")
# party_family = fread("Datasets/party_family.csv")
# info_id = fread("Datasets/info_id.csv")
pg_frame = fread("Datasets/viewcalc_country_year_share.csv")


# DMX
DMX = fread("Datasets/DemocracyMatrix_v3.csv")

consolidated_democracy = DMX %>% 
  filter(year >= 1950, year <= 1990) %>% 
  select(country_name = country, classification_context) %>% 
  na.omit() %>% 
  group_by(country_name, classification_context) %>% 
  summarise(cnt = n()) %>% 
  mutate(class = ifelse(classification_context == "Deficient Democracy" | 
                          classification_context == "Working Democracy", 1, 0))  %>% 
  mutate(class = class * cnt) %>% 
  ungroup() %>% 
  select(country_name, consdem_caus_bw = class)


# CHES
CHES_raw = fread("Datasets/2014_CHES_dataset_means.csv") %>% 
  select(chess = party_id, ch_cname = cname, ch_party_name = party_name, 
         antielite_salience, lrgen, eu_position)

CHES_expert = fread("Datasets/2014_CHES_dataset_expert-level.csv") %>% 
  mutate(antielite_salience = as.factor(antielite_salience),
         antielite_salience = fct_recode(antielite_salience, 
                                         "10" = "extremely important",
                                         "0" = "not important at all"),
         antielite_salience = as.numeric(as.character(antielite_salience))
         ) %>% 
  mutate(lrgen = as.factor(lrgen),
         lrgen = fct_recode(lrgen, 
                                         "10" = "extreme right",
                                         "5" = "center",
                                         "0" = "extreme left"),
         lrgen = as.numeric(as.character(lrgen))
  ) 


# MATCH CHES AND PARLGOV

party %>% 
  full_join(CHES_raw, by = "chess") %>% 
  filter(is.na(country_name_short) == T) %>% 
  select(ch_party_name, chess) %>% 
  write.csv2(file="Datasets/MatchingList.csv", row.names = F)

MatchingList = fread("Datasets/MatchingList_Done.csv") %>% 
  rename(party_id = ParlGov_Party_id) %>% 
  select(-ch_party_name)

party_matched = party %>% 
  filter(party_id %in% MatchingList$party_id) %>% 
  select(-chess) %>% 
  left_join(MatchingList,  by = "party_id") %>% 
  bind_rows(party %>% filter(party_id %!in% MatchingList$party_id) )

# QoG Dataset

QoG = fread("unzip -p Datasets/qog_std_ts_jan20.zip", encoding = "UTF-8") %>% 
  select(country_name = cname, year, gdp_growth_caus = wdi_gdpcapgr) %>% 
  mutate(gdp_growth_caus = na_locf(gdp_growth_caus),
         country_name = ifelse(country_name == "France (1963-)", "France", country_name),
         country_name = ifelse(country_name == "Cyprus (1975-)", "Cyprus", country_name),
         country_name = ifelse(country_name == "United States", "United States of America", country_name))

# VDem Dataset

V_dem = fread("unzip -p Datasets/V-Dem-CY-Full+Others-v10.zip", encoding = "UTF-8") %>% 
  select(country_name, year, socequal_caus = v2dlunivl, pubequal_caus = v2peapsecon)


# Own Dataset
pop_par_pres = fread("Datasets/Populistische Parteien_Praesidenten_071020.csv", encoding = "UTF-8") %>% 
  rename(pop_start = `Start Regierungsbeteiligung`, pop_end = `Ende Regierungsbeteiligung`) %>% 
  mutate(populist = 1)

# ParlGov and Own Dataset

party_matched_own = party %>% 
  left_join(pop_par_pres %>%  select(party_id = party_id_parlgov, populist), by="party_id") %>% 
  mutate(populist = if_else(is.na(populist) == T, 0, 1))
  

