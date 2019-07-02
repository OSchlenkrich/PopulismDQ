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
DMX = fread("Datasets/DemocracyMatrix_v1_1.csv")


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

# V-Dem Dataset

V_dem = fread("C:/RTest/V-Dem-CY+Others-v8.csv", encoding = "UTF-8") %>% 
  select(country_name, year, gdp_growth = e_migdpgro, v2csantimv)

# Own Dataset
pop_par_pres = fread("Datasets/Populistische Parteien_Praesidenten.csv", encoding = "UTF-8") %>% 
  rename(pop_start = `Start Regierungsbeteiligung`, pop_end = `Ende Regierungsbeteiligung`) %>% 
  mutate(populist = 1)

# ParlGov and Own Dataset

party_matched_own = party %>% 
  left_join(pop_par_pres %>%  select(party_id = party_id_parlgov, populist), by="party_id") %>% 
  mutate(populist = if_else(is.na(populist) == T, 0, 1))
  

