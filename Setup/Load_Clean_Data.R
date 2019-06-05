# LOAD and CLEAN DATASETS

source("Setup/Packages.R")
source("Setup/basicfunctions.R")


# Load All Datasets
cabinets = fread("Datasets/view_cabinet.csv")
party = fread("Datasets/view_party.csv")
election = fread("Datasets/view_election.csv")

pg_frame = fread("Datasets/viewcalc_country_year_share.csv")

DMX = fread("Datasets/DemocracyMatrix_v1_1.csv")


CHES = fread("Datasets/2014_CHES_dataset_means.csv") %>% 
  select(chess = party_id, ch_cname = cname, ch_party_name = party_name, 
         antielite_salience, lrgen, lrgen_sd)

CHES_expert = fread("Datasets/2014_CHES_dataset_expert-level.csv") %>% 
  mutate(antielite_salience = as.factor(antielite_salience),
         antielite_salience = fct_recode(antielite_salience, 
                                         "10" = "extremely important",
                                         "0" = "not important at all"),
         antielite_salience = as.numeric(as.character(antielite_salience))
         ) %>% 
  group_by(party_id) %>% 
  summarise(antielite_salience_mean = mean(antielite_salience, na.rm=T),
            antielite_salience_sd = sd(antielite_salience, na.rm=T)) %>% 
  rename(chess = party_id)

CHES = CHES %>% 
  left_join(CHES_expert, by="chess")

# MATCH CHES AND PARLGOV

party %>% 
  full_join(CHES, by = "chess") %>% 
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

  
