# Merge Datasets
source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")

election_matched = pg_frame %>% 
  filter(id_type == "parliament") %>% 
  rename(election_id = id) %>% 
  left_join(election %>%  filter(election_type == "parliament"), by="election_id") %>% 
  select(country_name, year, share, party_id, vote_share) %>% 
  group_by(country_name, year) %>% 
  filter(share == max(share)) %>% 
  arrange(country_name, year, party_id) %>% 
  select(-share)


populist_yearly = pg_frame %>% 
  filter(id_type == "cabinet") %>% 
  rename(cabinet_id = id) %>% 
  left_join(cabinets, by="cabinet_id") %>% 
  left_join(party_matched_own %>%  select(party_id, populist), by="party_id") %>% 
  left_join(election_matched, by=c("country_name", "year", "party_id")) %>% 
  group_by(country_name, year) %>% 
  filter(share == max(share)) %>% 
  filter(year >= 1950) %>% 
  mutate(perc_seats = seats/election_seats_total)


##
populist_yearly_populist_party = populist_yearly %>% 
  group_by(country_name, year) %>% 
  summarise(populist = sum(populist, na.rm=T)) 

populist_yearly_mean_cabinet = populist_yearly %>% 
  filter(cabinet_party==1) %>% 
  group_by(country_name, year) %>% 
  summarise(populist_cabinet = sum(populist, na.rm=T))

populist_yearly_mean_perc_seats = populist_yearly %>%
  filter(populist == 1) %>% 
  group_by(country_name, year) %>% 
  summarise(populist_perc_seats = sum(perc_seats, na.rm=T))

populist_yearly_mean_vote_share = populist_yearly %>% 
  filter(populist == 1) %>% 
  group_by(country_name, year) %>% 
  summarise(populist_vote_share = sum(vote_share, na.rm=T))

populist_yearly_mean_prime_minister = populist_yearly %>% 
  filter(prime_minister==1) %>% 
  group_by(country_name, year) %>% 
  summarise(populist_prime_minister = sum(populist, na.rm=T))

## Merge With DMX

DMX_context = DMX %>% 
  select_at(vars(country_name = country, year, regions, ends_with("context"))) %>% 
  filter(year >= 1950)


## Presidential Systems
pop_pres = pop_par_pres %>% 
  select(country_name, pop_start, pop_end) %>% 
  mutate(pop_end = if_else(is.na(pop_start) == F & is.na(pop_end) == T, 2019L, pop_end)) %>% 
  na.omit() %>%
  # exclude Argentina 1946-1955
  filter(pop_start!= 1946) %>% 
  melt(by="country_name") %>% 
  mutate(populist_pres = 1) %>% 
  group_by(country_name) %>% 
  complete(country_name, value = full_seq(value, period = 1), fill = list(populist_pres = 1)) %>% 
  select(country_name, year = value, populist_pres)



## Final Dataset
DMX_populist = DMX_context %>% 
  left_join(populist_yearly_populist_party, by=c("country_name", "year")) %>% 
  left_join(populist_yearly_mean_vote_share, by=c("country_name", "year")) %>% 
  left_join(populist_yearly_mean_perc_seats, by=c("country_name", "year")) %>%
  left_join(populist_yearly_mean_cabinet, by=c("country_name", "year")) %>%
  left_join(populist_yearly_mean_prime_minister, by=c("country_name", "year"))  %>%
  left_join(pop_pres, by=c("country_name", "year")) %>%  
  #left_join(consolidated_democracy, by=c("country_name")) %>%  
  mutate(populist_is_gov = if_else(populist_prime_minister == 1 | populist_pres == 1,1,0),
         populist_is_gov = if_else(is.na(populist_is_gov) == T, 0, populist_is_gov)) %>% 
  left_join(QoG, by=c("country_name", "year")) %>% 
  left_join(V_dem, by=c("country_name", "year"))%>% 
  mutate(pop_cat = if_else(populist_is_gov == 1, "HOG",
                           if_else(populist_cabinet >= 1 & populist_is_gov == 0, "Cabinet",
                                   if_else(populist_cabinet == 0 & populist_perc_seats >= 0.1, "Opposition", "No Populist Party")
                           )),
         pop_cat = if_else(is.na(pop_cat)==T, "No Populist Party", pop_cat),
         pop_cat = relevel(as.factor(pop_cat), ref="No Populist Party"))

# Select Only Countries with at least one populist party in history

Pop_Countries = DMX_populist %>% 
  ungroup() %>% 
  filter(populist == 1 | populist_is_gov == 1) %>% 
  distinct(country_name) %>% 
  pull(country_name)

## Save Dataset

write.csv(DMX_populist %>% 
            select(country = country_name, 
                   year, 
                   populist,
                   populist_vote_share,
                   populist_perc_seats,
                   populist_cabinet,
                   populist_prime_minister,
                   populist_pres, 
                   populist_is_gov), 
          file="Datasets/Populism_v3.csv", 
          fileEncoding="UTF-8",
          row.names = F)



# Create TSCS Dataset ####
library(rcompanion)
TSCS_data_trans = DMX_populist %>% 
  filter(country_name %in% Pop_Countries) %>% 
  mutate(classification_context = ifelse(country_name == "Mexico" & year < 1996, "", classification_context),
         classification_context = ifelse(country_name == "Estonia" & year <= 1991, "", classification_context),
         classification_context = ifelse(country_name == "Turkey" & year < 2000, "", classification_context),
         decision_control_context = ifelse(country_name == "Poland" & year == 1990, NA, decision_control_context)
         ) %>%
  filter(year >= 1990, classification_context != "", classification_context != "Hard Autocracy") %>% 
  dummy_cols(., "pop_cat", remove_first_dummy = TRUE) %>%
  group_by(country_name) %>% 
  mutate(summe = sum(pop_cat_Cabinet + pop_cat_HOG + pop_cat_Opposition)) %>% 
  filter(summe != 0)  %>% 
  dplyr::select(-summe) %>% 
  ungroup() %>% 
  
  rename(pop_cab_caus =  pop_cat_Cabinet, pop_HOG_caus = pop_cat_HOG, pop_Opp_caus = pop_cat_Opposition) %>%  
  
  # mutate_at(vars(ends_with("context"), -classification_context), funs(transformTukey((.-min(., na.rm=T)) + 1, statistic = 1, plotit=F, quiet=T)) ) %>% 
  
  group_by(country_name) %>% 
  mutate_at(vars(ends_with("caus")), funs(lag = dplyr::lag(., 1))) %>% 
  #mutate_at(vars(ends_with("caus")), funs(lag2 = dplyr::lag(., 2))) %>% 
  
  mutate_at(vars(ends_with("context"), -classification_context), funs(lag = dplyr::lag(., 1))) %>% 
  mutate_at(vars(ends_with("context"), -classification_context), funs(lag2 = dplyr::lag(., 2))) %>% 
  mutate_at(vars(ends_with("context"), -classification_context), funs(lag3 = dplyr::lag(., 3))) %>% 
  ungroup() %>% 
  mutate(trend = as.numeric(year - min(year))) 


TSCS_data_transformed = DMX_populist %>% 
  filter(country_name %in% Pop_Countries) %>% 
  mutate(classification_context = ifelse(country_name == "Mexico" & year < 1996, "", classification_context),
         classification_context = ifelse(country_name == "Estonia" & year <= 1991, "", classification_context),
         classification_context = ifelse(country_name == "Turkey" & year < 2000, "", classification_context),
         decision_control_context = ifelse(country_name == "Poland" & year == 1990, NA, decision_control_context)
  ) %>%
  filter(year >= 1990, classification_context != "", classification_context != "Hard Autocracy") %>% 
  dummy_cols(., "pop_cat", remove_first_dummy = TRUE) %>%
  group_by(country_name) %>% 
  mutate(summe = sum(pop_cat_Cabinet + pop_cat_HOG + pop_cat_Opposition)) %>% 
  filter(summe != 0)  %>% 
  dplyr::select(-summe) %>% 
  ungroup() %>% 
  
  rename(pop_cab_caus =  pop_cat_Cabinet, pop_HOG_caus = pop_cat_HOG, pop_Opp_caus = pop_cat_Opposition) %>%  
  
  mutate_at(vars(ends_with("context"), -classification_context), funs(transformTukey((.-min(., na.rm=T)) + 1, statistic = 1, plotit=F, quiet=T)) ) %>% 
  
  group_by(country_name) %>% 
  mutate_at(vars(ends_with("caus")), funs(lag = dplyr::lag(., 1))) %>% 
  #mutate_at(vars(ends_with("caus")), funs(lag2 = dplyr::lag(., 2))) %>% 
  
  mutate_at(vars(ends_with("context"), -classification_context), funs(lag = dplyr::lag(., 1))) %>% 
  mutate_at(vars(ends_with("context"), -classification_context), funs(lag2 = dplyr::lag(., 2))) %>% 
  mutate_at(vars(ends_with("context"), -classification_context), funs(lag3 = dplyr::lag(., 3))) %>% 
  ungroup() %>% 
  mutate(trend = as.numeric(year - min(year))) 
