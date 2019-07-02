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
  mutate(pop_end = if_else(is.na(pop_start) == F & is.na(pop_end) == T, as.integer(2018), pop_end)) %>% 
  na.omit() %>% 
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
  mutate(populist_is_gov = if_else(populist_prime_minister == 1 | populist_pres == 1,1,0),
         populist_is_gov = if_else(is.na(populist_is_gov) == T, 0, populist_is_gov)) %>% 
  left_join(V_dem, by=c("country_name", "year")) 
  

# Select Only Countries with at least one populist party in history

Pop_Countries = DMX_populist %>% 
  ungroup() %>% 
  filter(populist == 1 | populist_is_gov == 1) %>% 
  distinct(country_name) %>% 
  pull(country_name)


