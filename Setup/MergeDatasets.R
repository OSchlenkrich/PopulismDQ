# Merge Datasets
source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Identify_Populism/Identify_PopulistParties.R")

election_matched = pg_frame %>% 
  filter(id_type == "parliament") %>% 
  rename(election_id = id) %>% 
  left_join(election, by="election_id") %>% 
  select(country_name, year, party_id, vote_share)


party_matched_ches = party_matched %>% 
  left_join(CHES, by="chess") 

populist_yearly = pg_frame %>% 
  filter(id_type == "cabinet") %>% 
  rename(cabinet_id = id) %>% 
  left_join(cabinets, by="cabinet_id") %>% 
  left_join(party_matched_ches %>%  select(party_id, populist, populist_l, lrgen, populist_r, state_market), by="party_id") %>% 
  left_join(election_matched, by=c("country_name", "year", "party_id")) %>% 
  group_by(country_name, year) %>% 
  filter(share == max(share)) %>% 
  filter(year >= 1950) %>% 
  mutate(perc_seats = seats/election_seats_total)

populist_yearly_mean_cabinet = populist_yearly %>% 
  filter(cabinet_party==1) %>% 
  group_by(country_name, year) %>% 
  summarise(populist_cabinet = sum(populist, na.rm=T))

populist_yearly_mean_perc_seats = populist_yearly %>% 
  group_by(country_name, year) %>% 
  summarise(populist_perc_seats = sum(perc_seats*populist, na.rm=T))

populist_yearly_mean_vote_share = populist_yearly %>% 
  group_by(country_name, year) %>% 
  summarise(populist_vote_share = sum(vote_share*populist, na.rm=T))

populist_yearly_mean_prime_minister = populist_yearly %>% 
  filter(prime_minister==1) %>% 
  group_by(country_name, year) %>% 
  summarise(populist_prime_minister = sum(populist, na.rm=T))
## Merge With DMX

DMX_context = DMX %>% 
  select_at(vars(country_name = country, year, regions, ends_with("context"))) %>% 
  filter(year >= 1950)



# Final Dataset
DMX_populist = populist_yearly %>% 
  group_by(country_name, year) %>% 
  summarise(populist = sum(populist, na.rm=T),
            populist_l = sum(populist_l, na.rm=T),
            populist_r = sum(populist_r, na.rm=T),
  ) %>% 
  left_join(DMX_context, by=c("country_name", "year")) %>%
  left_join(populist_yearly_mean_cabinet, by=c("country_name", "year")) %>%
  left_join(populist_yearly_mean_prime_minister, by=c("country_name", "year"))  %>%
  left_join(populist_yearly_mean_perc_seats, by=c("country_name", "year")) %>%
  left_join(populist_yearly_mean_vote_share, by=c("country_name", "year")) %>% 
  left_join(V_dem, by=c("country_name", "year"))
  


