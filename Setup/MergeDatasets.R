# Merge Datasets
source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Identify_Populism/Identify_PopulistParties.R")


party_matched = party_matched %>% 
  left_join(CHES, by="chess")

populist_yearly = pg_frame %>% 
  filter(id_type == "cabinet") %>% 
  rename(cabinet_id = id) %>% 
  left_join(cabinets, by="cabinet_id") %>% 
  left_join(party_matched %>%  select(party_id, populist), by="party_id") 

## More Observations per year
populist_yearly_mean = populist_yearly %>% 
  group_by(country_name, year, party_id, party_name_short) %>% 
  summarise(left_right = mean(left_right, na.rm=T),
            cabinet_party = mean(cabinet_party, na.rm=T),
            prime_minister = mean(prime_minister, na.rm=T),
            election_seats_total = mean(election_seats_total, na.rm=T),
            seats = mean(seats, na.rm=T),
            populist = mean(populist, na.rm=T),
  ) %>% 
  mutate(perc_seats = seats/election_seats_total,
         cabinet_party = if_else(cabinet_party > 0, 1, 0),
         prime_minister = if_else(prime_minister > 0, 1, 0),
         ) %>% 
  filter(year >= 1950)

populist_yearly_mean_cabinet = populist_yearly_mean %>% 
  filter(cabinet_party==1) %>% 
  group_by(country_name, year) %>% 
  summarise(populist_cabinet = sum(populist, na.rm=T))

populist_yearly_mean_perc_seats = populist_yearly_mean %>% 
  group_by(country_name, year) %>% 
  summarise(populist_perc_seats = sum(perc_seats*populist, na.rm=T))

populist_yearly_mean_prime_minister = populist_yearly_mean %>% 
  filter(prime_minister==1) %>% 
  group_by(country_name, year) %>% 
  summarise(populist_prime_minister = sum(populist, na.rm=T))
## Merge With DMX

DMX_context = DMX %>% 
  select_at(vars(country_name = country, year, regions, ends_with("context"))) %>% 
  filter(year >= 1950)



# Final Dataset
DMX_populist = populist_yearly_mean %>% 
  group_by(country_name, year) %>% 
  summarise(populist = sum(populist, na.rm=T)) %>% 
  left_join(DMX_context, by=c("country_name", "year")) %>%
  left_join(populist_yearly_mean_cabinet, by=c("country_name", "year")) %>%
  left_join(populist_yearly_mean_prime_minister, by=c("country_name", "year"))  %>%
  left_join(populist_yearly_mean_perc_seats, by=c("country_name", "year")) 
  

