source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Identify_Populism/Identify_PopulistParties.R")
source("Setup/MergeDatasets.R")


# Plot Parties: Left/Right <-> State/Market
party_matched %>% 
  filter(populist == 1) %>% 
  ggplot(aes(y=left_right, x=state_market)) +
  geom_point() + 
  theme_bw() +
  geom_label_repel(aes(label=party_name_short))
  
party_matched %>% 
  filter(populist == 1) %>% 
  ggplot(aes(y=left_right, x=state_market)) +
  geom_point() + 
  theme_bw() +
  geom_label_repel(aes(label=party_name_short)) +
  facet_wrap(country_name ~ .)
  

####
populist_yearly_mean %>% 
  group_by(year) %>%
  filter(populist == 1, .preserve = F) %>% 
  summarise("No_Parties" = sum(populist, na.rm=T)) %>% 
  ggplot(aes(x=year, y=No_Parties)) + 
  geom_histogram(stat="identity")


populist_yearly_mean %>% 
  filter(populist == 1) %>% 
  group_by(year) %>% 
  summarise("seats" = sum(seats, na.rm=T)) %>% 
  ggplot(aes(x=year, y=seats)) + 
  geom_histogram(stat="identity")

populist_yearly_mean %>% 
  filter(populist == 1) %>% 
  group_by(year) %>% 
  summarise("seats" = mean(perc_seats, na.rm=T)) %>% 
  ggplot(aes(x=year, y=seats)) + 
  geom_histogram(stat="identity")

populist_yearly_mean %>%
  filter(populist == 1, cabinet_party == 1) %>%
  group_by(year) %>% 
  summarise("No_Parties" = sum(populist, na.rm=T)) %>% 
  ggplot(aes(x=year, y=No_Parties)) + 
  geom_histogram(stat="identity")


#### DMX

DMX_populist %>% 
  ggplot(aes(x=year, y=total_index_context, col=as.factor(populist))) +
  geom_path(aes(group = 1), size=1.5) +
  facet_wrap(country_name ~ .)

DMX_populist %>% 
  ggplot(aes(x=year, y=total_index_context, col=as.factor(populist_cabinet))) +
  geom_path(aes(group = 1), size=1.5) +
  facet_wrap(country_name ~ .)

DMX_populist %>% 
  ggplot(aes(x=year, y=total_index_context, col=as.factor(populist_prime_minister))) +
  geom_path(aes(group = 1), size=1.5) +
  facet_wrap(country_name ~ .)
