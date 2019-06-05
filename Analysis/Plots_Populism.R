source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Identify_Populism/Identify_PopulistParties.R")
source("Setup/MergeDatasets.R")


# Plot Parties: Left/Right <-> State/Market
party_matched_ches %>% 
  filter(populist == 1) %>% 
  ggplot(aes(y=left_right, x=state_market)) +
  geom_point() + 
  theme_bw() +
  geom_label_repel(aes(label=party_name_short), size = 3) +
  ggtitle("Populist Parties (Whole Dataset)")
  
party_matched_ches %>% 
  filter(populist == 1) %>% 
  ggplot(aes(y=left_right, x=state_market)) +
  geom_point() + 
  theme_bw() +
  geom_label_repel(aes(label=party_name_short), size = 3) +
  facet_wrap(country_name ~ .) +
  ggtitle("Populist Parties per country (Whole Dataset)")

# Seats in Parliament

populist_yearly %>% 
  filter(populist == 1, year > 2000)  %>% 
  group_by(country_name, party_name_short, party_id) %>% 
  summarise(left_right = mean(left_right, na.rm=T),
            state_market = mean(state_market, na.rm=T),
            perc_seats = mean(perc_seats, na.rm=T),
  ) %>% 
  ggplot(aes(y=left_right, x=state_market)) +
  geom_point(aes(size=perc_seats)) + 
  theme_bw() +
  scale_size_continuous(name="Seats Share (%)")  +
  geom_label_repel(aes(label=party_name_short), size = 3) +
  facet_wrap(country_name ~ .) +
  ggtitle("Populist Parties per country (Since 2000; Seat Share in Parliament)")

# Votes in Parliamentary Elections
populist_yearly %>% 
  filter(populist == 1, year > 2000) %>% 
  group_by(country_name, party_name_short, party_id) %>% 
  summarise(left_right = mean(left_right, na.rm=T),
            state_market = mean(state_market, na.rm=T),
            vote_share = mean(vote_share, na.rm=T),
  ) %>% 
  ggplot(aes(y=left_right, x=state_market)) +
  geom_point(aes(size=vote_share/100)) + 
  theme_bw() +
  scale_size_continuous(name="Vote Share (%)") +
  geom_label_repel(aes(label=party_name_short), size = 3) +
  facet_wrap(country_name ~ .)  +
  ggtitle("Populist Parties per country (Since 2000; Vote Share in Elections)")


####
populist_yearly %>% 
  group_by(year) %>%
  filter(populist == 1, .preserve = F) %>% 
  summarise("No_Parties" = sum(populist, na.rm=T)) %>% 
  ggplot(aes(x=year, y=No_Parties)) + 
  geom_histogram(stat="identity")


populist_yearly %>% 
  filter(populist == 1) %>% 
  group_by(year) %>% 
  summarise("seats" = sum(seats, na.rm=T)) %>% 
  ggplot(aes(x=year, y=seats)) + 
  geom_histogram(stat="identity")

populist_yearly %>% 
  filter(populist == 1) %>% 
  group_by(year) %>% 
  summarise("seats" = mean(perc_seats, na.rm=T)) %>% 
  ggplot(aes(x=year, y=seats)) + 
  geom_histogram(stat="identity")

populist_yearly %>%
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
