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
  # geom_label_repel(aes(label=party_name_short), size = 3) +
  ggtitle("Populist Parties (Whole Dataset)")
  
party_per_country = party_matched_ches %>% 
  filter(populist == 1) %>% 
  ggplot(aes(y=lrgen, x=state_market)) +
  geom_point() + 
  theme_bw() +
  geom_label_repel(aes(label=party_name_short), size = 1.5) +
  facet_wrap(country_name ~ .) +
  ggtitle("Populist Parties per country (Whole Dataset)") +
  xlab("Economy: State vs. Market") +
  ylab("Ideology: Left vs. Right")
party_per_country
ggsave("PDF/PNG/party_per_country.png",plot=party_per_country)


# Seats in Parliament

party_per_country_seat = populist_yearly %>% 
  filter(populist == 1, year > 2000)  %>% 
  group_by(country_name, party_name_short, party_id) %>% 
  summarise(lrgen = mean(lrgen, na.rm=T),
            state_market = mean(state_market, na.rm=T),
            perc_seats = mean(perc_seats, na.rm=T),
  ) %>% 
  ggplot(aes(y=lrgen, x=state_market)) +
  geom_point() + 
  theme_bw() +
  scale_size_continuous(name="Seats Share (%)")  +
  geom_label_repel(aes(label=party_name_short, size = perc_seats)) +
  facet_wrap(country_name ~ .) +
  ggtitle("Populist Parties per country (Since 2000; Seat Share in Parliament)")  +
  xlab("Economy: State vs. Market") +
  ylab("Ideology: Left vs. Right")
party_per_country_seat
ggsave("PDF/PNG/party_per_country_seat.png",plot=party_per_country_seat)



# Votes in Parliamentary Elections
party_per_country_vote = populist_yearly %>% 
  filter(populist == 1, year > 2000, vote_share > 5) %>% 
  group_by(country_name, party_name_short, party_id) %>% 
  summarise(left_right = mean(left_right, na.rm=T),
            state_market = mean(state_market, na.rm=T),
            vote_share = mean(vote_share, na.rm=T),
  ) %>%
  ggplot(aes(y=left_right, x=state_market)) +
  geom_point() + 
  theme_bw() +
  scale_size_continuous(name="Vote Share (%)") +
  geom_label_repel(aes(label=party_name_short, size = vote_share/100)) +
  facet_wrap(country_name ~ .)  +
  ggtitle("Populist Parties per country (Since 2000; Vote Share in Elections)")
party_per_country_vote
ggsave("PDF/PNG/party_per_country_vote.png",plot=party_per_country_vote)



####
populist_yearly %>% 
  filter(year >= 1990) %>% 
  group_by(year) %>%
  filter(populist == 1, .preserve = F) %>% 
  summarise("No_Parties" = sum(populist, na.rm=T)) %>% 
  ggplot(aes(x=year, y=No_Parties)) + 
  geom_histogram(stat="identity")


party_seat = populist_yearly %>% 
  filter(year >= 1990) %>% 
  filter(populist == 1) %>% 
  group_by(year) %>% 
  summarise("seats" = sum(seats, na.rm=T)) %>% 
  ggplot(aes(x=year, y=seats)) + 
  geom_histogram(stat="identity") +
  theme_bw() +
  scale_x_continuous(name = NULL, breaks = seq(1990, 2020, 5)) +
  ylab("Sum of Seats") +
  ggtitle("Number of Seats for Populist Parties")
party_seat
ggsave("PDF/PNG/party_seat.png",plot=party_seat)


party_vote = populist_yearly %>% 
  filter(year >= 1990) %>% 
  filter(populist == 1) %>% 
  group_by(year) %>% 
  summarise("votes" = sum(vote_share, na.rm=T)) %>% 
  ggplot(aes(x=year, y=votes)) + 
  geom_histogram(stat="identity") +
  theme_bw() +
  scale_x_continuous(name = NULL, breaks = seq(1990, 2020, 5)) +
  ylab("Sum of Vote Share") +
  ggtitle("Vote Share for Populist Parties")
party_vote
ggsave("PDF/PNG/party_vote.png",plot=party_vote)


populist_yearly %>% 
  filter(populist == 1) %>% 
  group_by(year) %>% 
  summarise("seats" = mean(perc_seats, na.rm=T)) %>% 
  ggplot(aes(x=year, y=seats)) + 
  geom_histogram(stat="identity")

party_cabinet = populist_yearly %>%
  filter(populist == 1, cabinet_party == 1, year>=1990) %>%
  group_by(year) %>% 
  summarise("No_Parties" = sum(populist, na.rm=T)) %>% 
  ggplot(aes(x=year, y=No_Parties)) + 
  geom_histogram(stat="identity")  +
  theme_bw() +
  scale_x_continuous(name = NULL, breaks = seq(1990, 2020, 5)) +
  ylab("Number of Populist Parties") +
  ggtitle("Number of Populist Parties in Cabinets")
party_cabinet
ggsave("PDF/PNG/party_cabinet.png",plot=party_cabinet)

#### DMX

Total_index_pop = DMX_populist %>% 
  filter(year >= 1990) %>% 
  mutate(populist = if_else(populist > 0, 1, 0)) %>% 
  ggplot(aes(x=year, y=total_index_context, col=as.factor(populist))) +
  geom_path(aes(group = 1), size=1.5) +
  facet_wrap(country_name ~ .) +
  theme_bw() +
  scale_y_continuous(name="Total Index Value (Context)", breaks=seq(0,1,0.5)) +
  scale_color_discrete(name="Populist Party", labels=c("No", "Yes")) + 
  ggtitle("Quality of Democracy and Populist Parties") +
  theme(axis.text.x = element_text(angle=90))
Total_index_pop
ggsave("PDF/PNG/Total_index_pop.png",plot=Total_index_pop)


DMX_populist %>% 
  ggplot(aes(x=year, y=total_index_context, col=as.factor(populist_cabinet))) +
  geom_path(aes(group = 1), size=1.5) +
  facet_wrap(country_name ~ .)

DMX_populist %>% 
  ggplot(aes(x=year, y=total_index_context, col=as.factor(populist_prime_minister))) +
  geom_path(aes(group = 1), size=1.5) +
  facet_wrap(country_name ~ .)
