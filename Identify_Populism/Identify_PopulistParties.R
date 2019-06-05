# Populist Parties

source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")


CHES = CHES %>% 
  mutate(
    lrgen_lower = lrgen - lrgen_sd,
    lrgen_upper = lrgen + lrgen_sd,
    antielite_salience_upper = antielite_salience_mean + antielite_salience_sd, 
    antielite_salience_lower = antielite_salience_mean - antielite_salience_sd,
    populist = if_else( antielite_salience_upper > 7 & lrgen_lower < 9 & lrgen_upper > 1, 1, 0)
  )

#

CHES %>%
  unite("party", ch_party_name, ch_cname) %>% 
  mutate(party = fct_reorder(party, antielite_salience_mean)) %>%
  select(party, antielite_salience_mean, antielite_salience_lower, antielite_salience_upper, populist) %>% 
  na.omit() %>% 
  ggplot(aes(x = party, y= antielite_salience_mean, col=as.factor(populist))) +
  geom_errorbar(aes(ymin=antielite_salience_lower, ymax=antielite_salience_upper)) +
  geom_point() + 
  geom_hline(yintercept = 7) +
  coord_flip() +
  theme_bw()

CHES %>%
  unite("party", ch_party_name, ch_cname) %>% 
  mutate(party = fct_reorder(party, antielite_salience_mean)) %>%
  select(party, lrgen, lrgen_lower, lrgen_upper, populist) %>% 
  na.omit() %>% 
  ggplot(aes(x = party, y= lrgen, col=as.factor(populist))) +
  geom_errorbar(aes(ymin=lrgen_lower, ymax=lrgen_upper)) +
  geom_point() + 
  geom_hline(yintercept = c(1,9)) + 
  coord_flip() +
  theme_bw()
