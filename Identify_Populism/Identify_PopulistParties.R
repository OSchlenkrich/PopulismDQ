# Populist Parties
source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")


# bootstrap mean of antielite_salience
bootstrap_sample = data.frame(chess = unique(CHES_expert$party_id),
                              array(NA, dim=c(length(unique(CHES_expert$party_id)),3))) %>% 
  rename(antielite_salience_lower=X1, antielite_salience_mean=X2, antielite_salience_upper=X3)
for (i in 1:length(unique(CHES_expert$party_id))) {
  
  data = CHES_expert %>%  
    filter(party_id == unique(CHES_expert$party_id)[i]) %>%  
    pull(antielite_salience) %>% 
    na.omit()
  
  if (length(data) == 1) {
    bootstrap_sample[i,-1]  = c(NA, NA, NA)
  } else {
    if (length(unique(data)) == 1) {
      bootstrap_sample[i,-1]  = rep(unique(data), 3)
    } else {
      results <- boot(data=data, 
                      statistic=mean_boot,
                      R=1000)
      
      results_bca = boot.ci(results, type="bca")
      bootstrap_sample[i,-1]  = c(results_bca$bca[,4], mean(results$t, na.rm=T), results_bca$bca[,5])
      rm(results_bca)
    }
  }
  
}

# bootstrap mean of lrgen
bootstrap_sample_lrgen = data.frame(chess = unique(CHES_expert$party_id),
                              array(NA, dim=c(length(unique(CHES_expert$party_id)),3))) %>% 
  rename(lrgen_lower=X1, lrgen_mean=X2, lrgen_upper=X3)
for (i in 1:length(unique(CHES_expert$party_id))) {
  
  data = CHES_expert %>%  
    filter(party_id == unique(CHES_expert$party_id)[i]) %>%  
    pull(lrgen) %>% 
    na.omit()
  
  if (length(data) == 1) {
    bootstrap_sample_lrgen[i,-1]  = c(NA, NA, NA)
  } else {
    if (length(unique(data)) == 1) {
      bootstrap_sample_lrgen[i,-1]  = rep(unique(data), 3)
    } else {
      results <- boot(data=data, 
                      statistic=mean_boot,
                      R=1000)
      
      results_bca = boot.ci(results, type="bca")
      bootstrap_sample_lrgen[i,-1]  = c(results_bca$bca[,4], mean(results$t, na.rm=T), results_bca$bca[,5])
    }
  }
  
}

####


CHES = CHES_raw %>% 
  left_join(bootstrap_sample, by="chess") %>% 
  left_join(bootstrap_sample_lrgen, by="chess")


# Some Density Plots
CHES %>% 
  select(antielite_salience_mean, lrgen) %>% 
  melt() %>% 
  ggplot(aes(x=value, fill=variable)) +
  geom_density(alpha=0.5) +
  theme_bw()


CHES = CHES %>% 
  mutate(
    populist = if_else( antielite_salience_upper > 6 & lrgen_lower < 9 & lrgen_upper > 1, 1, 0)
  )


table(CHES$populist)

##

CHES %>%
  unite("party", ch_party_name, ch_cname) %>% 
  mutate(party = fct_reorder(party, antielite_salience_mean)) %>%
  select(party, antielite_salience_mean, antielite_salience_lower, antielite_salience_upper, populist) %>% 
  na.omit() %>% 
  ggplot(aes(x = party, y= antielite_salience_mean, col=as.factor(populist))) +
  geom_errorbar(aes(ymin=antielite_salience_lower, ymax=antielite_salience_upper)) +
  geom_point() + 
  geom_hline(yintercept = 6) +
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
