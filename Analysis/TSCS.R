source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Identify_Populism/Identify_PopulistParties.R")
source("Setup/MergeDatasets.R")


names(DMX_populist)
Reg = DMX_populist %>% 
  mutate(populist_cabinet = dplyr::lag(if_else(populist_cabinet > 0, 1, 0), 1),
         populist = dplyr::lag(if_else(populist > 0, 1, 0), 1),
         populist_prime_minister = dplyr::lag(if_else(populist_prime_minister > 0, 1, 0), 1),
         total_index_context_lag = dplyr::lag(total_index_context, 1),
         rights_inst_index_context_lag = dplyr::lag(rights_inst_index_context, 1),
         communication_inst_index_context_lag = dplyr::lag(communication_inst_index_context, 1)
         ) %>% 
  filter(year > 1990) 
  # filter(country_name != "Turkey") %>% 
  # filter(country_name != "Hungary") %>% 
  # filter(country_name != "Poland")


library(plm)
library(lmtest)


test = plm(total_index_context ~ total_index_context_lag +  populist_cabinet + populist_perc_seats  + 
             populist_prime_minister + populist, 
           index=c("country_name", "year"), 
           as.data.frame(Reg),
           model="within")
summary(test)
coeftest(test, vcov=vcovBK)


test = plm(rights_inst_index_context ~ rights_inst_index_context_lag + populist_perc_seats + populist_cabinet + 
             populist_prime_minister + populist, 
           index=c("country_name", "year"), 
           as.data.frame(Reg),
           model="within")
summary(test)
coeftest(test, vcov=vcovBK)

test = plm(communication_inst_index_context ~ communication_inst_index_context_lag + populist_perc_seats + populist_cabinet + 
             populist_prime_minister + populist, 
           index=c("country_name", "year"), 
           as.data.frame(Reg),
           model="within")
summary(test)
coeftest(test, vcov=vcovBK)
