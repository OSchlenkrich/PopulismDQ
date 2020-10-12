source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Setup/MergeDatasets_w_OwnData.R")
source("Setup/Impulse_Unit_Response.R")

# Summary Statistics
library(psych)
library(pixiedust)
my_vars = TSCS_data_trans %>% 
  select_at(vars(matches("_caus"), ends_with("_context"), -matches("pubequal_caus"), -matches("inst"), -matches("_index"), -matches("classification")))
psych::describe(my_vars) %>% 
  mutate(Variable = row.names(.))  %>% 
  select(Variable, n, mean, sd, min, max) %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  dust() %>% 
  pixiedust::sprinkle_print_method(print_method = "html")
  
library(mice)
md.pattern(my_vars, rotate.names = T)  

#

TSCS_data_trans %>% 
  select_at(vars(ends_with("context"), -classification_context)) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  #mutate(value = transformTukey(value, statistic = 2, plotit=F, quiet=T)) %>%
  ungroup() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(name ~ ., scales="free")

TSCS_data_trans %>% 
  select_at(vars(ends_with("caus"), -classification_context)) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  #mutate(value = transformTukey(value, statistic = 2, plotit=F, quiet=T)) %>%
  ungroup() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(name ~ ., scales="free")

# Check Unit Root
round(confint(TSCS_reg("decision_freedom_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("decision_equality_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("decision_control_context", lag=1)[[2]]),3)

round(confint(TSCS_reg("intermediate_freedom_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("intermediate_equality_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("intermediate_control_context", lag=1)[[2]]),3)

round(confint(TSCS_reg("communication_freedom_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("communication_equality_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("communication_control_context", lag=1)[[2]]),3)

round(confint(TSCS_reg("rights_freedom_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("rights_equality_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("rights_control_context", lag=1)[[2]]),3)

round(confint(TSCS_reg("rule_settlement_freedom_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("rule_settlement_equality_context", lag=1)[[2]]),3)
round(confint(TSCS_reg("rule_settlement_control_context", lag=1)[[2]]),3)

# Check Autocorrelation
get_autocorrelation = function(varname, lag) {
  # order 1
  statistic = round(TSCS_reg(varname, lag=lag)[[3]]$statistic, 3)
  p.value = round(TSCS_reg(varname, lag=lag)[[3]]$p.value, 3)
  
  print(paste("1 :",statistic, ", p = ", p.value, sep=""))
  # order 2
  statistic = round(TSCS_reg(varname, lag=lag)[[4]]$statistic, 3)
  p.value = round(TSCS_reg(varname, lag=lag)[[4]]$p.value, 3)
  print(paste("2 :", statistic, ", p = ", p.value, sep=""))
  
}


get_autocorrelation("decision_freedom_context", lag=2)
get_autocorrelation("decision_equality_context", lag=1)
get_autocorrelation("decision_control_context", lag=1)

get_autocorrelation("intermediate_freedom_context", lag=2)
get_autocorrelation("intermediate_equality_context", lag=1)
get_autocorrelation("intermediate_control_context", lag=2)

get_autocorrelation("communication_freedom_context", lag=2)
get_autocorrelation("communication_equality_context", lag=2)
get_autocorrelation("communication_control_context", lag=2)

get_autocorrelation("rights_freedom_context", lag=2)
get_autocorrelation("rights_equality_context", lag=2)
get_autocorrelation("rights_control_context", lag=2)

get_autocorrelation("rule_settlement_freedom_context", lag=2)
get_autocorrelation("rule_settlement_equality_context", lag=1)
get_autocorrelation("rule_settlement_control_context", lag=2)



