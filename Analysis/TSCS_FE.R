source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Setup/MergeDatasets_w_OwnData.R")
source("Setup/Impulse_Unit_Response.R")

# Summary Statistics

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


# Results
m1 = TSCS_reg("decision_freedom_context", lag=2)
m2 = TSCS_reg("decision_equality_context", lag=1)
m3 = TSCS_reg("decision_control_context", lag=1)

m4 = TSCS_reg("intermediate_freedom_context", lag=2)
m5 = TSCS_reg("intermediate_equality_context", lag=1)
m6 = TSCS_reg("intermediate_control_context", lag=2)

m7 = TSCS_reg("communication_freedom_context", lag=2)
m8 = TSCS_reg("communication_equality_context", lag=2)
m9 = TSCS_reg("communication_control_context", lag=2)

m10 = TSCS_reg("rights_freedom_context", lag=2)
m11 = TSCS_reg("rights_equality_context", lag=2)
m12 = TSCS_reg("rights_control_context", lag=2)

m13 = TSCS_reg("rule_settlement_freedom_context", lag=2)
m14 = TSCS_reg("rule_settlement_equality_context", lag=1)
m15 = TSCS_reg("rule_settlement_control_context", lag=2)


modellist = list(m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14, m15)
modellabels = list("EV F", "EV G", "EV K",
                   "IV F", "IV G", "IV K",
                   "KO F", "KO G", "KO K",
                   "RG F", "RG G", "RG K",
                   "RS F", "RS G", "RS K")

makeTable = function(model, labels) {
  body = tidy(model[[2]]) %>% 
    filter(grepl("country", term) == F) %>% 
    filter(grepl("year", term) == F) %>% 
    mutate(stars = stars.pval(p.value),
           estimate = paste(round(estimate, 3), stars, sep=""),
           std.error = paste("(", round(std.error,3), ")", sep=""),
           estimate = paste(estimate, "</br>", std.error, sep="")) %>% 
    mutate(term = ifelse(grepl("_context_lag2", term), "Lag2", term),
           term = ifelse(grepl("_context_lag", term), "Lag", term)) %>% 
    select(term, !!labels := estimate) 
  
  
  foot = broom::glance(model[[1]]) %>% 
    select("R<sub>2" = r.squared, N = nobs) %>% 
    pivot_longer(everything()) %>% 
    mutate(value = as.character(round(value, 2))) %>% 
    rename(term = name, !!labels := value)

  return(list(body, foot))  
}

modeltable_body = makeTable(modellist[[1]], modellabels[[1]])[[1]]
modeltable_foot = makeTable(modellist[[1]], modellabels[[1]])[[2]]


for (i in 2:length(modellist)) {
  model_i = makeTable(modellist[[i]], modellabels[[i]])
  
  modeltable_body = modeltable_body %>% 
    full_join(model_i[[1]], by = "term")
  modeltable_foot = modeltable_foot %>% 
    full_join(model_i[[2]], by = "term")
}

modeltable_body %>% 
  mutate(term = gsub("_caus", "", term)) %>% 
  dust() %>% 
  redust(modeltable_foot, part="foot") %>% 
  sprinkle_colnames("term" = NA_character_) %>%
  
  sprinkle(rows = 1, border="bottom", part = "head") %>% 
  sprinkle(rows = 1, border="top", part = "foot") %>% 
  sprinkle(cols = 1, border="right", part = "body") %>% 
  
  sprinkle_na_string(na_string = "", part=c("head")) %>% 
  sprinkle_na_string(na_string = "", part=c("body")) %>% 
  sprinkle_print_method(print_method = "html")
