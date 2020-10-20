source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Setup/MergeDatasets_w_OwnData.R")
source("Setup/Impulse_Unit_Response.R")

# Calculate Within and Between Effects
brms_ml_data = TSCS_data_trans %>% 
  group_by(country_name) %>% 
  mutate_at(vars(ends_with("caus")), funs(cbw = mean(., na.rm=T)))  %>% 
  mutate_at(vars(ends_with("_context_lag")), funs(depcbw = mean(., na.rm=T)))  %>% 
  group_by(year) %>% 
  mutate_at(vars(ends_with("caus")), funs(ybw = mean(., na.rm=T)))  %>% 
  mutate_at(vars(ends_with("_context_lag")), funs(depybw = mean(., na.rm=T))) %>% 
  ungroup()

wi_effects =  brms_ml_data[, grepl("caus$", colnames(brms_ml_data))] - brms_ml_data[, grepl("caus_cbw$", colnames(brms_ml_data))] - brms_ml_data[, grepl("caus_ybw$", colnames(brms_ml_data))]
colnames(wi_effects) = gsub("_caus", "_caus_wi", colnames(wi_effects))
wi_effects_dep =  brms_ml_data[, grepl("_context_lag$", colnames(brms_ml_data))] - brms_ml_data[, grepl("depcbw$", colnames(brms_ml_data))] - brms_ml_data[, grepl("depybw$", colnames(brms_ml_data))]
colnames(wi_effects_dep) = gsub("_lag", "_wi_lag", colnames(wi_effects_dep))


brms_ml_data = brms_ml_data %>% 
  bind_cols(wi_effects) %>% 
  bind_cols(wi_effects_dep) %>% 
  group_by(country_name)  %>% 
  mutate_at(vars(ends_with("wi_lag")), funs(lag2 = dplyr::lag(., 1))) %>% 
  mutate_at(vars(ends_with("_wi")), funs(lag = dplyr::lag(., 1))) %>% 
  ungroup()

# Summary Statistics and Visualization

my_vars = brms_ml_data %>% 
  select_at(vars(ends_with("wi"), ends_with("wi_lag"), ends_with("bw"), -matches("context"), -matches("pubequal")))
psych::describe(my_vars) %>% 
  mutate(Variable = row.names(.))  %>% 
  select(Variable, n, mean, sd, min, max) %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>% 
  dust() %>% 
  pixiedust::sprinkle_print_method(print_method = "html")

brms_ml_data %>% 
  ungroup() %>% 
  select_at(vars(matches("_caus_"), -classification_context)) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  #mutate(value = transformTukey(value, statistic = 2, plotit=F, quiet=T)) %>%
  ungroup() %>% 
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(name ~ ., scales="free")


# Bayes Results ####

dec_f = TSCS_reg_brms("decision_freedom_context", lag=2)
dec_e = TSCS_reg_brms("decision_equality_context", lag=1)
dec_c = TSCS_reg_brms("decision_control_context", lag=1)


int_f = TSCS_reg_brms("intermediate_freedom_context", lag=2)
int_e = TSCS_reg_brms("intermediate_equality_context", lag=1)
int_c = TSCS_reg_brms("intermediate_control_context", lag=2)

com_f = TSCS_reg_brms("communication_freedom_context", lag=2)
com_e = TSCS_reg_brms("communication_equality_context", lag=1)
com_c = TSCS_reg_brms("communication_control_context", lag=2)

rights_f = TSCS_reg_brms("rights_freedom_context", lag=2)
rights_e = TSCS_reg_brms("rights_equality_context", lag=2)
rights_c = TSCS_reg_brms("rights_control_context", lag=2)

rs_f = TSCS_reg_brms("rule_settlement_freedom_context", lag=2)
rs_e = TSCS_reg_brms("rule_settlement_equality_context", lag=1)
rs_c = TSCS_reg_brms("rule_settlement_control_context", lag=2)

# Save Models

saveRDS(dec_f, file="brmsModels/dec_f.RDS")
saveRDS(dec_e, file="brmsModels/dec_e.RDS")
saveRDS(dec_c, file="brmsModels/dec_c.RDS")
saveRDS(int_f, file="brmsModels/int_f.RDS")
saveRDS(int_e, file="brmsModels/int_e.RDS")
saveRDS(int_c, file="brmsModels/int_c.RDS")
saveRDS(com_f, file="brmsModels/com_f.RDS")
saveRDS(com_e, file="brmsModels/com_e.RDS")
saveRDS(com_c, file="brmsModels/com_c.RDS")
saveRDS(rights_f, file="brmsModels/rights_f.RDS")
saveRDS(rights_e, file="brmsModels/rights_e.RDS")
saveRDS(rights_c, file="brmsModels/rights_c.RDS")
saveRDS(rs_f, file="brmsModels/rs_f.RDS")
saveRDS(rs_e, file="brmsModels/rs_e.RDS")
saveRDS(rs_c, file="brmsModels/rs_c.RDS")

# Load Models

dec_f = readRDS(file="brmsModels/dec_f.RDS")
dec_e = readRDS(file="brmsModels/dec_e.RDS")
dec_c = readRDS(file="brmsModels/dec_c.RDS")
int_f = readRDS(file="brmsModels/int_f.RDS")
int_e = readRDS(file="brmsModels/int_e.RDS")
int_c = readRDS(file="brmsModels/int_c.RDS")
com_f = readRDS(file="brmsModels/com_f.RDS")
com_e = readRDS(file="brmsModels/com_e.RDS")
com_c = readRDS(file="brmsModels/com_c.RDS")
rights_f = readRDS(file="brmsModels/rights_f.RDS")
rights_e = readRDS(file="brmsModels/rights_e.RDS")
rights_c = readRDS(file="brmsModels/rights_c.RDS")
rs_f = readRDS(file="brmsModels/rs_f.RDS")
rs_e = readRDS(file="brmsModels/rs_e.RDS")
rs_c = readRDS(file="brmsModels/rs_c.RDS")


# LRE ####
make_LRE(dec_f, "decision_freedom_context")
make_LRE(dec_e, "decision_equality_context")
make_LRE(dec_c, "decision_control_context")
make_LRE(int_f, "intermediate_freedom_context")
make_LRE(int_e, "intermediate_equality_context")
make_LRE(int_c, "intermediate_control_context")
make_LRE(com_f, "communication_freedom_context")
make_LRE(com_e, "communication_equality_context")
make_LRE(com_c, "communication_control_context")
make_LRE(rights_f, "rights_freedom_context")
make_LRE(rights_e, "rights_equality_context")
make_LRE(rights_c, "rights_control_context")
make_LRE(rs_f, "rule_settlement_freedom_context")
make_LRE(rs_e, "rule_settlement_equality_context")
make_LRE(rs_c, "rule_settlement_control_context")

make_LRE(dec_f, "decision_freedom_context", credmass = 0.9)
make_LRE(dec_e, "decision_equality_context", credmass = 0.9)
make_LRE(dec_c, "decision_control_context", credmass = 0.9)
make_LRE(int_f, "intermediate_freedom_context", credmass = 0.9)
make_LRE(int_e, "intermediate_equality_context", credmass = 0.9)
make_LRE(int_c, "intermediate_control_context", credmass = 0.9)
make_LRE(com_f, "communication_freedom_context", credmass = 0.9)
make_LRE(com_e, "communication_equality_context", credmass = 0.9)
make_LRE(com_c, "communication_control_context", credmass = 0.9)
make_LRE(rights_f, "rights_freedom_context", credmass = 0.9)
make_LRE(rights_e, "rights_equality_context", credmass = 0.9)
make_LRE(rights_c, "rights_control_context", credmass = 0.9)
make_LRE(rs_f, "rule_settlement_freedom_context", credmass = 0.9)
make_LRE(rs_e, "rule_settlement_equality_context", credmass = 0.9)
make_LRE(rs_c, "rule_settlement_control_context", credmass = 0.9)

summary(int_e, prob=0.9)
summary(com_e, prob=0.9)
summary(rights_e, prob=0.9)
summary(rs_e, prob=0.9)


###


library(bayesplot)
ppc_dens_overlay(y = dec_f$data$decision_freedom_context,
                 yrep = posterior_predict(dec_f)[50:150, ])

# Residuals

plot_residuals(dec_f, "Decision Freedrom") # Peru, Turkey, Venezuela, USA, Hungary
plot_residuals(dec_e, "Decision Equality") # Peru, Nicaragua, Venezuela
plot_residuals(dec_c, "Decision Control") # Peru, Venezuela

plot_residuals_fitted(dec_f, all=T)
plot_residuals_fitted(dec_e, all=T)

plot_residuals(int_f, "Int Freedom") # Turkey, Venezuela, Peru
plot_residuals(int_e, "Int Equality") # Turkey, Brazil, Venezuela, India
plot_residuals(int_c, "Int Control")# Ecuador, Venezuela, Peru

plot_residuals(com_f, "Com Freedom") # Ecuador, Venezuela, Peru
plot_residuals(com_e, "Com Equality") # Ecuador, Venezuela, Peru
plot_residuals(com_c, "Com Control")

plot_residuals(rights_f, "Rights Freedom")
plot_residuals(rights_e, "Rights Equality")
plot_residuals(rights_c, "Rights Control")

plot_residuals(rs_f, "RS Freedom")
plot_residuals(rs_e, "RS Equality")
plot_residuals(rs_c, "RS Control")

# Without Outliers ####
make_brms_sub = function(model) {
  # dataset =  get_residuals(model) %>%
  #   filter(residuals < 3, residuals > -3)

  dataset =  model$data %>%
    filter(country_name != "Peru", country_name != "Venezuela", country_name != "Nicaragua")
  return(dataset)

}


dec_f_sub = TSCS_reg_brms("decision_freedom_context", lag=2, 
                          make_brms_sub(dec_f))
dec_e_sub = TSCS_reg_brms("decision_equality_context", lag=1, 
                          make_brms_sub(dec_e))
dec_c_sub = TSCS_reg_brms("decision_control_context", lag=1, 
                          make_brms_sub(dec_c))

int_f_sub = TSCS_reg_brms("intermediate_freedom_context", lag=2, 
                          make_brms_sub(int_f))
int_e_sub = TSCS_reg_brms("intermediate_equality_context", lag=1, 
                          make_brms_sub(int_e))
int_c_sub = TSCS_reg_brms("intermediate_control_context", lag=2, 
                          make_brms_sub(int_c))

com_f_sub = TSCS_reg_brms("communication_freedom_context", lag=2, 
                          make_brms_sub(com_f))
com_e_sub = TSCS_reg_brms("communication_equality_context", lag=1, 
                          make_brms_sub(com_e))
com_c_sub = TSCS_reg_brms("communication_control_context", lag=2, 
                          make_brms_sub(com_c))

rights_f_sub = TSCS_reg_brms("rights_freedom_context", lag=2, 
                             make_brms_sub(rights_f))
rights_e_sub = TSCS_reg_brms("rights_equality_context", lag=2, 
                             make_brms_sub(rights_e))
rights_c_sub = TSCS_reg_brms("rights_control_context", lag=2, 
                             make_brms_sub(rights_c))

rs_f_sub = TSCS_reg_brms("rule_settlement_freedom_context", lag=2, 
                         make_brms_sub(rs_f))
rs_e_sub = TSCS_reg_brms("rule_settlement_equality_context", lag=1, 
                         make_brms_sub(rs_e))
rs_c_sub = TSCS_reg_brms("rule_settlement_control_context", lag=2, 
                         make_brms_sub(rs_c))

# saveRDS(rs_c_sub, file="brmsModels/rs_c_sub_wPVN.RDS")

saveRDS(dec_f_sub, file="brmsModels/dec_f_sub_wPVN.RDS")
saveRDS(dec_e_sub, file="brmsModels/dec_e_sub_wPVN.RDS")
saveRDS(dec_c_sub, file="brmsModels/dec_c_sub_wPVN.RDS")
saveRDS(int_f_sub, file="brmsModels/int_f_sub_wPVN.RDS")
saveRDS(int_e_sub, file="brmsModels/int_e_sub_wPVN.RDS")
saveRDS(int_c_sub, file="brmsModels/int_c_sub_wPVN.RDS")
saveRDS(com_f_sub, file="brmsModels/com_f_sub_wPVN.RDS")
saveRDS(com_e_sub, file="brmsModels/com_e_sub_wPVN.RDS")
saveRDS(com_c_sub, file="brmsModels/com_c_sub_wPVN.RDS")
saveRDS(rights_f_sub, file="brmsModels/rights_f_sub_wPVN.RDS")
saveRDS(rights_e_sub, file="brmsModels/rights_e_sub_wPVN.RDS")
saveRDS(rights_c_sub, file="brmsModels/rights_c_sub_wPVN.RDS")
saveRDS(rs_f_sub, file="brmsModels/rs_f_sub_wPVN.RDS")
saveRDS(rs_e_sub, file="brmsModels/rs_e_sub_wPVN.RDS")
saveRDS(rs_c_sub, file="brmsModels/rs_c_sub_wPVN.RDS")

# Load Models
# generelle Ausreßer eliminiert: Residuen > 3 & <-3
dec_f_sub = readRDS(file="brmsModels/dec_f_sub.RDS")
dec_e_sub = readRDS(file="brmsModels/dec_e_sub.RDS")
dec_c_sub = readRDS(file="brmsModels/dec_c_sub.RDS")
int_f_sub = readRDS(file="brmsModels/int_f_sub.RDS")
int_e_sub = readRDS(file="brmsModels/int_e_sub.RDS")
int_c_sub = readRDS(file="brmsModels/int_c_sub.RDS")
com_f_sub = readRDS(file="brmsModels/com_f_sub.RDS")
com_e_sub = readRDS(file="brmsModels/com_e_sub.RDS")
com_c_sub = readRDS(file="brmsModels/com_c_sub.RDS")
rights_f_sub = readRDS(file="brmsModels/rights_f_sub.RDS")
rights_e_sub = readRDS(file="brmsModels/rights_e_sub.RDS")
rights_c_sub = readRDS(file="brmsModels/rights_c_sub.RDS")
rs_f_sub = readRDS(file="brmsModels/rs_f_sub.RDS")
rs_e_sub = readRDS(file="brmsModels/rs_e_sub.RDS")
rs_c_sub = readRDS(file="brmsModels/rs_c_sub.RDS")

# Ohne Peru, Venezuela und Nicaragua
dec_f_sub = readRDS(file="brmsModels/dec_f_sub_wPVN.RDS")
dec_e_sub = readRDS(file="brmsModels/dec_e_sub_wPVN.RDS")
dec_c_sub = readRDS(file="brmsModels/dec_c_sub_wPVN.RDS")
int_f_sub = readRDS(file="brmsModels/int_f_sub_wPVN.RDS")
int_e_sub = readRDS(file="brmsModels/int_e_sub_wPVN.RDS")
int_c_sub = readRDS(file="brmsModels/int_c_sub_wPVN.RDS")
com_f_sub = readRDS(file="brmsModels/com_f_sub_wPVN.RDS")
com_e_sub = readRDS(file="brmsModels/com_e_sub_wPVN.RDS")
com_c_sub = readRDS(file="brmsModels/com_c_sub_wPVN.RDS")
rights_f_sub = readRDS(file="brmsModels/rights_f_sub_wPVN.RDS")
rights_e_sub = readRDS(file="brmsModels/rights_e_sub_wPVN.RDS")
rights_c_sub = readRDS(file="brmsModels/rights_c_sub_wPVN.RDS")
rs_f_sub = readRDS(file="brmsModels/rs_f_sub_wPVN.RDS")
rs_e_sub = readRDS(file="brmsModels/rs_e_sub_wPVN.RDS")
rs_c_sub = readRDS(file="brmsModels/rs_c_sub_wPVN.RDS")



make_LRE(dec_f_sub, "decision_freedom_context", credmass = 0.95)
make_LRE(dec_e_sub, "decision_equality_context", credmass = 0.95)
make_LRE(dec_c_sub, "decision_control_context", credmass = 0.95)
make_LRE(int_f_sub, "intermediate_freedom_context", credmass = 0.95)
make_LRE(int_e_sub, "intermediate_equality_context", credmass = 0.95)
make_LRE(int_c_sub, "intermediate_control_context", credmass = 0.95)
make_LRE(com_f_sub, "communication_freedom_context", credmass = 0.95)
make_LRE(com_e_sub, "communication_equality_context", credmass = 0.95)
make_LRE(com_c_sub, "communication_control_context", credmass = 0.95)
make_LRE(rights_f_sub, "rights_freedom_context", credmass = 0.95)
make_LRE(rights_e_sub, "rights_equality_context", credmass = 0.95)
make_LRE(rights_c_sub, "rights_control_context", credmass = 0.95)
make_LRE(rs_f_sub, "rule_settlement_freedom_context", credmass = 0.95)
make_LRE(rs_e_sub, "rule_settlement_equality_context", credmass = 0.95)
make_LRE(rs_c_sub, "rule_settlement_control_context", credmass = 0.95)

# Residuals
plot_residuals(dec_f_sub, "Decision Freedrom Sub")
plot_residuals(dec_e_sub, "Decision Equality Sub") 
plot_residuals(dec_c_sub, "Decision Control Sub")

plot_residuals(dec_e_sub, "Decision Equality Sub", all=T) 
plot_residuals_fitted(dec_e_sub, all=T)


plot_residuals(int_f_sub, "Int Freedom Sub") 
plot_residuals(int_e_sub, "Int Equality Sub") 
plot_residuals(int_c_sub, "Int Control Sub")

plot_residuals(com_f_sub, "Com Freedom Sub") 
plot_residuals(com_e_sub, "Com Equality Sub")
plot_residuals(com_c_sub, "Com Control Sub")

plot_residuals(rights_f_sub, "Rights Freedom Sub")
plot_residuals(rights_e_sub, "Rights Equality Sub")
plot_residuals(rights_c_sub, "Rights Control Sub")

plot_residuals(rs_f_sub, "RS Freedom Sub")
plot_residuals(rs_e_sub, "RS Equality Sub")
plot_residuals(rs_c_sub, "RS Control Sub")
