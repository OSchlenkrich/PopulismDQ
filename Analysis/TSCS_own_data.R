source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Setup/MergeDatasets_w_OwnData.R")



TSCS_data = DMX_populist %>% 
  mutate(
         populist_party_exists_lag = dplyr::lag(if_else(populist > 0, 1, 0), 1),
         populist_percent_seats_lag = dplyr::lag(if_else(populist_perc_seats > 1, 1, populist_perc_seats),1),
         populist_is_gov_lag = dplyr::lag(if_else(pop_gov > 0, 1, 0), 1),
         
    
         populist_in_cabinet_lag = dplyr::lag(if_else(populist_cabinet > 0, 1, 0), 1),

         gdp_growth_lag = dplyr::lag(gdp_growth, 1),
         v2csantimv_lag = dplyr::lag(v2csantimv, 1),
         
         total_index_context_lag = dplyr::lag(total_index_context, 1),
         decision_inst_index_context_lag = dplyr::lag(decision_inst_index_context, 1),
         intermediate_inst_index_context_lag = dplyr::lag(intermediate_inst_index_context, 1),
         communication_inst_index_context_lag = dplyr::lag(communication_inst_index_context, 1),
         rights_inst_index_context_lag = dplyr::lag(rights_inst_index_context, 1),
         rule_settlement_inst_index_context_lag = dplyr::lag(rule_settlement_inst_index_context, 1),
         
         freedom_dim_index_context_lag = dplyr::lag(freedom_dim_index_context, 1),
         equality_dim_index_context_lag = dplyr::lag(equality_dim_index_context, 1),
         control_dim_index_context_lag = dplyr::lag(control_dim_index_context, 1),
         
         decision_freedom_context_lag = dplyr::lag(decision_freedom_context, 1),
         decision_equality_context_lag = dplyr::lag(decision_equality_context, 1),
         decision_control_context_lag = dplyr::lag(decision_control_context, 1),
         
         intermediate_freedom_context_lag = dplyr::lag(intermediate_freedom_context, 1),
         intermediate_equality_context_lag = dplyr::lag(intermediate_equality_context, 1),
         intermediate_control_context_lag = dplyr::lag(intermediate_control_context, 1),
         
         communication_freedom_context_lag = dplyr::lag(communication_freedom_context, 1),
         communication_equality_context_lag = dplyr::lag(communication_equality_context, 1),
         communication_control_context_lag = dplyr::lag(communication_control_context, 1),
         
         rights_freedom_context_lag = dplyr::lag(rights_freedom_context, 1),
         rights_equality_context_lag = dplyr::lag(rights_equality_context, 1),
         rights_control_context_lag = dplyr::lag(rights_control_context, 1),
         
         rule_settlement_freedom_context_lag = dplyr::lag(rule_settlement_freedom_context, 1),
         rule_settlement_equality_context_lag = dplyr::lag(rule_settlement_equality_context, 1),
         rule_settlement_control_context_lag = dplyr::lag(rule_settlement_control_context, 1),
  )  %>% 
  filter(year >= 2000) %>% 
  mutate(trend = year - min(year)) %>% 
  filter(country_name %in% Pop_Countries)  %>% 
  filter(country_name != "Greece") 



TSCS_reg = function(variable) {
  raw_formula = paste("placeholder ~ -1 + 
          placeholder_lag  + 
          trend + 
          pop_gov + 
          gdp_growth_lag + country_name", collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))

  TSCS_obj = plm(my_tscs_formula, 
                 index=c("country_name", "year"), 
                 as.data.frame(TSCS_data),
                 model="pooling")
  print(summary(TSCS_obj))
  return(coeftest(TSCS_obj, vcov=vcovBK))
}



TSCS_reg("decision_freedom_context")
TSCS_reg("decision_equality_context")
TSCS_reg("decision_control_context")

TSCS_reg("intermediate_freedom_context")
TSCS_reg("intermediate_equality_context")
TSCS_reg("intermediate_control_context")

TSCS_reg("communication_freedom_context")
TSCS_reg("communication_equality_context")
TSCS_reg("communication_control_context")

TSCS_reg("rights_freedom_context")
TSCS_reg("rights_equality_context")
TSCS_reg("rights_control_context")

TSCS_reg("rule_settlement_freedom_context")
TSCS_reg("rule_settlement_equality_context")
TSCS_reg("rule_settlement_control_context")
