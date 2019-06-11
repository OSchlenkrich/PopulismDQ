TSCS_reg = function(variable) {
  raw_formula = paste("placeholder ~ -1 + 
          placeholder_lag  + 
          trend +
          populist_in_cabinet_lag +
          populist_percent_seats_lag  + 
          populist_is_prime_minister_lag + 
          gdp_growth_lag +", dummies_string, collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  TSCS_obj = plm(my_tscs_formula, 
                 index=c("country_name", "year"), 
                 as.data.frame(TSCS_data),
                 model="pooling")
  print(summary(decision_e_m))
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


