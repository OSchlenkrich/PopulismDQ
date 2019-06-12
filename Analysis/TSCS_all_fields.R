
Unit_Response = function(variable, coef_mat, Time = 11, x=1) {
  coef = grepl(variable, names(coef_mat[,1]))
  
  x_lag = 0
  
  results = array(NA, Time)
  results[1] = 0
  results[2] = sum(0 * y + coef_mat[coef,1]%*%c(x, x_lag))
  
  x_lag = x
  
  for (i in 3:Time) {
    results[i] = sum(coef_mat[1,1] * results[i-1] + coef_mat[coef,1]%*%c(x, x_lag))
  }
  
  results = data.frame(response = results, Time = 1:Time)
  
  ggplot(results, aes(x=Time, y=response)) +
    geom_line(size=1.1) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks=seq(1,Time,1))
}




## Vote Share
TSCS_reg_vote = function(variable) {
  raw_formula = paste("placeholder ~ -1 +",
                      dummies_string,
                      " + placeholder_lag  + 
          trend +
          populist_vote_share_tr + 
          populist_vote_share_tr_lag +
          gdp_growth_lag ", collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  TSCS_obj = plm(my_tscs_formula, 
                 index=c("country_name", "year"), 
                 as.data.frame(TSCS_data),
                 model="pooling")
  print(summary(TSCS_obj))
  coef_mat = coeftest(TSCS_obj, vcov=vcovBK)
  return(round(coef_mat,5))
}


TSCS_reg_vote("decision_freedom_context")
TSCS_reg_vote("decision_equality_context")
TSCS_reg_vote("decision_control_context")

TSCS_reg_vote("intermediate_freedom_context")
TSCS_reg_vote("intermediate_equality_context")
TSCS_reg_vote("intermediate_control_context")

TSCS_reg_vote("communication_freedom_context")
TSCS_reg_vote("communication_equality_context")
TSCS_reg_vote("communication_control_context")

TSCS_reg_vote("rights_freedom_context")
TSCS_reg_vote("rights_equality_context")
TSCS_reg_vote("rights_control_context")

TSCS_reg_vote("rule_settlement_freedom_context")
TSCS_reg_vote("rule_settlement_equality_context")
TSCS_reg_vote("rule_settlement_control_context")

Unit_Response("populist_vote_share", TSCS_reg_vote("decision_freedom_context"), 
              Time=20, 
              x=20)


Unit_Response("populist_vote_share", TSCS_reg_vote("rule_settlement_control_context"), 
              Time=20, 
              x=20)
##
TSCS_reg_cabinet = function(variable) {
  raw_formula = paste("placeholder ~ -1 +",
                      dummies_string,
          " + placeholder_lag  + 
          trend +
          populist_in_cabinet + 
          populist_in_cabinet_lag +
          gdp_growth_lag ", collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  TSCS_obj = plm(my_tscs_formula, 
                 index=c("country_name", "year"), 
                 as.data.frame(TSCS_data),
                 model="pooling")
  print(summary(TSCS_obj))
  coef_mat = coeftest(TSCS_obj, vcov=vcovBK)
  return(round(coef_mat,3))
}


TSCS_reg_cabinet("decision_freedom_context")
TSCS_reg_cabinet("decision_equality_context")
TSCS_reg_cabinet("decision_control_context")

TSCS_reg_cabinet("intermediate_freedom_context")
TSCS_reg_cabinet("intermediate_equality_context")
TSCS_reg_cabinet("intermediate_control_context")

TSCS_reg_cabinet("communication_freedom_context")
TSCS_reg_cabinet("communication_equality_context")
TSCS_reg_cabinet("communication_control_context")

TSCS_reg_cabinet("rights_freedom_context")
TSCS_reg_cabinet("rights_equality_context")
TSCS_reg_cabinet("rights_control_context")

TSCS_reg_cabinet("rule_settlement_freedom_context")
TSCS_reg_cabinet("rule_settlement_equality_context")
TSCS_reg_cabinet("rule_settlement_control_context")

Unit_Response("populist_in_cabinet", TSCS_reg("decision_freedom_context"), 10)

Unit_Response("populist_in_cabinet", TSCS_reg("communication_freedom_context"))
Unit_Response("populist_in_cabinet", TSCS_reg("communication_control_context"))

Unit_Response("populist_in_cabinet", TSCS_reg("rights_control_context"), 10)
Unit_Response("populist_in_cabinet", TSCS_reg("rule_settlement_control_context"), 10)


