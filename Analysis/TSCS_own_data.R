source("Setup/Packages.R")
source("Setup/Impulse_Unit_Response.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Setup/MergeDatasets_w_OwnData.R")



TSCS_data = DMX_populist %>% 
  mutate(pop_cat = if_else(populist_is_gov == 1, "HOG",
                           if_else(populist_cabinet >= 1 & populist_is_gov == 0, "Cabinet",
                                   if_else(populist_cabinet == 0 & populist_perc_seats >= 0.1, "Opposition", "No Populist Party")
                           )),
         pop_cat = if_else(is.na(pop_cat)==T, "No Populist Party", pop_cat),
         pop_cat = relevel(as.factor(pop_cat), ref="No Populist Party")) %>% 
  group_by(country_name) %>% 
  mutate(
         populist_party_exists_lag = dplyr::lag(if_else(populist > 0, 1, 0), 1),
         populist_percent_seats_lag = dplyr::lag(populist_perc_seats,1),
         populist_in_cabinet_lag = dplyr::lag(if_else(populist_cabinet > 0, 1, 0), 1),
         populist_is_gov_lag = dplyr::lag(if_else(populist_is_gov > 0, 1, 0), 1),
         pop_cat_lag = dplyr::lag(pop_cat, 1),
         

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
  filter(year >= 1990) %>% 
  mutate(trend = year - min(year)) %>% 
  filter(country_name %in% Pop_Countries)  %>% 
  filter(country_name != "Greece") 


# Government
TSCS_reg = function(variable) {
  my_tscs_formula = Make_formula(variable)

  TSCS_obj = plm(my_tscs_formula, 
                 index=c("country_name", "year"), 
                 as.data.frame(TSCS_data),
                 model="pooling")
  print(summary(TSCS_obj))
  coef_mat = coeftest(TSCS_obj, vcov.=function(x) vcovBK(x, cluster="time"))
  return(round(coef_mat,5))
}

P_W_reg = function(variable) {
  my_tscs_formula = Make_formulaAR(variable)
  

  mAR = panelAR(my_tscs_formula,
          panelVar = "country_name",
          timeVar = "year",
          autoCorr = "psar1",
          panelCorrMethod = "pcse",
          rhotype = "scorr",
          as.data.frame(TSCS_data))
  return(summary(mAR))
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

# Prais-Winsten
P_W_reg("decision_freedom_context")
P_W_reg("decision_equality_context")
P_W_reg("decision_control_context")

P_W_reg("intermediate_freedom_context")
P_W_reg("intermediate_equality_context")
P_W_reg("intermediate_control_context")

P_W_reg("communication_freedom_context")
P_W_reg("communication_equality_context")
P_W_reg("communication_control_context")

P_W_reg("rights_freedom_context")
P_W_reg("rights_equality_context")
P_W_reg("rights_control_context")

P_W_reg("rule_settlement_freedom_context")
P_W_reg("rule_settlement_equality_context")
P_W_reg("rule_settlement_control_context")


Unit_Response("populist_is_gov", 
              Time=10, 
              x=1, 
              "rights_control_context")

Impulse_Response("populist_is_gov", 
                 Time=10, 
                 x=1, 
                 "rights_control_context")



# Dynamic Simulaton


M1 = lm(Make_formula("rule_settlement_control_context"), TSCS_data)
summary(M1)
library(dynsim)

Sc1 = data.frame(rule_settlement_control_context_lag = mean(TSCS_data$rule_settlement_control_context_lag, na.rm=T),
           trend = mean(TSCS_data$trend, na.rm=T),
           country_nameTurkey = 1,
           pop_catHOG = 0,
           pop_cat_lagHOG = 0
           )
Sc2 = data.frame(rule_settlement_control_context_lag = mean(TSCS_data$rule_settlement_control_context_lag, na.rm=T),
                 trend = mean(TSCS_data$trend, na.rm=T),
                 country_nameTurkey = 1,
                 pop_catHOG = 1,
                 pop_cat_lagHOG = 1
)
mySim = dynsim(obj = M1, ldv = "rule_settlement_control_context_lag", scen = list(Sc1, Sc2), n = 20)
dynsimGG(mySim) + ylim(0,1)
