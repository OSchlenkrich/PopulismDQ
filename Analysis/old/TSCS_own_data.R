source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Setup/MergeDatasets_w_OwnData.R")
source("Setup/Impulse_Unit_Response.R")



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
              "decision_freedom_context")

Impulse_Response("populist_is_gov", 
                 Time=10, 
                 x=1, 
                 "rights_control_context")



# Dynamic Simulaton


make_dyn_sample = function(variable, Plot=F) {
  variable_lag = paste(variable, "_lag", sep="")
  
  M1 = lm(Make_formula(variable), TSCS_data)
  # summary(M1)
  library(dynsim)
  
  # Sc1 = data.frame(TSCS_data %>%
  #                    ungroup() %>% 
  #                    select(variable_lag) %>% 
  #                    summarize_all(mean, na.rm=T),
  #                  trend = mean(TSCS_data$trend, na.rm=T),
  #                  country_nameTurkey = 1,
  #                  pop_catHOG = 0,
  #                  pop_cat_lagHOG = 0
  # )
  
  
  Sc2 = data.frame(TSCS_data %>%
                     ungroup() %>% 
                     select(variable_lag) %>% 
                     summarize_all(mean, na.rm=T),
                   trend = mean(TSCS_data$trend, na.rm=T),
                   country_namePoland = 1,
                   pop_catHOG = 1,
                   pop_cat_lagHOG = 1
  )
  mySim = dynsim(obj = M1, ldv = variable_lag, scen = list(Sc2), n = 20)
  
  if (Plot == T) {
    return(  dynsimGG(mySim) + ylim(0,1))    
  } else {
    return(mySim  %>% 
             mutate(variable = variable) %>% 
             mutate(country_name = "Turkey"))
  }
}





matrix_dyn_sample = make_dyn_sample("decision_freedom_context") %>% 
  bind_rows(make_dyn_sample("decision_equality_context")) %>% 
  bind_rows(make_dyn_sample("decision_control_context")) %>% 
  bind_rows(make_dyn_sample("intermediate_freedom_context")) %>% 
  bind_rows(make_dyn_sample("intermediate_equality_context")) %>% 
  bind_rows(make_dyn_sample("intermediate_control_context")) %>% 
  bind_rows(make_dyn_sample("communication_freedom_context")) %>% 
  bind_rows(make_dyn_sample("communication_equality_context")) %>% 
  bind_rows(make_dyn_sample("communication_control_context")) %>% 
  bind_rows(make_dyn_sample("rights_freedom_context")) %>% 
  bind_rows(make_dyn_sample("rights_equality_context")) %>% 
  bind_rows(make_dyn_sample("rights_control_context")) %>% 
  bind_rows(make_dyn_sample("rule_settlement_freedom_context")) %>% 
  bind_rows(make_dyn_sample("rule_settlement_equality_context")) %>% 
  bind_rows(make_dyn_sample("rule_settlement_control_context"))

matrix_format = matrix_dyn_sample %>% 
  select(country_name, year = time, ldvMean, variable) %>% 
  spread(variable, ldvMean)  %>%
  mutate(
    freedom_dim_index_context =  (decision_freedom_context * intermediate_freedom_context * communication_freedom_context * rights_freedom_context *rule_settlement_freedom_context)^(1/5),
    equality_dim_index_context =  (decision_equality_context * intermediate_equality_context * communication_equality_context * rights_equality_context *rule_settlement_equality_context)^(1/5),
    control_dim_index_context =  (decision_control_context * intermediate_control_context * communication_control_context * rights_control_context *rule_settlement_control_context)^(1/5),
    
    decision_inst_index_context = (decision_freedom_context * decision_equality_context * decision_control_context)^(1/3),
    intermediate_inst_index_context = (intermediate_freedom_context * intermediate_equality_context * intermediate_control_context)^(1/3),
    communication_inst_index_context = (communication_freedom_context * communication_equality_context * communication_control_context)^(1/3),
    rights_inst_index_context = (rights_freedom_context * rights_equality_context * rights_control_context)^(1/3),
    rule_settlement_inst_index_context = (rule_settlement_freedom_context * rule_settlement_equality_context * rule_settlement_control_context)^(1/3),
    
    total_index_context = (freedom_dim_index_context * equality_dim_index_context * control_dim_index_context)^(1/3)
  ) 


test = vis_15_Felder_raw("Turkey", 1, "Context Measurement", matrix_format) %>% 
  bind_rows(vis_15_Felder_raw("Turkey", 2, "Context Measurement", matrix_format)) %>% 
  bind_rows(vis_15_Felder_raw("Turkey", 3, "Context Measurement", matrix_format)) %>% 
  bind_rows(vis_15_Felder_raw("Turkey", 4, "Context Measurement", matrix_format)) %>% 
  bind_rows(vis_15_Felder_raw("Turkey", 5, "Context Measurement", matrix_format)) %>% 
  bind_rows(vis_15_Felder_raw("Turkey", 6, "Context Measurement", matrix_format)) %>% 
  bind_rows(vis_15_Felder_raw("Turkey", 7, "Context Measurement", matrix_format)) %>% 
  bind_rows(vis_15_Felder_raw("Turkey", 8, "Context Measurement", matrix_format)) %>% 
  bind_rows(vis_15_Felder_raw("Turkey", 9, "Context Measurement", matrix_format)) %>% 
  bind_rows(vis_15_Felder_raw("Turkey", 10, "Context Measurement", matrix_format)) %>% 
  # bind_rows(vis_15_Felder_raw("Turkey", 11, "Context Measurement", matrix_format)) %>% 
  # bind_rows(vis_15_Felder_raw("Turkey", 12, "Context Measurement", matrix_format)) %>% 
  # bind_rows(vis_15_Felder_raw("Turkey", 13, "Context Measurement", matrix_format)) %>% 
  # bind_rows(vis_15_Felder_raw("Turkey", 14, "Context Measurement", matrix_format)) %>% 
  # bind_rows(vis_15_Felder_raw("Turkey", 15, "Context Measurement", matrix_format)) %>% 
  mutate(value = round(value, 2),
         year = as.integer(year)) 

yaxis <- factor(c("Dimension", "Rules\n settlement", "Rights", "Communication", "Intermediate\n Sphere", "Procedurs\n of \nDecision"), levels=c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement", "Dimension"))
xaxis <- factor(c("Freedom", "Equality", "Control", "Institution"), levels=c("Freedom", "Equality", "Control", "Institution"))
low = "#cf4c02"
mid = "#e9df10"
high= "#3cc816"
midpoint = 0.5
limit=c(0,1)

test$group <- seq_len(nrow(test))

mydynamicplot = ggplot(data = test, aes(x = Dim, y = Int, fill=value, group = group)) +  
  geom_raster ()  + 
  geom_text(aes(Dim, Int, label = round(value,2)), color = "black", size = 4, hjust=0.5, vjust=0.5) + 
  scale_y_discrete(limits=yaxis, expand = c(0, 0)) +  
  scale_x_discrete(limits=xaxis, expand = c(0, 0), position = "top") +  
  theme(legend.position = "none", axis.title = element_blank(), plot.title = element_text(size=10, hjust=0.5), axis.text.x = element_text(face="bold"), axis.text.y = element_text(face="bold")) + 
  geom_rect(aes(xmin=0.5, xmax=4.5, ymin=0.5, ymax=1.5), fill=NA, color="black", size=1.5) + 
  geom_rect(aes(xmin=3.5, xmax=4.5, ymin=0.5, ymax=6.5), fill=NA, color="black", size=1.5) + 
  scale_fill_gradient2(low = low, high = high, mid=mid, midpoint = midpoint, limit = limit, space = "Lab", name="DQ") +
  transition_states(year, wrap=F, state_length = 5, transition_length = 1)   +
  labs(title = "Populist Government - Time: {closest_state}") 

anim_save("PDF/DynMatrix_gut.gif", mydynamicplot)

