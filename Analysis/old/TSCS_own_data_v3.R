source("Setup/Packages.R")
source("Setup/basicfunctions.R")
source("Setup/Load_Clean_Data.R")
source("Setup/MergeDatasets_w_OwnData.R")
source("Setup/Impulse_Unit_Response.R")

library(fastDummies)
library(rcompanion)

TSCS_data_trans = DMX_populist %>% 
  filter(country_name %in% Pop_Countries) %>% 
  mutate(classification_context = ifelse(country_name == "Mexico" & year < 1996, "", classification_context),
         classification_context = ifelse(country_name == "Estonia" & year <= 1991, "", classification_context),
         classification_context = ifelse(country_name == "Turkey" & year < 2000, "", classification_context)) %>% 
  filter(year >= 1990, classification_context != "", classification_context != "Hard Autocracy") %>% 
  dummy_cols(., "pop_cat", remove_first_dummy = TRUE) %>%
  group_by(country_name) %>% 
  mutate(summe = sum(pop_cat_Cabinet + pop_cat_HOG + pop_cat_Opposition)) %>% 
  filter(summe != 0)  %>% 
  dplyr::select(-summe) %>% 
  ungroup() %>% 
  
  rename(pop_cab_caus =  pop_cat_Cabinet, pop_HOG_caus = pop_cat_HOG, pop_Opp_caus = pop_cat_Opposition) %>%  
  rename(gdp_growth_caus = gdp_growth)  %>% 
  
  #mutate_at(vars(ends_with("context"), -classification_context), funs(transformTukey((.-min(., na.rm=T)) + 1, statistic = 1, plotit=F, quiet=T)) ) %>% 
  
  group_by(country_name) %>% 
  mutate_at(vars(ends_with("caus")), funs(lag = dplyr::lag(., 1))) %>% 
  mutate_at(vars(ends_with("caus")), funs(df = . - dplyr::lag(., 1))) %>%
  
  group_by(country_name) %>% 
  mutate_at(vars(ends_with("context"), -classification_context), funs(lag = dplyr::lag(., 1))) %>% 
  mutate_at(vars(ends_with("context"), -classification_context), funs(lag2 = dplyr::lag(., 2))) %>% 
  mutate_at(vars(ends_with("context"), -classification_context), funs(lag3 = dplyr::lag(., 3))) %>% 
  mutate_at(vars(ends_with("context"), -classification_context), funs(df = . - dplyr::lag(., 1))) %>% 
  ungroup() %>% 
  mutate(trend = year - min(year)) 


tukey = TSCS_data %>% 
  filter(country_name %in% Pop_Countries) %>% 
  filter(year >= 1990, classification_context != "") %>% 
  dummy_cols(., "pop_cat", remove_first_dummy = TRUE) %>%
  group_by(country_name) %>% 
  mutate(summe = sum(pop_cat_Cabinet + pop_cat_HOG + pop_cat_Opposition)) %>% 
  filter(summe != 0)  %>% 
  dplyr::select(-summe) %>% 
  ungroup() %>% 
  select_at(vars(ends_with("context"), -classification_context)) %>% 
  data.frame()



store_lambdas = data.frame(variable = rep(NA, dim(tukey)[2]), lambda = rep(NA, dim(tukey)[2]))

for (i in 1:dim(tukey)[2]) {
  store_lambdas$variable[i] = colnames(tukey)[i]
  store_lambdas$min[i] = min(tukey[,i], na.rm = T)
  store_lambdas$lambda[i] = transformTukey((tukey[,i]- store_lambdas$min[i]) + 1, statistic = 1, plotit =F, quiet = T, returnLambda = T)
}


reverse_transformation = function(values, selvariable) {
  
  selectedlambda = store_lambdas %>% 
    filter(grepl(selvariable, variable)) %>% 
    pull(lambda)
  selectedmin = store_lambdas %>% 
    filter(grepl(selvariable, variable)) %>% 
    pull(min)
  
  x = values
  
  x_trans = (x ^ (1/selectedlambda)) + selectedmin - 1
  return(x_trans)
}


# Visualize
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


test = TSCS_data_trans %>% 
  group_by(country_name) %>% 
  filter(is.na(decision_freedom_context_lag) == F) %>% 
  summarise(cnt = n())

test = TSCS_data_trans %>% 
  group_by(country_name) %>% 
  summarise_at(vars(starts_with("pop"), -pop_cat, -ends_with("lag"), -ends_with("df")), funs(sum(., na.rm=T)))

# Functions
Make_formulalag1 = function(variable) {
  raw_formula = paste("placeholder ~ 1 + 
          country_name +
          year +
          placeholder_lag +
          pop_Opp_caus +
          pop_cab_caus +
          pop_HOG_caus +
          
          pop_Opp_caus_lag +
          pop_cab_caus_lag +
          pop_HOG_caus_lag +
          
          gdp_growth_caus +
          gdp_growth_caus_lag",
          collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}

Make_formulalag2 = function(variable) {
  raw_formula = paste("placeholder ~ 1 + 
          country_name +
          year +
          placeholder_lag +
          placeholder_lag2 +
          pop_Opp_caus +
          pop_cab_caus +
          pop_HOG_caus +
          
          pop_Opp_caus_lag +
          pop_cab_caus_lag +
          pop_HOG_caus_lag +
          
          gdp_growth_caus +
          gdp_growth_caus_lag",
                      collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}

Make_formulalag3 = function(variable) {
  raw_formula = paste("placeholder ~ 1 + 
          country_name +
          year + 
          placeholder_lag +
          placeholder_lag2 +
          placeholder_lag3 +
          pop_Opp_caus +
          pop_cab_caus +
          pop_HOG_caus +
          
          pop_Opp_caus_lag +
          pop_cab_caus_lag +
          pop_HOG_caus_lag +
          
          gdp_growth_caus +
          gdp_growth_caus_lag",
                      collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}


# Government
TSCS_reg = function(variable, lag=1, dataset = TSCS_data_trans) {
  if (lag == 1) {
    my_tscs_formula = Make_formulalag1(variable)
    
  }
  if (lag == 2) {
    my_tscs_formula = Make_formulalag2(variable)
    
  }
  if (lag == 3) {
    my_tscs_formula = Make_formulalag3(variable)
    
  }  

  TSCS_obj = plm(my_tscs_formula,
                 index=c("country_name", "year"),
                 as.data.frame(dataset),
                 model="pooling")

  
  print(summary(TSCS_obj))
  coef_mat = coeftest(TSCS_obj, vcov.=function(x) vcovBK(x, cluster="time", type="HC0"))
  auto_corr1 = pbgtest(TSCS_obj, order=1)
  auto_corr2 = pbgtest(TSCS_obj, order=2)
  auto_corr3 = pbgtest(TSCS_obj, order=3)
  return(list(TSCS_obj, round(coef_mat,5), auto_corr1,auto_corr2,auto_corr3))
}

# Check Autocorrelation

TSCS_reg("decision_freedom_context", lag=2)
TSCS_reg("decision_equality_context", lag=1)
TSCS_reg("decision_control_context", lag=1)

TSCS_reg("intermediate_freedom_context", lag=2)
TSCS_reg("intermediate_equality_context", lag=1)
TSCS_reg("intermediate_control_context", lag=2)

TSCS_reg("communication_freedom_context", lag=2)
TSCS_reg("communication_equality_context", lag=1)
TSCS_reg("communication_control_context", lag=2)

TSCS_reg("rights_freedom_context", lag=2)
TSCS_reg("rights_equality_context", lag=2)
TSCS_reg("rights_control_context", lag=3)

TSCS_reg("rule_settlement_freedom_context", lag=2)
TSCS_reg("rule_settlement_equality_context", lag=1)

TSCS_reg("rule_settlement_control_context", lag=2)

TSCS_reg("rule_settlement_control_context", lag=2, dataset = TSCS_data_trans)

# Calculate LRE with BRMS
library(brms)
options(mc.cores = parallel::detectCores())

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
brms_ml_data$decision_control_context_lag_depcbw

brms_ml_data[2, grepl("_context_lag$", colnames(brms_ml_data))]
brms_ml_data[2, grepl("depcbw$", colnames(brms_ml_data))]

brms_ml_data = brms_ml_data %>% 
  bind_cols(wi_effects) %>% 
  bind_cols(wi_effects_dep) %>% 
  group_by(country_name)  %>% 
  mutate_at(vars(ends_with("wi_lag")), funs(lag2 = dplyr::lag(., 1))) %>% 
  mutate_at(vars(ends_with("_wi")), funs(lag = dplyr::lag(., 1))) %>% 
  ungroup()
wi_effects_dep[2,]
names(brms_ml_data)

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
 
  

Make_formulalag1_ml = function(variable) {
  raw_formula = paste("placeholder ~ 1 + 
          placeholder_wi_lag +          
          

          trend +
          pop_Opp_caus_wi +
          pop_cab_caus_wi +
          pop_HOG_caus_wi +
          
          pop_Opp_caus_wi_lag +
          pop_cab_caus_wi_lag +
          pop_HOG_caus_wi_lag +
          
          gdp_growth_caus_wi +
          gdp_growth_caus_wi_lag +
          
          pop_Opp_caus_cbw +
          pop_cab_caus_cbw +
          pop_HOG_caus_cbw +
          gdp_growth_caus_cbw +
          pop_Opp_caus_ybw +
          pop_cab_caus_ybw +
          pop_HOG_caus_ybw +
          gdp_growth_caus_ybw +   
          
          (1|country_name)  +
          (1|year)",
                      collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}

Make_formulalag2_ml = function(variable) {
  raw_formula = paste("placeholder ~ 1 + 
          placeholder_wi_lag +
          placeholder_wi_lag_lag2  +

          trend +
          pop_Opp_caus_wi +
          pop_cab_caus_wi +
          pop_HOG_caus_wi +
          
          pop_Opp_caus_wi_lag +
          pop_cab_caus_wi_lag +
          pop_HOG_caus_wi_lag +
          
          gdp_growth_caus_wi +
          gdp_growth_caus_wi_lag +
          
          pop_Opp_caus_cbw +
          pop_cab_caus_cbw +
          pop_HOG_caus_cbw +
          gdp_growth_caus_cbw +
          pop_Opp_caus_ybw +
          pop_cab_caus_ybw +
          pop_HOG_caus_ybw +
          gdp_growth_caus_ybw +   
          
          (1|country_name)  +
          (1|year)",
                      collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}
TSCS_reg_brms = function(variable, lag=1) {
  
  if (lag == 1) {
    my_tscs_formula = Make_formulalag1_ml(variable)
    
  }
  if (lag == 2) {
    my_tscs_formula = Make_formulalag2_ml(variable)
    
  }
  priors = c(set_prior("normal(0,10)", class = "b"))
  TSCS_obj = brm(bf(my_tscs_formula), 
                 brms_ml_data %>% 
                   mutate(year = as.factor(year)),
                 warmup = 500,
                 iter = 1500,
                 prior =priors,
                 chains=5,
                 backend = "cmdstanr")

  
  return(TSCS_obj)
}




make_LRE = function(model, var, credmass = 0.95, unit=1) {
  library(HDInterval)
  library(ggpubr)
  
  p1 = lag_distribution_both(model, var, 
                        IndV_label = "pop_cab_caus" , 
                        dep_label = var, 
                        unit = unit, time_periods=4, ci=0.95, ecm = F)
  p2 = lag_distribution_both(model, var, 
                             IndV_label = "pop_Opp_caus" , 
                             dep_label = var, 
                             unit = unit, time_periods=4, ci=0.95, ecm = F)
  p3 = lag_distribution_both(model, var, 
                             IndV_label = "pop_HOG_caus" , 
                             dep_label = var, 
                             unit = unit, time_periods=4, ci=0.95, ecm = F)
  
  return(ggarrange(p1,p2,p3))
}


dec_f = TSCS_reg_brms("decision_freedom_context", lag=2)
dec_e = TSCS_reg_brms("decision_equality_context", lag=1)
dec_c = TSCS_reg_brms("decision_control_context", lag=1)

library(bayesplot)
ppc_dens_overlay(y = dec_f$data$decision_freedom_context,
                 yrep = posterior_predict(dec_f)[1:50, ])



make_LRE(dec_f, "decision_freedom_context")
make_LRE(dec_e, "decision_equality_context")
make_LRE(dec_c, "decision_control_context")


int_f = TSCS_reg_brms("intermediate_freedom_context", lag=2)
int_e = TSCS_reg_brms("intermediate_equality_context", lag=1)
int_c = TSCS_reg_brms("intermediate_control_context", lag=2)


make_LRE(int_f, "intermediate_freedom_context")
make_LRE(int_e, "intermediate_equality_context")
make_LRE(int_c, "intermediate_control_context")


com_f = TSCS_reg_brms("communication_freedom_context", lag=2)
com_e = TSCS_reg_brms("communication_equality_context", lag=1)
com_c = TSCS_reg_brms("communication_control_context", lag=2)


make_LRE(com_f, "communication_freedom_context")
make_LRE(com_e, "communication_equality_context")
make_LRE(com_c, "communication_control_context")


# saveRDS(rights_f, file="brmsModels/rights_f.RDS")
# rights_f = readRDS(file="brmsModels/rights_f.RDS")

rights_f = TSCS_reg_brms("rights_freedom_context", lag=2)
rights_e = TSCS_reg_brms("rights_equality_context", lag=2)
rights_c = TSCS_reg_brms("rights_control_context", lag=2)

make_LRE(rights_f, "rights_freedom_context")
make_LRE(rights_e, "rights_equality_context")
make_LRE(rights_c, "rights_control_context")

rs_f = TSCS_reg_brms("rule_settlement_freedom_context", lag=2)
rs_e = TSCS_reg_brms("rule_settlement_equality_context", lag=1)
rs_c = TSCS_reg_brms("rule_settlement_control_context", lag=2)

make_LRE(rs_f, "rule_settlement_freedom_context")
make_LRE(rs_e, "rule_settlement_equality_context")
make_LRE(rs_c, "rule_settlement_control_context")

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


get_prediction = function(model, effect, lagvar, lagvar2 = NULL, upper = T, cred = 0.95, time = 5) {
  library(HDInterval)
  
  pred_frame = model$data %>% 
    #filter(country_name == "Hungary") %>% 
    summarise_if(is.numeric, mean)
  
    # filter(country_name == "Hungary") %>% 
    # slice(1) %>% 
    #mutate_at(vars(matches("caus_wi")), funs(. -.) ) %>%
    #mutate_at(vars(matches("caus_cbw")), funs(. -.) ) %>%
    #mutate_at(vars(matches("caus_ybw")), funs(. -.) ) %>%
  
  if (upper == T) {
    pop_effect = 1 - (pred_frame$pop_HOG_caus_cbw + pred_frame$pop_HOG_caus_ybw)
    
  }  else {
    pop_effect = 0 - (pred_frame$pop_HOG_caus_cbw + pred_frame$pop_HOG_caus_ybw)
  }

  pred_frame  =  pred_frame   %>% 
    mutate(country_name = factor("Germany", levels = levels(as.factor(model$data$country_name))),
           year = factor("1994", levels = levels(as.factor(model$data$year)))) %>% 
    slice(rep(1:n(), each = time)) %>% 
    mutate(pop_HOG_caus_wi = c(pred_frame$pop_HOG_caus_wi, rep(pop_effect, time-1))) %>% 
    mutate(pop_HOG_caus_wi_lag = c(pred_frame$pop_HOG_caus_wi, pred_frame$pop_HOG_caus_wi, rep(pop_effect, time-2)))
  
  print(pred_frame)
  
  # substract to get wi
  cvariable = gsub("_wi_lag", "_lag_depcbw", lagvar)
  yvariable = gsub("_wi_lag", "_lag_depybw", lagvar)
  
  test = brms_ml_data %>% 
    select(country_name, year, cbw = cvariable, ybw = yvariable) %>% 
    #filter(country_name == "Germany") %>% 
    summarise(cbw = mean(cbw, na.rm=T), 
              ybw = mean(ybw, na.rm=T))
    
  #mean_sub = 500 - test$cbw - test$ybw 
  
  res = data.frame(mean = rep(NA, time),
             lower = rep(NA, time),
             upper = rep(NA, time))
 
  for (i in 2:time) {
    draws = fitted(model, newdata = pred_frame[i-1,], nsamples = 2000, summary = F, re_formula = NA)
    estimate = mean(draws) - test$cbw - test$ybw

    
    pred_frame[i,lagvar] = estimate
    if(is.null(lagvar2) == F) {
      pred_frame[i,lagvar2] = pred_frame[i-1,lagvar]
    }
    
    res$mean[i] =  mean(draws)
    res$lower[i] = hdi(draws, credMass = cred)[1]
    res$upper[i] = hdi(draws, credMass = cred)[2]
    
    
  }
  
  # transform values
  print(res)
  # lambda_sel = gsub("_wi_lag", "", lagvar)
  # res = res %>% 
  #   mutate_all(funs(ifelse( . < 0, 1, .)))
  # res$mean = reverse_transformation(res$mean, lambda_sel)
  # res$lower = reverse_transformation(res$lower, lambda_sel)
  # res$upper = reverse_transformation(res$upper, lambda_sel)
  
  return(res%>% 
           mutate(time = 1:time))
}

plot_prediction = function(model, variablel1, variablel2 = NULL, cred=0.95, time=9) {
  testu = get_prediction(model, 1, variablel1, variablel2, upper=T, cred=cred, time = time) %>% 
    mutate(Scen = "upper") 
  
  testl = get_prediction(model, -1, variablel1, variablel2, upper=F, cred=cred, time = time) %>% 
    mutate(Scen = "lower") 
  
  #
  title = gsub("_wi_lag","",variablel1)
  
  p1 = testu %>% 
    bind_rows(testl) %>% 
    ggplot(aes(x=time, y= mean, ymin =lower, ymax = upper,  col=Scen,  fill=Scen)) +
    geom_line() +
    geom_ribbon(alpha = 0.5) +
    ggtitle(title) +
    theme_bw()
  
  return(p1)
}

plot_prediction(dec_f, "decision_freedom_context_wi_lag", "decision_freedom_context_wi_lag_lag2")
plot_prediction(dec_e, "decision_equality_context_wi_lag")
plot_prediction(dec_c, "decision_control_context_wi_lag")
plot_prediction(int_f, "intermediate_freedom_context_wi_lag", "intermediate_freedom_context_wi_lag_lag2")
plot_prediction(int_e, "intermediate_equality_context_wi_lag", time=30)
plot_prediction(int_c, "intermediate_control_context_wi_lag", "decision_control_context_wi_lag_lag2")
plot_prediction(com_f, "communication_freedom_context_wi_lag", "communication_freedom_context_wi_lag_lag2")
plot_prediction(com_e, "communication_equality_context_wi_lag")
plot_prediction(com_c, "communication_control_context_wi_lag", "communication_control_context_wi_lag_lag2")
plot_prediction(rights_f, "rights_freedom_context_wi_lag", "rights_freedom_context_wi_lag_lag2")
plot_prediction(rights_e, "rights_equality_context_wi_lag", "rights_equality_context_wi_lag_lag2", time=30)
plot_prediction(rights_c, "rights_control_context_wi_lag", "rights_control_context_wi_lag_lag2")
plot_prediction(rs_f, "rule_settlement_freedom_context_wi_lag", "rule_settlement_freedom_context_wi_lag_lag2")
plot_prediction(rs_e, "rule_settlement_equality_context_wi_lag")
plot_prediction(rs_c, "rule_settlement_control_context_wi_lag", "rule_settlement_control_context_wi_lag_lag2")







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

