# BASIC FUNCTIONS


'%!in%' <- function(x,y)!('%in%'(x,y))


# function to obtain mean bootstrap
mean_boot <- function(data, indices) {
  d <- data[indices] # allows boot to select sample
  fit <- mean(d, na.rm=T)
  return(fit)
} 


# Dynamic Simulation
dynamicSim = function(model, 
                      timepoints, 
                      variable_name, 
                      variable_value, 
                      name_ldv, 
                      name_scenario,
                      iterations = 1000) {
  
  all_means_numeric = model$model %>% 
    mutate_at(vars(matches("country_name.")), as.factor) %>% 
    summarise_if(is.numeric, mean, na.rm=T) %>% 
    melt() 
  all_means_numeric = model$model %>% 
    select_at(vars(matches("country_name."))) %>% 
    melt() %>% 
    group_by(variable) %>% 
    slice(1) %>% 
    mutate(value = 0) %>% 
    bind_rows(all_means_numeric,.)


  # create scenario
  scenario = data.frame(variable = names(coef(model)), row.names = NULL) %>% 
    left_join(all_means_numeric) %>% 
    mutate(value = if_else(variable==variable_name, variable_value, value),
           value = if_else(variable=="(Intercept)", 1, value),
           value = if_else(variable=="TSCS_data.country_name.Germany", 1, value)
           ) 
  

  Timep = timepoints
  library(mvtnorm)
  
  
  get_timepoints = function(scenario, name_scenario) {
    
    my_results = array(NA, dim=c(Timep, iterations))
    
    coef_mat = rmvnorm(iterations, coef(model), vcovBK(model))
    
    my_results[1,] =  coef_mat %*% scenario$value
    
    for(i in 2:Timep) {
      scenario = scenario %>% 
        mutate(value = if_else(variable==name_ldv, mean( my_results[i-1,]), value))
      
      my_results[i,] =  coef_mat %*% scenario$value
    }
    
    
    plot_data = data.frame( 
      y_mean = apply(my_results, 1, FUN=function(x) mean(x)),
      y_min = apply(my_results, 1, FUN=function(x) quantile(x, 0.05)),
      y_max = apply(my_results, 1, FUN=function(x) quantile(x, 0.95)),
      x = 1:Timep
    ) %>% 
      mutate(Scenario = name_scenario)
    
    return(plot_data)  
  }
  
  plot_data = get_timepoints(scenario, name_scenario)
  return(plot_data)
}



# 15-Field-Matrix

vis_15_Felder <- function(country_sel, year_used, radio, dataset) {
  
  low = "#cf4c02"
  mid = "#e9df10"
  high= "#3cc816"
  midpoint = 0.5
  limit=c(0,1)
  
  if (radio == "Core Measurement") {
    variables_sel = "_core"
  }
  if (radio == "Context Measurement") {
    variables_sel = "_context"
  }
  if (radio == "Trade-Off Measurement") {
    variables_sel = "_trade_off"
    low = "#AED6F1"
    mid = "#5DADE2"
    high= "#2874A6"
    midpoint= 0.78
    limit = c(0.4,1)
  }
  
  if (radio == "Difference") {
    variables_sel = "_context"
    
  }
  
  plot_heat <- dataset %>%
    filter(country_name == country_sel, year == year_used) %>%
    select_at(vars(ends_with(variables_sel)))
  
  
  if (dim(plot_heat)[1] == 0)  {
    plot_heat[1,1] = country_sel
  }
  
  if (radio == "Difference") {
    low = "#cf4c02"
    mid = "#ffffff"
    high="#3cc816"
    midpoint= 0
    limit = c(min(plot_heat[-1], na.rm=T),max(plot_heat[-1], na.rm=T))
  }
  
  names(plot_heat) = gsub(variables_sel, "", names(plot_heat))
  
  df_grid = data.frame(Dim = c(rep(c("Freedom", "Equality", "Control"), 5),rep("Institution", 5), c("Freedom", "Equality", "Control", "Institution")),
                       Int = c(rep(c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement"), each=3), c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement", rep("Dimension",4)))
  )
  
  df_grid$merge = paste(df_grid$Dim, df_grid$Int)
  
  df_grid$value[df_grid$merge == "Freedom Procedurs\n of \nDecision"] =  plot_heat$decision_freedom
  df_grid$value[df_grid$merge == "Equality Procedurs\n of \nDecision"] =  plot_heat$decision_equality
  df_grid$value[df_grid$merge == "Control Procedurs\n of \nDecision"] =  plot_heat$decision_control
  
  df_grid$value[df_grid$merge == "Freedom Intermediate\n Sphere"] =  plot_heat$intermediate_freedom
  df_grid$value[df_grid$merge == "Equality Intermediate\n Sphere"] =  plot_heat$intermediate_equality
  df_grid$value[df_grid$merge == "Control Intermediate\n Sphere"] =  plot_heat$intermediate_control
  
  df_grid$value[df_grid$merge == "Freedom Communication"] =  plot_heat$communication_freedom
  df_grid$value[df_grid$merge == "Equality Communication"] =  plot_heat$communication_equality
  df_grid$value[df_grid$merge == "Control Communication"] =  plot_heat$communication_control
  
  df_grid$value[df_grid$merge == "Freedom Rights"] =  plot_heat$rights_freedom
  df_grid$value[df_grid$merge == "Equality Rights"] =  plot_heat$rights_equality
  df_grid$value[df_grid$merge == "Control Rights"] =  plot_heat$rights_control
  
  df_grid$value[df_grid$merge == "Freedom Rules\n settlement"] =  plot_heat$rule_settlement_freedom
  df_grid$value[df_grid$merge == "Equality Rules\n settlement"] =  plot_heat$rule_settlement_equality
  df_grid$value[df_grid$merge == "Control Rules\n settlement"] =  plot_heat$rule_settlement_control
  
  df_grid$value[df_grid$merge == "Institution Procedurs\n of \nDecision"] =  plot_heat$decision_inst_index
  df_grid$value[df_grid$merge == "Institution Intermediate\n Sphere"] =  plot_heat$intermediate_inst_index
  df_grid$value[df_grid$merge == "Institution Communication"] =  plot_heat$communication_inst_index
  df_grid$value[df_grid$merge == "Institution Rights"] =  plot_heat$rights_inst_index
  df_grid$value[df_grid$merge == "Institution Rules\n settlement"] =  plot_heat$rule_settlement_inst_index
  
  df_grid$value[df_grid$merge == "Freedom Dimension"] =  plot_heat$freedom_dim_index
  df_grid$value[df_grid$merge == "Equality Dimension"] =  plot_heat$equality_dim_index
  df_grid$value[df_grid$merge == "Control Dimension"] =  plot_heat$control_dim_index
  df_grid$value[df_grid$merge == "Institution Dimension"] =  plot_heat$total_index
  
  yaxis <- factor(c("Dimension", "Rules\n settlement", "Rights", "Communication", "Intermediate\n Sphere", "Procedurs\n of \nDecision"), levels=c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement", "Dimension"))
  xaxis <- factor(c("Freedom", "Equality", "Control", "Institution"), levels=c("Freedom", "Equality", "Control", "Institution"))
  
  plot_heat_ma = ggplot(data = df_grid, aes(x = Dim, y = Int, fill=value)) +  
    geom_raster ()  + 
    geom_text(aes(Dim, Int, label = round(value,2)), color = "black", size = 4, hjust=0.5, vjust=0.5) + 
    scale_y_discrete(limits=yaxis, expand = c(0, 0)) +  
    scale_x_discrete(limits=xaxis, expand = c(0, 0), position = "top") +  
    ggtitle(paste(year_used)) + 
    theme(legend.position = "none", axis.title = element_blank(), plot.title = element_text(size=10, hjust=0.5), axis.text.x = element_text(face="bold"), axis.text.y = element_text(face="bold")) + 
    geom_rect(aes(xmin=0.5, xmax=4.5, ymin=0.5, ymax=1.5), fill=NA, color="black", size=1.5) + 
    geom_rect(aes(xmin=3.5, xmax=4.5, ymin=0.5, ymax=6.5), fill=NA, color="black", size=1.5) + 
    scale_fill_gradient2(low = low, high = high, mid=mid, midpoint = midpoint, limit = limit, space = "Lab", name="DQ")
  return(plot_heat_ma)
  
}

# 15-Field-Matrix

vis_15_Felder_raw <- function(country_sel, year_used, radio, dataset) {
  
  low = "#cf4c02"
  mid = "#e9df10"
  high= "#3cc816"
  midpoint = 0.5
  limit=c(0,1)
  
  if (radio == "Core Measurement") {
    variables_sel = "_core"
  }
  if (radio == "Context Measurement") {
    variables_sel = "_context"
  }
  if (radio == "Trade-Off Measurement") {
    variables_sel = "_trade_off"
    low = "#58ACFA"
    mid = "#0040FF"
    high= "#0101DF"
    midpoint= 0.8
    limit = c(0.4,1)
  }
  
  if (radio == "Difference") {
    variables_sel = "_context"
    
  }
  
  plot_heat <- dataset %>%
    filter(country_name == country_sel, year == year_used) %>%
    select_at(vars(ends_with(variables_sel)))
  
  
  if (dim(plot_heat)[1] == 0)  {
    plot_heat[1,1] = country_sel
  }
  
  if (radio == "Difference") {
    low = "#cf4c02"
    mid = "#ffffff"
    high="#3cc816"
    midpoint= 0
    limit = c(min(plot_heat[-1], na.rm=T),max(plot_heat[-1], na.rm=T))
  }
  
  names(plot_heat) = gsub(variables_sel, "", names(plot_heat))
  
  df_grid = data.frame(Dim = c(rep(c("Freedom", "Equality", "Control"), 5),rep("Institution", 5), c("Freedom", "Equality", "Control", "Institution")),
                       Int = c(rep(c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement"), each=3), c("Procedurs\n of \nDecision", "Intermediate\n Sphere", "Communication", "Rights", "Rules\n settlement", rep("Dimension",4)))
  )
  
  df_grid$merge = paste(df_grid$Dim, df_grid$Int)
  
  df_grid$value[df_grid$merge == "Freedom Procedurs\n of \nDecision"] =  plot_heat$decision_freedom
  df_grid$value[df_grid$merge == "Equality Procedurs\n of \nDecision"] =  plot_heat$decision_equality
  df_grid$value[df_grid$merge == "Control Procedurs\n of \nDecision"] =  plot_heat$decision_control
  
  df_grid$value[df_grid$merge == "Freedom Intermediate\n Sphere"] =  plot_heat$intermediate_freedom
  df_grid$value[df_grid$merge == "Equality Intermediate\n Sphere"] =  plot_heat$intermediate_equality
  df_grid$value[df_grid$merge == "Control Intermediate\n Sphere"] =  plot_heat$intermediate_control
  
  df_grid$value[df_grid$merge == "Freedom Communication"] =  plot_heat$communication_freedom
  df_grid$value[df_grid$merge == "Equality Communication"] =  plot_heat$communication_equality
  df_grid$value[df_grid$merge == "Control Communication"] =  plot_heat$communication_control
  
  df_grid$value[df_grid$merge == "Freedom Rights"] =  plot_heat$rights_freedom
  df_grid$value[df_grid$merge == "Equality Rights"] =  plot_heat$rights_equality
  df_grid$value[df_grid$merge == "Control Rights"] =  plot_heat$rights_control
  
  df_grid$value[df_grid$merge == "Freedom Rules\n settlement"] =  plot_heat$rule_settlement_freedom
  df_grid$value[df_grid$merge == "Equality Rules\n settlement"] =  plot_heat$rule_settlement_equality
  df_grid$value[df_grid$merge == "Control Rules\n settlement"] =  plot_heat$rule_settlement_control
  
  df_grid$value[df_grid$merge == "Institution Procedurs\n of \nDecision"] =  plot_heat$decision_inst_index
  df_grid$value[df_grid$merge == "Institution Intermediate\n Sphere"] =  plot_heat$intermediate_inst_index
  df_grid$value[df_grid$merge == "Institution Communication"] =  plot_heat$communication_inst_index
  df_grid$value[df_grid$merge == "Institution Rights"] =  plot_heat$rights_inst_index
  df_grid$value[df_grid$merge == "Institution Rules\n settlement"] =  plot_heat$rule_settlement_inst_index
  
  df_grid$value[df_grid$merge == "Freedom Dimension"] =  plot_heat$freedom_dim_index
  df_grid$value[df_grid$merge == "Equality Dimension"] =  plot_heat$equality_dim_index
  df_grid$value[df_grid$merge == "Control Dimension"] =  plot_heat$control_dim_index
  df_grid$value[df_grid$merge == "Institution Dimension"] =  plot_heat$total_index
  

  return(df_grid %>% mutate(year=year_used))
  
}
#

Make_formula = function(variable) {
  raw_formula = paste("placeholder ~ 1 + 
          country_name +
          placeholder_lag +
          trend +
          pop_cat +
          pop_cat_lag", collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}

Make_formula2 = function(variable) {
  raw_formula = paste("placeholder_df ~ 1 + 
          country_name +
          year + 
          placeholder_lag +
          placeholder_lag2 +
          trend",
          collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}

Make_formulaAR = function(variable) {
  raw_formula = paste("placeholder ~ 1 + 
          country_name +
          trend +
          pop_cat +
          pop_cat_lag", collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}



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
          gdp_growth_caus_lag +
          socequal_caus + 
          socequal_caus_lag",
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
          gdp_growth_caus_lag  +
          socequal_caus + 
          socequal_caus_lag",
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
  
  
  #print(summary(TSCS_obj))
  coef_mat = coeftest(TSCS_obj, vcov.=function(x) vcovBK(x, cluster="time", type="HC0"))
  auto_corr1 = pbgtest(TSCS_obj, order=1)
  auto_corr2 = pbgtest(TSCS_obj, order=2)
  auto_corr3 = pbgtest(TSCS_obj, order=3)
  return(list(TSCS_obj, round(coef_mat,5), auto_corr1,auto_corr2,auto_corr3))
}

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

lag_distribution_both = function(brms_model, LDV_label, IndV_label, dep_label, unit = 1, time_periods=4, ci=0.95, ecm = F, justHDI = F) {
  
  if (unit == "sd") {
    unit = brms_model$data %>% 
      select_at(vars(matches(IndV_label)))  %>% 
      select_at(vars(-matches("_cbw"), -matches("_ybw"), -matches("_lag"))) %>% 
      summarise_all(funs(unit = sd(.))) %>% 
      pull(unit)
    
    print(unit)
  } else {
    unit = 1
  }
  
  posterior_coefs = posterior_samples(brms_model, pars = IndV_label) %>% 
    select_at(vars(-matches("_cbw")))  %>% 
    select_at(vars(-matches("_ybw"))) 
  
  print(colnames(posterior_coefs))
  posterior_LDV = posterior_samples(brms_model, pars = LDV_label)
  print(colnames(posterior_LDV))
  
  
  X = unit
  
  a =  as.matrix(posterior_LDV)
  b0 = as.matrix(posterior_coefs[,1])
  b1 = as.matrix(posterior_coefs[,2])
  
  
  if (ecm == T) {
    ECrate = abs(rowSums(a))
    
    LRM = (b1/ECrate)
    LRM = LRM * X # scaling
  } else {
    ECrate = 1-abs(rowSums(a))
    
    LRM = ((b1 + b0)/ECrate) 
    LRM = LRM * X # scaling
  }
  
  time_periods = 6
  
  Y = array(NA, dim=c(dim(posterior_coefs)[1], time_periods))
  
  Y[,1] = b0 * X # T0
  LR_effect = LRM - Y[,1]
  
  for (i in 2:time_periods) {
    Y[,i] = LR_effect * ECrate
    LR_effect = LR_effect - Y[,i]
  }
  
  
  estimates = data.frame(array(NA, dim=c(time_periods,3))) %>% 
    rename(est = X1, l = X2, u = X3) %>% 
    mutate(time = paste("t + ", 0:(time_periods-1), sep=""))
  
  #for (i in 1:dim(estimates)[1]) {
  for (i in 1:dim(estimates)[1]) {
    estimates$est[i] = median(Y[,i])
    if (i == 1) {
      estimates$l[i] = HDInterval::hdi(Y[,i], credMass = ci)[1]
      estimates$u[i] = HDInterval::hdi(Y[,i], credMass = ci)[2]
    }
  }
  

  print(hdi(LRM, credMass = ci))
  CI = round(hdi(LRM, credMass = ci),3)
  
  if (justHDI == T) {
    return(LRM)
  }  
  label = paste("LRM: ", round(median(LRM),3), " (", CI[1], " \u2013 ",CI[2],")", sep="")
  
  title = gsub("b_","",colnames(posterior_coefs))
  title = gsub("_wi","",title)
  title = gsub("_pr_ctl","",title)
  title = gsub("_num_ctl","",title)
  title = gsub("_df","",title)
  
  p1 = ggplot(estimates, aes(x=time, y=est)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=l, ymax=u)) +
    geom_hline(yintercept = 0) +
    theme_bw()  +
    ggtitle(title, subtitle = label) +
    xlab("") +
    ylab(paste("Change in", dep_label, sep=" "))
  return(p1)
}


# Bayesian ####


Make_formulalag1_ml = function(variable) {
  raw_formula = paste("placeholder ~ 1 + 
          placeholder_wi_lag +          
          
          pop_Opp_caus_wi +
          pop_cab_caus_wi +
          pop_HOG_caus_wi +
          
          pop_Opp_caus_wi_lag +
          pop_cab_caus_wi_lag +
          pop_HOG_caus_wi_lag +
          
          gdp_growth_caus_wi +
          gdp_growth_caus_wi_lag +
          
          socequal_caus_wi + 
          socequal_caus_wi_lag+ 

          pop_Opp_caus_cbw +
          pop_cab_caus_cbw +
          pop_HOG_caus_cbw +
          gdp_growth_caus_cbw +
          socequal_caus_cbw +

          pop_Opp_caus_ybw +
          pop_cab_caus_ybw +
          pop_HOG_caus_ybw +
          gdp_growth_caus_ybw +
          socequal_caus_ybw +

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

          pop_Opp_caus_wi +
          pop_cab_caus_wi +
          pop_HOG_caus_wi +
          
          pop_Opp_caus_wi_lag +
          pop_cab_caus_wi_lag +
          pop_HOG_caus_wi_lag +

          gdp_growth_caus_wi +
          gdp_growth_caus_wi_lag +
          
          socequal_caus_wi + 
          socequal_caus_wi_lag+ 

          pop_Opp_caus_cbw +
          pop_cab_caus_cbw +
          pop_HOG_caus_cbw +
          gdp_growth_caus_cbw +
          socequal_caus_cbw +

          pop_Opp_caus_ybw +
          pop_cab_caus_ybw +
          pop_HOG_caus_ybw +
          gdp_growth_caus_ybw +
          socequal_caus_ybw +

          (1|country_name)  +
          (1|year)",
                      collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}

TSCS_reg_brms = function(variable, lag=1, dataset = brms_ml_data) {
  
  if (lag == 1) {
    my_tscs_formula = Make_formulalag1_ml(variable)
    
  }
  if (lag == 2) {
    my_tscs_formula = Make_formulalag2_ml(variable)
    
  }
  priors = c(set_prior("normal(0,10)", class = "Intercept"),
             set_prior("normal(0,10)", class = "b"),
             set_prior("cauchy(0,5)", class="sd"))
  TSCS_obj = brm(bf(my_tscs_formula), 
                 dataset %>% 
                   mutate(year = as.factor(year)),
                 warmup = 500,
                 iter = 1500,
                 prior =priors,
                 chains=5,
                 backend = "cmdstanr",
                 threads = threading(2))
  
  
  return(TSCS_obj)
}




make_LRE = function(model, var, credmass = 0.95, unit=1, justHDI = F) {
  library(HDInterval)
  library(ggpubr)
  
  p1 = lag_distribution_both(model, var, 
                             IndV_label = "pop_Opp_caus" , 
                             dep_label = var, 
                             unit = unit, time_periods=4, ci=credmass, ecm = F, justHDI = justHDI) + 
    ggtitle("Opposition")
  p2 = lag_distribution_both(model, var, 
                             IndV_label = "pop_cab_caus" , 
                             dep_label = var, 
                             unit = unit, time_periods=4, ci=credmass, ecm = F, justHDI = justHDI)+ 
    ggtitle("Kabinett")
 
  p3 = lag_distribution_both(model, var, 
                             IndV_label = "pop_HOG_caus" , 
                             dep_label = var, 
                             unit = unit, time_periods=4, ci=credmass, ecm = F, justHDI = justHDI)  + 
    ggtitle(enc2utf8("Regierungschef/Präsident"))
  
  return(ggarrange(p1,p2,p3, ncol=3) %>%  annotate_figure(var))
}


plot_residuals = function(model, plottitle = NULL, all=F) {
  modeldata = model$data 

  modeldata = modeldata %>% 
    bind_cols(residuals = data.frame(residuals(model, type="pearson"))[,1])
  if (all == F) {
    p1 = modeldata %>% 
      mutate(year = as.numeric(levels(year))[year]) %>% 
      
      ggplot(aes(x=year, y = residuals, grp=country_name)) +
      geom_line(size=1.1) +
      geom_smooth(se=F, alpha=0.5) +
      geom_hline(yintercept=c(-2,2), linetype ="dashed") +
      facet_wrap(country_name ~ .)  +
      theme_bw() +
      ggtitle(plottitle) +
      xlab("") +
      ylab("Pearson Residuals") 
    
  } else {
    p1 = modeldata %>% 
      mutate(year = as.numeric(levels(year))[year]) %>% 
      
      ggplot(aes(x=year, y = residuals, grp=country_name)) +
      geom_smooth(size=1.1, se=F, alpha=0.5) +
      geom_point(size=1.1) +
      geom_hline(yintercept=c(-2,2), linetype ="dashed") +
      theme_bw() +
      ggtitle(plottitle) +
      xlab("") +
      ylab("Pearson Residuals")
  }
  
  return(p1)
}


plot_residualsX = function(model, X_lab, plottitle = NULL, all=F) {
  modeldata = model$data 
  
  modeldata = modeldata %>% 
    bind_cols(residuals = data.frame(residuals(model, type="pearson"))[,1]) %>% 
    rename(X = X_lab)
    
  if (all == F) {
    p1 = modeldata %>% 
      ggplot(aes(x=X, y = residuals, grp=country_name)) +
      geom_point(size=1.1) +
      geom_hline(yintercept=c(-2,2), linetype ="dashed") +
      facet_wrap(country_name ~ .)  +
      theme_bw() +
      ggtitle(plottitle) +
      xlab("") +
      ylab("Pearson Residuals")
  } else {
    p1 = modeldata %>% 
      ggplot(aes(x=X, y = residuals, grp=country_name)) +
      geom_point(size=1.1) +
      geom_hline(yintercept=c(-2,2), linetype ="dashed") +
      theme_bw() +
      ggtitle(plottitle) +
      xlab("") +
      ylab("Pearson Residuals")
  }
  
  return(p1)
}


plot_residuals_fitted = function(model, plottitle = NULL, all=F) {
  modeldata = model$data 
  
  modeldata = modeldata %>% 
    bind_cols(residuals = data.frame(residuals(model, type="pearson"))[,1]) %>% 
    bind_cols(fitted = data.frame(fitted(model))[,1]) 
  
  if (all == F) {
    p1 = modeldata %>% 
      ggplot(aes(x=fitted, y = residuals, grp=country_name)) +
      geom_point(size=1.1) +
      geom_hline(yintercept=c(-2,2), linetype ="dashed") +
      facet_wrap(country_name ~ .)  +
      theme_bw() +
      ggtitle(plottitle) +
      xlab("") +
      ylab("Pearson Residuals")
  } else {
    p1 = modeldata %>% 
      ggplot(aes(x=fitted, y = residuals, grp=country_name)) +
      geom_point(size=1.1) +
      geom_hline(yintercept=c(-2,2), linetype ="dashed") +
      theme_bw() +
      ggtitle(plottitle) +
      xlab("") +
      ylab("Pearson Residuals")
  }
  
  return(p1)
}


get_residuals = function(model) {
  modeldata =  model$data  %>% 
    bind_cols(residuals = data.frame(residuals(model, type="pearson"))[,1])
  return(modeldata)
}
