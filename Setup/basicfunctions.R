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
    scale_fill_gradient2(low = low, high = high, mid=mid,
                                                                                                                                                                                                               midpoint = midpoint, limit = limit, space = "Lab", name="DQ")
  return(plot_heat_ma)
  
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


Make_formulaAR = function(variable) {
  raw_formula = paste("placeholder ~ 1 + 
          country_name +
          trend +
          pop_cat +
          pop_cat_lag", collapse=" + ")
  
  my_tscs_formula = as.formula(gsub("placeholder", variable, raw_formula))
  
  return(my_tscs_formula)
}
