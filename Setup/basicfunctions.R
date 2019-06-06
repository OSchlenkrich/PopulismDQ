# BASIC FUNCTIONS


'%!in%' <- function(x,y)!('%in%'(x,y))


# function to obtain mean bootstrap
mean_boot <- function(data, indices) {
  d <- data[indices] # allows boot to select sample
  fit <- mean(d, na.rm=T)
  return(fit)
} 


# Dynamic Simulation

dynamicSim = function(model, timepoints, variable_name, variable_value, name_ldv, name_scenario) {
  
  all_means_numeric = model$model %>% 
    summarise_if(is.numeric, mean) %>% 
    melt() 
  
  # create scenario
  scenario = data.frame(variable = names(coef(model)), row.names = NULL) %>% 
    left_join(all_means_numeric) %>% 
    mutate(value = if_else(variable==variable_name, variable_value, value)) 
  
  
  LDV_mean = mean(all_means_numeric %>%  filter(variable == name_ldv) %>%  pull(value), na.rm=T)
  Timep = timepoints
  library(mvtnorm)
  
  
  get_timepoints = function(Scenario_type_data, name_scenario) {
    my_results = array(NA, dim=c(Timep, 1000))
    
    coef_mat = rmvnorm(1000, coef(model), vcovBK(model))
    
    my_results[1,] =  coef_mat %*% Scenario_type_data$value
    
    for(i in 2:Timep) {
      
      Scenario_type_data = Scenario_type_data %>% 
        mutate(value = if_else(variable==name_ldv, mean( my_results[i-1,]), value))
      
      my_results[i,] =  coef_mat %*% Scenario_type_data$value
    }
    
    
    plot_data = data.frame( 
      y_mean = apply(my_results, 1, FUN=function(x) quantile(x, 0.5)),
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
