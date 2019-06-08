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
