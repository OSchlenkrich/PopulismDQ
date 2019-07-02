# Impulse and Unit Response Function


variable = "populist_is_gov"
coef_mat = TSCS_reg("rule_settlement_control_context")
var_lag = "rule_settlement_control_context"
Time=20
x=1

Unit_Response = function(variable, Time = 11, x=1, var_lag ) {

  coef_mat = TSCS_reg(var_lag)

  coef = grepl(variable, names(coef_mat[,1]))
  coef_lag = grepl(paste(var_lag, "_lag", sep="", collapse=""), names(coef_mat[,1]))


  x_lag = 0
  results = array(NA, Time)
  results[1] = 0
  results[2] = 0 + sum(coef_mat[coef,1]%*%c(x, x_lag))
  
  x_lag = x
  
  for (i in 3:Time) {
    results[i] = sum(coef_mat[coef_lag,1] * results[i-1] + coef_mat[coef,1]%*%c(x, x_lag))
  }

  results = data.frame(response = results, Time = 1:Time)
  
  ggplot(results, aes(x=Time, y=response)) +
    geom_line(size=1.1) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks=seq(1,Time,1))
}


Impulse_Response = function(variable, Time = 11, x=1, var_lag ) {
  
  coef_mat = TSCS_reg(var_lag)
  
  coef = grepl(variable, names(coef_mat[,1]))
  coef_lag = grepl(paste(var_lag, "_lag", sep="", collapse=""), names(coef_mat[,1]))
  
  x_lag = 0
  
  results = array(NA, Time)
  results[1] = 0
  results[2] = sum(coef_mat[coef,1]%*%c(x, x_lag))
  
  x_lag = x
  x = 0
  for (i in 3:Time) {
    results[i] = sum(coef_mat[coef_lag,1] * results[i-1] + coef_mat[coef,1]%*%c(x, x_lag))
    x_lag = 0
  }
  
  results = data.frame(response = results, Time = 1:Time)
  
  ggplot(results, aes(x=Time, y=response)) +
    geom_line(size=1.1) +
    theme_bw() +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks=seq(1,Time,1))
}



