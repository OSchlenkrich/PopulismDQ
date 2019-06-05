# BASIC FUNCTIONS


'%!in%' <- function(x,y)!('%in%'(x,y))


# function to obtain mean bootstrap
mean_boot <- function(data, indices) {
  d <- data[indices] # allows boot to select sample
  fit <- mean(d, na.rm=T)
  return(fit)
} 

