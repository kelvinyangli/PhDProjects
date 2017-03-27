computeCI = function(x) {

  se = 1.96 * sd(x) /sqrt(length(x)) 
  upper = mean(x) + se
  lower = mean(x) - se
  
  return(data.frame(upper = upper, lower = lower))
  
}


