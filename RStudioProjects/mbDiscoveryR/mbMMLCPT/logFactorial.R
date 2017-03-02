
logFactorial = function(n, base) {
  
  if (n <= 10000) { # read from logFactorialSheet 
    
    lf = logFactorialSheet[n + 1, ceiling(exp(1)/base)]
    
  } else { # use stirling's approximation
    
    lf =   n * log(n, base) - n * log(exp(1), base) + 0.5 * (log(n, base) + log(2 * pi, base))
    
  }
  
  return(lf)
  
}



# direct computation is slower 
#logFactorial = function(n, base) {
  
  # if n = 0 then log of 0 factorial is 0
#  ss = 0 
  
#  if (n > 0) {
    
#    for (i in 1:n) ss = ss + log(i, base = base)
    
#  }
  
#  return(ss)
  
#}