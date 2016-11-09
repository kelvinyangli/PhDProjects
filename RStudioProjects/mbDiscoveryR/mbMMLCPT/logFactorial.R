logFactorial = function(n, base) {
  
  # if n = 0 then log of 0 factorial is 0
  ss = 0 
  
  if (n > 0) {
    
    for (i in 1:n) ss = ss + log(i, base = base)
    
  }
  
  return(ss)
  
}