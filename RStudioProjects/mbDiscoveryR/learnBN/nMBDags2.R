# this is a better way of count the number of mb dags but still with duplications
f1 = function(n) {
  
  count = 0 
  
  for (i in 0:n) count = count + choose(n, i)
  
  return(count)
  
}

# n is the total number of variables, m is the number of colliders, and k is the number of spouses
f2 = function(n, m, k) {
  
  if (n < 2) {
    
    count = 0    
    
  } else {
    
    count = choose(n, k + 1) * (k + 1) 
    
    if (m < 2) {
      
      count = count * f1(n - k - 1)
      
    } else {
      
      sum_count = 0 
      
      for (k_dash in 1:min((n - k - 2), k)) {
        
        sub_count = f2(n - k - 1, m - 1, k_dash)
        
        if (k_dash == k) sub_count = (1 / m) * sub_count
        
        sum_count = sum_count + sub_count
        
      } # end for k_dash
      
      count = count * sum_count
      
    } # end else
    
  } # end else
  
  return(count)
  
}

f = function(n) {
  
  sum_count = f1(n) 
  cat("-----------------------------------\n")
  cat("|mb|=", n, "\n")
  cat("|spouses|=", 0, "=> |mbDags|=", sum_count, "\n")
  cat("-------------------------------------------------\n")
  
  for (m in 1:floor(n / 2)) {
    
    cat("|colliders|=", m, "\n")
    
    for (k in 1:(n - 2 * m + 1)) {
      
      sub_count = f2(n, m, k)
      sum_count = sum_count + sub_count 
      cat("|spouses|=", k, "=> |mbDags|=", sub_count, "\n")
      
    } # end for k 
    
    cat("-------------------------------------------------\n")
    
  } # end for m
  
  return(sum_count)
  
}






