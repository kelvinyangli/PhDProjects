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
        
        sum_count = sum_count + f2(n - k - 1, m - 1, k_dash)
        
      } # end for k_dash
      
      count = count * sum_count
      
    } # end else
    
  } # end else
  
  return(count)
  
}

f = function(n) {
  
  sum_count = f1(n) 
  
  for (m in 1:floor(n / 2)) {
    
    for (k in 1:(n - 2 * m + 1)) {
      
      sum_count = sum_count + f2(n, m, k)
      
    } # end for k 
    
  } # end for m
  
  return(sum_count)
  
}






