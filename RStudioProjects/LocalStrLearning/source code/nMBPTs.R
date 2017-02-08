# the polytrees that satisfiy the MB adjacencies is denoted by MBPTs
# nMBPTs is the function that computes the number of MBPTs in a function of mb size
# this formula has been manually verified by drawing all dags in a mb with up to 6 variables in a mb
# nWithNoSp is a function to computes the number of mb dags with no spouses, i.e., there is no collider
# n is the total number of variables in mb
nWithNoSp = function(n) {
  
  count = 0 
  
  for (i in 0:n) count = count + choose(n, i)
  
  return(count)
  
}

# nWithSp is a function to computes the number of mb dags with spouses, i.e., there exists colliders
# n is the total number of variables, m is the number of colliders, and k is the number of spouses
nWithSp = function(n, m, k) {
  
  if (n < 2) {
    
    count = 0    
    
  } else {
    
    count = choose(n, k + 1) * (k + 1) 
    
    if (m < 2) {
      
      count = count * nWithNoSp(n - k - 1)
      
    } else {
      
      sum_count = 0 
      
      for (k_dash in 1:min((n - k - 2 * m + 2), k)) {
      # the maximum that k_dash can be is (n-k-1)-2(m-2)-1, where (n-k-1) is # remaining
      # vars after removing k spouses and 1 common child, 2(m-2) is the minimum # vars
      # needed for having m-2 v-structures, and -1 removes another child
        sub_count = nWithSp(n - k - 1, m - 1, k_dash)
        
        if (k_dash == k) sub_count = (1 / m) * sub_count
        
        sum_count = sum_count + sub_count
        
      } # end for k_dash
      
      count = count * sum_count
      
    } # end else
    
  } # end else
  
  return(count)
  
}

nMBPTs = function(n) {
  
  sum_count = nWithNoSp(n) 
  cat("-------------------------------\n")
  cat("|mb|=", n, "\n")
  cat("|spouses|=", 0, "=> |mbDags|=", sum_count, "\n")
  cat("-------------------------------\n")
  
  for (m in 1:floor(n / 2)) {
    
    cat("|colliders|=", m, "\n")
    
    for (k in 1:(n - 2 * m + 1)) {
      
      sub_count = nWithSp(n, m, k)
      sum_count = sum_count + sub_count 
      cat("|spouses|=", k, "=> |mbDags|=", sub_count, "\n")
      
    } # end for k 
    
    cat("-------------------------------\n")
    
  } # end for m
  
  return(sum_count)
  
}

# this is the number of DAGs on n labelled variables
# this fomula is from wikipedia
nDags = function(n) {
  
  count = 0
  
  if (n < 1) {
    
    count = 1
    
  } else {
    
    for (i in 1:n) {
      
      count = count + ((-1) ^ (i - 1)) * choose(n, i) * (2 ^ (i * (n - i))) * nDags(n - i)
      
    } # end for i
    
  }
  
  return(count)
  
}




