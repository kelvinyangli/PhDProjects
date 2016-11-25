# the number of DAGs that could be formed by MB(x) \cup x
# this formula has been manually verified by drawing all dags in a mb with up to 6 variables in a mb
# there should be no need for further justification
# linear dag is defined as a dag that has no collider
nLinearDags = function(n) {
  
  count = 0 # initialize count
  
  for (i in 0:n) count = count + sum(choose(n, i))
  
  return(count)
  
}

# collider dag is defined as a dag that has at least one collider 
nColliderDags = function(n) {
  
  count = 0 # initialize count
  
  # when there is less than or equal to one variable in mb
  # there is no way to have a collider
    
  if (n > 1) {# when there are more than one variables in mb
    # it is possible to have collider
    
    for (i in 1:(n - 1)) {
      
      count = count + choose(n, i + 1) * choose(i + 1, 1) * (nLinearDags(n - 1 - i) + nColliderDags(n - 1 - i))
      
    } # end for i
    
  } # end else
  
  return(count)
  
}

# there exists some duplicated dags when considering collider dags
# for example when there are 5 nodes is a mb, then adding a collider of size 2 to an existing dag with a collider of size 3
# will result in isomorphic dags as adding a collider of size 3 to an existing dag with a collider of size 2
# for large n, the number of duplicated dags are more
nDuplicatedColliderDags = function(n) {
  
  count = 0
  
  for (i in 2:floor(n / 2)) {
    
    j = i + 1
    
    while (i + j <= n) {
      
      count = count + choose(n, i) * choose(i, 1) * choose(n - i, j) * choose(j, 1) * nMBDags(n - i - j)
      
      j = j + 1
      
    } # end while
    
  } # end for i
  
  return(count)
  
}

# the total number of DAGs that could be formed by using variable mb(x) \cup x
# it equals the sum of the number of linear dags and the number of collider dags minus the duplicated collider dags
nMBDags = function(n) {
  
  if (n < 5) {
    
    return(nLinearDags(n) + nColliderDags(n)) 
    
  } else {
    
    return(nLinearDags(n) + nColliderDags(n) - nDuplicatedColliderDags(n)) 
    
  } # end else
  
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













