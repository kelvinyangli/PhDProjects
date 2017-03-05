# these are the functions required when enumerating mb dags
# z is a subset contains all k spouses and the common child
# index is from [1, length(z)], to indicate which variable in z is taken as a common child
# each variable in z will be considered as a common child iteratively
buildBranch = function(z, index) {
  
  dag = matrix(0, ncol = length(z), nrow = length(z))
  dimnames(dag) = list(z, z)
  #dag = empty.graph(z)
  #parents(dag, z[index]) = z[-index]
  dag[-index, index] = 1
  
  return(dag)
  
}

# merge two dags x and y into a new dag that retains structures of x and y and connects them by a arc from one node to another
mergeDags = function(x, y, from, to) {
  
  z = adiag(x, y)
  z[from, to] = 1
  #z = empty.graph(c(nodes(x), nodes(y)))
  #arcs(z) = rbind(arcs(x), arcs(y))
  #z = set.arc(z, from, to)
  return(z)
  
}


subEnumeration = function(x, y, m, k, subDagList) {
  
  dagList = list()
  count = 1
  colliderComb = combn(x, (k + 1)) # choose k+1 variables from x to form a collider with k spouses
  
  for (j in 1:ncol(colliderComb)) {# for each combination
    
    varRemain = x[!x %in% colliderComb[, j]] # remaining variables 
    if (length(varRemain) > 0) {# if varRemain is non-empty, then substitute variable names
      
      subDagList = substituteVar(subDagList, y, varRemain)
      
    } # end of variable substitution
    
    for (i in 1:length(colliderComb[, j])) {# for each variable it will be considered as a common child 
      # of y and its k spouses
      
      branch = buildBranch(colliderComb[, j], i)
      
      for (dagIndex in 1:length(subDagList)) {
        
        # merge branch into each dag in subDagList
        dagList[[count]] = mergeDags(branch, subDagList[[dagIndex]], y, colliderComb[i, j])
        count = count + 1
        
      } # end for each dag in subDagList
      
    } # end for each variable i in comb[, j]
    
  } # end for each combination j
  
  return(dagList)
  
}


