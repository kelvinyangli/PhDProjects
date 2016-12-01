# these are the functions required when enumerating mb dags
# z is a subset contains all k spouses and the common child
# index is from [1, length(z)], to indicate which variable in z is taken as a common child
# each variable in z will be considered as a common child iteratively
buildBranch = function(z, index) {
  
  dag = empty.graph(z)
  parents(dag, z[index]) = z[-index]
  
  return(dag)
  
}

# merge two dags x and y into a new dag that retains structures of x and y and connects them by a arc from one node to another
mergeDags = function(x, y, from, to) {
  
  z = empty.graph(c(nodes(x), nodes(y)))
  arcs(z) = rbind(arcs(x), arcs(y))
  z = set.arc(z, from, to)
  return(z)
  
}

# ls is a list of dags whose variable names will be changed into variable names in z
# y is the target variable
substituteVar = function(ls, y, z) {
  
  for (i in 1:length(ls)) {
    
    nodes(ls[[i]])[nodes(ls[[i]]) != y][order(nodes(ls[[i]])[nodes(ls[[i]]) != y])] = z
    
  } # end for i 
  
  return(ls)
  
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


