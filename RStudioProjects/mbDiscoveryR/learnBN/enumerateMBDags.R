# y is the target node, x is mb(y)
enumerateMBDags = function(x, y) {
  
  if (length(x) == 0) {
    
    dagList = list(empty.graph(y))
    
  } else if (length(x) == 1) {
    
    dagList = noColliderDag(x, y)
    
  } else {
    
    dagList = c(noColliderDag(x, y), colliderDag(x, y))
    
  } # end else 
  
  return(dagList)
  
}

# y is a target variable
# x = mb(y), where x is non-empty
noColliderDag = function(x, y) {
  
  dag = empty.graph(c(x, y))
  parents(dag, y) = x
  dagList = list() 
  dagList[[1]] = dag
  #size = nLinearDags(length(x))
  count = 2
  # for each parent set size starting from 1
  for (numPa in 1:length(x)) {
    
    childSetComb = combn(x, numPa)
    
    for (i in 1:ncol(childSetComb)) {# for each possible children set combination
      
      potentialDag = dag
      children(potentialDag, y) = childSetComb[, i]
      dagList[[count]] = potentialDag
      count = count + 1
      
    } # end for i
    
  } # end for numPa
  
  return(dagList)
  
} 

colliderDag = function(x, y) {
  
  dagList = list()
  count = 1
  m = length(x) - 1 # the maximum number of spouses in a single collider
  
  for (nSpouse in 1:m) {# for each spouse size nSpouse
    
    # all possible combinations of nSpouse + 1 variables that forms a single collider with n spouses 
    colliderComb = combn(1:length(x), nSpouse + 1) # return index of each variable in the order of x
    
    for (i in 1:ncol(colliderComb)) {# for each combination
      
      subDagList = enumerateMBDags(x[-colliderComb[, i]], y) # all mb dags with length(x) - 1 - nSpouse variables
      
      for (subDagIndex in 1:length(subDagList)) {# for each sub dag add the collider
        
        for (j in 1:length(colliderComb[, i])) {# each variable in colliderComb is considered as a common effect
          
          dagList[[count]] = empty.graph(c(x, y))
          arcs(dagList[[count]]) = arcs(subDagList[[subDagIndex]]) # add existing arcs from subDag to dag
          children(dagList[[count]], y) = c(children(dagList[[count]], y), 
                                            x[colliderComb[j, i]]) # add variable j in colliderComb[, i] as the common effect
          # then add the remaining variables in colliderComb[, i] as spouses
          parents(dagList[[count]], x[colliderComb[j, i]]) = c(parents(dagList[[count]], x[colliderComb[j, i]]), 
                                                                 x[colliderComb[-j, i]])
          
          count = count + 1
          
        } # end for each variable j
        
      } # end for each sub dag subDagIndex
       
    } # end for combination i
    
  } # end for each cardinality nSpouse
  
  return(dagList)
  
}

ls = enumerateMBDags(paste0("V", 1:7), "T")
















