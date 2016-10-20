# identify occurance of each variable state and save into a list for future use
getDataInfo = function(data) {
  
  ##############################################################
  # get the arity of each node 
  # get the indices for each value of each node
  
  arities = rep(0, ncol(data))
  
  indexListPerNodePerValue = list()
  
  for (i in 1:ncol(data)) {
    
    arities[i] = nlevels(data[, i])
    
    # get the indecides for each value of node i
    indexListPerValue = list()
    
    for (j in 1:arities[i]) {
      
      indexListPerValue[[j]] = which(data[, i] == levels(data[, i])[j]) 
      
    } # end for arity j
    
    indexListPerNodePerValue[[i]] = indexListPerValue
    
  } # end for node i
  
  ls = list(arities = arities, indexListPerNodePerValue = indexListPerNodePerValue)
  
  return(ls)
  
}