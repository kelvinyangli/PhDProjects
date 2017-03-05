# this function couts the number of occurence of each var state and save information into a list
# it also extracts the arity of each var 
# this function helps speed up computing mmlcpt 
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