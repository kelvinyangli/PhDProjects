multiParentsComputation = function(nodeIndex, arityChild, indexListPerNodePerValue, commonParentsIndices, base) {
  
  cumSum = 0 
  
  for (j in 1:arityChild) {
    
    N_pa_i_x_j = length(intersect(commonParentsIndices, indexListPerNodePerValue[[nodeIndex]][[j]]))
    
    cumSum = cumSum + logFactorial(N_pa_i_x_j, base = base)
    
  }
  
  return(cumSum)
  
}

