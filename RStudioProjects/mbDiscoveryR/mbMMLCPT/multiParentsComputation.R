multiParentsComputation = function(nodeIndex, arityChild, indexListPerNodePerValue, commonParentsIndecies, base) {
  
  cumSum = 0 
  
  for (j in 1:arityChild) {
    
    N_pa_i_x_j = length(intersect(commonParentsIndecies, indexListPerNodePerValue[[nodeIndex]][[j]]))
    
    cumSum = cumSum + logFactorial(N_pa_i_x_j, base = base)
    
  }
  
  return(cumSum)
  
}