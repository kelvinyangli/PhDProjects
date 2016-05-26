singleParentComputation = function(nodeIndex, commonParentsIndecies, arityChild, indexListPerNodePerValue, base) {
  
  cumSum = 0
  
  for (j in 1:arityChild) { # for each child value j
    
    # N(pa_i, x_j)
    N_pa_i_x_j = length(intersect(commonParentsIndecies, indexListPerNodePerValue[[nodeIndex]][[j]]))
    
    # cumulative sum of log(N(pa_i, x_j)!)
    cumSum = cumSum + logFactorial(N_pa_i_x_j, base = base)
    
  } # end for j
  
  return(cumSum)
  
}