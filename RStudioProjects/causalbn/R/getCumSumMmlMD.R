# this function computes the cumulative sum for mml for multinomial distribution with symmetric dirichlet-multinomial prior
# it is the term: 
# sum_1^k(log(gamma(x_k + alpha_k)) - log(gamma(alpha_k))) = sum_1^k(sum_1^xk(log(x_k + alpha_k - i)))
# it is the same as singleParentComputation and multiParentsComputation functions for mmlCPT, except this function takes into
# account dirichletParameters

getCumSumMmlMD  = function(nodeIndex, commonParentsIndices, arityChild, indexListPerNodePerValue, dirichletParameters, base) {
  
  cumSum = 0 
  
  for (j in 1:arityChild) {
    
    # number of observations under a particular parent value that takes on value j
    # N_pa_i_x_j is equivalent to x_k
    N_pa_i_x_j = length(intersect(commonParentsIndices, indexListPerNodePerValue[[nodeIndex]][[j]]))
    
    if (N_pa_i_x_j > 0) {
      
      for (i in 1:N_pa_i_x_j) cumSum = cumSum - log(N_pa_i_x_j + dirichletParameters[j] - i)
      
    } # end if 
    
  } # end for j
  
  return(cumSum)
  
}

