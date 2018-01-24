#' Auxiliary function to both mml_cpt and mml_cpt_fast 
#'
#' This is an auxiliary function to both mml_cpt() and mml_cpt_fast(). It calculates the mml score of 
#' a node without any parents. 
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained 
#' by the function count_occurance().
#' @param arities A vector of variable ariteis.
#' @param sampleSize Sample size of a given data set. 
#' @param targetIndex Index of the target node.  
#' @param base The base of the logarithm.  
#' @export
mml_without_parents = function(indexListPerNodePerValue, arities, sampleSize, targetIndex, base) {
  arity = arities[targetIndex]
  #constantDiff = 0.5 * (arity - 1) * log((pi * exp(1) / 6), base = base)
  constantDiff = 0
  # log_gamma(n+1) is an approximation of log(factorial(n))
  fixedTerm = constantDiff + log_gamma(sampleSize + arity) - 
    log(factorial(arity - 1), base = base)
  cumSum = 0
  for (i in 1:arity) {
    N_x_i = length(indexListPerNodePerValue[[targetIndex]][[i]])
    cumSum = cumSum + log_gamma(N_x_i + 1)
  }
  return(fixedTerm - cumSum)
}