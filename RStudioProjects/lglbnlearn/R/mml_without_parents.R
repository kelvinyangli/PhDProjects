#' Auxiliary function to both mml_cpt and mml_cpt_fast 
#'
#' This is an auxiliary function to both mml_cpt() and mml_cpt_fast(). It calculates the mml score of 
#' a node without any parents. 
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained 
#' by the function count_occurance().
#' @param arities 
#' @param sampleSize
#' @param targetIndex Index of the target node.  
#' @param logFactorialSheet A pre-saved log factorial sheet for non-negative integers <= 10000.
#' @param base The base of the logarithm.  
#' @export
mml_without_parents = function(indexListPerNodePerValue, arities, sampleSize, targetIndex, 
                               log_factorial, base) {
  arity = arities[targetIndex]
  fixedTerm = 0.5 * (arity - 1) * log((pi * exp(1) / 6), base = base) + 
    log_factorial(logFactorialSheet, sampleSize + arity - 1, base) - 
    log(factorial(arity - 1), base = base)
  cumSum = 0
  for (i in 1:arity) {
    N_x_i = length(indexListPerNodePerValue[[targetIndex]][[i]])
    cumSum = cumSum + log_factorial(logFactorialSheet, N_x_i, base)
  }
  return(fixedTerm - cumSum)
}