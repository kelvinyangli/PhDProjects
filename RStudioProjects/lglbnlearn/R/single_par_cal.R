#' Auxiliary function to mml_with_parents() and mml_with_parents_fast()
#'
#' This is an auxiliary function to both mml_with_parents() and mml_with_parents_fast() when the target 
#' node has only one parent. 
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained 
#' by the function count_occurance().
#' @param commonParentsIndices 
#' @param arityChild 
#' @param targetIndex Index of the target node.  
#' @param logFactorialSheet A pre-saved log factorial sheet for non-negative integers <= 10000.
#' @param base The base of the logarithm.  
#' @export
single_par_cal = function(indexListPerNodePerValue, commonParentsIndices, arityChild, targetIndex, 
                          logFactorialSheet, base) {
  cumSum = 0
  for (j in 1:arityChild) { # for each child value j
    # N(pa_i, x_j)
    N_pa_i_x_j = length(intersect(commonParentsIndices, indexListPerNodePerValue[[targetIndex]][[j]]))
    # cumulative sum of log(N(pa_i, x_j)!)
    cumSum = cumSum + log_factorial(logFactorialSheet, N_pa_i_x_j, base = base)
  } 
  return(cumSum)
}