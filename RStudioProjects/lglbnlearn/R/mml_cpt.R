#' A function to calculate the mml of cpt model
#'
#' There is no difference between mml_cpt_fast() and mml_cpt(), other than the speed difference. 
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained 
#' by the function count_occurance().
#' @param arities 
#' @param sampleSize
#' @param parentsIndices Indices of parents nodes. 
#' @param targetIndex Index of the target node.  
#' @param logFactorialSheet A pre-saved log factorial sheet for non-negative integers <= 10000.
#' @param base The base of the logarithm.  
#' @export
mml_cpt = function(indexListPerNodePerValue, arities, sampleSize, parentsIndices, targetIndex, 
                   logFactorialSheet, base = exp(1)) {
  
  if (length(parentsIndices) < 1) {
    
    msgLen = mml_without_parents(indexListPerNodePerValue, arities, sampleSize, targetIndex, 
                                 logFactorialSheet, base)
    
  } else {
    
    msgLen = mml_with_parents(indexListPerNodePerValue, arities, sampleSize, parentsIndices, targetIndex, 
                                 logFactorialSheet, base)
    
  }
  
  return(msgLen)
  
}