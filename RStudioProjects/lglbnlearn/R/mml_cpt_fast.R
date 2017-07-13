#' A faster version of mml_cpt
#'
#' There is no difference between mml_cpt_fast() and mml_cpt(), other than the speed difference. 
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained 
#' by the function count_occurance().
#' @param cachedIndicesList
#' @param arities 
#' @param sampleSize
#' @param parentsIndices Indices of parents nodes. 
#' @param targetIndex Index of the target node.  
#' @param logFactorialSheet A pre-saved log factorial sheet for non-negative integers <= 10000.
#' @param base The base of the logarithm.  
#' @export
mml_cpt_fast = function(indexListPerNodePerValue, cachedIndicesList, arities, sampleSize, 
                        parentsIndices, targetIndex, logFactorialSheet, base) {
  if (length(parentsIndices) < 1) {
    res = mml_without_parents(indexListPerNodePerValue, arities, sampleSize, targetIndex, 
                              logFactorialSheet, base)
  } else {
    res = mml_with_parents_fast(indexListPerNodePerValue, cachedIndicesList, arities, sampleSize, 
                                parentsIndices, targetIndex, logFactorialSheet, base)
  }
  return(res)
}