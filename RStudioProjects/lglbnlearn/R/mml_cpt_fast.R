#' A faster version of mml_cpt
#'
#' There is no difference between mml_cpt_fast() and mml_cpt(), other than the speed difference.
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained
#' by the function count_occurance().
#' @param cachedIndicesList A vector of indices stored to speed up calculations.
#' @param arities A vector of variable ariteis.
#' @param sampleSize Sample size of a given data set.
#' @param parentsIndices Indices of parents nodes.
#' @param targetIndex Index of the target node.
#' @export
mml_cpt_fast = function(indexListPerNodePerValue, cachedIndicesList, arities, sampleSize,
                        parentsIndices, targetIndex) {
  if (length(parentsIndices) < 1) {
    res = mml_without_parents(indexListPerNodePerValue, arities, sampleSize, targetIndex)
  } else {
    res = mml_with_parents_fast(indexListPerNodePerValue, cachedIndicesList, arities, sampleSize,
                                parentsIndices, targetIndex)
  }
  return(res)
}
