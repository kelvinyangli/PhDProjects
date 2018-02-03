#' A function to calculate the mml of cpt model
#'
#' There is no difference between mml_cpt_fast() and mml_cpt(), other than the speed difference.
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained
#' by the function count_occurance().
#' @param arities A vector of varaible arities.
#' @param sampleSize Sample size of the given data set.
#' @param parentsIndices Indices of parents nodes.
#' @param targetIndex Index of the target node.
#' @export
mml_cpt = function(indexListPerNodePerValue, arities, sampleSize, parentsIndices, targetIndex) {

  if (length(parentsIndices) < 1) {

    msgLen = mml_without_parents(indexListPerNodePerValue, arities, sampleSize, targetIndex)

  } else {

    msgLen = mml_with_parents(indexListPerNodePerValue, arities, sampleSize, parentsIndices, targetIndex)

  }

  return(msgLen)

}
