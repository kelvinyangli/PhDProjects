#' A faster version of a forward greedy search 
#'
#' This is a faster version of a forward greedy search. It can only be used with mml_cpt_fast() 
#' as the objective function. 
#' @param data A categorical data set. 
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained 
#' by the function count_occurance().
#' @param arities A vector of arities. 
#' @param sampleSize Sample size of the given data. 
#' @param node The target node. 
#' @param base The base of the logarithm. Default is the natural log. 
#' @param logFactorialSheet A pre-saved log factorial sheet for non-negative integers <= 10000.
#' @param debug A boolearn argument to display more details. 
#' @export
forward_greedy_fast = function(data, indexListPerNodePerValue, arities, sampleSize, node, logFactorialSheet,
                                   base = exp(1), debug = FALSE) {
  
  vars = names(data)
  targetIndex = which(vars == node) # get index of the target node
  nvars = ncol(data)
  mb = c()
  unCheckedIndices = (1:nvars)[-targetIndex]
  tempCachedIndicesList = list()
  
  minMsgLen = mml_cpt_fast(indexListPerNodePerValue, cachedIndicesList, arities, sampleSize, 
                           c(), targetIndex, logFactorialSheet, base)
  
  if (debug) {
    cat("Search: Forward greedy with mmlCPT \n")
    cat("0 parent:", minMsgLen, "\n")
  }
  
  # repeat the process of computing mml for remaining unCheckedIndices
  # if unCheckedIndices is empty or all msg len > min msg len then stop
  repeat {
    
    index = 0 # initialize index to 0
    cachedIndicesList = tempCachedIndicesList
    if (length(unCheckedIndices) == 0) {
      if (debug) cat("BM is full! \n")
      break
    }
    
    # compute msg len for the target given each unchecked node as its parents
    for (i in 1:length(unCheckedIndices)) {
      parentsIndices = c(mb, unCheckedIndices[i])
      res = mml_cpt_fast(indexListPerNodePerValue, cachedIndicesList, arities, sampleSize, 
                         parentsIndices, targetIndex, logFactorialSheet, base)
      msgLenCurrent = res$msgLen
      #cachedIndicesList = res$cachedIndicesList
      
      if (debug) cat("parents =", vars[c(mb, unCheckedIndices[i])], ":", msgLenCurrent, "\n")
      
      # if the current msg len is smaller then replace minMsgLen by the current 
      # and record the current index
      # else go to the next available node
      if (msgLenCurrent < minMsgLen) { 
        minMsgLen = msgLenCurrent
        index = i
        tempCachedIndicesList = res$cachedIndicesList
      } # end if 
    } # end for i 
    
    if (index == 0) {
      if (debug) cat("Stop! No better choice for MB! \n")
      break 
    } else {
      if (debug) cat("add", vars[unCheckedIndices[index]], "into mb \n")
      # add the node index with the minimum msg len into mb and remove it from unCheckedIndices
      mb = c(mb, unCheckedIndices[index])
      if (debug) cat("current mb is {", vars[mb], "} with msg len", minMsgLen, "\n")
      if (debug) cat("------------------------------- \n")
      unCheckedIndices = unCheckedIndices[-index]
    } # end else 
  } # end repeat

  return(vars[mb])
  
}




