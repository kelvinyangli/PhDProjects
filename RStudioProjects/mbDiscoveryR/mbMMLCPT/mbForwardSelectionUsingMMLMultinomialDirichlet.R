
# MB discovery using 
mbForwardSelectionUsingMMLMultinomialDirichlet = function(data, node, arities, indexListPerNodePerValue, conPar, 
                                                          base = exp(1), debug = FALSE) {
  
  ##############################################################
  # get the basic information and 
  # create empty vectors for storing mb
  
  allNodes = names(data)
  nodeIndex = which(allNodes == node) # get index of the target node
  
  numNodes = ncol(data)
  sampleSize = nrow(data)
  
  mb = c()
  
  unCheckedIndices = (1:numNodes)[-nodeIndex]
  
  tempCachedIndicesList = list()
  ##############################################################
  # msg len for a single node with no parents
  # parentsIndices is given as an empty vector
  
  minMsgLen = mmlMultinomialDirichlet(nodeIndex, c(), indexListPerNodePerValue, c(), arities, sampleSize, conPar = conPar, base = base)
  
  if (debug) {
    
    cat("Search: Greedy search --- Score: mmlMultinomialDirichlet \n")
    cat("0 parent:", minMsgLen, "\n")
    
  }
  
  repeat {
    
    # repeat the process of computing mml for remaining unCheckedIndices
    # if unCheckedIndices is empty or all msg len > min msg len then stop
    
    index = 0 # initialize index to 0
    
    cachedIndicesList = tempCachedIndicesList
    
    if (length(unCheckedIndices) == 0) {
      
      if (debug) cat("BM is full! \n")
      break
      
    }
    
    # compute msg len for the target given each unchecked node as its parents
    for (i in 1:length(unCheckedIndices)) {
      
      parentsIndices = c(mb, unCheckedIndices[i])
      
      res = mmlMultinomialDirichlet(nodeIndex, parentsIndices, indexListPerNodePerValue, cachedIndicesList, 
                                    arities, sampleSize, conPar = conPar, base = base)
      msgLenCurrent = res$msgLen

      if (debug) cat("parents =", allNodes[c(mb, unCheckedIndices[i])], ":", msgLenCurrent, "\n")
      
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
      
      if (debug) cat("add", allNodes[unCheckedIndices[index]], "into mb \n")
      
      # add the node index with the minimum msg len into mb and remove it from unCheckedIndices
      mb = c(mb, unCheckedIndices[index])
      
      if (debug) cat("current mb is {", allNodes[mb], "} with msg len", minMsgLen, "\n")
      if (debug) cat("------------------------------- \n")
      
      unCheckedIndices = unCheckedIndices[-index]
      
    } # end else 
    
  } # end repeat
  
  return(allNodes[mb])
  
}




