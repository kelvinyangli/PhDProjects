# MB discovery using 
# indicatorMatrix = getIndicator(data)
mbForwardSelectionForMML2ndOrderLogit = function(data, node, arities, indexListPerNodePerValue,
                                   base = exp(1), indicatorMatrix = NULL, interactData = NULL, completeIndicatorMatrix = NULL, debug = FALSE) {
  
  ##############################################################
  # get the basic information and 
  # create empty vectors for storing mb
  allNodes = names(data)
  nodeIndex = which(allNodes == node) # get index of the target node
  numNodes = ncol(data)
  #sampleSize = nrow(data)
  mb = c()
  unCheckedIndices = (1:numNodes)[-nodeIndex]
  #tempCachedIndicesList = list()
  score = mmlLogit2ndOrder
  ##############################################################
  # msg len for a single node with no parents
  # parentsIndices is given as an empty vector
  minMsgLen = score(data, indicatorMatrix, nodeIndex, c(), arities, allNodes, interactData, completeIndicatorMatrix, sigma = 3)
    
  if (debug) {
    
    cat("Search: Greedy search --- Score: mmlLogit2ndOrder \n")
    cat("0 parent:", minMsgLen, "\n")
    
  }
  
  repeat {
    
    # repeat the process of computing mml for remaining unCheckedIndices
    # if unCheckedIndices is empty or all msg len > min msg len then stop
    index = 0 # initialize index to 0
    #cachedIndicesList = tempCachedIndicesList
    
    if (length(unCheckedIndices) == 0) {
      
      if (debug) cat("BM is full! \n")
      break
      
    }
    
    # compute msg len for the target given each unchecked node as its parents
    for (i in 1:length(unCheckedIndices)) {
      
      parentsIndices = c(mb, unCheckedIndices[i])
      
      msgLenCurrent = score(data, indicatorMatrix, nodeIndex, parentsIndices, arities, allNodes, interactData, 
                    completeIndicatorMatrix, sigma = 3)
        
      if (debug) cat("parents =", allNodes[c(mb, unCheckedIndices[i])], ":", msgLenCurrent, "\n")
      
      # if the current msg len is smaller then replace minMsgLen by the current 
      # and record the current index
      # else go to the next available node
      if (msgLenCurrent < minMsgLen) { 
        
        minMsgLen = msgLenCurrent
        index = i
        #tempCachedIndicesList = res$cachedIndicesList
        
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




