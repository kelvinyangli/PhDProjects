# MB discovery using mml + cpt
# 
mbBackwardElemination = function(data, node, score, base = 2, indicatorMatrix = NULL, debug = FALSE) {
  
  ##############################################################
  # get the basic information and 
  # create empty vectors for storing mb
  
  allNodes = names(data)
  nodeIndex = which(allNodes == node) # get index of the target node
  
  numNodes = ncol(data)
  sampleSize = nrow(data)
  
  # start with the full mb
  mb = (1:numNodes)[-nodeIndex]
  
  #blackList = c()
  
  ##############################################################
  # get the arity of each node 
  # get the indices for each value of each node
  # 
  arities = rep(0, numNodes)
  
  indexListPerNodePerValue = list()
  
  for (i in 1:numNodes) {
    
    arities[i] = nlevels(data[, i])
    
    # get the indecides for each value of node i
    indexListPerValue = list()
    
    for (j in 1:arities[i]) {
      
      indexListPerValue[[j]] = which(data[, i] == levels(data[, i])[j]) 
      
    } # end for arity j
    
    indexListPerNodePerValue[[i]] = indexListPerValue
    
  } # end for node i
  
  ##############################################################
  # msg len for a single node with no parents
  # parentsIndices is given as an empty vector
  
  if (!is.null(indicatorMatrix)) { # use mmlLogit
    
    minMsgLen = score(data, indicatorMatrix, nodeIndex, mb, arities, allNodes, sigma = 3, base, noPredictors = TRUE)
    
  } else { # use mmlCPT
    
    minMsgLen = score(nodeIndex, mb, indexListPerNodePerValue, arities, sampleSize, base)
    
  } # end if 
  
  if (debug) {
    
    if (!is.null(indicatorMatrix)) {
      
      scoreName = "mmlLogit"
      
    } else {
      
      scoreName = "mmlCPT"
      
    }
    
    cat("Search: Backward elemination --- Score:", scoreName, "\n")
    cat("full mb:", minMsgLen, "\n")
    
  }
  
  repeat {
    
    # delete one node at a time and find the node which has the largest decreasing of mml score
    # if the resulting mml score is less than global mini then replace and keep going
    # if mb is empty or all msg len > min msg len then stop
    
    index = 0 # initialize index to 0
    
    if (length(mb) == 0) {
      
      if (debug) cat("BM is empty! \n")
      break
      
    }
    
    # compute msg len for the target given each mb-x_i as the parents
    for (i in 1:length(mb)) {
      

      if (!is.null(indicatorMatrix)) {
        
        msgLenCurrent = score(data, indicatorMatrix, nodeIndex, mb[-i], arities, allNodes, sigma = 3, base, noPredictors = FALSE)
        
      } else {
        
        msgLenCurrent = score(nodeIndex, mb[-i], indexListPerNodePerValue, arities, sampleSize, base)
        
      } # end if else 
      
      if (debug) cat("parents =", allNodes[mb[-i]], ":", msgLenCurrent, "\n")
      
      # if the current msg len is smaller then replace minMsgLen by the current 
      # and record the current index
      # else go to the next available node
      if (msgLenCurrent < minMsgLen) { 
        
        minMsgLen = msgLenCurrent
        index = i
        
      } # end if 
      
    } # end for i 
    
    if (index == 0) {
      
      if (debug) cat("Stop! No better node to delete from current MB! \n")
      
      break 
      
    } else {
      
      if (debug) cat("delete", allNodes[mb[index]], "from current mb \n")
      
      # add the node index with the minimum msg len into mb and remove it from unCheckedIndices
      mb = mb[-index]
      
      if (debug) cat("current mb is {", allNodes[mb], "} with msg len", minMsgLen, "\n")
      if (debug) cat("------------------------------- \n")
      
      #unCheckedIndices = unCheckedIndices[-index]
      
    } # end else 
    
  } # end repeat
  
  return(allNodes[mb])
  
}




