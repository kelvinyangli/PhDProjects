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
  minMsgLen = msgLenWithNoPredictors2ndOrder(data, indicatorMatrix, nodeIndex, arities, allNodes, sigma = 3)$mml[[1]]
  
  if (debug) {
    
    cat("Search: Greedy search --- Score: mmlLogit2ndOrder \n")
    cat("0 parent:", minMsgLen, "\n")
    
  }
  
  formula1stOrder = formula2ndOrder = cachedFormula1stOrder = cachedFormula2ndOrder = c()
  
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
      
      #formula1stOrder = paste0(c(allNodes[mb], allNodes[unCheckedIndices[i]]), collapse = "+")
      formula1stOrder = paste0(cachedFormula1stOrder, "+", allNodes[unCheckedIndices[i]])
      
      if (length(mb) > 0) {
        
        for (j in 1:length(mb)) {
          
          formula2ndOrder = paste0(c(cachedFormula2ndOrder, paste0(allNodes[mb[j]], "*", allNodes[unCheckedIndices[i]])), collapse = "+")
          
          formula = paste0(allNodes[nodeIndex], "~", formula1stOrder, "+", formula2ndOrder)
          
          parentsIndices = c(mb, unCheckedIndices[i])
          
          msgLenCurrent = msgLenWithPredictors2ndOrder(data, indicatorMatrix, nodeIndex, parentsIndices, arities, allNodes, interactData, 
                                                       completeIndicatorMatrix, formula, sigma = 3)$mml[[1]]
          
          if (debug) cat("parents =", allNodes[c(mb, unCheckedIndices[i])], "(", formula, "):", msgLenCurrent, "\n")
          
          # if the current msg len is smaller then replace minMsgLen by the current 
          # and record the current index
          # else go to the next available node
          if (msgLenCurrent < minMsgLen) { 
            
            minMsgLen = msgLenCurrent
            index = i
            cachedFormula1stOrder = formula1stOrder # store 1st order terms in formula
            cachedFormula2ndOrder = formula2ndOrder # store 2nd order terms in formula
            
          } # end if 
          
        } # end for each variable j in current mb 
        
      } else {# if the current mb is empty
        
        formula = paste0(allNodes[nodeIndex], "~", formula1stOrder)
          
        parentsIndices = unCheckedIndices[i]
        
        msgLenCurrent = msgLenWithPredictors2ndOrder(data, indicatorMatrix, nodeIndex, parentsIndices, arities, allNodes, interactData, 
                                                     completeIndicatorMatrix, formula, sigma = 3)$mml[[1]]
        
        if (debug) cat("parents =", allNodes[unCheckedIndices[i]], "(", formula, "):", msgLenCurrent, "\n")
        
        # if the current msg len is smaller then replace minMsgLen by the current 
        # and record the current index
        # else go to the next available node
        if (msgLenCurrent < minMsgLen) { 
          
          minMsgLen = msgLenCurrent
          index = i
          cachedFormula = formula
          cachedFormula1stOrder = formula1stOrder # store 1st order terms in formula
  
        } # end if  
        
      } # end else if the current mb is empty
      
    } # end for each unCheckedIndices i
    
    if (index == 0) {
      
      if (debug) cat("Stop! No better choice for MB! \n")
      
      break 
      
    } else {
      
      if (debug) cat("add", allNodes[unCheckedIndices[index]], "into mb(", cachedFormula, ") \n")
      
      # add the node index with the minimum msg len into mb and remove it from unCheckedIndices
      mb = c(mb, unCheckedIndices[index])
      
      if (debug) cat("current mb is {", allNodes[mb], "} with msg len", minMsgLen, "\n")
      if (debug) cat("------------------------------- \n")
      
      unCheckedIndices = unCheckedIndices[-index]
      
    } # end else 
    
  } # end repeat
  
  return(allNodes[mb])
  
}




