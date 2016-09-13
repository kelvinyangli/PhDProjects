# MB discovery using mml + cpt

getDataInfo = function(data) {
  
  ##############################################################
  # get the arity of each node 
  # get the indices for each value of each node
  
  arities = rep(0, ncol(data))
  
  indexListPerNodePerValue = list()
  
  for (i in 1:ncol(data)) {
    
    arities[i] = nlevels(data[, i])
    
    # get the indecides for each value of node i
    indexListPerValue = list()
    
    for (j in 1:arities[i]) {
      
      indexListPerValue[[j]] = which(data[, i] == levels(data[, i])[j]) 
      
    } # end for arity j
    
    indexListPerNodePerValue[[i]] = indexListPerValue
    
  } # end for node i
  
  ls = list(arities = arities, indexListPerNodePerValue = indexListPerNodePerValue)
  
  return(ls)
  
}

mbForwardSelection = function(data, node, score, arities, indexListPerNodePerValue, 
                              base = exp(1), indicatorMatrix = NULL, mbSize = 1000, interaction = FALSE, debug = FALSE) {
  
  ##############################################################
  # get the basic information and 
  # create empty vectors for storing mb
  
  allNodes = names(data)
  nodeIndex = which(allNodes == node) # get index of the target node
  
  numNodes = ncol(data)
  sampleSize = nrow(data)
  
  mb = c()
  
  unCheckedIndices = (1:numNodes)[-nodeIndex]
  
  ##############################################################
  # when considering interaction between predictors, ie. 2nd order logit
  if (interaction) {
    
    # compute all pairs of interaction data
    interactData = getInteractData(indicatorMatrix)
    
    # joint the original indicator matrix with the interaction matrix
    completeIndicatorMatrix = cbind(indicatorMatrix, interactData)
    
    completeIndicatorMatrix = as.matrix(completeIndicatorMatrix)
    
  }

  ##############################################################
  # msg len for a single node with no parents
  # parentsIndices is given as an empty vector
  
  if (!is.null(indicatorMatrix)) {# mmlLogit
    
    if (interaction) { # 2nd order logit
      
      minMsgLen = score(data, indicatorMatrix, nodeIndex, NULL, arities, allNodes, interactData, completeIndicatorMatrix, sigma = 3)
      
    } else { # 1st order logit
      
      minMsgLen = score(data, indicatorMatrix, nodeIndex, NULL, arities, allNodes, sigma = 3)
      
    }
    
  } else {# mmlCPT
    
    minMsgLen = score(nodeIndex, c(), indexListPerNodePerValue, arities, sampleSize, base)
    
  } # end if 
  ##############################################################
  
  if (debug) {
    
    if (!is.null(indicatorMatrix)) {
      
      scoreName = "mmlLogit"
      
    } else {
      
      scoreName = "mmlCPT"
      
    }
    
    cat("Search: Greedy search --- Score:", scoreName, "\n")
    cat("0 parent:", minMsgLen, "\n")
   
  }
  
  repeat {
    
    # repeat the process of computing mml for remaining unCheckedIndices
    # if unCheckedIndices is empty or all msg len > min msg len then stop
    
    index = 0 # initialize index to 0
    
    if (length(unCheckedIndices) == 0) {
      
      if (debug) cat("BM is full! \n")
      break
      
    }
    
    # compute msg len for the target given each unchecked node as its parents
    for (i in 1:length(unCheckedIndices)) {
      
      parentsIndices = c(mb, unCheckedIndices[i])
      
      ##############################################################
      # msg len with at least 1 parent
      if (!is.null(indicatorMatrix)) { # mml logit
        
        if (interaction) { # 2nd order logit
          
          msgLenCurrent = score(data, indicatorMatrix, nodeIndex, parentsIndices, arities, allNodes, interactData, completeIndicatorMatrix, sigma = 3)
          
        } else { # 1st order logit
          
          msgLenCurrent = score(data, indicatorMatrix, nodeIndex, parentsIndices, arities, allNodes, sigma = 3)
          
        }
        
      } else { # mmlCPT
        
        msgLenCurrent = score(nodeIndex, parentsIndices, indexListPerNodePerValue, arities, sampleSize, base)
        
      }
      ##############################################################
      
      if (debug) cat("parents =", allNodes[c(mb, unCheckedIndices[i])], ":", msgLenCurrent, "\n")
      
      # if the current msg len is smaller then replace minMsgLen by the current 
      # and record the current index
      # else go to the next available node
      if (msgLenCurrent < minMsgLen) { 
        
        minMsgLen = msgLenCurrent
        index = i
        
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




