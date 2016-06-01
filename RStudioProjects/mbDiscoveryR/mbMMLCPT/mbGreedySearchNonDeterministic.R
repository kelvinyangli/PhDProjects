# MB discovery using mml + cpt
# 
mbGSWithMMLCPT = function(data, node, base = 2, nonDeterministic = FALSE, debug = FALSE) {
  
  ##############################################################
  # get the basic information and 
  # create empty vectors for storing mb
  
  allNodes = names(data)
  nodeIndex = which(allNodes == node) # get index of the target node
  
  numNodes = ncol(data)
  sampleSize = nrow(data)
  
  mb = c()
  #mb = rep(0, numNodes - 1)
  #unCheckedNodes = allNodes[allNodes != node] # remove target node
  unCheckedIndecies = (1:numNodes)[-nodeIndex]
  
  ##############################################################
  # get the arity of each node 
  # get the indecies for each value of each node
  
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
  
  # msg len for a single node with no parents
  minMsgLen = mmlSingleNode(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base = base)
  
  if (debug) cat("0 parent:", minMsgLen, "\n")
  
  repeat { # iteratively check for inclusion 
    
    # repeat the process of computing mml for remaining unCheckedIndecies
    # if unCheckedIndecies is empty or all msg len > min msg len then stop
    
    index = 0 # initialize index to 0
    
    if (length(unCheckedIndecies) == 0) {
      
      if (debug) cat("BM is full! \n")
      break
      
    }
    
    minMsgLenCurrent = Inf # minimum msg len in the current iterative step
    
    indexCurrentMin = 0 # to store index for current node with min msg len
    
    ##############################################################
    # compute msg len for the target given each unchecked node as its parents
    for (i in 1:length(unCheckedIndecies)) {
      
      parentsIndecies = c(mb, unCheckedIndecies[i])
      
      msgLenCurrent = mmlCPT(nodeIndex, parentsIndecies, indexListPerNodePerValue, arities, sampleSize, base = base)
      
      if (msgLenCurrent <= minMsgLenCurrent) { # if current msg len is less than current minimum
        
        minMsgLenCurrent = msgLenCurrent 
        indexCurrentMin = i
        
      } # end if
      
      if (debug) cat("parents =", allNodes[c(mb, unCheckedIndecies[i])], ":", msgLenCurrent, "\n")
      
    } # end for i 
    
    ##############################################################
    # if the local minimum is smaller than global minimum, then update global minimum  
    # for both msg len and the node index
    # else update the global minimum with larger msg len with certain probability
    # the idea is similar to simulated annealing, that is generate a random number u b/w 0 and 1
    # and compare u with the ratio of global min/local mini, if u is small that is when the global and
    # local min are quite close, then update global min by local min, else do nothing
    
    if (nonDeterministic) { # if nonDeterministic is true
      
      if (minMsgLenCurrent < minMsgLen) { 
        
        minMsgLen = minMsgLenCurrent
        index = indexCurrentMin
        
      } else {
        
        u = runif(1, 0.98, 1) # a random number b/w 0.95 and 1 
        
        ratio = minMsgLen/minMsgLenCurrent
        
        if (debug) cat("u =", u, "; ratio = ", ratio, "\n")
        
        if (u < ratio) {
          
          minMsgLen = minMsgLenCurrent
          index = indexCurrentMin
          
        } # end if 
        
      } # end if else
      
    } else { # if deterministic greedy search is true
      
      if (minMsgLenCurrent < minMsgLen) { 
        
        minMsgLen = minMsgLenCurrent
        index = indexCurrentMin
        
      } # end if 
      
    } # end if else 
    
    ##############################################################
    if (index == 0) {
      
      if (debug) cat("Stop! No better choice for MB! \n")
      
      break 
      
    } else {
      
      if (debug) cat("add", allNodes[unCheckedIndecies[index]], "into mb \n")
      
      # add the node index with the minimum msg len into mb and remove it from unCheckedIndecies
      mb = c(mb, unCheckedIndecies[index])
      
      if (debug) cat("current mb is {", allNodes[mb], "} with msg len", minMsgLen, "\n")
      if (debug) cat("------------------------------- \n")
      
      unCheckedIndecies = unCheckedIndecies[-index]
      
    } # end else 
    
  } # end repeat
  
  return(allNodes[mb])
  
}




