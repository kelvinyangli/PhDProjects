# MB discovery using mml + cpt
# 
mbMMLCPT.revised = function(data, node, base = 2, debug = FALSE) {
  
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
  minMsgLen = mmlSingleNode.revised(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base = base)
  
  if (debug) cat("0 parent:", minMsgLen, "\n")
  
  repeat {
    
    # repeat the process of computing mml for remaining unCheckedIndecies
    # if unCheckedIndecies is empty or all msg len > min msg len then stop
    
    index = 0 # initialize index to 0
    
    if (length(unCheckedIndecies) == 0) {
      
      if (debug) cat("BM is full! \n")
      break
      
    }
    
    # compute msg len for the target given each unchecked node as its parents
    for (i in 1:length(unCheckedIndecies)) {
      
      parentsIndecies = c(mb, unCheckedIndecies[i])
      
      msgLenCurrent = mmlCPT.revised(nodeIndex, parentsIndecies, indexListPerNodePerValue, arities, sampleSize, base = base)
      
      if (debug) cat("parents =", allNodes[c(mb, unCheckedIndecies[i])], ":", msgLenCurrent, "\n")
      
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




