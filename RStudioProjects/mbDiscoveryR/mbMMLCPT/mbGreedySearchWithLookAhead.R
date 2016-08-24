# MB discovery using mml + cpt
# 
mbGreedySearchWithLookAhead = function(data, node, score, base = 2, debug = FALSE) {
  
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
  minMsgLen = score(nodeIndex, c(), indexListPerNodePerValue, arities, sampleSize, base)
  
  if (debug) cat("0 parent:", minMsgLen, "\n")
  
  repeat {
    
    # repeat the process of computing mml for remaining unCheckedIndecies
    # if unCheckedIndecies is empty or all msg len > min msg len then stop
    
    index = 0 # initialize index to 0
    
    if (length(unCheckedIndecies) == 0) {
      
      if (debug) cat("BM is full! \n")
      break
      
    }
    
    localMini = Inf
    indexLocalMini = 0
    
    # compute msg len for the target given each unchecked node as its parents
    for (i in 1:length(unCheckedIndecies)) {
      
      parentsIndecies = c(mb, unCheckedIndecies[i])
      
      msgLenCurrent = score(nodeIndex, parentsIndecies, indexListPerNodePerValue, arities, sampleSize, base)
      
      # updating local minimum and node index
      if (msgLenCurrent < localMini) {
        
        localMini = msgLenCurrent
        indexLocalMini = i
        
      }
      
      if (debug) cat("parents =", allNodes[c(mb, unCheckedIndecies[i])], ":", msgLenCurrent, "\n")
      
    } # end for i 
    
    # if localMini < minMsgLen then replace minMsgLen by the localMini 
    # add node with localMini into mb
    if (localMini < minMsgLen) { 
      
      minMsgLen = localMini
      index = indexLocalMini
      
      # add the node index with the minimum msg len into mb and remove it from unCheckedIndecies
      mb = c(mb, unCheckedIndecies[index])
      
      if (debug) {
        
        cat("current mb is {", allNodes[mb], "} with msg len", minMsgLen, "\n")
        cat("add", allNodes[unCheckedIndecies[index]], "into mb \n")
        cat("------------------------------- \n")
        
      }
      
      unCheckedIndecies = unCheckedIndecies[-index] # remove node index from unCheckedIndecies
      
    } else { # if localMini >= minMsgLen then start look ahead
      
      # only start look ahead if there are more than one node remaining
      if (length(unCheckedIndecies) > 1) {
        
        ##############################################################
        # start look ahead
        if (debug) cat("***Start look ahead*** \n")
        
        tempUncheckedIndecies = unCheckedIndecies[-indexLocalMini]
        tempMB = c(mb, unCheckedIndecies[indexLocalMini])
        
        tempLocalMini = Inf
        tempIndexLocalMini = 0 
        
        for (i in 1:length(tempUncheckedIndecies)) {
          
          tempParentsIndecies = c(tempMB, tempUncheckedIndecies[i])
          
          tempMsgLenCurrent = score(nodeIndex, tempParentsIndecies, indexListPerNodePerValue, arities, sampleSize, base)
          
          # updating local minimum and node index
          if (tempMsgLenCurrent < tempLocalMini) {
            
            tempLocalMini = tempMsgLenCurrent
            tempIndexLocalMini = i
            
          }
          
          if (debug) cat("parents =", allNodes[c(tempMB, tempUncheckedIndecies[i])], ":", tempMsgLenCurrent, "\n")
          
        } # end for i 
        
        # deciding whether to accept look ahead result or not 
        if (tempLocalMini < minMsgLen) {
          
          minMsgLen = tempLocalMini
          index = tempIndexLocalMini
          
          # add the node index with the minimum msg len into mb and remove it from unCheckedIndecies
          mb = c(mb, unCheckedIndecies[c(indexLocalMini, index)])
          
          if (debug) {
            
            cat("add", allNodes[unCheckedIndecies[c(indexLocalMini, index)]], "into mb \n")
            cat("current mb is {", allNodes[mb], "} with msg len", minMsgLen, "\n")
            cat("***End look ahead*** \n")
            cat("------------------------------- \n")
            
          }
          
          unCheckedIndecies = unCheckedIndecies[-c(indexLocalMini, index)] # remove node index from unCheckedIndecies
          
        } else { # stop look ahead
          
          if (debug) cat("Stop! No better choice for MB! \n")
          
          break 
          
        } # end else 
        
        # end look ahead
        ##############################################################
        
      } # end if 
      
    } # end else 
    
  } # end repeat
  
  return(allNodes[mb])
  
}




