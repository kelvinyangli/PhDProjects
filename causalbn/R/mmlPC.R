# use mmlcpt to select pc(T)
# if the mml score of adding a node is less than the mml score of empty parents 
# then add this node 

# MB discovery using mml + cpt

mmlPC = function(data, node, dataInfo, base = 2, debug = FALSE) {
  
  ##############################################################
  # get the basic information and 
  # create empty vectors for storing mb
  
  allNodes = names(data)
  nodeIndex = which(allNodes == node) # get index of the target node
  
  sampleSize = nrow(data)
  
  pc = c()
  msgLen = c()
  
  # mml for empty pc
  minMsgLen = mmlCPT(nodeIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, sampleSize, base)
  
  ##############################################################
  
  if (debug) {
    
    scoreName = "mmlCPT"
    cat("Search: Greedy search --- Score:", scoreName, "\n")
    cat("0 parent:", minMsgLen, "\n")
    
  }
  
  for (i in 1:length(allNodes)) {
    
    if (i != nodeIndex) {
      
      msgLenCurrent = mmlCPT(nodeIndex, i, dataInfo$indexListPerNodePerValue, dataInfo$arities, sampleSize, base)
      
      if (msgLenCurrent < minMsgLen) {
        
        pc = c(pc, i) 
        msgLen = c(msgLen, msgLenCurrent)
        
        if (debug) cat("parents =", allNodes[i], ":", msgLenCurrent, "* \n") 
        
      } else {
        
        if (debug) cat("parents =", allNodes[i], ":", msgLenCurrent, "\n") 
        
      } # end else 
      
    } # end if 
    
  } # end for i
  
  if (length(pc) > 0) {
    
    ord = order(msgLen)
    pc = pc[ord]
    
  }
    
  return(allNodes[pc])
  
}




