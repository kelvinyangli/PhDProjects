# this function randomly generates polytrees according to the specified number 
# of variables and the maximum number of parents
randPolytree = function(nVars, maxNPas) {
  
  vars = paste0("V", 1:nVars) 
  adjmtx = matrix(0, nVars, nVars, dimnames = list(vars, vars))
  # starts from V2 since V1 is added into the graph with no choice for being 
  # a parent or child
  for (i in 2:nVars) {  
    
    # 1:(i-1) are the indices of nodes prior to the current node
    # when adding a new node into the current graph as either a child or parent
    # of a random node, each option is considered equal likely except when the 
    # maximum number of parents is reached at a selected node
    # c(beingCh, beingPa) is a vector of all valid choices, where -indices are 
    # used to distinguish b/w adding as a child or parent
    beingCh = -1:-(i - 1) 
    beingPa = 1:(i - 1)
    
    # only check if the maxNPas of a node is reached when the number of nodes 
    # prior is greater than maxNPas
    if (i > (maxNPas + 1)) { 
      
      # identify indices where maxNPas is reached 
      maxNPasReached = which(colSums(adjmtx[, beingPa]) == maxNPas) 
      # then remove these indices from consideration 
      if (length(maxNPasReached) > 0) beingPa = beingPa[-maxNPasReached]
      
    } # end if
    
    # sample from all valid choices, i.e. either a child or parent of a random 
    # node. Notice only one role is sampled for otherwise multiple paths could 
    # be generated
    nbr = sample(c(beingPa, beingCh), 1) # sample nbrs for v[i]
    
    # if the sampled indices is possitive then add the current node as a parent 
    # of its nbr, else add it as a child
    if (nbr > 0) {
      
      adjmtx[i, nbr] = 1
      
    } else {
      
      adjmtx[-nbr, i] = 1
      
    } # end else 
    
  } # end for i
  
  return(adjmtx)
  
}