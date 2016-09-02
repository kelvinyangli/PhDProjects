# same as generateCPT, expect specific arities are given 
generateCPTs2 = function(dag, arities, concentration, debug = FALSE) {

  allNodes = bnlearn::nodes(dag)
  numNodes = length(allNodes)
  
  concentration = rep(concentration, max(arities)) # equal concentration parameters for all values
  
  cpts = list()
  
  if (debug) cat("* sampling cpt values \n")
  
  # generate cpts for all nodes
  for (i in 1:numNodes) {
    
    parents = dag$nodes[[i]]$parents
    
    if (length(parents) < 1) {
      
      sampledCPT = rdirichlet(1, concentration[1:arities[i]]) # sample single cpt from dirichlet  
      
      cpts[[i]] = array(sampledCPT, dim = c(1, arities[i]), dimnames = list(NULL, LETTERS[1:arities[i]]))
      
    } else {
      
      parentsIndex = which(allNodes %in% parents)
      
      sampledCPT = rdirichlet(prod(arities[parentsIndex]), concentration[1:arities[i]]) # sample multiple cpts from dirichlet with the above concentration
      sampledCPT = t(sampledCPT) # take the transpose
      
      dimNames = list(LETTERS[1:arities[i]])
      
      for (j in 1:length(parents)) {
        dimNames[[j + 1]] = LETTERS[1:arities[parentsIndex[j]]]   
      }
      
      names(dimNames) = c(allNodes[i], parents)
      
      cpts[[i]] = array(sampledCPT, dim = c(arities[i], arities[parentsIndex]), 
                        dimnames = dimNames)
      
    }
  }
  
  names(cpts) = allNodes
  
  if (debug) cat("* converting to bn.fit \n")
  
  bnFit = custom.fit(dag, cpts) # convert cpts into bn.fit format
  
  return(bnFit)
  
}