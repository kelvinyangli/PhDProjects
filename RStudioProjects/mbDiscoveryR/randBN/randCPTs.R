############################################################################################################
# generate a random CPT for each node of a given DAG
# parameter values are sampled with symmetric Dirichlet distribution 
# the input variables are: a directed acyclic graph (dag), the maximum number of values of each node (maxNumValues), 
# and the concentration parameter for a symmetric Dirichelet distribution (concentration)
# the range of concentration is (0, infinity)
# the larger the concentration is, the closer to uniform the sampled parameters look like, and hence the more 
# difficulty for any learner to discovery its dependency with the child node
# library(gtools)
############################################################################################################

randCPTs = function(dag, maxNumValues, concentration, debug = FALSE) {
  # the larger concentration is, the more concentration the distribution is, hence values are more close to the middle point 
  
  allNodes = bnlearn::nodes(dag)
  numNodes = length(allNodes)
  
  # sample cardinalities for nodes 
  if (maxNumValues == 2) {
    cardinalities = rep(2, numNodes)
    concentration = rep(concentration, 2) # equal concentration parameters for all values
  } else {
    cardinalities = sample(2:maxNumValues, numNodes, replace = TRUE)  
    concentration = rep(concentration, max(cardinalities)) # equal concentration parameters for all values
  }
  
  cpts = list()
  
  if (debug) cat("* sampling cpt values \n")
  
  # generate cpts for all nodes
  for (i in 1:numNodes) {
    
    parents = dag$nodes[[i]]$parents
    
    if (length(parents) < 1) {
      
      sampledCPT = rdirichlet(1, concentration[1:cardinalities[i]]) # sample single cpt from dirichlet  
      
      cpts[[i]] = array(sampledCPT, dim = c(1, cardinalities[i]), dimnames = list(NULL, LETTERS[1:cardinalities[i]]))
      
    } else {
      
      parentsIndex = which(allNodes %in% parents)
      
      sampledCPT = rdirichlet(prod(cardinalities[parentsIndex]), concentration[1:cardinalities[i]]) # sample multiple cpts from dirichlet with the above concentration
      sampledCPT = t(sampledCPT) # take the transpose
      
      dimNames = list(LETTERS[1:cardinalities[i]])
      
      for (j in 1:length(parents)) {
        dimNames[[j + 1]] = LETTERS[1:cardinalities[parentsIndex[j]]]   
      }
      
      names(dimNames) = c(allNodes[i], parents)
      
      cpts[[i]] = array(sampledCPT, dim = c(cardinalities[i], cardinalities[parentsIndex]), 
                        dimnames = dimNames)
      
    }
  }
  
  names(cpts) = allNodes
  
  if (debug) cat("* converting to bn.fit \n")
  
  bnFit = custom.fit(dag, cpts) # convert cpts into bn.fit format
  
  return(bnFit)
  
}
