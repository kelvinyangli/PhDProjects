############################################################################################################
# generate a CPT for each node of a given DAG
# parameter values are sampled with Dirichlet distribution (not necessary symmetric)
# the input variables are: a directed acyclic graph (dag), the maximum number of values of each node (maxNumValues), 
# and the concentration parameter for a symmetric Dirichelet distribution (concentration)
# the range of concentration is (0, infinity)
# the larger the concentration is, the closer to uniform the sampled parameters look like, and hence the more 
# difficulty for any learner to discovery its dependency with the child node
# library(gtools)
############################################################################################################

generateCPTsWithAsymmetricDir = function(dag, maxNumValues, lower, upper, debug = FALSE) {
  
  allNodes = bnlearn::nodes(dag)
  numNodes = length(allNodes)
  
  # sample cardinalities for nodes 
  if (maxNumValues == 2) {
    cardinalities = rep(2, numNodes)
    #concentration = rep(concentration, 2) # equal concentration parameters for all values
  } else {
    cardinalities = sample(2:maxNumValues, numNodes, replace = TRUE)  
    #concentration = rep(concentration, max(cardinalities)) # equal concentration parameters for all values
  }
  
  cpts = list()
  
  if (debug) cat("* sampling cpt values \n")
  
  # generate cpts for all nodes
  for (i in 1:numNodes) {# for each node
    
    parents = dag$nodes[[i]]$parents
    
    concentration = runif(cardinalities[i], lower, upper) # sample a vector of concentration parameters
    
    if (length(parents) < 1) {# if current node has no parents
      
      sampledCPT = rdirichlet(1, concentration) # sample single cpt from dirichlet  
      
      #if (sum(sampledCPT) != 1) sampledCPT = sampledCPT / sum(sampledCPT) # if doesn't add up to 1 due to rounding then normalize 
        
      #if (any(sampledCPT == 0)) {# if 0 occurs in cpt then add 1 to the last digit and normalize
      #  # e.g. c(1, 0), and round to 2 digits, then get c(1.01, 0.01) and normalize
        
      #  if (debug) cat("before normalize:", sampledCPT, "\n")
        
      #  sampledCPT = sampledCPT + 10 ^ (-digits) # add 1 to last digit at each index
      #  sampledCPT = round(sampledCPT / sum(sampledCPT), digits) # normalize and round to the nth digits
        
      #  if (debug) cat("after normalize:", sampledCPT, "\n")
        
      #} # end if 
      
      cpts[[i]] = array(sampledCPT, dim = c(1, cardinalities[i]), dimnames = list(NULL, LETTERS[1:cardinalities[i]]))
      
    } else {
      
      parentsIndex = which(allNodes %in% parents)
      
      sampledCPT = rdirichlet(prod(cardinalities[parentsIndex]), concentration) # sample multiple cpts from dirichlet with the above concentration
      sampledCPT = t(sampledCPT) # take the transpose
      #sampledCPT = round(sampledCPT, digits)
      
      #columnSums = colSums(sampledCPT)
      
      #if (!all(columnSums == 1)) {
        
      #  indices = which(columnSums != 1) # find those columns that don't sum up to 1
        
      #  for (index in indices) sampledCPT[, index] = round(sampledCPT[, index] / columnSums[index], digits)
        
      #} # end checking if all columns sum up to 1
        
      #if (any(sampledCPT == 0)) {# if 0 occurs in cpt then add 1 to the last digit and normalize
        # e.g. c(1, 0), and round to 2 digits, then get c(1.01, 0.01) and normalize
          
      #  for (j in 1:ncol(sampledCPT)) {# for each column in the sampled cpt
          
      #    if (any(sampledCPT[, j] == 0)) {
            
      #      if (debug) cat("before normalize:", sampledCPT[, j], "\n")
            
      #      sampledCPT[, j] = sampledCPT[, j] + 10 ^ (-digits) # add 1 to last digit at each index
      #      sampledCPT[, j] = round(sampledCPT[, j] / sum(sampledCPT[, j]), digits) # normalize and round to the nth digits
            
      #      if (debug) cat("after normalize:", sampledCPT[, j], "\n")
            
      #    } # end if any column contains 0
          
      #  } # end for each column j
        
      #} # end checking if the entire sampledCPT contains 0
      
      dimNames = list(LETTERS[1:cardinalities[i]])
      
      for (k in 1:length(parents)) dimNames[[k + 1]] = LETTERS[1:cardinalities[parentsIndex[k]]]   
      
      names(dimNames) = c(allNodes[i], parents)
      
      cpts[[i]] = array(sampledCPT, dim = c(cardinalities[i], cardinalities[parentsIndex]), 
                        dimnames = dimNames)
      
    } # end else 
    
    if (any(is.na(cpts[[i]]))) {
      
      print("sampled CPT contains NA!")
      break 
      
    } # end if
    
  } # end for each node i
  
  names(cpts) = allNodes
  
  if (debug) cat("* converting to bn.fit \n")
  
  bnFit = custom.fit(dag, cpts) # convert cpts into bn.fit format
  
  return(bnFit)
  
}





