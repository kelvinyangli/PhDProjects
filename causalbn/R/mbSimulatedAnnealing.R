################################################################################
# Perform simulated annealing to find minimal value of passed-in function func.
# (If you want to maximize something, consider multiplying by -1 first)
#
# INPUT
#  func:  the function name to be optimized - mml+cpt or revised
#  initial:    initial state - empty mb
#  maxIterations: number of iterations
#  step:  controls how fast Temp declines
#
# OUTPUT is printed
#
################################################################################
mbSimulatedAnnealing = function(data, node, score, initial, maxIterations, nbStep = 2, step = 0.1, base = 2, 
                              debug = FALSE) {
  
  ##############################################################
  # get the basic information and 
  # create empty vectors for storing mb
  
  allNodes = names(data)
  nodeIndex = which(allNodes == node) # get index of the target node
  
  numNodes = ncol(data)
  sampleSize = nrow(data)
  
  unCheckedIndices = (1:numNodes)[-nodeIndex]
  
  ##############################################################
  # get the arity of each node 
  # get the indices for each value of each node
  
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
  # initialize
  
  currentState = nbState = bestState = initial
  
  if (sum(initial) == 0) {
    
    currentScore = nbScore = bestScore = score(nodeIndex, c(), indexListPerNodePerValue, 
                                               arities, sampleSize, base, noParents = TRUE)
    
  } else {
    
    currentScore = nbScore = bestScore = score(nodeIndex, unCheckedIndices[initial == 1], indexListPerNodePerValue, 
                                               arities, sampleSize, base, noParents = FALSE)
    
  }
  
  
  if (debug) cat("Initial state:", allNodes[unCheckedIndices[initial == 1]], "-- score:", currentScore, "-- temp:", 1, "\n")
  
  for (i in 1:maxIterations) {  
    
    # update Temp
    Temp = (1 - step) ^ i
    
    # consider a random neighbor
    # flip nbstep values on the current state
  
    indices = sample(1:length(currentState), nbStep, replace = FALSE)
    
    nbState = currentState
    nbState[indices] = abs(currentState[indices] - 1)
    
    if (sum(nbState) == 0) {
      
      nbScore = score(nodeIndex, c(), indexListPerNodePerValue, arities, sampleSize, base, noParents = TRUE)
      
    } else {
      
      nbScore = score(nodeIndex, unCheckedIndices[nbState == 1], indexListPerNodePerValue, arities, sampleSize, base, noParents = FALSE)
      
    } # end if else 
    
    # update current state
    # this is where the jump to a worse value may be made, controlled by uniform variate
    if (nbScore < currentScore || runif(1, 0, 1) < exp(-(nbScore - currentScore) / Temp)) {
      
      currentState <- nbState
      currentScore <- nbScore
      
    } # end if
    
    # keep track of the best state so far
    if (nbScore < bestScore) {
      
      bestState <- nbState
      bestScore <- nbScore  
      
    } # end if 
    
    # message(sprintf("%i\t%.4f\t%.4f\t%.4f\t%.4f\t%.4f", k, bestState, bestScore, currentScore, nbScore, Temp))
    
    if (debug) {
      
      cat("current state:", allNodes[unCheckedIndices[currentState == 1]], "-- score:", currentScore, "-- temp:", Temp, "\n")
      
    } # end debug
  
  } # end for i
  
  return(allNodes[unCheckedIndices[bestState == 1]])
  
}


