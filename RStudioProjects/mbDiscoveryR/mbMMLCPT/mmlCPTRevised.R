###############################################################################
# this is a revised version of the original mmlCPT formula by Wallace & Boulton 1968
# in the original formula, a node value frequencies can take extreme values such as 0 
# and hence when encoding n[m], Wallace & Boulton used the formula for "weak decomposition"
# weak decomposition - choose(N+M-1, M-1)
# in this revised version, we don't allow extreme values to be taken, and hence "weak 
# decomposition" is replace by "strong decomposition"
# strong decomposition - chose(N-1, M-1)
# in terms of the actual results, the difference b/w the two version is very small
# most of the times, there is no noticeable difference
###############################################################################
msgLenWithParents.revised = function(nodeIndex, parentsIndices, indexListPerNodePerValue, arities, sampleSize, base = 2) {
  
  arityChild = arities[nodeIndex]
  
  numParents = length(parentsIndices)
  
  numParentsInstantiations = prod(arities[parentsIndices])
  
  fixedTerm = 0.5 * (numParentsInstantiations * (arityChild - 1)) * log((pi * exp(1) / 6), base = base)
  
  nonFixedTerm = 0
  
  # log((|x| - 1)!)
  logConstant = log(factorial(arityChild - 1), base = base)
  
  for (i in 1:numParentsInstantiations) {
    
    if (numParents == 1) { # if single parent then just use index i
      
      commonParentsIndices = indexListPerNodePerValue[[parentsIndices]][[i]]
      
      N_pa_i = length(commonParentsIndices)
      
      # sum_i^arityChild log(N(pa_i, x_i))!
      cumSum = singleParentComputation(nodeIndex, commonParentsIndices, arityChild, indexListPerNodePerValue, base = base)
      
    } else { # if more than 1 parent, use function to get potential combination
      
      # fix this part
      potentialCombination = getParentsInstantiationIndices(arities, numParents, parentsIndices, numParentsInstantiations, i)
      
      commonParentsIndices = intersectIndices(numParents, parentsIndices, indexListPerNodePerValue, potentialCombination)
      
      N_pa_i = length(commonParentsIndices)
      
      cumSum = multiParentsComputation(nodeIndex, arityChild, indexListPerNodePerValue, commonParentsIndices, base = base)
      
    } # end if else 
    
    nonFixedTerm = nonFixedTerm + logFactorial(N_pa_i - 1, base = base) - logConstant - 
      logFactorial(N_pa_i - arityChild, base = base) + logFactorial(N_pa_i, base = base) - cumSum
    
  } # end for i
  
  return(fixedTerm + nonFixedTerm)
  
}


# message length for a single node with no parents
msgLenWithoutParents.revised = function(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base) {
  
  arity = arities[nodeIndex]
  
  fixedTerm = 0.5 * (arity - 1) * log((pi * exp(1) / 6), base = base) + logFactorial(sampleSize - 1, base = base) - 
    log(factorial(arity - 1), base = base) - logFactorial(sampleSize - arity, base = base) + 
    logFactorial(sampleSize, base = base)
  
  cumSum = 0
  
  for (i in 1:arity) {
    
    N_x_i = length(indexListPerNodePerValue[[nodeIndex]][[i]])
    
    cumSum = cumSum + logFactorial(N_x_i, base = base)
    
  } # end for arity i
  
  return(fixedTerm - cumSum)
  
}


mmlCPT.revised = function(nodeIndex, parentsIndices, indexListPerNodePerValue, arities, sampleSize, base, noParents = FALSE) {
  
  if (noParents) {
    
    msgLen = msgLenWithoutParents.revised(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base)
    
  } else {
    
    msgLen = msgLenWithParents.revised(nodeIndex, parentsIndices, indexListPerNodePerValue, arities, sampleSize, base)
    
  }
  
  return(msgLen)
  
}



