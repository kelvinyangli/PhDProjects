#############################################################################################
# this is an earlier version of mmlCPT
# this version works good, but the speed is slightly slower than the version which cach indices
#############################################################################################


logFactorial = function(n, base) {

  # if n = 0 then log of 0 factorial is 0
  ss = 0 
  
  if (n > 0) {
    
    for (i in 1:n) ss = ss + log(i, base = base)
    
  }
  
  return(ss)
  
}


getParentsInstantiationIndices = function(arities, numParents, parentsIndices, numParentsInstantiations, index) {
  
  if (index <= numParentsInstantiations) {
    
    indices = rep(0, numParents)
    
    indices[1] = ceiling(index / prod(arities[parentsIndices][2:numParents]))
    
    for (i in 1:(numParents - 1)) {
      
      indicator = ceiling(index / prod(arities[parentsIndices][(i + 1):numParents]))
      
      indices[i] = indicator %% arities[parentsIndices][i]
      
      if (indices[i] == 0) indices[i] = arities[parentsIndices][i]
      
    }
    
    
    lastIndex = index %% arities[parentsIndices][numParents] # take modular 
    
    if (lastIndex == 0) {
      
      indices[numParents] = arities[parentsIndices][numParents]
      
    } else {
      
      indices[numParents] = lastIndex
      
    }
    
    return(indices)
    
  } else {
    
    return(0)
    
  }
  
}


msgLenWithParents = function(nodeIndex, parentsIndices, indexListPerNodePerValue, arities, sampleSize, base) {
  
  arityChild = arities[nodeIndex]
  
  numParents = length(parentsIndices)
  
  numParentsInstantiations = prod(arities[parentsIndices])
  
  #fixedTerm = 0.5 * (numParentsInstantiations * (arityChild - 1)) * log((pi * exp(1) / 6), base = base)
  
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
      #ll[[i]]=commonParentsIndices
      N_pa_i = length(commonParentsIndices)
      
      cumSum = multiParentsComputation(nodeIndex, arityChild, indexListPerNodePerValue, commonParentsIndices, base = base)
      
    } # end if else 
    
    # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
    logNumerator = logFactorial(N_pa_i + arityChild - 1, base = base)
    
    nonFixedTerm = nonFixedTerm + logNumerator - logConstant - cumSum
    
    #cat(logNumerator - logConstant - cumSum, "\n")
  } # end for i
  
  return(nonFixedTerm)

}


# message length for a single node with no parents
msgLenWithoutParents = function(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base) {
  
  arity = arities[nodeIndex]
  
  # 0.5 * (arity - 1) * log((pi * exp(1) / 6), base = base)
  
  fixedTerm = logFactorial(sampleSize + arity - 1, base = base) - log(factorial(arity - 1), base = base)
  
  cumSum = 0
  
  for (i in 1:arity) {
    
    N_x_i = length(indexListPerNodePerValue[[nodeIndex]][[i]])
    
    cumSum = cumSum + logFactorial(N_x_i, base = base)
    
  } # end for arity i
  
  return(fixedTerm - cumSum)

}


mmlCPT = function(nodeIndex, parentsIndices, indexListPerNodePerValue, arities, sampleSize, base) {
  
  if (length(parentsIndices) < 1) {
    
    msgLen = msgLenWithoutParents(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base)
    
  } else {
    
    msgLen = msgLenWithParents(nodeIndex, parentsIndices, indexListPerNodePerValue, arities, sampleSize, base)
    
  }
  
  return(msgLen)
  
}
