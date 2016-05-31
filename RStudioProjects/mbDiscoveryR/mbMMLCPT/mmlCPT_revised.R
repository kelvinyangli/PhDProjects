# code is correct
# sanity check has done

mmlCPT.revised = function(nodeIndex, parentsIndices, indexListPerNodePerValue, arities, sampleSize, base = 2) {
  
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
mmlSingleNode.revised = function(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base) {
  
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



