# code is correct
# sanity check has done

mmlCPT.revised = function(nodeIndex, parentsIndecies, indexListPerNodePerValue, arities, sampleSize, base = 2) {
  
  arityChild = arities[nodeIndex]
  
  numParents = length(parentsIndecies)
  
  numParentsInstantiations = prod(arities[parentsIndecies])
  
  fixedTerm = 0.5 * (numParentsInstantiations * (arityChild - 1)) * log((pi * exp(1) / 6), base = base)
  
  nonFixedTerm = 0
  
  # log((|x| - 1)!)
  #logConstant = log(factorial(arityChild - 1), base = base)
  
  for (i in 1:numParentsInstantiations) {
    
    if (numParents == 1) { # if single parent then just use index i
      
      commonParentsIndecies = indexListPerNodePerValue[[parentsIndecies]][[i]]
      
      N_pa_i = length(commonParentsIndecies)
      
      # sum_i^arityChild log(N(pa_i, x_i))!
      cumSum = singleParentComputation(nodeIndex, commonParentsIndecies, arityChild, indexListPerNodePerValue, base = base)
      
    } else { # if more than 1 parent, use function to get potential combination
      
      # fix this part
      potentialCombination = getParentsInstantiationIndecies(arities, numParents, parentsIndecies, numParentsInstantiations, i)
      
      commonParentsIndecies = intersectIndecies(numParents, parentsIndecies, indexListPerNodePerValue, potentialCombination)
      
      N_pa_i = length(commonParentsIndecies)
      
      cumSum = multiParentsComputation(nodeIndex, arityChild, indexListPerNodePerValue, commonParentsIndecies, base = base)
      
    } # end if else 
    
    nonFixedTerm = nonFixedTerm + (arityChild - 1) * log(N_pa_i, base = base) + 
      logFactorial(N_pa_i, base = base) - cumSum
    
  } # end for i
  
  return(fixedTerm + nonFixedTerm)
  
}


# message length for a single node with no parents
mmlSingleNode.revised = function(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base) {
  
  arity = arities[nodeIndex]
  
  fixedTerm = 0.5 * (arity - 1) * log((pi * exp(1) / 6), base = base) + (arity - 1) * log(sampleSize, base = base) + 
    logFactorial(sampleSize, base = base)
  
  cumSum = 0
  
  for (i in 1:arity) {
    
    N_x_i = length(indexListPerNodePerValue[[nodeIndex]][[i]])
    
    cumSum = cumSum + logFactorial(N_x_i, base = base)
    
  } # end for arity i
  
  return(fixedTerm - cumSum)
  
}



