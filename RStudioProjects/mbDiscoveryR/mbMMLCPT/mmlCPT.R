# code is correct
# sanity check has done


logFactorial = function(n, base) {

  # if n = 0 then log of 0 factorial is 0
  ss = 0 
  
  if (n > 0) {
    
    for (i in 1:n) ss = ss + log(i, base = base)
    
  }
  
  return(ss)
  
}


getParentsInstantiationIndecies = function(arities, numParents, parentsIndecies, numParentsInstantiations, index) {
  
  if (index <= numParentsInstantiations) {
    
    indecies = rep(0, numParents)
    
    indecies[1] = ceiling(index / prod(arities[parentsIndecies][2:numParents]))
    
    for (i in 1:(numParents - 1)) {
      
      indicator = ceiling(index / prod(arities[parentsIndecies][(i + 1):numParents]))
      
      indecies[i] = indicator %% arities[parentsIndecies][i]
      
      if (indecies[i] == 0) indecies[i] = arities[parentsIndecies][i]
      
    }
    
    
    lastIndex = index %% arities[parentsIndecies][numParents] # take modular 
    
    if (lastIndex == 0) {
      
      indecies[numParents] = arities[parentsIndecies][numParents]
      
    } else {
      
      indecies[numParents] = lastIndex
      
    }
    
    return(indecies)
    
  } else {
    
    return(0)
    
  }
  
}


mmlCPT = function(nodeIndex, parentsIndecies, indexListPerNodePerValue, arities, sampleSize, base = 2) {
  
  arityChild = arities[nodeIndex]
  
  numParents = length(parentsIndecies)
  
  numParentsInstantiations = prod(arities[parentsIndecies])
  
  fixedTerm = 0.5 * (numParentsInstantiations * (arityChild - 1)) * log((pi * exp(1) / 6), base = base)
  
  nonFixedTerm = 0
  
  # log((|x| - 1)!)
  logConstant = log(factorial(arityChild - 1), base = base)
  
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
    
    # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
    logNumerator = logFactorial(N_pa_i + arityChild - 1, base = base)
    
    nonFixedTerm = nonFixedTerm + logNumerator - logConstant - cumSum
    
    #cat(logNumerator - logConstant - cumSum, "\n")
  } # end for i
  
  return(fixedTerm + nonFixedTerm)
  
}


# message length for a single node with no parents
mmlSingleNode = function(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base) {
  
  arity = arities[nodeIndex]
  
  fixedTerm = 0.5 * (arity - 1) * log((pi * exp(1) / 6), base = base) + 
    logFactorial(sampleSize + arity - 1, base = base) - log(factorial(arity - 1), base = base)
  
  cumSum = 0
  
  for (i in 1:arity) {
    
    N_x_i = length(indexListPerNodePerValue[[nodeIndex]][[i]])
    
    cumSum = cumSum + logFactorial(N_x_i, base = base)
    
  } # end for arity i
  
  return(fixedTerm - cumSum)
  
}



