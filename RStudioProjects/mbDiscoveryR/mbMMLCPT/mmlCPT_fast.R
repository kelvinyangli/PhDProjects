######################################################################################
# this is an updated version of mmlCPT in terms of computational speed
# this version takes advantage of caching common parents indices for the use of the 
# next step, and hence it is slightly faster than the non-cached version
######################################################################################

logFactorial = function(n, base) {
  
  # if n = 0 then log of 0 factorial is 0
  ss = 0 
  
  if (n > 0) {
    
    for (i in 1:n) ss = ss + log(i, base = base)
    
  }
  
  return(ss)
  
}


msgLenWithParents.fast = function(nodeIndex, parentsIndices, indexListPerNodePerValue, 
                                 cachedIndicesList, arities, sampleSize, base) {
  
  newAddedParentIndex = parentsIndices[length(parentsIndices)]
  
  arityChild = arities[nodeIndex]
  
  numParents = length(parentsIndices)
  
  numParentsInstantiations = prod(arities[parentsIndices])
  
  fixedTerm = 0.5 * (numParentsInstantiations * (arityChild - 1)) * log((pi * exp(1) / 6), base)
  
  nonFixedTerm = 0
  
  # log((|x| - 1)!)
  logConstant = log(factorial(arityChild - 1), base)
  
  # store curent cachedIndices in a temp list
  tempList = cachedIndicesList
  
  if (numParents == 1) { # if single parent then just use index i
    
    for (i in 1:arities[newAddedParentIndex]) {
      
      cachedIndicesList[[i]] = indexListPerNodePerValue[[parentsIndices]][[i]]
      
      N_pa_i = length(cachedIndicesList[[i]])
      
      # sum_i^arityChild log(N(pa_i, x_i))!
      cumSum = singleParentComputation(nodeIndex, cachedIndicesList[[i]], arityChild, indexListPerNodePerValue, base)
      
      # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
      logNumerator = logFactorial(N_pa_i + arityChild - 1, base)
      
      nonFixedTerm = nonFixedTerm + logNumerator - logConstant - cumSum
      
    } # end for i
    
  } else { # if multiple parents
    
    j = 1
  
    for (i in 1:length(cachedIndicesList)) {
      
      for (ii in 1:arities[newAddedParentIndex]) {
      
      cachedIndicesList[[j]] = intersect(tempList[[i]], indexListPerNodePerValue[[newAddedParentIndex]][[ii]])
      
      N_pa_i = length(cachedIndicesList[[j]])
      
      cumSum = multiParentsComputation(nodeIndex, arityChild, indexListPerNodePerValue, cachedIndicesList[[j]], base)
       
      # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
      logNumerator = logFactorial(N_pa_i + arityChild - 1, base)
      
      nonFixedTerm = nonFixedTerm + logNumerator - logConstant - cumSum
      
      j = j + 1
      
      #cat(nonFixedTerm, "\n")
      
      } # end for ii
        
    } # end for i
    
  } # end else 
  
  msgLen = fixedTerm + nonFixedTerm
  
  ls = list(msgLen = msgLen, cachedIndicesList = cachedIndicesList)
  
  return(ls)
  
}


# message length for a single node with no parents
msgLenWithoutParents = function(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base) {
  
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


mmlCPT.fast = function(nodeIndex, parentsIndices, indexListPerNodePerValue, cachedIndicesList, 
                  arities, sampleSize, base) {
  
  if (length(parentsIndices) < 1) {
    
    res = msgLenWithoutParents(nodeIndex, indexListPerNodePerValue, arities, sampleSize, base)
    
  } else {
    
    res = msgLenWithParents.fast(nodeIndex, parentsIndices, indexListPerNodePerValue, 
                                   cachedIndicesList, arities, sampleSize, base)
    
  }
  
  return(res)
  
}
