# code is correct
# sanity check has done
# this is a user friendly, but slower version

logFactorial2 = function(n, base) {

  # if n = 0 then log of 0 factorial is 0
  ss = 0 
  
  if (n > 0) {
    
    for (i in 1:n) ss = ss + log(i, base = base)
    
  }
  
  return(ss)
  
}


getParentStatesIndecies2 = function(numEachParentStates, numParentStates, index) {
  
  if (index <= numParentStates) {
    
    numIndecies = length(numEachParentStates)
    
    indecies = rep(0, numIndecies)
    
    indecies[1] = ceiling(index / prod(numEachParentStates[2:numIndecies]))
    
    for (i in 1:(numIndecies - 1)) {
      
      indicator = ceiling(index / prod(numEachParentStates[(i + 1):numIndecies]))
      
      indecies[i] = indicator %% numEachParentStates[i]
      
      if (indecies[i] == 0) indecies[i] = numEachParentStates[i]
      
    }
    
    
    lastIndex = index %% numEachParentStates[numIndecies] # take modular 
    
    if (lastIndex == 0) {
      
      indecies[numIndecies] = numEachParentStates[numIndecies]
      
    } else {
      
      indecies[numIndecies] = lastIndex
      
    }
    
    return(indecies)
    
  } else {
    
    return(0)
    
  }
  
}

mmlCPT2 = function(childNode, parentNodes, data, base = 2) {
  
  childStates = levels(data[, childNode])
  
  numChildStates = length(childStates)
  
  parentStates = list()
  
  numEachParentStates = rep(2, length(parentNodes))
  
  # states of each parent nodes
  for (k in 1:length(parentNodes)) {
    
    parentStates[[k]] = levels(data[, parentNodes[k]])
    
    numEachParentStates[k] = length(parentStates[[k]])
    
  }
  
  numParentStates = prod(numEachParentStates)
  
  fixedTerm = 0.5 * (numParentStates * (numChildStates - 1)) * log((pi * exp(1) / 6), base = base)
  
  nonFixedTerm = 0
  
  # log((|x| - 1)!)
  logConstant = log(factorial(numChildStates - 1), base = base)
  
  ######
  for (i in 1:numParentStates) { # for each parent state i
    
    matchIndecies = rep(0, nrow(data))
    
    if (length(parentNodes) < 2) {
      
      indecies = i
      
    } else {
      
      indecies = getParentStatesIndecies2(numEachParentStates, numParentStates, i)
      
    }
    
    for (ii in 1:nrow(data)) {
      
      equal = 1
      
      for (jj in 1:length(parentNodes)) {
        
        equal = equal * (data[ii, parentNodes[jj]] == parentStates[[jj]][indecies[jj]])
        
      } # end for jj
      
      matchIndecies[ii] = equal
      
    } # end for ii
    
    
    # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
    logNumerator = logFactorial2(sum(matchIndecies) + numChildStates - 1, base = base)
    
    cumSum = 0 
    
    for (j in 1:numChildStates) { # for each child state j
      
      # N(pa_i, x_j)
      numChildParentStates = sum(data[which(matchIndecies == 1), childNode] == childStates[j])
      
      # cumulative sum of log(N(pa_i, x_j)!)
      cumSum = cumSum + logFactorial2(numChildParentStates, base = base)
      
    } # end for j
    
    nonFixedTerm = nonFixedTerm + logNumerator - logConstant - cumSum
    
  } # end for i
  
  return(fixedTerm + nonFixedTerm)
  
}


# message length for a single node with no parents
mmlSingleNode2 = function(node, data, base = 2) {
  
  states = levels(data[, node])
  
  numStates = length(states)
  
  fixedTerm = 0.5 * (numStates - 1) * log((pi * exp(1) / 6), base = base) + 
    logFactorial2(nrow(data) + numStates - 1, base = base) - log(factorial(numStates - 1), base = base)
  
  cumSum = 0
  
  for (i in 1:numStates) {
    
    N_x_i = 0
    
    for (j in 1:nrow(data)) N_x_i = N_x_i + (data[j, node] == states[i])
    
    cumSum = cumSum + logFactorial2(N_x_i, base = base)
    
  } # end for i
  
  return(fixedTerm - cumSum)
  
}




