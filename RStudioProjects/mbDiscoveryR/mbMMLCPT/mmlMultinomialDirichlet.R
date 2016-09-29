######################################################################################
# this is the mml for multinomial distribution with specific prior
# the prior used here is the symmetric dirichlet-multinomial distribution
# this code is based on the fast version of mmlCPT 
######################################################################################


mmlMDWithParents = function(nodeIndex, parentsIndices, indexListPerNodePerValue, 
                                  cachedIndicesList, arities, sampleSize, conPar, base) {
  
  newAddedParentIndex = parentsIndices[length(parentsIndices)] # the last index in the parentsIndices vector is the index for the newly added node
  
  arityChild = arities[nodeIndex]
  
  dirichletParameters = rep(conPar, arityChild) # a vector of dirichlet parameters, when symmetric all parameters are the same
  alpha_0 = sum(dirichletParameters) # alpha_0 for dirichlet-multinomial distribution, alpha_0 = sum(dirichletParameters)

  numParents = length(parentsIndices)
  
  numParentsInstantiations = prod(arities[parentsIndices])
  
  error = 0.5 * (numParentsInstantiations * (arityChild - 1)) * log((pi * exp(1) / 6), base) # error term for achieving the explanatory form 
  
  # store curent cachedIndices in a temp list
  tempList = cachedIndicesList
  
  cumSum = 0 
  
  if (numParents == 1) { # if single parent then just use index i
    
    for (k in 1:arities[newAddedParentIndex]) {# for each value k of the parent node
      
      cachedIndicesList[[k]] = indexListPerNodePerValue[[parentsIndices]][[k]]
      
      N_pa_i = length(cachedIndicesList[[k]]) # number of observations that the signle parent takes on value i
      
      cumSum = cumSum + getCumSumMmlMD(nodeIndex, cachedIndicesList[[k]], arityChild, indexListPerNodePerValue, dirichletParameters, base)
      
      if (N_pa_i > 0) {# if there is more than 1 observation for pa = i then do the following
        # else don't do anything
        
        for (i in 1:N_pa_i) cumSum = cumSum + log(N_pa_i + alpha_0 - i, base = base)
        
      }
      
    } # end for k
    
  } else { # if multiple parents
    
    j = 1
    
    for (existingParentsCombination in 1:length(cachedIndicesList)) {
      
      for (ii in 1:arities[newAddedParentIndex]) {
        
        cachedIndicesList[[j]] = intersect(tempList[[existingParentsCombination]], indexListPerNodePerValue[[newAddedParentIndex]][[ii]])
        
        N_pa_i = length(cachedIndicesList[[j]])
        
        cumSum = cumSum + getCumSumMmlMD(nodeIndex, cachedIndicesList[[j]], arityChild, indexListPerNodePerValue, dirichletParameters, base)
        #cat(cumSum, "\n")
        
        if (N_pa_i > 0) {# if there is more than 1 observation for pa = i then do the following
          # else don't do anything
          
          for (i in 1:N_pa_i) cumSum = cumSum + log(N_pa_i + alpha_0 - i, base = base)
          
        }
        
        
        j = j + 1
        
        #cat(cumSum, "\n")
        
      } # end for ii
      
    } # end for existingParentsCombination
    
  } # end else 
  
  msgLen = cumSum + error
  
  ls = list(msgLen = msgLen, cachedIndicesList = cachedIndicesList)
  
  return(ls)
  
}


# message length for a single node with no parents
# conPar is the concentration parameter for a symmetric dirichlet distribution
# when conPar = 1, this function is the same as the old function that uses uniform prior to compute msg len
mmlMDWithoutParents = function(nodeIndex, indexListPerNodePerValue, arities, sampleSize, conPar, base) {
  
  arity = arities[nodeIndex] # arity of the target node
  dirichletParameters = rep(conPar, arity) # a vector of dirichlet parameters, when symmetric all parameters are the same
  alpha_0 = sum(dirichletParameters) # alpha_0 for dirichlet-multinomial distribution, alpha_0 = sum(dirichletParameters)
  
  error = 0.5 * (arity - 1) * log((pi * exp(1) / 6), base = base) # the error term to achieve the msg len for an explanatory form
  
  cumSum = 0
  
  for (k in 1:arity) {# for each value k of the target node
    
    x_k = length(indexListPerNodePerValue[[nodeIndex]][[k]]) # number of observations for value k
    
    if (x_k > 0) {# if there is more than 1 observation then do the following
      # else don't do anything
      
      for (i in 1:x_k) cumSum = cumSum - log(x_k + dirichletParameters[k] - i, base = base)
      
    } # end if
    
  } # end for value k
  
  for (i in 1:sampleSize) cumSum = cumSum + log(sampleSize + alpha_0 - i)
  
  return(cumSum + error)
  
}


mmlMultinomialDirichlet = function(nodeIndex, parentsIndices, indexListPerNodePerValue, cachedIndicesList, 
                       arities, sampleSize, conPar, base) {
  
  if (length(parentsIndices) < 1) {
    
    res = mmlMDWithoutParents(nodeIndex, indexListPerNodePerValue, arities, sampleSize, conPar, base)
    
  } else {
    
    res = mmlMDWithParents(nodeIndex, parentsIndices, indexListPerNodePerValue, 
                                 cachedIndicesList, arities, sampleSize, conPar, base)
    
  }
  
  return(res)
  
}























