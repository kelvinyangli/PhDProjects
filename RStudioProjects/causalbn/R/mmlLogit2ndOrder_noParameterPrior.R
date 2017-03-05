msgLenWithNoPredictors2ndOrder.noprior = function(data, indicatorMatrix, yIndex, arities, allNodes, sigma) {
  
  # formula for empty model
  formula = paste(allNodes[yIndex], "~ 1")
  
  # estimate parameter of logit model using glm
  beta = glm(formula, family = binomial(link = "logit"), data = data)$coefficients

  # if there is no parent, then beta*X = beta0 = beta
  betaDotX = beta
  
  # value for the negative log likelihood 
  nll = negLogLike2ndOrder(indicatorMatrix, yIndex, NULL, betaDotX)
  
  # pre-compute expConstants
  expConstants = exp(betaDotX) / (1 + exp(betaDotX)) ^ 2
  
  # when there is no parents, expConstants is a single number
  # FIM is a 1x1 matrix = sum(expConstants) = sampleSize * expConstants
  # log of the determinant of the FIM
  logFisher = log(expConstants * nrow(indicatorMatrix))
  
  # computing mml 
  mml = 0.5 * log(2 * pi) + log(sigma) - 0.5 * log(arities[yIndex]) + 
    0.5 * beta ^ 2 / sigma ^ 2 + 0.5 * logFisher + nll + 0.5 * (1 + log(0.083333))
  
  # store results in a list 
  lst = list(beta, nll, logFisher, mml)
  
  names(lst) = c("par", "nll", "logFisher", "mml")
  
  return(lst)
  
}


msgLenWithPredictors2ndOrder.noprior = function(data, indicatorMatrix, yIndex, xIndices, arities, 
                                        allNodes, interactData, completeIndicatorMatrix, sigma) {
  
  # arity of dependent variable y
  arityOfY = arities[yIndex]
  
  # lattice constant
  k = c(0.083333, 0.080188, 0.077875, 0.07609, 0.07465, 0.07347, 0.07248, 0.07163)
  
  # create formula for fitting logit model using glm
  formula = makeFormula(allNodes, yIndex, xIndices)
  
  # parameter estimation of negative log likelihood using GLM
  # glm always use the first level (in this case "A") for reference when estimating coefficients
  # the reference can be changed by change the order of levels in data frame using relevel()
  beta = glm(formula, family = binomial(link = "logit"), data = data)$coefficients

  # compute the indices for all 1st and 2nd order terms
  # predictors = c(1st order indices, 2nd order indices) in completeIndicatorMatrix
  if (length(xIndices) > 1) { # only have interaction if there are more than 1 parent
    
    # interaction of predictors
    interactionTermsIndices = getInteractionIndices(interactData, allNodes, xIndices) 
    
    # joint of 1st and 2nd order predictors
    predictors = c(xIndices, ncol(indicatorMatrix) + interactionTermsIndices)
    
  } else {# if there is at most one parent, then the predictors contain only 1st order predictors
    
    predictors = xIndices
    
  }
  
  # notice that the fitted model may return NA as coefficinet of a variable due to 
  # this variable has linear relation with the other variables in the model
  # this happens on the interaction terms most of the time (so far didn't encounter
  # such a problem on 1st order terms), hence the interaction term should be dropped
  # without consideration. 
  
  # if there is NA in the fitted coefficients then 
  # remove na from beta, also remove corresponding indices from predictors
  if (any(is.na(beta))) {
    
    naIndices = which(is.na(beta))
    beta = beta[-naIndices] # remove na from fitted coefficients
    predictors = predictors[-(naIndices - 1)] # remove corresponding indices
    
  }
  
  nFreePar = length(beta)
  
  if (nFreePar <= length(k)) {
    
    latticeConst = k[nFreePar]
  } else {
    
    latticeConst = min(k)
    
  }
  
  betaDotX = rep(0, nrow(indicatorMatrix))
  expConstants = rep(0, nrow(indicatorMatrix))
  
  # pre-compute betaDotX for each row
  # pre-compute exp(beta*X)/(1 + exp(beta*X))^2 for reach row
  for (i in 1:nrow(indicatorMatrix)) {
    
    betaDotX[i] = beta[1] + beta[-1] %*% as.numeric(completeIndicatorMatrix[i, predictors])
    
    expConstants[i] = (exp(betaDotX[i]) / (1 + exp(betaDotX[i])) ^ 2)
    
  } # end for i
  
  # value for the negative log likelihood 
  nll = negLogLike2ndOrder(indicatorMatrix, yIndex, xIndices, betaDotX)
  
  # fisher information matrix 
  fisherInfoMatrix = fisherMatrix2ndOrder(indicatorMatrix, yIndex, predictors, expConstants, completeIndicatorMatrix)
  
  # log of the determinant of the FIM
  logFisher = logDeterminant(fisherInfoMatrix)
  
  # computing mml 
  mmlFixedPart =  0.5 * nFreePar * log(2 * pi) + nFreePar * log(sigma) - 0.5 * log(arityOfY) - 
    0.5 * sum((arities[xIndices] - 1) * log(arityOfY) + 
                (arityOfY - 1) * log(arities[xIndices])) + 0.5 * nFreePar*(1 + log(latticeConst)) 
  
  # sum of logit parameters square
  sumParSquare = 0 
  for (i in 1:length(nFreePar)) sumParSquare = sumParSquare + beta[i] ^ 2
  
  mmlNonFixedPart = 0.5 * sumParSquare / sigma ^ 2 + 0.5 * logFisher + nll
  
  mml = mmlFixedPart + mmlNonFixedPart
  
  # store results in a list 
  lst = list(beta, nll, logFisher, mml)
  
  names(lst) = c("par", "nll", "logFisher", "mml")
  
  return(lst)
  
}

mmlLogit2ndOrder.noprior = function(data, indicatorMatrix, yIndex, xIndices, arities, allNodes, interactData, completeIndicatorMatrix, sigma) {
  
  if (length(xIndices) < 1) {
    
    msgLen = msgLenWithNoPredictors2ndOrder.noprior(data, indicatorMatrix, yIndex, arities, allNodes, sigma)$mml[[1]]
    
  } else {
    
    msgLen = msgLenWithPredictors2ndOrder.noprior(data, indicatorMatrix, yIndex, xIndices, arities, allNodes, interactData, 
                                          completeIndicatorMatrix, sigma)$mml[[1]]
    
  }
  
  return(msgLen)
  
}

# MB discovery using 
# indicatorMatrix = getIndicator(data)
mbForwardSelectionForMML2ndOrderLogit.noprior = function(data, node, arities, indexListPerNodePerValue,
                                                 base = exp(1), indicatorMatrix = NULL, interactData = NULL, completeIndicatorMatrix = NULL, debug = FALSE) {
  
  ##############################################################
  # get the basic information and 
  # create empty vectors for storing mb
  allNodes = names(data)
  nodeIndex = which(allNodes == node) # get index of the target node
  numNodes = ncol(data)
  #sampleSize = nrow(data)
  mb = c()
  unCheckedIndices = (1:numNodes)[-nodeIndex]
  #tempCachedIndicesList = list()
  score = mmlLogit2ndOrder.noprior
  ##############################################################
  # msg len for a single node with no parents
  # parentsIndices is given as an empty vector
  minMsgLen = score(data, indicatorMatrix, nodeIndex, c(), arities, allNodes, interactData, completeIndicatorMatrix, sigma = 3)
  
  if (debug) {
    
    cat("Search: Greedy search --- Score: mmlLogit \n")
    cat("0 parent:", minMsgLen, "\n")
    
  }
  
  repeat {
    
    # repeat the process of computing mml for remaining unCheckedIndices
    # if unCheckedIndices is empty or all msg len > min msg len then stop
    index = 0 # initialize index to 0
    #cachedIndicesList = tempCachedIndicesList
    
    if (length(unCheckedIndices) == 0) {
      
      if (debug) cat("BM is full! \n")
      break
      
    }
    
    # compute msg len for the target given each unchecked node as its parents
    for (i in 1:length(unCheckedIndices)) {
      
      parentsIndices = c(mb, unCheckedIndices[i])
      
      msgLenCurrent = score(data, indicatorMatrix, nodeIndex, parentsIndices, arities, allNodes, interactData, 
                            completeIndicatorMatrix, sigma = 3)
      
      if (debug) cat("parents =", allNodes[c(mb, unCheckedIndices[i])], ":", msgLenCurrent, "\n")
      
      # if the current msg len is smaller then replace minMsgLen by the current 
      # and record the current index
      # else go to the next available node
      if (msgLenCurrent < minMsgLen) { 
        
        minMsgLen = msgLenCurrent
        index = i
        #tempCachedIndicesList = res$cachedIndicesList
        
      } # end if 
      
    } # end for i 
    
    if (index == 0) {
      
      if (debug) cat("Stop! No better choice for MB! \n")
      
      break 
      
    } else {
      
      if (debug) cat("add", allNodes[unCheckedIndices[index]], "into mb \n")
      
      # add the node index with the minimum msg len into mb and remove it from unCheckedIndices
      mb = c(mb, unCheckedIndices[index])
      
      if (debug) cat("current mb is {", allNodes[mb], "} with msg len", minMsgLen, "\n")
      if (debug) cat("------------------------------- \n")
      
      unCheckedIndices = unCheckedIndices[-index]
      
    } # end else 
    
  } # end repeat
  
  return(allNodes[mb])
  
}




