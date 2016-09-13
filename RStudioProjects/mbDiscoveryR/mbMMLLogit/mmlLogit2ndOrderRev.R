# revised version of makeFormula function
# add one interaction term at a time
makeInteractTerms = function(allNodes, xIndices) {
  
  pairs = vector(length = sum(1:(length(xIndices) - 1)))
  
  k = 1
  
  for (i in 1:(length(xIndices) - 1)) {
    
    for (j in (i + 1):length(xIndices)) {
      
      pairs[k] = paste0(allNodes[xIndices[i]], "*", allNodes[xIndices[j]])  
      
      k = k + 1
      
    } # end for j
    
  } # end for i
  
  return(pairs)
  
}

makeFormula.rev = function(allNodes, yIndex, xIndices) {
  
  if (length(xIndices) > 1) { # if there are more than 1 predictors
    
    pairs = vector(length = sum(1:(length(xIndices) - 1)))
    
    k = 1
    
    for (i in 1:(length(xIndices) - 1)) {
      
      for (j in (i + 1):length(xIndices)) {
        
        pairs[k] = paste0(allNodes[xIndices[i]], "*", allNodes[xIndices[j]])  
        
        k = k + 1
        
      } # end for j
      
    } # end for i
    
    formula = paste(allNodes[yIndex], "~", paste0(c(allNodes[xIndices], pairs), collapse = "+"))
    
  } else { # if there is only one predictor
    
    formula = paste(allNodes[yIndex], "~", allNodes[xIndices])
    
  }
  
  
  return(formula)
  
}

msgLenWithPredictors2ndOrder.rev = function(data, indicatorMatrix, yIndex, xIndices, arities, 
                                        allNodes, interactData, completeIndicatorMatrix, sigma) {
  
  # arity of dependent variable y
  arityOfY = arities[yIndex]
  
  # lattice constant
  k = c(0.083333, 0.080188, 0.077875, 0.07609, 0.07465, 0.07347, 0.07248, 0.07163)
  
  # create all interaction terms first
  interactTerms = makeInteractTerms(allNodes, xIndices)
  
  # create formula for fitting logit model using glm
  formula = makeFormula(allNodes, yIndex, xIndices)
  
  # parameter estimation of negative log likelihood using GLM
  # glm always use the first level (in this case "A") for reference when estimating coefficients
  # the reference can be changed by change the order of levels in data frame using relevel()
  beta = glm(formula, family = binomial(link = "logit"), data = data)$coefficients
  
  nFreePar = length(beta)
  
  if (nFreePar <= length(k)) {
    
    latticeConst = k[nFreePar]
  } else {
    
    latticeConst = min(k)
    
  }
  
  
  # compute the indices for all 1st and 2nd order terms
  # predictors = c(1st order indices, 2nd order indices) in completeIndicatorMatrix
  if (length(xIndices) > 1) { # only have interaction if there are more than 1 parent
    
    # interaction of predictors
    interactXs = interactPredictors(interactData, allNodes, xIndices) 
    
    # joint of 1st and 2nd order predictors
    predictors = c(xIndices, ncol(indicatorMatrix) + interactXs)
    
  } else { # if there is at most one parent, then the predictors contain only 1st order predictors
    
    predictors = xIndices
    
  }
  
  # pre-compute betaDotX for each row
  betaDotX = rep(0, nrow(indicatorMatrix))
  
  # pre-compute exp(beta*X)/(1 + exp(beta*X))^2 for reach row
  expConstants = rep(0, nrow(indicatorMatrix))
  
  for (i in 1:nrow(indicatorMatrix)) {
    
    betaDotX[i] = beta[1] + beta[-1] %*% completeIndicatorMatrix[i, predictors]
    
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

