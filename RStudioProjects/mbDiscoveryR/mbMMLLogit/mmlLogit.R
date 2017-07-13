####################################################################################################  
# mmlLogit is a function to compute the msg len of a 1st order logit model, where the target is 
# given as yIndex, and predictor variables Xs are given as xIndices
# mmlLogit uses the two functions msgLenWithNoPredictors and msgLenWithPredictors, depends on whether
# the vector xIndices is empty or not
# msgLenWithNoPredictors is a function to compute the msg len for a target with no predictors
# msgLenWithPredictors is a function to compute the msg len for a target with at least one predictors
# both of these two functions follow the mml logit model formula proposed by Neil, Wallace and Korb 1999.
# the estimation of regression coefficients beta is obtained using glm in R
# glm selects the 1st value of a variable as the reference when fitting a model, for example: 
# if a variable contains values such as "A", "B", "C", ..., then "A" is used as the reference group 
# by glm, since it appears to be the 1st value by alphabet 
# the use of glm should obtain the same answer as optim, because both optim and glm use mle 
# the log used for mmlLogit is natural log, and hence the unit of returned msg len is in nit
####################################################################################################

# msg len with no predictors 
msgLenWithNoPredictors = function(data, indicatorMatrix, yIndex, arities, allNodes, sigma) {
  
  # formula for empty model
  formula = paste(allNodes[yIndex], "~ 1")
  
  # estimate parameter of logit model using glm
  beta = glm(formula, family = binomial(link = "logit"), data = data)$coefficients
  
  # when there is no parent, betaDotX = beta_0 = beta
  betaDotX = beta
  
  # value for the negative log likelihood 
  nll = negLogLike(indicatorMatrix, yIndex, NULL, betaDotX)
  
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

# msg len with at least one predictors
msgLenWithPredictors = function(data, indicatorMatrix, yIndex, xIndices, arities, 
                                allNodes, sigma) {
  
  # arity of dependent variable y
  arityOfY = arities[yIndex]
  
  # create formula for fitting logit model using glm
  formula = paste(allNodes[yIndex], "~", paste0(allNodes[xIndices], collapse = "+"))
  
  # parameter estimation of negative log likelihood using GLM
  # glm always use the first level (in this case "A") for reference when estimating coefficients
  # the reference can be changed by change the order of levels in data frame using relevel()
  beta = glm(formula, family = binomial(link = "logit"), data = data)$coefficients
  
  # the number of free parameters = |beta|
  nFreePar = length(beta)
  
  # lattice constant
  k = c(0.083333, 0.080188, 0.077875, 0.07609, 0.07465, 0.07347, 0.07248, 0.07163)
  
  if (nFreePar <= length(k)) {
    
    latticeConst = k[nFreePar]
    
  } else {
    
    latticeConst = min(k)
    
  }
  
  # pre-compute betaDotX for each row
  betaDotX = rep(0, nrow(indicatorMatrix))
  
  # pre-compute exp(beta*X)/(1 + exp(beta*X))^2 for reach row
  expConstants = rep(0, nrow(indicatorMatrix))
  
  for (i in 1:nrow(indicatorMatrix)) {
    
    betaDotX[i] = beta[1] + beta[-1] %*% indicatorMatrix[i, xIndices]
    
    expConstants[i] = (exp(betaDotX[i]) / (1 + exp(betaDotX[i])) ^ 2)
      
  } # end for i
  
  # value for the negative log likelihood 
  nll = negLogLike(indicatorMatrix, yIndex, xIndices, betaDotX)
  
  # fisher information matrix 
  fisherInfoMatrix = fisherMatrix(indicatorMatrix, yIndex, xIndices, expConstants)
  
  # log of the determinant of the FIM
  logFisher = logDeterminant(fisherInfoMatrix)
  
  # computing mml 
  # mmlFixedPart =  0.5 * nFreePar * log(2 * pi) + nFreePar * log(sigma) - 0.5 * log(arityOfY) - 
  #   0.5 * sum((arities[xIndices] - 1) * log(arityOfY) + 
  #               (arityOfY - 1) * log(arities[xIndices])) + 0.5 * nFreePar*(1 + log(latticeConst)) 
  
  # sum of logit parameters square
  # sumParSquare = 0 
  # for (i in 1:nFreePar) sumParSquare = sumParSquare + beta[i] ^ 2
  sumParSquare = sum(beta ^ 2)
  
  nlogPrior = 0.5 * nFreePar * log(2 * pi) + nFreePar * log(sigma) - 0.5 * log(arityOfY) - 
    0.5 * sum((arities[xIndices] - 1) * log(arityOfY) + (arityOfY - 1) * log(arities[xIndices])) + 
    0.5 * sumParSquare / sigma ^ 2
  
  logLattice = 0.5 * nFreePar*(1 + log(latticeConst))
  
  mml = nlogPrior + logLattice + 0.5 * logFisher + nll
  
  # store results in a list 
  lst = list(beta, nlogPrior, logLattice, nll, logFisher, mml)
  
  names(lst) = c("par", "nlogPrior", "logLattice", "nll", "logFisher", "mml")
  
  return(lst)
  
}

# merge the above two functions into one function
mmlLogit = function(data, indicatorMatrix, yIndex, xIndices, arities, allNodes, sigma) {
  
  if (is.null(xIndices)) {
    
    msgLen = msgLenWithNoPredictors(data, indicatorMatrix, yIndex, arities, allNodes, sigma)$mml[[1]]
    
  } else {
    
    msgLen = msgLenWithPredictors(data, indicatorMatrix, yIndex, xIndices, arities, allNodes, sigma)$mml[[1]]
    
  }
  
  return(msgLen)
  
}


