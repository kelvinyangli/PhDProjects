# compute mml using glm
# glm selects the 1st value of a variable as the reference when fitting a model, hence when dealing with binar data
# an indicator matrix that contains 0 and 1 is required, where 0 -- A and 1 -- B, since "A" appears to be the 1st value
# when our dataset contains values such as "A", "B", "C", ...  
# should obtain same answer as using optim, because glm also use mle 

################################################## pre process data ############################################\
# transform binary data into 0 and 1
getIndicator = function(data) {
  
  indicatorMatrix = matrix(nrow = nrow(data), ncol = ncol(data), dimnames = list(NULL, colnames(data)))
  
  for (i in 1:ncol(data)) {
    
    # as.numeric convert categrical data values to 1, 2, 3, ...
    # -1 transfers them to 0, 1, 2, ...
    indicatorMatrix[, i] = as.numeric(data[, i]) - 1 
    
  }
  
  # add an extra column of 1s in front for the intercept of a logit model
  #indicatorMatrix = cbind(1, indicatorMatrix)
  
  return(indicatorMatrix)
  
}

# compute all pairs of interaction data
getInteractData = function(data) {
  
  interact = matrix(0, nrow = nrow(data), ncol = choose(ncol(data), 2))
  
  interact = as.data.frame(interact)
  
  k = 1
  
  for (i in 1:(ncol(data) - 1)) {
    
    for (j in (i + 1):ncol(data)) {
      
      interact[, k] = data[, i] * data[, j]
      
      colnames(interact)[k] = paste0(colnames(data)[i], colnames(data)[j], collapse = "")
      
      k = k + 1
      
    } # end for j
    
  } # end for i
  
  return(interact)
  
}

# create interact predictors according to 1st order predictors and return their column indices in interactData
interactPredictors = function(interactData, allNodes, xIndices) {
  
  interactXs = rep(0, choose(length(xIndices), 2))
  
  k = 1 
  
  for (i in 1:(length(xIndices) - 1)) {
    
    for (j in (i + 1):length(xIndices)) {
      
      # since when compute the interactData, interactions are considered in the order of the column labels
      # however, when adding node into predictor sets, nodes may be added in a different order
      # hence when considering interaction terms, pair of nodes are ordered so that it align with the column names of interactData
      interactXs[k] = which(colnames(interactData) == paste0(allNodes[xIndices[c(i, j)][order(xIndices[c(i, j)])]], collapse = ""))
      
      k = k + 1
      
    } # end for j
    
  } # end for i
  
  return(interactXs)
  
}

# negative log likelihood of the 2nd order logit model for the entire dataset
negLogLike2ndOrder = function(indicatorMatrix, yIndex, xIndices, betaDotX) {
  
  if (is.null(xIndices)) { # if there is no parent
    
    logLike = -log(1 + exp(betaDotX)) * nrow(indicatorMatrix) + sum(indicatorMatrix[, yIndex]) * betaDotX
    
  } else { # if there is at least one parent
    
    logLike = sum(-log(1 + exp(betaDotX))) + indicatorMatrix[, yIndex] %*% betaDotX
    
  }
  
  return(-logLike) 
  
}

# computing entries of fisher information matrix when the target has at least 1 parent
# where there is no parent, FIM is a 1x1 matrix, which is just a sigle value 
# and hence can be computed easily without using this function
fisherMatrix2ndOrder = function(indicatorMatrix, yIndex, predictors, expConstants, completeIndicatorMatrix) {
  
  # FIM is a square matrix, with dimensions = |beta| = |predictors| + 1
  # where predictors = c(1st order terms, 2nd order terms)
  FIM = matrix(NA, length(predictors) + 1, length(predictors) + 1) 
  
  #fill in the (1, 1) entry of FIM
  FIM[1, 1] = sum(expConstants)
  
  # fill in the lower triangular FIM from the 2nd row
  for (j in 2:nrow(FIM)) {
    
    # fill in the lower traingular FIM from the 2nd column 
    # since the 1st column is idential to the diagnose
    for (k in 2:j) {
      
      # x[, j] = indicatorMatrix[, xIndices[j - 1]] = indicatorMatrix[, xIndices][, j - 1]
      # take j-1 instead of j is because the jth index in FIM corresponds to the (j-1)th index in xIndice
      # since an integer 1 is added in front of the Xs for the intercept
      # conduct vector multiplication x_ij * x_ik first, then inner product with expConstants
      # notice that x_i here is the joint of the original data and interaction data
      FIM[j, k] = expConstants %*% (completeIndicatorMatrix[, predictors[j - 1]] * completeIndicatorMatrix[, predictors[k - 1]])
      
    } # end for k
    
  } # end for j
  
  # the 1st column is identical to the diagnose of FIM
  FIM[, 1] = diag(FIM)
  
  # the upper triangular is identical to the lower triangular FIM
  FIM[upper.tri(FIM)] = t(FIM)[upper.tri(t(FIM))]
  
  return(FIM) # return the complete FIM
  
}

# calculate log of the determinant of a matrix
# matrix has to be symmetric positive definite
# use cholesky decomposition to decompose matrix FIM = L*transpose(L)
logDeterminant = function(matrix) {
  
  choleskeyUpper = chol(matrix)
  
  logDet = 0 
  
  for (i in 1:nrow(matrix)) {
    
    logDet = logDet + log(diag(choleskeyUpper)[i])
    
  }
  
  return(logDet)
  
}

msgLenWithNoPredictors2ndOrder = function(data, indicatorMatrix, yIndex, arities, allNodes, sigma) {
  
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

makeFormula = function(allNodes, yIndex, xIndices) {
  
  if (length(xIndices) > 1) { # if there are more than 1 predictors
    
    # re-order xIndices to make it consistent with the colnames(interactData)
    #tempIndices = xIndices[order(xIndices)]
    
    pairs = vector(length = sum(1:(length(xIndices) - 1)))
    
    k = 1
    
    for (i in 1:(length(xIndices) - 1)) {
      
      for (j in (i + 1):length(xIndices)) {
        
        pairs[k] = paste0(allNodes[xIndices[i]], "*", allNodes[xIndices[j]])  
        
        k = k + 1
        
      } # end for j
      
    } # end for i
    
    formula = paste(allNodes[yIndex], "~", paste0(c(allNodes[xIndices], pairs), collapse = "+"))
    
  } else {
    
    formula = paste(allNodes[yIndex], "~", allNodes[xIndices])
    
  }
  
  
  return(formula)
  
}

################################################## msg len ############################################
msgLenWithPredictors2ndOrder = function(data, indicatorMatrix, yIndex, xIndices, arities, 
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


mmlLogit2ndOrder = function(data, indicatorMatrix, yIndex, xIndices, arities, allNodes, interactData, completeIndicatorMatrix, sigma) {
  
  if (length(xIndices) < 1) {
    
    msgLen = msgLenWithNoPredictors2ndOrder(data, indicatorMatrix, yIndex, arities, allNodes, sigma)$mml[[1]]
    
  } else {
    
    msgLen = msgLenWithPredictors2ndOrder(data, indicatorMatrix, yIndex, xIndices, arities, allNodes, interactData, 
                                          completeIndicatorMatrix, sigma)$mml[[1]]
    
  }
  
  return(msgLen)
  
}


