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

################################################## negative log likelihood
# inner product of vectors X and Beta with the same length, where
# # X[1] = 1 and beta[1] = beta0 
innerProd = function(beta, X) {
  
  summation = 0 
  
  for (i in 1:length(beta)) summation = summation + X[i] * beta[i]
  
  return(summation)
  
}


# log likelihood for a single row of data
# use natural log base, since the simplification of -log like cancels log and exp, hence base is natural
logLikeSingle = function(indicatorMatrix, yIndex, xIndices, beta, rowIndex) {
  
  betaDotX = innerProd(beta, c(1, indicatorMatrix[rowIndex, xIndices]))
  
  logLike = -log(1 + exp(betaDotX)) + indicatorMatrix[rowIndex, yIndex] * betaDotX
  
  return(logLike)
  
}


# negative log likelihood for the entire dataset
negLogLike = function(indicatorMatrix, yIndex, xIndices, beta) {
  
  logLike = 0 
  
  # cumulative sum log likelihood for the entire data set
  for (i in 1:nrow(indicatorMatrix)) {
    
    logLike = logLike + logLikeSingle(indicatorMatrix, yIndex, xIndices, beta, i)
    
  }
  
  return(-logLike) 
  
}


# 2nd derivative of the negative log likelihood for computing the fisher information matrix
# defferentiate w.r.t j and k, where j, k = 1, 2, ..., m+1, where m+1 = lenght(beta)
negLoglike2ndDerivativeSingle = function(indicatorMatrix, xIndices, beta, rowIndex, j, k) {
  
  betaDotX = innerProd(beta, c(1, indicatorMatrix[rowIndex, xIndices]))
  
  # the jth coordinate of the vector x_i
  x_ij = c(1, indicatorMatrix[rowIndex, ])[j]
  
  # the kth coordinate of the vector x_i
  x_ik = c(1, indicatorMatrix[rowIndex, ])[k]
  
  nll2ndSingle = (exp(betaDotX) / (1 + exp(betaDotX)) ^ 2) * x_ij * x_ik
  
  return(nll2ndSingle)
  
}

#
#
negLoglike2ndDerivative = function(indicatorMatrix, xIndices, beta, j, k) {
  
  nll2nd = 0
  
  for (i in 1:nrow(indicatorMatrix)) {
    
    nll2nd = nll2nd + negLoglike2ndDerivativeSingle(indicatorMatrix, xIndices, beta, i, j, k)
    
  }
  
  return(nll2nd)
  
}


##################################################  computing entries of fisher information matrix
fisherMatrix = function(indicatorMatrix, yIndex, xIndices, beta) {
  
  FIM = matrix(NA, length(beta), length(beta)) 
  
  #fill in the (1, 1) entry of FIM
  FIM[1, 1] = negLoglike2ndDerivative(indicatorMatrix, xIndices, beta, 1, 1)
  
  # fill in the lower triangular FIM
  for (j in 2:nrow(FIM)) {
    
    for (k in 2:j) {
      
      FIM[j, k] = negLoglike2ndDerivative(indicatorMatrix, xIndices, beta, j, k)
      
    } # end for k
    
  } # end for j
  
  # the 1st column is identical ti the diagnose of FIM
  FIM[, 1] = diag(FIM)
  
  # the upper triangular is identical to the lower triangular FIM
  FIM[upper.tri(FIM)] = t(FIM)[upper.tri(t(FIM))]
  
  return(FIM) # return the complete FIM
  
}

# calculate log of the determinant of a matrix
# matrix has to be symmetric positive definite
# use cholesky decomposition to decompose matrix FIM = L*transpose(L)
# since log like used natural base, we use natural base for log det(fisher) too
logDeterminant = function(matrix) {
  
  choleskeyUpper = chol(matrix)
  
  logDet = 0 
  
  for (i in 1:nrow(matrix)) {
    
    logDet = logDet + log(diag(choleskeyUpper)[i])
    
  }
  
  return(logDet)
  
}

######################################  msg len with no predictor #####################################
# check for this 
msgLenWithNoPredictors = function(data, indicatorMatrix, yIndex, cardinalities, allNodes, sigma) {
  
  # formula for empty model
  formula = paste(allNodes[yIndex], "~ 1")
  
  # estimate parameter of logit model using glm
  beta = glm(formula, family = binomial(link = "logit"), data = data)$coefficients
  
  # value for the negative log likelihood 
  nll = negLogLike(indicatorMatrix, yIndex, NULL, beta)
  
  # fisher information matrix 
  fisherInfoMatrix = negLoglike2ndDerivative(indicatorMatrix, NULL, beta, 1, 1)
  
  # log of the determinant of the FIM
  logFisher = log(fisherInfoMatrix)
  
  # computing mml 
  mml = 0.5 * log(2 * pi) + log(sigma) - 0.5 * log(cardinalities[yIndex]) + 
    0.5 * beta ^ 2 / sigma ^ 2 + 0.5 * logFisher + nll + 0.5 * (1 + log(0.083333))
  
  # store results in a list 
  lst = list(beta, nll, logFisher, mml)
  
  names(lst) = c("par", "nll", "logFisher", "mml")
  
  return(lst)
  
}


################################################## msg len ############################################
msgLenWithPredictors = function(data, indicatorMatrix, yIndex, xIndices, cardinalities, 
                                allNodes, sigma) {
  
  # arity of dependent variable y
  arityOfY = cardinalities[yIndex]
  
  # this is for binary case
  nFreePar = length(xIndices) + 1
  
  # lattice constant
  k = c(0.083333, 0.080188, 0.077875, 0.07609, 0.07465, 0.07347, 0.07248, 0.07163)
  
  if (nFreePar <= length(k)) {
    
    latticeConst = k[nFreePar]
  } else {
    
    latticeConst = min(k)
    
  }
  
  # create formula for fitting logit model using glm
  formula = paste(allNodes[yIndex], "~", paste0(allNodes[xIndices], collapse = "+"))
  
  # parameter estimation of negative log likelihood using GLM
  # glm always use the first level (in this case "A") for reference when estimating coefficients
  # the reference can be changed by change the order of levels in data frame using relevel()
  fittedLogit = glm(formula, family = binomial(link = "logit"), data = data)
  
  # value for the negative log likelihood 
  nll = negLogLike(indicatorMatrix, yIndex, xIndices, fittedLogit$coefficients)
  
  # fisher information matrix 
  fisherInfoMatrix = fisherMatrix(indicatorMatrix, yIndex, xIndices, fittedLogit$coefficients)
  
  # log of the determinant of the FIM
  logFisher = logDeterminant(fisherInfoMatrix)
  
  # computing mml 
  mmlFixedPart =  0.5 * nFreePar * log(2 * pi) + nFreePar * log(sigma) - 0.5 * log(arityOfY) - 
    0.5 * sum((cardinalities[xIndices] - 1) * log(arityOfY) + 
                (arityOfY - 1) * log(cardinalities[xIndices])) + 0.5 * nFreePar*(1 + log(latticeConst)) 
  
  # sum of logit parameters square
  sumParSquare = 0 
  for (i in 1:length(nFreePar)) sumParSquare = sumParSquare + fittedLogit$coefficients[i] ^ 2
  
  mmlNonFixedPart = 0.5 * sumParSquare / sigma ^ 2 + 0.5 * logFisher + nll
  
  mml = mmlFixedPart + mmlNonFixedPart
  
  # store results in a list 
  lst = list(fittedLogit$coefficients, nll, logFisher, mml)
  
  names(lst) = c("par", "nll", "logFisher", "mml")
  
  return(lst)
  
}


mmlLogit = function(data, indicatorMatrix, yIndex, xIndices, cardinalities, allNodes, sigma) {
  
  if (length(xIndices) < 1) {
    
    msgLen = msgLenWithNoPredictors(data, indicatorMatrix, yIndex, cardinalities, allNodes, sigma)$mml[[1]]
    
  } else {
    
    msgLen = msgLenWithPredictors(data, indicatorMatrix, yIndex, xIndices, cardinalities, allNodes, sigma)$mml[[1]]
    
  }
  
  return(msgLen)
  
}


