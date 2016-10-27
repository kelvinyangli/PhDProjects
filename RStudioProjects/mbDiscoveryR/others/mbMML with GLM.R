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
logLikeSingle = function(indicatorMatrix, yIndex, xIndices, beta, rowIndex, base) {
  
  betaDotX = innerProd(beta, c(1, indicatorMatrix[rowIndex, xIndices]))
  
  logLike = -log(1 + exp(betaDotX), base) + indicatorMatrix[rowIndex, yIndex] * betaDotX
  
  return(logLike)
  
}


# negative log likelihood for the entire dataset
negLogLike = function(indicatorMatrix, yIndex, xIndices, beta, base) {
  
  logLike = 0 
  
  # cumulative sum log likelihood for the entire data set
  for (i in 1:nrow(indicatorMatrix)) {
    
    logLike = logLike + logLikeSingle(indicatorMatrix, yIndex, xIndices, beta, i, base)
    
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
logDeterminant = function(matrix) {
  
  choleskeyUpper = chol(matrix)
  
  logDet = 0 
  
  for (i in 1:nrow(matrix)) {
    
    logDet = logDet + log(diag(choleskeyUpper)[i], base = 2)
    
  }

  return(logDet)
  
}

######################################  msg len with no predictor #####################################
# check for this 
msgLenWithNoPredictors = function(data, indicatorMatrix, yIndex, cardinalities, allNodes, sigma, base) {
  
  # formula for empty model
  formula = paste(allNodes[yIndex], "~ 1")
  
  # estimate parameter of logit model using glm
  beta = glm(formula, family = binomial(link = "logit"), data = data)$coefficients
  
  # value for the negative log likelihood 
  nll = negLogLike(indicatorMatrix, yIndex, NULL, beta, base)
  
  # fisher information matrix 
  fisherInfoMatrix = negLoglike2ndDerivative(indicatorMatrix, NULL, beta, 1, 1)
  
  # log of the determinant of the FIM
  logFisher = log(fisherInfoMatrix, base)
  
  # computing mml 
  mml = 0.5 * log(2 * pi, base) + log(sigma, base) - 0.5 * log(cardinalities[yIndex], base) + 
    0.5 * beta ^ 2 / sigma ^ 2 + 0.5 * logFisher + nll + 0.5 * (1 + log(0.083333, base))
  
  # store results in a list 
  lst = list(beta, nll, logFisher, mml)
  
  names(lst) = c("par", "nll", "logFisher", "mml")
  
  return(lst)
  
}


################################################## msg len ############################################
msgLenWithPredictors = function(data, indicatorMatrix, yIndex, xIndices, cardinalities, 
                                allNodes, sigma, base) {
  
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
  nll = negLogLike(indicatorMatrix, yIndex, xIndices, fittedLogit$coefficients, base)
  
  # fisher information matrix 
  fisherInfoMatrix = fisherMatrix(indicatorMatrix, yIndex, xIndices, fittedLogit$coefficients)
  
  # log of the determinant of the FIM
  logFisher = logDeterminant(fisherInfoMatrix)
  
  # computing mml 
  mmlFixedPart =  0.5 * nFreePar * log(2 * pi, base) + nFreePar * log(sigma, base) - 0.5 * log(arityOfY, base) - 
    0.5 * sum((cardinalities[xIndices] - 1) * log(arityOfY, base) + 
                (arityOfY - 1) * log(cardinalities[xIndices], base)) + 0.5 * nFreePar*(1 + log(latticeConst, base)) 
  
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

#################################################### function ####################################
# search for mb using mml 
mbMMLLogit = function(data, indicatorMatrix, y, sigma = 3, base = 2, debug = FALSE) {
  
  allNodes = colnames(data)
  
  numNodes = length(allNodes)
  
  yIndex = which(allNodes == y)
  unUsedNodesIndexes = (1:numNodes)[-yIndex]
  
  cardinalities = rep(2, numNodes) 
  
  for (j in 1:ncol(data)) cardinalities[j] = nlevels(data[, j])
  
  cmb = c()
  # x = c()
  
  if (debug) {
    cat("----------------------------------------------------------------\n")
    cat("* learning the Markov blanket of", y, "\n")
  } # then
  
  # msg len to encode the size k of mb 
  # k takes integer value from [0, n - 1] uniformly
  # logK = log(numNodes)
  
  # msg len of empty markov blanket
  mmlMini = msgLenWithNoPredictors(data, indicatorMatrix, yIndex, cardinalities, allNodes, sigma, base)$mml[[1]]
  
  if (debug) {
    
    cat("    > empty MB has msg len:", round(mmlMini, 2), "\n")  
    
  } # then
  
  repeat{ # use greedy search for an optimal mb
    
    if (debug) {
      
      cat("    * calculating msg len \n")
      
    } # end debug
    
    toAdd = NULL
    
    # msg len to encode the mb
    # logK is explained above
    # the second part is the msg len to encode which k nodes to choose from all n - 1 nodes
    # log (n - 1 choose k) is the second part
    # logK + log(choose(numNodes - 1, length(cmb) + 1))
    
    for (i in 1:length(unUsedNodesIndexes)) { # combine each remaining node with current mb and compute mml
      
      mmlCurrent = msgLenWithPredictors(data, indicatorMatrix, yIndex, c(unUsedNodesIndexes[i], cmb), 
                       cardinalities, allNodes, sigma, base)$mml
      
      if (debug) {
        
        cat("    >", allNodes[unUsedNodesIndexes[i]], "has msg len:", round(mmlCurrent, 2), "\n") 
  
      } # end debug
      
      if (mmlCurrent < mmlMini) { # if adding this node decreases the mml score, then replace mml and add into cmb
        
        mmlMini = mmlCurrent
        
        toAdd = i
        
      }
  
    }
    
    # stop when there is nothing to add from the remaining nodes
    # that is when mml score does not decrease 
    # it indicates adding more nodes into cmb does not make current model better
    if (is.null(toAdd)) {
      
      print("No better candidate to add \n")
      break
    
    } # end if 
    
    cmb = c(cmb, unUsedNodesIndexes[toAdd])
    
    if (debug) {
      
      cat("    @", allNodes[unUsedNodesIndexes[toAdd]], "include in the Markov blanket", "\n")
      cat("    > Markov blanket (", length(cmb), "nodes ) now is '", allNodes[cmb], "'\n")
      
    } # end debug
    
    # remove added node index from unchecked nodes indices
    unUsedNodesIndexes = unUsedNodesIndexes[-toAdd]
    
    # if 0 node left for inclusion stop
    if (length(unUsedNodesIndexes) == 0) {
      
      print("No more node to add \n")
      break
  
    } # end if 
    
  } # end repeat
  
  if (debug) {
    
    cat("    * Algorithm stops! \n")
    
  }
  
  return(allNodes[cmb])
  
}

# when testing on asia net, glm shows warnings, glm.fit: fitted probabilities numerically 0 or 1 occurred 

