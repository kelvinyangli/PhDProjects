################################################## pre process data ############################################\
# transform binary data into 0 and 1
dataToIndicator = function(data) {
  
  indicatorMatrix = matrix(nrow = nrow(data), ncol = ncol(data), dimnames = list(NULL, colnames(data)))
  
  for (i in 1:nrow(data)) {
      
    for (j in 1:ncol(data)) {
      
      indicatorMatrix[i, j] = as.numeric(data[i, j] == "B") # use "A" as the reference 
      
    }
    
  }
  
  indicatorMatrix = cbind(1, indicatorMatrix)
  
  return(indicatorMatrix)
  
}


########################################## MAP estimation of the parameters ###################################
# first find the negative log posterior 
# second find the 1st derivation of the negative log posterior 
# in order to get fast and more accurate estimation when using optim in R
# still quite slow when nInstances is large 
################################################################################################################
# log posterior is the sum of log prior and log likelihood
# take negative log posterior in order to find minimum
negLogLike = function(indicatorMatrix, dependVarIndex, independVarIndexes, beta) {
  
  logLike = 0 
  
  # cumulative sum log likelihood for the entire data set
  for (i in 1:nrow(indicatorMatrix)) {
    
    # compute the inner product of beta and x 
    # beta[1] is always 1 for intercepts
    betaDotX = sum(beta * indicatorMatrix[i, c(1, independVarIndexes + 1)])
    
    logLike = logLike - log(1 + exp(betaDotX)) + indicatorMatrix[i, dependVarIndex + 1] * betaDotX
    
  }
  
  return(-logLike) # return negative log likelihood
  
}

negLoglike1stDerivative = function(indicatorMatrix, dependVarIndex, independVarIndexes, beta) {
  
  negLogLike1stDeri = 0 
  
  for (i in 1:nrow(indicatorMatrix)) {
    
    expBetaDotX = exp(sum(beta * indicatorMatrix[i, c(1, independVarIndexes + 1)]))
    
    negLogLike1stDeri = negLogLike1stDeri + 
      indicatorMatrix[i, c(1, independVarIndexes + 1)] * (1/(1 + expBetaDotX^-1) - indicatorMatrix[i, dependVarIndex + 1])
    
  }
  
  return(negLogLike1stDeri)
  
}


#negLogLike = cmpfun(negLogLike)
#negLoglike1stDerivative = cmpfun(negLoglike1stDerivative)
############################################## Fisher information ###################################################
# given MAP estimation of parameter beta
# compute determinant of expected FIM
######################################################################################################################
# 2nd derivative of the negative log likelihood 
# for computing the fisher information matrix
negLoglike2ndDerivative = function(indicatorMatrix, dependVarIndex, independVarIndexes, beta, j, k) {
  
  if (j * k == 1) { # when both j and k = 1
    
    rowIndex = 1
    colIndex = 1
    
  } else if ((j > 1) && (k > 1)) {
    
    rowIndex = independVarIndexes[j - 1] + 1
    colIndex = independVarIndexes[k - 1] + 1
    
  }
  
  negLogLike2ndDeri = 0 
  
  for (i in 1:nrow(indicatorMatrix)) {
    
    expBetaDotX = exp(sum(beta * indicatorMatrix[i, c(1, independVarIndexes + 1)]))
    
    negLogLike2ndDeri = negLogLike2ndDeri + 
      indicatorMatrix[i, rowIndex] * indicatorMatrix[i, colIndex] * 
      expBetaDotX/(1 + expBetaDotX)^2
    
  }
  
  return(negLogLike2ndDeri)
  
}

# computing entries of fisher information matrix
fisherMatrix = function(indicatorMatrix, dependVarIndex, independVarIndexes, beta) {
  
  FIM = matrix(NA, length(independVarIndexes) + 1, length(independVarIndexes) + 1) 
  
  # fill in the (1,1) entry of FIM
  FIM[1, 1] = negLoglike2ndDerivative(indicatorMatrix, dependVarIndex, independVarIndexes, beta, 1, 1)
  
  # fill in the lower triangular FIM
  for (j in 2:nrow(FIM)) {
    
    for (k in 2:j) {
      
      FIM[j, k] = negLoglike2ndDerivative(indicatorMatrix, dependVarIndex, independVarIndexes, beta, j, k)
      
    }
    
  }
  
  # the 1st column is identical ti the diagnose of FIM
  FIM[,1] = diag(FIM)
  
  # the upper triangular is identical to the lower triangular FIM
  FIM[upper.tri(FIM)] = t(FIM)[upper.tri(t(FIM))]
  
  return(FIM) # return the complete FIM
  
}

# calculate log of the determinant of a matrix
# matrix has to be symmetric positive definite
# use cholesky decomposition to decompose matrix FIM = L*transpose(L)
logDeterminant = function(matrix) {
  
  choleskeyUpper = chol(matrix)
  
  logDet = 2 * sum(log(diag(choleskeyUpper)))
  
  return(logDet)
  
}

#negLoglike2ndDerivative = cmpfun(negLoglike2ndDerivative)
#fisherMatrix = cmpfun(fisherMatrix)
#logDeterminant = cmpfun(logDeterminant)
################################################## msg len ############################################
msgLen = function(indicatorMatrix, dependVarIndex, independVarIndexes, cardinalities, sigma) {
  
  nFreePar = length(independVarIndexes)
  
  # lattice constant
  k = c(0.083333, 0.080188, 0.077875, 0.07609, 0.07465, 0.07347, 0.07248, 0.07163)
  
  latticeConst = ifelse(nFreePar <= length(k), k[length(independVarIndexes)], min(k))
  
  # parameter estimation of negative log likelihood using MLE 
  mleLogit = optim(par = rep(0, length(independVarIndexes) + 1), fn = negLogLike, gr = negLoglike1stDerivative, 
                   indicatorMatrix = indicatorMatrix, dependVarIndex = dependVarIndex, 
                   independVarIndexes = independVarIndexes, method = "L-BFGS-B")
  
  # fisher information matrix 
  fisherInfoMatrix = fisherMatrix(indicatorMatrix, dependVarIndex, independVarIndexes, mleLogit$par)
  
  # log of the determinant of the FIM
  logFisher = logDeterminant(fisherInfoMatrix)
  
  # computing mml 
  mml =  0.5 * nFreePar * log(2 * pi) + nFreePar * log(sigma) - 0.5 * log(cardinalities[dependVarIndex]) - 
    0.5 * sum((cardinalities[independVarIndexes] - 1) * log(cardinalities[dependVarIndex]) + 
    (cardinalities[dependVarIndex] - 1) * log(cardinalities[independVarIndexes])) + 
    0.5 * nFreePar*(1 + log(latticeConst)) + sum(mleLogit$par^2)/(2 * sigma^2) + 0.5 * logFisher + mleLogit$value 
  
  # store results in a list 
  lst = list(mleLogit$par, mleLogit$value, logFisher, mml)
  
  names(lst) = c("par", "nll", "logFisher", "mml")
  
  return(lst)
  
}

msgLen = cmpfun(msgLen)
#################################################### function ####################################
# search for mb using mml 
mbMML = function(indicatorMatrix, y, debug = FALSE, sigma = 3, mbSize = 100) {
  
  allNodes = colnames(indicatorMatrix)[-1]
  allNodesIndexes = 1:length(allNodes)
  
  dependVarIndex = which(allNodes == y)
  unUsedNodesIndexes = allNodesIndexes[-dependVarIndex]
  
  cardinalities = rep(2, length(allNodes)) # since only deal with binary nodes for now
  
  cmb = c()
  x = c()
  
  mmlForEachNode = vector(length = length(unUsedNodesIndexes))
  
  # set default msg len to infinity, as we want the minimum msg len
  min.current = min.previous = Inf
  
  if (debug) {
    cat("----------------------------------------------------------------\n")
    cat("* learning the Markov blanket of", y, "\n")
  } # then
  
  repeat{ # finding optimal mb using heuristics
    
    if (debug) {
      cat("    * calculating msg len \n")
    } # then
    
    for (i in 1:length(unUsedNodesIndexes)) { # combine each left node with current mb and compute mml
      
      result = msgLen(indicatorMatrix, dependVarIndex, c(unUsedNodesIndexes[i], cmb), cardinalities, sigma)
      
      mmlForEachNode[i] = result$mml
      
      if (debug) {
        
        cat("    >", allNodes[unUsedNodesIndexes[i]], "has msg len:", round(mmlForEachNode[i], 2), paste0("(", round(result$logFisher, 2), ")"), "\n")  
        
      } # then
      
    }
    
    min.previous = min.current
    
    min.current = min(mmlForEachNode[1:length(unUsedNodesIndexes)])
    
    # stop when mml score does not decrease 
    # means adding more unUsedNodes into cmb does not make current model better
    if (min.current >= min.previous) break 
    
    nodeIndexWithMinMML = which.min(mmlForEachNode[1:length(unUsedNodesIndexes)])
    to.add = unUsedNodesIndexes[nodeIndexWithMinMML] # returns the index of the node to add
    
    cmb = c(cmb, to.add)
    
    if (debug) {
      
      cat("    @", allNodes[to.add], "include in the Markov blanket", "\n")
      cat("    > Markov blanket (", length(cmb), "nodes ) now is '", allNodes[cmb], "'\n")
      
    } # then
    
    if (length(cmb) >= mbSize) break
    
    unUsedNodesIndexes = unUsedNodesIndexes[-nodeIndexWithMinMML]
    
    if(length(unUsedNodesIndexes) == 0) break
    
  } # end repeat
  
  if (debug) {
    
    cat("    * Algorithm stops no more to add", "\n")
    
  }
  
  return(allNodes[cmb])
  
}

mbMML = cmpfun(mbMML)
################### call function #################



