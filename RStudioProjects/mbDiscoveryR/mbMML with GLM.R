# compute mml using glm
# should obtain same answer as using optim, because glm also use mle 

################################################## pre process data ############################################\
# transform binary data into 0 and 1
dataToIndicator = function(data) {
  
  indicatorMatrix = matrix(nrow = nrow(data), ncol = ncol(data), dimnames = list(NULL, colnames(data)))
  
  for (i in 1:ncol(data)) {
    
    # since glm always use the first levels appears as the reference when estimating coefficients
    # here we convert data into its levels and then minus 1 
    # since we only deal with binary data for now, this convert data into 0 and 1
    # where 0 and 1 correspond to the 1st and 2nd levels in data frame for each variable
    indicatorMatrix[,i] = as.numeric(data[,i]) - 1 
    
  }
  
  indicatorMatrix = cbind(1, indicatorMatrix)
  
  return(indicatorMatrix)
  
}

dataToIndicator = cmpfun(dataToIndicator)
################################################## negative log likelihood
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


##################################################  computing entries of fisher information matrix
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
  
  logDet = 2 * sum(log2(diag(choleskeyUpper)))
  
  return(logDet)
  
}

negLogLike = cmpfun(negLogLike)
negLoglike2ndDerivative = cmpfun(negLoglike2ndDerivative)
fisherMatrix = cmpfun(fisherMatrix)
logDeterminant = cmpfun(logDeterminant)


######################################  msg len with no predictor #####################################
msgLenNoPredictor = function(indicatorMatrix, dependVarIndex, variableCounts, sigma) {
  
  odds = variableCounts[[dependVarIndex]][2]/variableCounts[[dependVarIndex]][1]

  beta = log(odds)
  
  # value for the negative log likelihood 
  negLogLikeValue = negLogLike(indicatorMatrix, dependVarIndex, NULL, beta)
  
  # fisher information matrix 
  fisherInfoMatrix = negLoglike2ndDerivative(indicatorMatrix, dependVarIndex, NULL, beta, 1, 1)
  
  # log of the determinant of the FIM
  logFisher = log2(fisherInfoMatrix)
  
  # computing mml 
  mml =  - 0.5 * log2(2) + (beta^2)/(2 * sigma^2) + 0.5 * logFisher + negLogLikeValue
  
  # store results in a list 
  lst = list(beta, negLogLikeValue, logFisher, mml)
  
  names(lst) = c("par", "nll", "logFisher", "mml")
  
  return(lst)
  
}

msgLenNoPredictor = cmpfun(msgLenNoPredictor)


################################################## msg len ############################################
msgLen = function(data, indicatorMatrix, dependVarIndex, independVarIndexes, cardinalities, allNodes, sigma) {
  
  nFreePar = length(independVarIndexes)
  
  # lattice constant
  k = c(0.083333, 0.080188, 0.077875, 0.07609, 0.07465, 0.07347, 0.07248, 0.07163)
  
  latticeConst = ifelse(nFreePar <= length(k), k[length(independVarIndexes)], min(k))
  
  # create formula for fitting logit model using glm
  formula = paste(allNodes[dependVarIndex], "~", paste0(allNodes[independVarIndexes], collapse = "+"))
  
  # parameter estimation of negative log likelihood using GLM
  # glm always use the first level (in this case "A") for reference when estimating coefficients
  # the reference can be changed by change the order of levels in data frame using relevel()
  glmEstimation = glm(formula, family = binomial(link = "logit"), data = data)
  
  # value for the negative log likelihood 
  negLogLikeValue = negLogLike(indicatorMatrix, dependVarIndex, independVarIndexes, glmEstimation$coefficients)
  
  # fisher information matrix 
  fisherInfoMatrix = fisherMatrix(indicatorMatrix, dependVarIndex, independVarIndexes, glmEstimation$coefficients)
  
  # log of the determinant of the FIM
  logFisher = logDeterminant(fisherInfoMatrix)
  
  # computing mml 
  mml1 =  0.5 * nFreePar * log2(2 * pi) + nFreePar * log2(sigma) - 0.5 * log2(cardinalities[dependVarIndex]) - 
    0.5 * sum((cardinalities[independVarIndexes] - 1) * log2(cardinalities[dependVarIndex]) + 
                (cardinalities[dependVarIndex] - 1) * log2(cardinalities[independVarIndexes])) + 
    0.5 * nFreePar*(1 + log2(latticeConst)) 
  
  mml2 = sum(glmEstimation$coefficients^2)/(2 * sigma^2) + 
    0.5 * logFisher + negLogLikeValue
  
  mml = mml1 + mml2
  
  # store results in a list 
  lst = list(glmEstimation$coefficients, negLogLikeValue, logFisher, mml)
  
  names(lst) = c("par", "nll", "logFisher", "mml")
  
  return(lst)
  
}

msgLen = cmpfun(msgLen)


#################################################### function ####################################
# search for mb using mml 
mbMML = function(data, indicatorMatrix, y, debug = FALSE, sigma = 3, mbSize = 100) {
  
  allNodes = colnames(data)
  
  numNodes = length(allNodes)
  
  allNodesIndexes = 1:numNodes
  
  dependVarIndex = which(allNodes == y)
  unUsedNodesIndexes = allNodesIndexes[-dependVarIndex]
  
  cardinalities = rep(2, numNodes) # since only deal with binary nodes for now
  
  # count the occurances of values for each variable
  variableCounts = list()
  
  for (x in colnames(data)) {
    
    variableCounts[[x]] = table(data[, x])
    
  }
  
  cmb = c()
  x = c()
  
  if (debug) {
    cat("----------------------------------------------------------------\n")
    cat("* learning the Markov blanket of", y, "\n")
  } # then
  
  # msg len to encode the size k of mb 
  # k takes integer value from [0, n - 1] uniformly
  # logK = log(numNodes)
  
  # msg len of empty markov blanket
  MML = msgLenNoPredictor(indicatorMatrix, dependVarIndex, variableCounts, sigma)$mml[[1]]
  
  if (debug) {
    
    cat("    > empty MB has msg len:", round(MML, 2), "\n")  
    
  } # then
  
  repeat{ # finding optimal mb using heuristics
    
    if (debug) {
      cat("    * calculating msg len \n")
      
    } # then
    
    toAdd = NULL
    
    # msg len to encode the mb
    # logK is explained above
    # the second part is the msg len to encode which k nodes to choose from all n - 1 nodes
    # log (n - 1 choose k) is the second part
    # logK + log(choose(numNodes - 1, length(cmb) + 1))
    
    for (i in 1:length(unUsedNodesIndexes)) { # combine each remaining node with current mb and compute mml
      
      tempMML = msgLen(data, indicatorMatrix, dependVarIndex, c(unUsedNodesIndexes[i], cmb), cardinalities, allNodes, sigma)$mml
      
      if (debug) {
        
        cat("    >", allNodes[unUsedNodesIndexes[i]], "has msg len:", round(tempMML, 2), "\n") 
  
      } # then
      
      if (tempMML < MML) { # if adding this node decreases the mml score, then replace mml and add into cmb
        
        MML = tempMML
        
        toAdd = i
        
      }
  
    }
    
    # stop when there is nothing to add from the remaining nodes
    # that is when mml score does not decrease 
    # it indicates adding more nodes into cmb does not make current model better
    if (is.null(toAdd)) break 
    
    cmb = c(cmb, unUsedNodesIndexes[toAdd])
    
    if (debug) {
      
      cat("    @", allNodes[unUsedNodesIndexes[toAdd]], "include in the Markov blanket", "\n")
      cat("    > Markov blanket (", length(cmb), "nodes ) now is '", allNodes[cmb], "'\n")
      
    } # then
    
    # the size of mb reaches the pre-determined maximum stop
    if (length(cmb) >= mbSize) break
    
    unUsedNodesIndexes = unUsedNodesIndexes[-toAdd]
    
    # if 0 node left for inclusion stop
    if(length(unUsedNodesIndexes) == 0) break
    
  } # end repeat
  
  if (debug) {
    
    cat("    * Algorithm stops no more to add", "\n")
    
  }
  
  return(allNodes[cmb])
  
}

mbMML = cmpfun(mbMML)

# when testing on asia net, glm shows warnings, glm.fit: fitted probabilities numerically 0 or 1 occurred 

