########################################## MAP estimation of the parameters ###################################
# first find the negative log posterior 
# second find the 1st derivation of the negative log posterior 
# in order to get fast and more accurate estimation when using optim in R
# still quite slow when nInstances is large 
################################################################################################################

mapEst = function(dependVar, independVar, sigma, nPar) {
  # log prior
  logPrior = function(beta) {
    -sum(beta^2)/(2*sigma^2)
  }
  # log likelihood singler term
  logLikeSingle = function(beta, i) {
    betaDotX = sum(beta*independVar[i,])
    -log(1 + exp(betaDotX)) + dependVar[i]*betaDotX
  }
  
  # log likelihood for entire dataset
  logLike = function(beta) {
    LL = 0 
    for (i in 1:length(dependVar)) LL = LL + logLikeSingle(beta, i)
    return(LL)
  }
  
  # log posterior is the sum of log prior and log likelihood
  # take negative log posterior in order to find minimum
  negLogPost = function(beta) {
    -logLike(beta) - logPrior(beta)
  }
  
  # 1st derivative of log piror wrt beta_j
  logPriorDeri = function(beta) {
    -beta/sigma^2
  }
  
  # 1st derivative of log likelihood wrt beta_j
  logLikeSingleDeri = function(beta, i) {
    expBetaDotX = exp(sum(beta*independVar[i,]))
    -independVar[i,]*expBetaDotX/(1 + expBetaDotX) + dependVar[i]*independVar[i,]
  }
  
  # for entire dataset
  logLikeDeri = function(beta) {
    LL = 0 
    for (i in 1:length(dependVar)) LL = LL + logLikeSingleDeri(beta, i)
    return(LL)
  }
  
  # derivative of negative log posterior
  negLogPostDeri = function(beta) {
    -logPriorDeri(beta) - logLikeDeri(beta)
  }
  
  estimation = optim(par = rep(0, nPar), fn = negLogPost, gr = negLogPostDeri, method = "L-BFGS-B")
  lst = list(estimation$par, estimation$value)
  names(lst) = c("par", "value")
  return(lst)
}

############################################## Fisher information ###################################################
# given MAP estimation of parameter beta
# compute determinant of expected FIM
# 
#
######################################################################################################################
detFisher = function(dependVar, independVar, beta, nFreePar = nFreePar) {
  # 2nd derivative of single term of nll
  secondDeriSingle = function(beta, i, j, k) {
    expBetaDotX = exp(sum(beta*independVar[i,]))
    independVar[i, j]*independVar[i, k]*expBetaDotX/(1 + expBetaDotX)^2  
  }
  
  # 2nd derivative of the negative log likelihood
  secondDeri = function(beta, j, k) {
    LL = 0 
    for (i in 1:length(dependVar)) LL = LL + secondDeriSingle(beta, i, j, k)
    return(LL)
  }
  
  FIM = matrix(NA, nFreePar, nFreePar) 
  for (j in 1:nFreePar) {
    for (k in 1:nFreePar) {
      FIM[j, k] = secondDeri(beta, j, k)
    }
  }
  return(det(FIM))
}

################################################## pre process data ############################################\
# get all the second order pairs for logit model
getPairs = function(x, valueIndependVar) {
  nPairs = sum(1:(length(x)-1))
  allPairs = c()
  for (i in 1:(length(x)-1)) {
    for (j in (i+1):length(x)) {
      allPairs = rbind(allPairs, c(i, j))
    }
  }  
  
  allComb = list()
  for (i in 1:nPairs) {
    allComb[[i]] = expand.grid(valueIndependVar[[allPairs[i,1]]],valueIndependVar[[allPairs[i,2]]])
  }
  
  lst = list(allPairs = allPairs, allComb = allComb)
  return(lst)
}

# when secondOrder = TRUE compute all second order terms
# when secondOrder = FALSE compute only first order terms
dataPreprocess = function(data, y, x, secondOrder = FALSE) {
  # format X and Y for function NLL and Fisher to be used
  dependVar = data[,y]
  valueDependVar = unique(dependVar)
  tempDepend = c()
  for (i in 1:(length(valueDependVar)-1)) tempDepend = c(tempDepend, dependVar == valueDependVar[i])
  dependVar = as.numeric(tempDepend)
  arityDependVar = length(valueDependVar)
  
  valueIndependVar = list()
  arityIndependVar = c()
  for (i in 1:length(x)) {
    valueIndependVar[[i]] = unique(data[,x[i]])
    arityIndependVar = c(arityIndependVar, length(valueIndependVar[[i]]))
    names(valueIndependVar)[i] = x[i]
  }
  
  if (secondOrder && (length(x) > 1)) {
    res = getPairs(x, valueIndependVar)
    secondOrderPair = res$allPairs
    secondOrderValueComb = res$allComb
    newArity = c()
    for (i in 1:length(secondOrderValueComb)) newArity = c(newArity, nrow(secondOrderValueComb[[i]]))
    arityIndependVar = c(arityIndependVar, newArity)
  }
  
  # make a column for each value of an independent variable
  independVar = data[,x]
  if (length(x) < 2) independVar = as.matrix(independVar)
  tempIndepend = c()
  for (i in 1:length(x)) {
    for (j in 1:(length(valueIndependVar[[i]]))) {
      tempIndepend = cbind(tempIndepend, independVar[,i] == valueIndependVar[[i]][j])
    }
  }
  
  if (secondOrder && (length(x) > 1)) {
    for (i in 1:length(secondOrderValueComb)) {
      pairIndex = secondOrderPair[i,]
      for (j in 1:nrow(secondOrderValueComb[[i]])) {
        match = c()
        for (k in 1:nrow(independVar)) {
          match = c(match, prod(independVar[k, pairIndex] == secondOrderValueComb[[i]][j,]))
        }
        tempIndepend = cbind(tempIndepend, match)  
      }  
    }  
  }
  colnames(tempIndepend) = c()
  independVar = cbind(1, tempIndepend)
  
  # keep the free parameters 
  freeCol = c()
  for (i in 1:length(arityIndependVar)) {
    freeCol = c(freeCol, sum(arityIndependVar[1:i]))
  }
  independFreeVar = tempIndepend[,-freeCol]
  independFreeVar = cbind(1, independFreeVar)
  
  lst = list(independVar = independVar, independFreeVar = independFreeVar, dependVar = dependVar, arityDependVar = arityDependVar, arityIndependVar = arityIndependVar)
  return(lst)
}

################################################## msg len ############################################
msgLen = function(data, y, x, sigma = 3, secondOrder = FALSE) {
  tempData = dataPreprocess(data = data, y = y, x = x, secondOrder = secondOrder)
  dependVar = tempData$dependVar
  independVar = tempData$independVar
  independFreeVar = tempData$independFreeVar
  # arity of dependent variable
  arityDependVar = tempData$arityDependVar
  # arity of each independent variable x
  arityIndependVar = tempData$arityIndependVar
  nFreePar = ncol(independFreeVar)
  nPar = ncol(independVar)
  # nFreePar = sum(arityIndependVar - 1) + 1
  # lattice constant
  k = c(0.083333, 0.080188, 0.077875, 0.07609, 0.07465, 0.07347, 0.07248, 0.07163)
  latticeConst = ifelse(nFreePar <= length(k), k[nFreePar], min(k))
  # MAP estimation of the negative log likelihood 
  negLogLike = mapEst(dependVar = dependVar, independVar = independVar, sigma = sigma, nPar = nPar)
  # determinant of fisher information matrix 
  index = c()
  for (i in 1:length(arityIndependVar)) {
    index = c(index, sum(arityIndependVar[1:i]) + 1)
  }
  fisher = detFisher(dependVar = dependVar, independVar = independFreeVar, beta = negLogLike$par[-index], nFreePar = nFreePar)
  # total msg len 
  mml = 0.5*nFreePar*log(2*pi) + nFreePar*log(sigma) - 0.5*log(arityDependVar) - 
    0.5*sum((arityIndependVar - 1)*log(arityDependVar) + (arityDependVar - 1)*log(arityIndependVar)) + 
    (1/(2*sigma^2))*sum(negLogLike$par^2) + 0.5*log(fisher) + negLogLike$value + 
    0.5*nFreePar*(1 + log(latticeConst))
  lst = list(negLogLike$par, negLogLike$value, fisher, mml)
  names(lst) = c("par", "nll", "fisher", "mml")
  return(lst)
}

#################################################### function ####################################
# search for mb using mml 
mbMML = function(data, y, debug = FALSE, nNodeInMB = FALSE, secondOrder = FALSE) {
  allNodes = names(data)
  unUsedNodes = allNodes[allNodes != y]
  cmb = x = c()
  # default msn.len is infinity 
  min.current = min.previous = Inf
  
  if (debug) {
    cat("----------------------------------------------------------------\n")
    cat("* learning the Markov blanket of", y, "\n")
  } # then
  
  repeat{
    if (debug) {
      cat("    * calculating msg len \n")
    } # then
    
    res = c()
    for (i in 1:length(unUsedNodes)) {
      res[i] = msgLen(data = data, y = y, x = c(unUsedNodes[i], cmb), secondOrder = secondOrder)$mml
      
      if (debug) {
        cat("    >", unUsedNodes[i], "has msg len:", res[i], "\n")  
      } # then
    }
    min.previous = min.current
    min.current = min(res)
    # stop when mml score does not decrease 
    # means adding more unUsedNodes into cmb does not make current model better
    if (!nNodeInMB) {
      if (min.current >= min.previous) break 
    }
      
    to.add = unUsedNodes[which.min(res)]
    cmb = c(cmb, to.add)
    
    if (debug) {
      cat("    @", to.add, "include in the Markov blanket", "\n")
      cat("    > Markov blanket (", length(cmb), "nodes ) now is '", 
          cmb, "'\n")
    } # then
    
    if (is.numeric(nNodeInMB)) {
      if (!length(cmb) < nNodeInMB) break
    }
    
    unUsedNodes = unUsedNodes[unUsedNodes != to.add]
    if(length(unUsedNodes) ==0) break
  }
  
  if (debug) {
    cat("    * Algorithm stops no more to add", "\n")
  }
  return(cmb)
}

################### call function #################



