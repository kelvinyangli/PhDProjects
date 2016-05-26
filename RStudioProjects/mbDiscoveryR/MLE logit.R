# parameter estimation using glm
# glm uses MLE for parameters of logit model


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


dependVarIndex = which(colnames(data) == y)
independVarIndexes = which(colnames(data) %in% x)

mleLogit = optim(par = rep(0, length(independVarIndexes) + 1), fn = negLogLike, gr = negLoglike1stDerivative, 
      indicatorMatrix = indicatorMatrix, dependVarIndex = dependVarIndex, 
      independVarIndexes = independVarIndexes, method = "L-BFGS-B")




