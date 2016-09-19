####################################################################################################
# negative log likelihood for the entire dataset
# for single row nll = -log(1 + exp(beta %*% X)) + y[i]*(beta %*% X)
# for the entire data set nll = sum_{i=1}^n[-log(1 + exp(beta %*% X)) + y[i]*(beta %*% X)]
# the log used here is natural log, since there are cancellation with exponential functions
####################################################################################################
negLogLike = function(indicatorMatrix, yIndex, xIndices, betaDotX) {
  
  if (is.null(xIndices)) {
    
    # when is no parent, betaDotX is a single value rather than a vector
    
    logLike = -log(1 + exp(betaDotX)) * nrow(indicatorMatrix) + sum(indicatorMatrix[, yIndex]) * betaDotX
    
  } else {
    
    # where there exists parent, betaDotX is a vector where each element corresponds to a single row of data
    logLike = sum(-log(1 + exp(betaDotX))) + indicatorMatrix[, yIndex] %*% betaDotX
    
  }
  
  return(-logLike) 
  
}
