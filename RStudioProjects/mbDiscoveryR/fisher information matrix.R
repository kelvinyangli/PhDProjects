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







