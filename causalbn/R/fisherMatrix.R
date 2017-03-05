####################################################################################################  
# computing the fisher information matrix, given that the target has at least 1 parent
# where there is no parent, FIM is a 1x1 matrix, which is just a sigle value 
# and hence can be computed easily without using this function
# expConstants is a vector contains the exp(beta*X)/(1 + exp(beta*X))^2 for each X_i 
# this vector is pre-computed and saved for fisherMatrix
# the output of fisherMatrix is the entire fisher information matrix (FIM)
####################################################################################################
fisherMatrix = function(indicatorMatrix, yIndex, xIndices, expConstants) {
  
  # FIM is a square matrix, with dimensions = |beta| = |xIndices| + 1 for binary case
  FIM = matrix(NA, length(xIndices) + 1, length(xIndices) + 1) 
  
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
      FIM[j, k] = expConstants %*% (indicatorMatrix[, xIndices[j - 1]] * indicatorMatrix[, xIndices[k - 1]])
      
    } # end for k
    
  } # end for j
  
  # the 1st column is identical to the diagnose of FIM
  FIM[, 1] = diag(FIM)
  
  # the upper triangular is identical to the lower triangular FIM
  FIM[upper.tri(FIM)] = t(FIM)[upper.tri(t(FIM))]
  
  return(FIM) # return the complete FIM
  
}
