####################################################################################################
# calculate log of the determinant of a matrix, in particular the fisher information matrix (FIM)
# the input matrix has to be symmetric positive definite, because this function uses
# cholesky decomposition to decompose the matrix FIM = L*transpose(L)
# the log used in this function is the natural log, due to the same reason for nll, because there
# are cancellations with exponential functions
# this function returns the determinant of a matrix 
# this function could cause mbMMLLogit to stop due to the input FIM is not positive definite
# the non-positive definite happends often for 2nd order logit, perhaps due to the lack of data
####################################################################################################
logDeterminant = function(matrix) {
  
  choleskeyUpper = chol(matrix)
  
  logDet = 0 
  
  for (i in 1:nrow(matrix)) {
    
    logDet = logDet + log(diag(choleskeyUpper)[i])
    
  }
  
  return(logDet)
  
}

