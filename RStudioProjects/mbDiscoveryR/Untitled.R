# compute log determinant of FIM using cholesky decomposition
hessian = function(dependVar, independVar, beta, j, k) {
  LL = 0 
  for (i in 1:length(dependVar)) {
    expBetaDotX = exp(sum(beta*independVar[i,]))
    LL = LL + independVar[i, j]*independVar[i, k]*expBetaDotX/(1 + expBetaDotX)^2
  }
  return(LL)
}

lowerTriangularDiagnal = function(lowerTriangularMatrix, dependVar, independVar, beta, j) {
  hessianDiagnal = hessian(dependVar, independVar, beta, j, j)
  offDiagnalSum = 0
  if (j > 1) {
    for (k in 1:(j - 1)) {
      offDiagnalSum = offDiagnalSum + lowerTriangularMatrix[j, k]^2
    }
  }
  return(sqrt(hessianDiagnal - offDiagnalSum))
}

lowerTriangularMatrix = function(dependVar, independVar, beta, j, k) {
  ltm = matrix(0, nrow = ncol(independVar), ncol = ncol(independVar))
  ltm[1, 1]
}