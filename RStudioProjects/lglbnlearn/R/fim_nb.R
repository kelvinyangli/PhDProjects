#' Calculate FIM of NB
#'
#' This function calculates the expected fisher information matrix of a Naive Bayes model. 
#' @param prodPij1 A vector of \prod_j p(x_ij|y=1).
#' @param prodPij0 A vector of \prod_j p(x_ij|y=0).
#' @param px A vector of py1 * prodPij1 + py0 * prodPij0.
#' @param probsMatrix A matrix of p(x_j|y=1) and p(y_j|y=0)
#' @param py1 p(y=T)
#' @param py0 p(y=F)
#' @param arities A vector of variable arities.
#' @param xIndices A vector of input variables indices.
#' @param yIndex The index of the output variable. 
#' @export
fim_nb = function(prodPij1, prodPij0, px, probsMatrix, py1, py0, arities, xIndices, yIndex) {
  # empty FIM
  fimDim = (arities[yIndex] - 1) + length(xIndices) * arities[yIndex]
  fim = matrix(0, nrow = fimDim, ncol = fimDim)
  
  # off diagonal entries
  mm = prodPij1 * prodPij0 / (px ^ 2) # a common contant 
  fim[1, -1] = colSums(mm / probsMatrix) # fill in 1st row of FIM
  fim[1, (2:ncol(fim))[odd(2:ncol(fim))]] = -1 * fim[1, (2:ncol(fim))[odd(2:ncol(fim))]]
  for (rowIndex in 2:(nrow(fim) - 1)) {
    if (ncol(fim) - rowIndex == 1) {
      fim[rowIndex, -(1:rowIndex)] = 
        sum(-mm * py1 * py0 / (probsMatrix[, rowIndex - 1] * probsMatrix[, -(1:(rowIndex - 1))]))
    } else {
      fim[rowIndex, -(1:rowIndex)] = 
        colSums(-mm * py1 * py0 / (probsMatrix[, rowIndex - 1] * probsMatrix[, -(1:(rowIndex - 1))]))
    } # end else
  }
  fim = fim + t(fim) # duplicate upper to lower triangular fim
  
  # diagonal entries
  # assume all variables are binary, hence the diag[1] is always the 2nd derivative w.r.t. p_i0
  diag(fim)[1] = sum(prodPij1 / (py1 * px) + prodPij0 / (py0 * px) - ((prodPij1 - prodPij0) / px) ^ 2)
  diag(fim)[-1] = colSums(py1 * py0 * prodPij1 * prodPij0 / (px * probsMatrix) ^ 2)
  
  return(fim)
}