#' A function to calculate the MML for the Naive Bayes model
#'
#' This function calculates the total MML score of the Naive Bayes model. It is derived from the general 
#' MML87 formula, but for Naive Bayes only. The parameters used are not MML estimations but maximum 
#' likelihood estimations due to simplicity. 
#' @param data A given data set. 
#' @param vars A vector of all variables. 
#' @param arities A vector of variables arities.
#' @param sampleSize Sample size of a given data set. 
#' @param x A vector of input variables. 
#' @param y The output variable. 
#' @param debug A boolean variable to display each part of the MML score. 
#' @export
mml_nb = function(data, vars, arities, sampleSize, x, y, debug = FALSE) {
  
  yIndex = which(colnames(data) == y)
  xIndices = which(colnames(data) %in% x)
  
  pars = mle_est_nb(data, vars, xIndices, yIndex, smoothing = 0.5) # mle of parameters with smoothing
  # negative log likelihood
  nll = -sum(apply(data, 1, nll_nb, pars = pars, xIndices = xIndices, yIndex = yIndex))
  
  # p(y=T)
  py1 = pars[[length(pars)]][[1]]
  # p(y=F)
  py0 = pars[[length(pars)]][[2]]
  if (length(x) > 0) {# if x is not empty
    # a vector of \prod_j p(x_ij|y=1)
    prodPij1 = apply(data, 1, prod_pijk, pars = pars, xIndices = xIndices, yValue = 1)
    # a vector of \prod_j p(x_ij|y=2)
    prodPij0 = apply(data, 1, prod_pijk, pars = pars, xIndices = xIndices, yValue = 2)
    # a vector of p_xi 
    px = py1 * prodPij1 + py0 * prodPij0
    nll = nll + sum(log(px)) # add additional log(px) value to nll when x is not empty
    # a matrix of p(x_j|y=T) and p(x_j|y=F)
    probsMatrix = c()
    for (j in xIndices) {
      probsMatrix = 
        cbind(probsMatrix, apply(data, 1, p_ijk, pars = pars, xIndices = xIndices, xIndex = j, yValue = 1), apply(data, 1, p_ijk, pars = pars, xIndices = xIndices, xIndex = j, yValue = 2))
    }
    # FIM
    fim = fim_nb(prodPij1, prodPij0, px, probsMatrix, py1, py0, arities, xIndices, yIndex)
    # log determinant of FIM
    logF = log(det(fim))
    # number of free parameters
    d = nrow(fim)
  } else {# if x is empty fim is a 1x1 matrix 
    logF = log(sampleSize * (1 / py1 + 1 / py0))
    d = arities[yIndex] - 1
  }
  
  # log prior
  logPrior = sum(unlist(lapply(pars, log)))
  # lattice constant
  k = c(0.083333, 0.080188, 0.077875, 0.07609, 0.07465, 0.07347, 0.07248, 0.07163)
  if (d <= length(k)) {
    kd = k[d]
  } else {
    kd = min(k)
  }
  # mml
  l = -logPrior + 0.5 * logF + nll + 0.5 * d * (1 + log(kd))
  
  if (debug) {
    cat("mml=", l, "\n")
    cat("@ 1st part=", l-nll, "\n")
    cat("*** -logPrior=", -logPrior, "\n")
    cat("*** logFisher=", logF, "\n")
    cat("*** logLattice=", 0.5*d*(1+log(kd)), "\n")
    cat("@ 2nd part=nll=", nll, "\n")
  }
  
  return(l)
  
}
