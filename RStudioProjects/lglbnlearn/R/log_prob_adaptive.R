#' An auxiliary function to mml_fixed_str()
#'
#' This function calculates log of the conditional probability for a variable given its parents. It is used 
#' to obtain p(x|pa of x) in the numerator of the conditional prob p(T|Xs).
#' @param data A categorical data set.
#' @param sampleSize The sample size. That is, the number of rows of data. 
#' @param targetIndex The target node's index in vars, whose Markov blanket we are interested in. 
#' @param condProbsAdpt A conditional probabilities matrix that stores a variable's conditional probability 
#' at each data point. This variable is either the target, or has more than one parent. 
#' @return The function outputs a single log probability value.  
#' @export
log_prob_adaptive = function(data, sampleSize, targetIndex, condProbsAdpt) {
  
  lp = 0  
  for (i in 1:sampleSize) {
    
    targetValue = as.numeric(data[i, targetIndex])
    lp = lp + log(condProbsAdpt[targetValue, i])
    
  }
  
  return(lp)
  
}