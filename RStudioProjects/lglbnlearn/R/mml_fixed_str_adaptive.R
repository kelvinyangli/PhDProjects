#' MML for a fixed Markov blanket model using adaptive code approach
#'
#' This function calculates the mml score of the target under a fixed Markov blanket model. The structure 
#' can be a general DAG. This returns the 2nd part of MML that is the likelihood of data given the 
#' structure, since the MML cost of the structure is not stated. 
#' @param data A categorical data set.
#' @param vars A vector of all variables in data, in the same order as the column names of data.
#' @param arities A vector of variable arities in data, in the same order as the column names of data.
#' @param sampleSize The sample size. That is, the number of rows of data. 
#' @param targetIndex The target node's index in vars, whose Markov blanket we are interested in. 
#' @param targetProbsAdpt A matrix that stores the target's probability for each of its value at each 
#' data point. The matrix has dimension arity(target) by sampleSize. 
#' @param str A fixed Markov blanket model stores in the bnlearn dag format. 
#' @return The function outputs the message length of a fixed structure. 
#' @keywords This function has dependencies on cond_probs_adaptive(), log_prob_adaptive().  
#' @export
mml_fixed_str_adaptive = function(data, vars, arities, sampleSize, targetIndex, targetProbsAdpt, str) {
  
  lp = 0 
  # a matrix to store the normalizting constant in p(T|Xs)
  margProbs = matrix(1, arities[targetIndex], sampleSize)
  tempVars = bnlearn::nodes(str)
  for (x in tempVars) {# go through each node in a given str
    
    # if it is the target or it has at least one parent,  
    # then get the adaptive count of it given its parent set
    curIndex = which(vars == x)
    curPa = bnlearn::parents(str, x)
    curPaIndices = which(vars %in% curPa)
    
    if (length(curPaIndices) > 0) {
      
      condProbsAdpt = cond_probs_adaptive(data, arities, sampleSize, targetIndex, curIndex, curPaIndices)
      lp = lp + log_prob_adaptive(data, sampleSize, targetIndex, condProbsAdpt)
      margProbs = margProbs * condProbsAdpt
      
    } else if (curIndex == targetIndex) {
      
      lp = lp + log_prob_adaptive(data, sampleSize, targetIndex, targetProbsAdpt)
      margProbs = margProbs * targetProbsAdpt
      
    }
    
  }
  
  # return log(p(T|Xs))
  return(-(lp - sum(log(apply(margProbs, 2, sum)))))
  
}

