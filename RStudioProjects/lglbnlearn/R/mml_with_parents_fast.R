#' Auxiliary function to mml_cpt_fast
#'
#' This is an auxiliary function to mml_cpt_fast. It calculates the mml score of a node given its parents. 
#' @param indexListPerNodePerValue A list of stored indices for each value of each node. It is obtained 
#' by the function count_occurance().
#' @param cachedIndicesList A vector of indices stored to speed up calculations.
#' @param arities A vector of varaible arities.
#' @param sampleSize Sample size of a given data set. 
#' @param parentsIndices Indices of parents nodes. 
#' @param targetIndex Index of the target node.  
#' @param logFactorialSheet A pre-saved log factorial sheet for non-negative integers <= 10000.
#' @param base The base of the logarithm.  
#' @export
mml_with_parents_fast = function(indexListPerNodePerValue, cachedIndicesList, arities, sampleSize, 
                                 parentsIndices, targetIndex, logFactorialSheet, base) {
  
  newAddedParentIndex = parentsIndices[length(parentsIndices)]
  arityChild = arities[targetIndex]
  numParents = length(parentsIndices)
  numParentsInstantiations = prod(arities[parentsIndices])

  fixedTerm = 0.5 * (numParentsInstantiations * (arityChild - 1)) * log((pi * exp(1) / 6), base)
  nonFixedTerm = 0
  # log((|x| - 1)!)
  logConstant = log(factorial(arityChild - 1), base)
  # store curent cachedIndices in a temp list
  tempList = cachedIndicesList
  
  if (numParents == 1) { # if single parent then just use index i
    
    for (i in 1:arities[newAddedParentIndex]) {
      cachedIndicesList[[i]] = indexListPerNodePerValue[[parentsIndices]][[i]]
      N_pa_i = length(cachedIndicesList[[i]])
      # sum_i^arityChild log(N(pa_i, x_i))!
      cumSum = single_par_cal(indexListPerNodePerValue, cachedIndicesList[[i]], arityChild, targetIndex, 
                              logFactorialSheet, base)
      # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
      logNumerator = log_factorial(logFactorialSheet, N_pa_i + arityChild - 1, base)
      nonFixedTerm = nonFixedTerm + logNumerator - logConstant - cumSum
    } 
    
  } else { # if multiple parents
    
    j = 1
    for (i in 1:length(cachedIndicesList)) {
      for (ii in 1:arities[newAddedParentIndex]) {
        cachedIndicesList[[j]] = intersect(tempList[[i]], indexListPerNodePerValue[[newAddedParentIndex]][[ii]])
        N_pa_i = length(cachedIndicesList[[j]])
        cumSum = multi_pars_cal(indexListPerNodePerValue, cachedIndicesList[[j]], arityChild, targetIndex, 
                                logFactorialSheet, base)
        # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
        logNumerator = log_factorial(logFactorialSheet, N_pa_i + arityChild - 1, base)
        nonFixedTerm = nonFixedTerm + logNumerator - logConstant - cumSum
        j = j + 1
      } # end for ii
    } # end for i
    
  } # end else 
  
  msgLen = fixedTerm + nonFixedTerm
  ls = list(msgLen = msgLen, cachedIndicesList = cachedIndicesList)
  return(ls)
  
}
