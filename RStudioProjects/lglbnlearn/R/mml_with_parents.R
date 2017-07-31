#' Auxiliary function to mml_cpt()
#'
#' This function calculates the mml score of a target node given its parents. There has to be at least one 
#' parent for the target. 
#' @param indexListPerNodePerValue 
#' @param arities arities
#' @param sampleSize sampleSize
#' @param parentsIndices parentsIndices
#' @param targetIndex targetIndex
#' @param logFactorialSheet A pre-saved log factorial sheet for non-negative integers <= 10000.
#' @param base base
#' @export
mml_with_parents = function(indexListPerNodePerValue, arities, sampleSize, parentsIndices, targetIndex, 
                            logFactorialSheet, base) {
  
  arityChild = arities[targetIndex]
  
  numParents = length(parentsIndices)
  
  numParentsInstantiations = prod(arities[parentsIndices])
  
  fixedTerm = 0.5 * (numParentsInstantiations * (arityChild - 1)) * log((pi * exp(1) / 6), base = base)
  
  nonFixedTerm = 0
  
  # log((|x| - 1)!)
  logConstant = log(factorial(arityChild - 1), base = base)
  
  for (i in 1:numParentsInstantiations) {
    
    if (numParents == 1) { # if single parent then just use index i
      
      commonParentsIndices = indexListPerNodePerValue[[parentsIndices]][[i]]
      
      N_pa_i = length(commonParentsIndices)
      
      # sum_i^arityChild log(N(pa_i, x_i))!
      cumSum = single_par_cal(indexListPerNodePerValue, commonParentsIndices, arityChild, targetIndex, 
                              logFactorialSheet, base)
      
    } else { # if more than 1 parent, use function to get potential combination
      
      # fix this part
      potentialCombination = get_parents_instantiation_indices(arities, numParents, parentsIndices, 
                                                               numParentsInstantiations, i)
      
      commonParentsIndices = intersect_indices(numParents, parentsIndices, indexListPerNodePerValue, potentialCombination)
      #ll[[i]]=commonParentsIndices
      N_pa_i = length(commonParentsIndices)
      
      cumSum = multi_pars_cal(indexListPerNodePerValue, commonParentsIndices, arityChild, targetIndex, 
                              logFactorialSheet, base)
      
    } # end if else 
    
    # log(numerator), where numerator = (N(Pa_i) + |x| - 1)!
    logNumerator = log_factorial(logFactorialSheet, N_pa_i + arityChild - 1, base)
    #cat(logNumerator - logConstant - cumSum, "\n")
    nonFixedTerm = nonFixedTerm + logNumerator - logConstant - cumSum
    
    #cat(logNumerator - logConstant - cumSum, "\n")
  } # end for i
  
  return(fixedTerm + nonFixedTerm)
  
}
