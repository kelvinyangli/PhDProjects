#' MML random Markov blanket models using adaptive code approach
#'
#' This function takes a list of given random Markov blanket models and calculates the mml score of the target
#' under each Markov blanket model. The output is a weighted average over all message lengths. 
#' @param data A categorical data set.
#' @param vars A vector of all variables in data, in the same order as the column names of data.
#' @param arities A vector of variable arities in data, in the same order as the column names of data.
#' @param sampleSize The sample size. That is, the number of rows of data. 
#' @param varCnt This parameter is for mml_cpt. As explained by argument name. 
#' It is obtained by getting the detailed information of the given data using the function 
#' count_occurance(). 
#' @param targetIndex The target node's index in vars, whose Markov blanket we are interested in. 
#' @param targetProbsAdpt A matrix that stores the target's probability for each of its value at each 
#' data point. The matrix has dimension arity(target) by n. 
#' @param strList A list of random (or all) Markov blanket structures to go through. Each structure is 
#' stored in a matrix format. 
#' @param mbIndices The indices of potential Markov blanket variables. 
#' @param debug A boolean argument to show the detailed Markov blanket inclusion steps based on each 
#' mml score. 
#' @return The function outputs the weighted average message length over all given structures. Noticing it is 
#' the probabilies over all models are averaged then use this averaged probability to calculate the averaged 
#' message length, not to average the message lengths directly. This is done by the function msg_len_ave() 
#' function.
#' @keywords This function has dependencies on mml_cpt(), mml_nb_adaptive(), mml_fixed_str_adaptive(),
#' msg_len_ave(), matrix2dag(), is_substr().
#' @export
mml_rand_str_adaptive = function(data, vars, arities, sampleSize, varCnt, targetIndex, targetProbsAdpt, 
                             strList, mbIndices, debug = FALSE) {
  
  l = rep(0, length(strList)) 
  for (j in 1:length(strList)) {# calculate mml(T|mb) for each structure in the given structure list
    
    str = strList[[j]]
    dimnames(str) = rep(list(vars[c(mbIndices, targetIndex)]), 2) # rename dimension names
    str = matrix2dag(str)
    pa = bnlearn::parents(str, vars[targetIndex])
    ch = bnlearn::children(str, vars[targetIndex])
    if (length(pa) == length(mbIndices)) {# if all nodes are parents
      
      res = mml_cpt(varCnt, arities, sampleSize, which(vars %in% pa), targetIndex, base = exp(1))       
      
    } else if (length(ch) == length(mbIndices)) {# if all nodes are children
      
      res = mml_nb_adaptive(data, arities, targetIndex, which(vars %in% ch))
      
    } else {# if there is a mixture of parents, children and spouses
      
      res = mml_fixed_str_adaptive(data, vars, arities, sampleSize, targetIndex, targetProbsAdpt, str)
      
    }
    
    if (debug) cat(j, res, "\n")
    l[j] = res
    
  }
  
  # the function msg_len_ave averages the probabilities then returns an averaged message length based 
  # on the averaged probability
  avgL = msg_len_ave(l) 
  return(avgL)
  
}

