#' MML random Markov blanket models using adaptive code approach
#'
#' This function takes a list of given random Markov blanket models and calculates the mml score of the target
#' under each Markov blanket model. The output is a weighted average over all message lengths.
#' @param data A dataset whost variables are in numeric/integer format. Any categorical variables must be
#' converted into numeric/integer first.
#' @param vars A vector of all variables in data, in the same order as the column names of data.
#' @param arities A vector of variable arities in data, in the same order as the column names of data.
#' @param sampleSize The sample size. That is, the number of rows of data.
#' @param varCnt This parameter is for mml_cpt. As explained by argument name.
#' It is obtained by getting the detailed information of the given data using the function
#' count_occurance().
#' @param targetIndex The target node's index in vars, whose Markov blanket we are interested in.
#' @param logProbTarget Log of the probability of the target.
#' @param cachedPXGivenT cachedPXGivenT
#' @param probsMtx probsMtx
#' @param strList A list of random (or all) Markov blanket models (structures) to go through. Each structure is
#' stored in a matrix format.
#' @param mbIndices The indices of potential Markov blanket variables.
#' @param weights A vector of weights for random Markov blanket models.
#' @param cachedPXGivenY cachedPXGivenY
#' @param cachInd cachInd
#' @param debug A boolean argument to show the detailed Markov blanket inclusion steps based on each
#' mml score.
#' @return The function outputs the weighted average message length over all given structures. Noticing it is
#' the probabilies over all models are averaged then use this averaged probability to calculate the averaged
#' message length, not to average the message lengths directly. This is done by the function msg_len_ave()
#' function.
#' @keywords This function has dependencies on mml_cpt(), mml_nb_adaptive_slow(), mml_fixed_str_adaptive(),
#' msg_len_weighted_avg(), matrix2dag().
#' @export
mml_rand_str_adaptive = function(data, vars, arities, sampleSize, varCnt, targetIndex, logProbTarget,
                                 cachedPXGivenT, probsMtx, strList, mbIndices, weights, cachedPXGivenY, cachInd,
                                 debug = FALSE) {

  l = rep(0, length(strList))
  for (j in 1:length(strList)) {# calculate mml(T|mb) for each structure in the given structure list

    str = strList[[j]]
    dimnames(str) = rep(list(vars[c(mbIndices, targetIndex)]), 2) # rename dimension names

    if (sum(str[, vars[targetIndex]]) == length(mbIndices)) {# if all nodes are parents

      res = mml_cpt(varCnt, arities, sampleSize, mbIndices, targetIndex)

    } else if (sum(str[vars[targetIndex], ]) == length(mbIndices)) {# if all nodes are children

      res = mml_nb_adaptive(data, arities, sampleSize, targetIndex, logProbTarget, cachedPXGivenT, mbIndices)

    } else {# if there is a mixture of parents, children and spouses

      lst = mml_fixed_str_adaptive(data, vars, arities, sampleSize, targetIndex, logProbTarget,
                                   cachedPXGivenT, probsMtx, str, mbIndices, cachedPXGivenY, cachInd)
      res = lst$llh
      cachedPXGivenY = lst$cachedPXGivenY
      cachInd = lst$cachInd

    }

    if (debug) cat(j, res, "\n")
    l[j] = res

  }

  # the function msg_len_weighted_avg averages the probabilities then returns an averaged message length based
  # on the averaged probability
  avgL = msg_len_weighted_avg(l, weights)
  lst = list(avgL = avgL, cachedPXGivenY = cachedPXGivenY, cachInd = cachInd)

  return(lst)

}
