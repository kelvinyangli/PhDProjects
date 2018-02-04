#' A forward greedy search function
#'
#' This is a forward greedy seearch function that can be used incorprate with an objective function mml
#' cpt, logit, or naive bayes (adaptive code) to discovery Markov blanket candidates. It is a greedy search, so the
#' function will stop if there is no better option to add into the current Markov blanket.
#' @param data A dataset whost variables are in numeric/integer format. Any categorical variables must be
#' converted into numeric/integer first.
#' @param arities A vector of variable arities in data, in the same order as the column names of data.
#' @param vars A vector of all variables in data, in the same order as the column names of data.
#' @param sampleSize The sample size. That is, the number of rows of data.
#' @param target The target node, whose Markov blanket we are interested in.
#' @param model The options are cpt, logit (binary only), naive bayes and random models.
#' @param sigma The standard derivation of the assumed Gaussian distribution for parameter prior. The
#' default value is 3 as suggested by the original paper.
#' @param dataNumeric This parameter is for mml_logit. The numeric format of the given data set.
#' Variable values start from 0.
#' @param varCnt This parameter is for mml_cpt. As explained by argument name.
#' It is obtained by getting the detailed information of the given data using the function
#' count_occurance().
#' @param prior A character parameter with options "uniform", "tom" and "bayes" indicate the uniform
#' prior (default), TOM (totally ordered model) and Bayesian prior when averaging the message lengths
#' for random structures. The Bayesian prior starts with the uniform prior then calculates the
#' posteriors and use them as priors for the next step.
#' @param debug A boolean argument to show the detailed Markov blanket inclusion steps based on each
#' mml score.
#' @return The function returns the learned Markov blanket candidates according to the assigned objective
#' function.
#' @keywords This function has dependencies on resample(), mml_cpt(), mml_logit(), mml_nb_adaptive(),
#' mml_rand_str_adaptive(), probs_adaptive(), cond_probs_adaptive(), log_prob_adaptive().
#' @export
forward_greedy = function(data, arities, vars, sampleSize, target, model, sigma = 3,
                          dataNumeric = NULL, varCnt = NULL, prior = "uniform", debug = FALSE) {

  targetIndex = which(vars == target) # get index of the target node
  nvars = length(vars)
  mb = c()
  unCheckedIndices = (1:nvars)[-targetIndex]


  if (model == "random") {

    cachedPXGivenY = list() # empty list to cach condProbsAdpt calculated by mml_fixed_str_adaptive()
    cachInd = 1 # starting cachedPXGivenY index from 1
    nSECs = c(1, 4, 14, 64, 332, 1924, 12294) # give the number of SECs for each mb size
    nIgnored = c(0, 1, 3, 16, 85, 506, 3299)
    ignored = readRDS("~/Documents/PhDProjects/RStudioProjects/local2global/ignored_strs.rds")

  }


  # initializing with empty model
  if (model == "cpt") {

    minMsgLen = mml_cpt(varCnt, arities, sampleSize, c(), targetIndex)

  } else if (model == "logit") {

    minMsgLen = mml_logit(data, arities, sampleSize, c(), target, sigma = sigma)

  } else {# if nb or random, do the following:

    # 1. initializing a probs matrix for furture use
    # 2. caching p(x|T) for each x in the list cachedPXGivenT to avoid redundent computations
    # 3. calculating log(p(T)) for the empty model
    probsMtx = matrix(0.5, arities[targetIndex], sampleSize)
    cachedPXGivenT = list() # empty list to cach condProbsAdpt calculated by mml_fixed_str_adaptive()

    for (i in 1:nvars) {

      if (i == targetIndex) {

        cachedPXGivenT[[i]] = probs_adaptive(data, arities, sampleSize, probsMtx, targetIndex)

      } else {

        cachedPXGivenT[[i]] = cond_probs_adaptive(data, arities, sampleSize, targetIndex, probsMtx, i, targetIndex)

      }

    }

    logProbTarget = sum(log(t(cachedPXGivenT[[targetIndex]])[cbind(seq_along(data[, targetIndex]), data[, targetIndex])]))
    minMsgLen = -logProbTarget

  }

  if (debug) {
    cat("Search: Forward greedy with", model, " model \n")
    cat("0 parent:", minMsgLen, "\n")
  }

  # repeat the process of computing mml for remaining unCheckedIndices
  # if unCheckedIndices is empty or all msg len > min msg len then stop
  repeat {

    index = 0 # initialize index to 0

    if (length(unCheckedIndices) == 0) {

      if (debug) cat("MB is full! \n")
      break

    }

    if (length(mb) == 7) {

      if (debug) cat("Stop searching due to lack of pre-saved mbpts!")
      break

    }

    if (model == "random") {

      # random sampling over SEC space if there are more than 10 SECs
      nsamples = 100
      if ((nSECs[length(mb) + 1] - nIgnored[length(mb) + 1]) > nsamples) {

        sampledSECIndices = sample((1:nSECs[length(mb) + 1])[-ignored[[length(mb) + 1]]], nsamples)
        sampledSECs = readRDS(paste0("~/Documents/PhDProjects/RStudioProjects/local2global/MBPT_SECsInd/",
                                     length(mb) + 1, ".rds"))[sampledSECIndices]

      } else {# else use all SECs

        sampledSECs = readRDS(paste0("~/Documents/PhDProjects/RStudioProjects/local2global/MBPT_SECsInd/",
                                     length(mb) + 1, ".rds"))
        if (nIgnored[length(mb) + 1] > 0) {

          sampledSECs = sampledSECs[-ignored[[length(mb) + 1]]]

        }

      }

      # sampling 1 structure from each sampled SEC
      # since strs belong to the same SEC have similar scores
      sampledStrIndices = sapply(sampledSECs, resample, size = 1)
      strList = readRDS(paste0("~/Documents/PhDProjects/RStudioProjects/local2global/MBPTs_ordered/",
                               length(mb) + 1, ".rds"))[sampledStrIndices]

      if (prior == "uniform") {

        weights = rep(1 / length(strList), length(strList))

      }

    } # end if "random"

    # calculate mml of target given each unchecked node as input
    for (i in 1:length(unCheckedIndices)) {

      mbIndices = c(mb, unCheckedIndices[i])
      if (model == "cpt") {# cpt

        msgLenCurrent = mml_cpt(varCnt, arities, sampleSize, mbIndices, targetIndex)

      } else if (model == "logit") {#logit

        msgLenCurrent = mml_logit(data, arities, sampleSize, vars[mbIndices], target, sigma)

      } else if (model == "nb") {#nb

        msgLenCurrent = mml_nb_adaptive(data, arities, sampleSize, targetIndex, logProbTarget, cachedPXGivenT, mbIndices)

      } else if (model == "random") {#random

        res = mml_rand_str_adaptive(data, vars, arities, sampleSize, varCnt, targetIndex, logProbTarget,
                                    cachedPXGivenT, probsMtx, strList, mbIndices, weights, cachedPXGivenY, cachInd, debug)
        msgLenCurrent = res$avgL
        cachedPXGivenY = res$cachedPXGivenY
        cachInd = res$cachInd

      }# end else if

      if (debug) cat("parents =", vars[c(mb, unCheckedIndices[i])], ":", msgLenCurrent, "\n")

      # if the current msg len is smaller then replace minMsgLen by the current
      # and record the current index
      # else go to the next available node
      if (msgLenCurrent < minMsgLen) {

        minMsgLen = msgLenCurrent
        index = i

      }

    } # end for each unchecked node

    if (index == 0) {# if no better choice then break

      if (debug) cat("Stop! No better choice for MB! \n")
      break

    } else {

      if (debug) cat("add", vars[unCheckedIndices[index]], "into mb \n")
      # add the node index with the minimum msg len into mb and remove it from unCheckedIndices
      mb = c(mb, unCheckedIndices[index])
      if (debug) {

        cat("current mb is {", vars[mb], "} with msg len", minMsgLen, "\n")
        cat("------------------------------- \n")

      }

      unCheckedIndices = unCheckedIndices[-index]

    }

  } # end repeat

  return(vars[mb])

}


