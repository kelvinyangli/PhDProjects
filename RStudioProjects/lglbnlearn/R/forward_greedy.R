#' A forward greedy search function
#'
#' This is a forward greedy seearch function that can be used incorprate with an objective function mml
#' cpt, logit, or naive bayes (adaptive code) to discovery Markov blanket candidates. It is a greedy search, so the 
#' function will stop if there is no better option to add into the current Markov blanket. 
#' @param data A categorical data set.
#' @param arities A vector of variable arities in data, in the same order as the column names of data.
#' @param vars A vector of all variables in data, in the same order as the column names of data.
#' @param sampleSize The sample size. That is, the number of rows of data. 
#' @param target The target node, whose Markov blanket we are interested in. 
#' @param score The objective function to pass to. Curernt choices are mml_cpt, mml_logit and mml_nb.  
#' @param base The base of logarithm. The default is the natural log. 
#' @param sigma The standard derivation of the assumed Gaussian distribution for parameter prior. The 
#' default value is 3 as suggested by the original paper. 
#' @param dataNumeric This parameter is for mml_logit. The numeric format of the given data set. 
#' Variable values start from 0. 
#' @param indexListPerNodePerValue This parameter is for mml_cpt. As explained by argument name. 
#' It is obtained by getting the detailed information of the given data using the function 
#' count_occurance(). 
#' @param probSign This parameter is for mml_nb. A data frame with 1 and -1, which corresponds to the 
#' 1st and 2nd level of a varaible. 
#' @param debug A boolean argument to show the detailed Markov blanket inclusion steps based on each 
#' mml score. 
#' @return The function returns the learned Markov blanket candidates according to the assigned objective 
#' function. 
#' @export
forward_greedy = function(data, arities, vars, sampleSize, target, score, base = exp(1), sigma = 3, 
                          dataNumeric = NULL, indexListPerNodePerValue = NULL, probSign = NULL, 
                          debug = FALSE) {
  
  options = c("mmlCPT", "mmlLogit", "mmlNB")
  if (!is.null(indexListPerNodePerValue)) {
    scoreIndex = 1
  } else if (!is.null(dataNumeric)) {
    scoreIndex = 2
  } else {
    scoreIndex = 3
  }
  
  targetIndex = which(vars == target) # get index of the target node
  nvars = length(vars)
  mb = c()
  unCheckedIndices = (1:nvars)[-targetIndex]
  
  # initialize minMsgLen as mml when target has no parents
  
  if (scoreIndex == 1) {#cpt
    minMsgLen = score(indexListPerNodePerValue, arities, sampleSize, c(), targetIndex, base = base)
  } else if (scoreIndex == 2) {#logit
    minMsgLen = score(data, arities, sampleSize, c(), target, sigma = sigma)
  } else if (scoreIndex == 3) {#nb
    #minMsgLen = score(data, probSign, vars, arities, sampleSize, c(), target)
    minMsgLen = score(data, arities, targetIndex, c())
  }
  
  if (debug) {
    cat("Search: Forward greedy with", options[scoreIndex], "\n")
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
    
    # calculate mml of target given each unchecked node as input 
    for (i in 1:length(unCheckedIndices)) {
      inputIndices = c(mb, unCheckedIndices[i])
      
      # msg len with at least 1 parent
      if (scoreIndex == 1) {# cpt
        msgLenCurrent = 
          score(indexListPerNodePerValue, arities, sampleSize, inputIndices, targetIndex, base = base)
      } else if (scoreIndex == 2) {#logit
        msgLenCurrent = score(data, arities, sampleSize, vars[inputIndices], target, sigma = sigma)
      } else if (scoreIndex == 3) {#nb
        #msgLenCurrent = score(data, probSign, vars, arities, sampleSize, vars[inputIndices], target) 
        msgLenCurrent = score(data, arities, targetIndex, inputIndices)
      }
      
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