#' A function to calculate the classification accuracy of a Markov blanket learner
#'
#' This is a function to calculate the classification accuracy of a Markov blanket learner. In details, 
#' the function calculates TP, TN, FP, FN, and returns precision and recall as outputs. If true mb is
#' empty but learned mb is not, then both precision and recall are 0; if learned mb is empty but true 
#' mb is not, then both precision and recall are 0; if both true and learned mbs are empty, then both
#' precision and recall are 1. 
#' @param mbTrue The true Markov blanket
#' @param mbLearned The learned Markov blanket
#' @param vars A vector of all variables. 
#' @param target The target node. 
#' @export
classification_accuracy_mb = function(mbTrue, mbLearned, vars, target) {
  if ((length(mbLearned) == 0) && (length(mbTrue) == 0)) { # if both true and learned mbs are empty
    precision = 1
    recall = 1
  } else if ((length(mbLearned) > 0) && (length(mbTrue) == 0)) {
    precision = 0
    recall = 0
  } else  if ((length(mbLearned) == 0) && (length(mbTrue) > 0)) {
    precision = 0
    recall = 0
  } else {
    nonMBTrue = vars[!vars %in% mbTrue] # remove true mb from all nodes
    nonMBTrue = nonMBTrue[nonMBTrue != target] # then remove target node 
    nonMBLearned = vars[!vars %in% mbLearned] # remove learned mb from all nodes
    nonMBLearned = nonMBLearned[nonMBLearned != target] # then remove target node 
    TP = sum(mbLearned %in% mbTrue) # the number of correct findings
    FP = length(mbLearned) - TP
    TN = sum(nonMBLearned %in% nonMBTrue) # the number of correct excluding nodes
    FN = length(nonMBLearned) - TN
    precision = TP/(TP + FP)
    recall = TP/(TP + FN)
  }
  accuracy = c(precision, recall)
  return(accuracy)
}


