# recall ratio and exclude ratio according to "Identifying Markov blanket using Lasso estimation"

# a perfect learner has sensitivity 1 and specificity 1

# the recall ratio is the number of correctly identified mb nodes divide by the size of true mb
# recall ratio is the same as the sensitivity, i.e true positive rate
sensitivity = function(cpts, node, mbLearned) {
  
  mbTrue = mb(cpts, node)
  
  P = length(mbTrue)
  
  TP = sum(mbLearned %in% mbTrue)
  
  if ((length(mbLearned) == 0) && (P == 0)) {
    
    TPR = 1
    
  } else if ((length(mbLearned) > 0) && (P == 0)) {
    
    TPR = 0
    
  } else {
    
    TPR = TP/P
    
  }
  
  return(round(TPR, 2))
  
}

# the exclude raito according to the paper is the number of identified non-mb nodes divide by the number of true non-mb nodes
# but I modified to the number of true negative divide by the number of true non-mb nodes
# because if we use what the paper defined for exclude ratio, we could get exclude ratio = 1, which could just be the number of 
# identified negatives is the same as the number of non-mb nodes, but the actual nodes are different
# exclude ratio is the same as the specificity, i.e true negative rate
specificity = function(cpts, node, mbLearned) {
  
  allNodes = bnlearn::nodes(cpts)
  
  mbTrue = mb(cpts, node)
  
  nonMBTrue = allNodes[!allNodes %in% mbTrue]
  
  nonMBTrue = nonMBTrue[nonMBTrue != node]
  
  N = length(nonMBTrue)
  
  nonMBLearned = allNodes[!allNodes %in% mbLearned]
  
  nonMBLearned = nonMBLearned[nonMBLearned != node]
  
  TN = sum(nonMBLearned %in% nonMBTrue)
  
  if ((N == 0) && (length(nonMBLearned) == 0)) {
    
    TNR = 1
    
  } else if ((N == 0) && (length(nonMBLearned) > 0)) {
    
    TNR = 0
    
  } else {
    
    TNR = TN/N
    
  }
  
  return(round(TNR, 2))
  
}

