mbAccuracy = function(mbTrue, mbLearned, target, allNodes) {
  
  numNodes = length(allNodes)
  
  nonMBTrue = allNodes[!allNodes %in% mbTrue]
  nonMBTrue = nonMBTrue[nonMBTrue != target]
  
  nonMBLearned = allNodes[!allNodes %in% mbLearned]
  nonMBLearned = nonMBLearned[nonMBLearned != target]
  
  P = length(mbTrue) # cardinality of the true mb 
  
  N = length(nonMBTrue) # cardinality of the non-mb subset
    
  TP = sum(mbLearned %in% mbTrue) # the number of correct findings
  
  TN = sum(nonMBLearned %in% nonMBTrue) # the number of correct excluding nodes
  
  FP = sum(!mbLearned %in% mbTrue) # the number of incorrect findings
  
  FN = numNodes - (TP + TN + FP) - 1
    
  if ((length(mbLearned) == 0) && (length(mbTrue) == 0)) {
    
    precision = 1
    recall = 1
    
  } else if ((length(mbLearned) > 0) && (length(mbTrue) == 0)) {
    
    precision = 0
    recall = 0
    
  } else  if ((length(mbLearned) == 0) && (length(mbTrue) > 0)) {
    
    precision = 0
    recall = 0
    
  } else {
    
    precision = TP/(TP + FP)
    recall = TP/(TP + FN)
    
  }
  
  accuracy = c(TP, TN, FP, FN, precision, recall)
  names(accuracy) = c("tp", "tn", "fp", "fn", "precision", "recall")
  
  return(accuracy)

}


