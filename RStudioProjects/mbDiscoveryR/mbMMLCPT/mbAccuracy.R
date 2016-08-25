# this function computes the accuracies and precision, recall over the true mb
# if true mb is empty but learned mb is not, then both precision and recall are 0
# if learned mb is empty but true mb is not, then both precision and recall are 0

mbAccuracy = function(mbTrue, mbLearned, targetNode, allNodes) {
    
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
    
    #numNodes = length(allNodes)
    
    nonMBTrue = allNodes[!allNodes %in% mbTrue] # remove true mb from all nodes
    nonMBTrue = nonMBTrue[nonMBTrue != targetNode] # then remove target node 
    
    nonMBLearned = allNodes[!allNodes %in% mbLearned] # remove learned mb from all nodes
    nonMBLearned = nonMBLearned[nonMBLearned != targetNode] # then remove target node 
    
    #P = length(mbTrue) # cardinality of the true mb 
    
    #N = length(nonMBTrue) # cardinality of the non-mb subset
    
    TP = sum(mbLearned %in% mbTrue) # the number of correct findings
    
    FP = length(mbLearned) - TP
    
    TN = sum(nonMBLearned %in% nonMBTrue) # the number of correct excluding nodes
    
    FN = length(nonMBLearned) - TN
    
    #FP = sum(!mbLearned %in% mbTrue) # the number of incorrect findings
    
    #FN = numNodes - (TP + TN + FP) - 1
    #
    precision = TP/(TP + FP)
    recall = TP/(TP + FN)
    
  }
  
  accuracy = data.frame(precision, recall)
  #names(accuracy) = c("tp", "tn", "fp", "fn", "precision", "recall")
  
  return(accuracy)

}


