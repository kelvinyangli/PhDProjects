mbEditDist = function(mbTrue, mbLearned, targetNode, allNodes) {
  
  if ((length(mbLearned) == 0) && (length(mbTrue) == 0)) { # if both true and learned mbs are empty
    
    d = 0

  } else if ((length(mbLearned) > 0) && (length(mbTrue) == 0)) {
    
    d = length(mbLearned)
    
  } else  if ((length(mbLearned) == 0) && (length(mbTrue) > 0)) {
    
    d = length(mbTrue)
    
  } else {
    
    nonMBTrue = allNodes[!allNodes %in% mbTrue] # remove true mb from all nodes
    nonMBTrue = nonMBTrue[nonMBTrue != targetNode] # then remove target node 
    
    nonMBLearned = allNodes[!allNodes %in% mbLearned] # remove learned mb from all nodes
    nonMBLearned = nonMBLearned[nonMBLearned != targetNode] # then remove target node 
    
    TP = sum(mbLearned %in% mbTrue) # the number of correct findings
    
    FP = length(mbLearned) - TP
    
    TN = sum(nonMBLearned %in% nonMBTrue) # the number of correct excluding nodes
    
    FN = length(nonMBLearned) - TN
    
    d = FP + FN
    
  }
  
  return(d)
  
}


