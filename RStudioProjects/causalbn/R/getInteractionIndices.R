# this function returns column indices in interactData matrix for interaction terms
# e.g. if allNodes = A, B, C, D, then interactData has column names AB, AC, AD, BC, BD, CD
# if xIndices = c(1, 3), which means parents = c(A, C), then this function will return
# 2 as the result, which indicates the 2nd column in interactData
getInteractionIndices = function(interactData, allNodes, xIndices) {
  
  interactXs = rep(0, choose(length(xIndices), 2))
  
  k = 1 
  
  for (i in 1:(length(xIndices) - 1)) {
    
    for (j in (i + 1):length(xIndices)) {
      
      # since when compute the interactData, interactions are considered in the order of the column labels
      # however, when adding node into predictor sets, nodes may be added in a different order
      # hence when considering interaction terms, pair of nodes are ordered so that it align with the column names of interactData
      interactXs[k] = which(colnames(interactData) == paste0(allNodes[xIndices[c(i, j)][order(xIndices[c(i, j)])]], collapse = ""))
      
      k = k + 1
      
    } # end for j
    
  } # end for i
  
  return(interactXs)
  
}
