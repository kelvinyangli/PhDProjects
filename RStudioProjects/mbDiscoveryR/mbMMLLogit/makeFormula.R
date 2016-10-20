# this function creates a formula based on Y and Xs for the use of the glm function
makeFormula = function(allNodes, yIndex, xIndices) {
  
  if (length(xIndices) > 1) { # if there are more than 1 predictors
    
    # re-order xIndices to make it consistent with the colnames(interactData)
    #tempIndices = xIndices[order(xIndices)]
    
    pairs = vector(length = sum(1:(length(xIndices) - 1)))
    
    k = 1
    
    for (i in 1:(length(xIndices) - 1)) {
      
      for (j in (i + 1):length(xIndices)) {
        
        pairs[k] = paste0(allNodes[xIndices[i]], "*", allNodes[xIndices[j]])  
        
        k = k + 1
        
      } # end for j
      
    } # end for i
    
    formula = paste(allNodes[yIndex], "~", paste0(c(allNodes[xIndices], pairs), collapse = "+"))
    
  } else {
    
    formula = paste(allNodes[yIndex], "~", allNodes[xIndices])
    
  }
  
  
  return(formula)
  
}
