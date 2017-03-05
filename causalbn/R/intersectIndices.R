intersectIndices = function(numParents, parentsIndecies, indexListPerNodePerValue, potentialCombination) {
  
  commonIndecies = indexListPerNodePerValue[[parentsIndecies[1]]][[potentialCombination[1]]]
  
  for (i in 2:numParents) {
    
    commonIndecies = intersect(commonIndecies, indexListPerNodePerValue[[parentsIndecies[i]]][[potentialCombination[i]]])
    
  }
  
  return(commonIndecies)
  
}

