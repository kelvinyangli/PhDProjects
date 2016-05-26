identifyCollider = function(x, data, delta = 20) {
  
  # delta is a threshold for deciding wheather an increasing in conditional mutual information
  # should be count as critical
  
  allNodes = names(data) 
  nodesLeft = allNodes[allNodes != x]
  
  adjacentNodeIndex = 0 
  maxMI = 0
  
  for (i in 1:length(nodesLeft)) {
    
    mi = ci.test(x, nodesLeft[i], data = data)$statistic
    
    if (mi > maxMI) {
      
      maxMI = mi
      index = i
      adjacentNode = nodesLeft[index]
      
    } # end if
    
  } # end for i
  
  # check if there is a third node that forms a collider with x and the adjancent node 
  
  nodesLeft = nodesLeft[-index]
  maxRatio = 1
  
  for (j in 1:length(nodesLeft)) {
    
    a = ci.test(adjacentNode, nodesLeft[j], x, data = data)$statistic
    b = ci.test(adjacentNode, nodesLeft[j], data = data)$statistic
    
    ratio = a/b
    
    if (ratio > maxRatio) {
      
      maxRatio = ratio
      index = j
      y = nodesLeft[index]
      maxCMI = a
      
    } # end if 
    
  } # end for j
  
}



