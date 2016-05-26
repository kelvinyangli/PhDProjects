nodeToIndex = function(data, nodes) {
  
  allNodes = names(data)
  
  indecies = rep(0, length(nodes))
  
  for (i in 1:length(nodes)) {
    
    indecies[i] = which(allNodes %in% nodes[i])
    
  }
  
  return(indecies)
  
}