# a function to extimate arc prior using bootstrapping 
arcPrior = function(arcs, maxProb, mbList, localStrs, mbptsList, vars, data, nResamples) {
  
  if (length(arcs$directed) > 0) {
    
    p = rep(0, nrow(arcs$directed))
    for (i in 1:nrow(arcs$directed)) {
      
      counts = featureUncertainty(arcs$directed[i, 1], arcs$directed[i, 2], mbList, localStrs, mbptsList, vars, data, nResamples)
      p[i] = round(min(counts[1] / sum(counts[1:2]), maxProb), 2)
      
    }
    
  } else {
    
    p = c()
    
  } 
  
  if (length(arcs$undirected) > 0) {
    
    q = rep(0, nrow(arcs$undirected))
    for (i in 1:nrow(arcs$undirected)) {
      
      counts = featureUncertainty(arcs$undirected[i, 1], arcs$undirected[i, 2], mbList, localStrs, mbptsList, vars, data, nResamples)
      q[i] = round(min(sum(counts[1:3]) / sum(counts), maxProb), 2)
      
    }
    
  } else {
    
    q = c()
    
  }
  
  return(list(directed = p, 2, undirected = q, 2))
  
}


