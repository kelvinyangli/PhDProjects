# a function to extimate arc prior using bootstrapping 
arcPrior = function(arcs, maxProb, mbList, localStrs, mbptsList, vars, data, nResamples) {
  
  if (length(arcs$directed) > 0) {
    
    p = rep(0, nrow(arcs$directed))
    for (i in 1:nrow(arcs$directed)) {
      
      counts = featureUncertainty(arcs$directed[i, 1], arcs$directed[i, 2], mbList, localStrs, mbptsList, vars, data, nResamples)
      p[i] = min(counts[1] / sum(counts[1:2]), maxProb)
      
    }
    
  } else {
    
    p = c()
    
  } 
  
  if (length(arcs$undirected) > 0) {
    
    q = rep(0, nrow(arcs$undirected))
    for (i in 1:nrow(arcs$undirected)) {
      
      counts = featureUncertainty(arcs$undirected[i, 1], arcs$undirected[i, 2], mbList, localStrs, mbptsList, vars, data, nResamples)
      q[i] = min(sum(counts[1:3]) / sum(counts), maxProb)
      
    }
    
  } else {
    
    q = c()
    
  }
  
  return(list(directedPrior = round(p, 2), undirectedPrior = round(q, 2)))
  
}


