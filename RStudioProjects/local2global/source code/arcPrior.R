# a function to extimate arc prior using bootstrapping 
arcPrior = function(arcs, maxProb, mbList, localStrs, mbptsList, vars, data, nResamples) {
  cat(nrow(arcs), "arcs \n")
  if (length(arcs) > 0) {
    
    p_directed = p_undirected = rep(0, nrow(arcs))
    for (i in 1:nrow(arcs)) {
      cat(i, "\n")
      counts = featureUncertainty(arcs[i, 1], arcs[i, 2], mbList, localStrs, mbptsList, vars, data, nResamples)
      #p[i] = round(min(counts[1] / sum(counts[1:2]), maxProb), 2)
      if (counts[3] == 0) {
        
        p_undirected[i] = p_directed[i] = 0
        
      } else if ((counts[3] != 0) && (counts[4] == 0)) {
        
        p_undirected[i] = 1
        p_directed[i] = (counts[1] / counts[3])[[1]]
        
      } else if ((counts[3] != 0) && (counts[4] != 0)) {
        
        p_undirected[i] = (counts[3] / sum(counts[3:4]))[[1]]
        p_directed[i] = ((counts[1] / counts[3]))[[1]] * p_undirected[i]
        
      } # ene else if 
      
      
    } # end for i
    
  } else {
    
    p_directed = p_undirected = c()
    
  } 
  
  #return(list(directed = p, undirected = q))
  for (i in 1:length(p_directed)) {
    
    if (p_directed[i] > maxProb) p_directed[i] = maxProb
    if (p_undirected[i] > maxProb) p_undirected[i] = maxProb
    
  }
  
  return(list(directed = round(p_directed, 2), undirected = round(p_undirected, 2)))
}


