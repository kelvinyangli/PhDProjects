count2Prior = function(count, maxProb) {
  
  if (length(count) > 1) {# when there are arcs 
    
    p_directed = p_undirected = rep(0, nrow(count))
    for (i in 1:nrow(count)) {
   
      if (sum(count[i, 1:2]) == 0) {
        
        p_undirected[i] = p_directed[i] = 0
        
      } else if ((sum(count[i, 1:2]) != 0) && (count[i, 3] == 0)) {
        
        p_undirected[i] = 1
        p_directed[i] = (count[i, 1] / sum(count[i, 1:2]))
        
      } else if ((sum(count[i, 1:2]) != 0) && (count[i, 3] != 0)) {
        
        p_undirected[i] = (sum(count[i, 1:2]) / sum(count[i, ]))
        p_directed[i] = ((count[i, 1] / sum(count[i, 1:2]))) * p_undirected[i]
        
      } # ene else if 
      
    } # end for i
    
  } else {
    
    p_directed = p_undirected = 0
    
  } 
  
  #return(list(directed = p, undirected = q))
  for (i in 1:length(p_directed)) {
    
    if (p_directed[i] > maxProb) p_directed[i] = maxProb
    if (p_undirected[i] > maxProb) p_undirected[i] = maxProb
    
  }
  
  return(list(directed = p_directed, undirected = p_undirected))
  
}