# a function to extract directed and undirected arcs from the merged global matrix
extractArcs = function(mtx) {
  
  vars = colnames(mtx)
  arc_directed = arc_undirected = c()
  for (i in 1:(nrow(mtx) - 1)) {
    
    for (j in (i + 1):ncol(mtx)) {
      
      if ((mtx[i, j] + mtx[j, i]) > 1) {
        
        if (mtx[i, j] >= mtx[j, i]) {
          
          arc_directed = rbind(arc_directed, c(vars[i], vars[j]))
          
        } else {
          
          arc_directed = rbind(arc_directed, c(vars[j], vars[i]))
          
        } # end else 
        
      } else if ((mtx[i, j] + mtx[j, i]) == 1) {
        
        arc_undirected = rbind(arc_undirected, c(vars[i], vars[j]))
        
      } # end else if 
      
    } # end for j
    
  } # end for i 
  
  return(list(directed = arc_directed, undirected = arc_undirected))
  
}




