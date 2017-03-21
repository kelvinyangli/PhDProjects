get_arcs = function(adjmtx) {
  
  m = matrix(0, (nrow(adjmtx) ^ 2 - nrow(adjmtx)), 2, 
             dimnames = list(NULL, c("from", "to")))
  m = c()
  for (i in 1:nrow(adjmtx)) {
    
    for (j in 1:ncol(adjmtx)) {
      
      if (adjmtx[i, j] == 1) {
        
        m = rbind(m, c(rownames(adjmtx)[i], colnames(adjmtx)[j]))

      } # end if 
      
    } # end for j 
    
  } # end for i 
  
  return(m)
  
}