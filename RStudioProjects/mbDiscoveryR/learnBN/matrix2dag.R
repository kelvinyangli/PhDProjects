# convert a matrix to dag 
# rows are parents and columns are children
# this is a new version of matrix to dag convertion
# when there is a contradiction with edge directions, it returns an undirected edge
matrix2dag = function(mtx) {
  
  dag = empty.graph(colnames(mtx)) 
  
  for (i in 1:nrow(mtx)) {
    
    for (j in 1:ncol(mtx)) {
      
      if (mtx[i, j] == 1) {
        
        if (mtx[j, i] == 1) {
          
          dag = set.edge(dag, rownames(mtx)[i], colnames(mtx)[j], check.cycles = FALSE)
          mtx[j, i] = 0
          
        } else {
          
          dag = set.arc(dag, rownames(mtx)[i], colnames(mtx)[j], check.cycles = FALSE)
          
        } # end else 
        
        mtx[i, j] = 0
        
      } # end if 
      
    }
    
  }
 
  return(dag)
  
}




