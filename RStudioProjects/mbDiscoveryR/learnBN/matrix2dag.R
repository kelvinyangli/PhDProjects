# convert a matrix to dag 
# rows are parents and columns are children
# 

#matrix2dag = function(matrix) {
  
  #dagBN = empty.graph(colnames(matrix))
  
  #for (i in 1:nrow(matrix)) {
    
    # adjacent nodes to allnodes[i]
   # adjacentNodes = colnames(matrix)[matrix[i,] == 1]
    
    #if (length(adjacentNodes) > 0) {
      
     # for (j in 1:length(adjacentNodes)) dagBN = set.arc(dagBN, rownames(matrix)[i], adjacentNodes[j])
      
    #} # end if 
    
  #} # end for i
  
  #return(dagBN)
  
#}

# from col to row
matrix2dag.reverse = function(matrix) {
  
  dagBN = empty.graph(colnames(matrix))
  
  for (i in 1:ncol(matrix)) {
    
    # adjacent nodes to allnodes[i]
    adjacentNodes = rownames(matrix)[matrix[, i] == 1]
    
    if (length(adjacentNodes) > 0) {
      
      for (j in 1:length(adjacentNodes)) dagBN = set.arc(dagBN, colnames(matrix)[i], adjacentNodes[j])
      
    } # end if 
    
  } # end for i
  
  return(dagBN)
  
}

# this is a new version of matrix to dag convertion
# when there is a contradiction with edge directions, it returns an undirected edge
matrix2dag = function(mtx) {
  
  dag = empty.graph(colnames(mtx)) 
  
  for (i in 1:nrow(mtx)) {
    
    for (j in 1:ncol(mtx)) {
      
      if (mtx[i, j] == 1) {
        
        if (mtx[j, i] == 1) {
          
          dag = set.edge(dag, rownames(mtx)[i], colnames(mtx)[j])
          mtx[j, i] = 0
          
        } else {
          
          dag = set.arc(dag, rownames(mtx)[i], colnames(mtx)[j])
          
        } # end else 
        
        mtx[i, j] = 0
        
      } # end if 
      
    }
    
  }
 
  return(dag)
  
}




