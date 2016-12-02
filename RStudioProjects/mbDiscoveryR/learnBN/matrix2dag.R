# convert a matrix to dag 
# rows are parents and columns are children
# 

matrix2dag = function(matrix) {
  
  dagBN = empty.graph(colnames(matrix))
  
  for (i in 1:nrow(matrix)) {
    
    # adjacent nodes to allnodes[i]
    adjacentNodes = colnames(matrix)[matrix[i,] == 1]
    
    if (length(adjacentNodes) > 0) {
      
      for (j in 1:length(adjacentNodes)) dagBN = set.arc(dagBN, rownames(matrix)[i], adjacentNodes[j])
      
    } # end if 
    
  } # end for i
  
  return(dagBN)
  
}

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