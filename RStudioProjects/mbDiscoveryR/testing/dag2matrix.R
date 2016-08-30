# dag2matrix is a function that converts a dag to a matrix containing 0s and 1s, where 1s represents connected nodes
# an arc V1 -> V2 will have a 1 at matrix["V1", "V2"]
# column and row names are in the order of bnlearn::nodes(dag)

dag2matrix = function(dag) {
  
  allNodes = bnlearn::nodes(dag)
  nNodes = length(allNodes)
  
  mtx = matrix(0, nrow = nNodes, ncol = nNodes)
  
  for (i in 1:nNodes) {
    
    children = dag$nodes[[i]]$children # set of children of node i
    
    if (length(children) > 0) {
      
      for (j in 1:length(children)) {
        
        mtx[i, which(allNodes == children[j])] = 1
        
      } # end for j
      
    } # end if 
    
  } # end for i
  
  return(mtx)
  
}