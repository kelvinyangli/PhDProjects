#' A function to convert a dag from the bnlearn format to an adjacency matrix
#'
#' This converts a dag stored in the bnlearn format to an adjacency matrix contains 0s and 1s. A directed arc 
#' V1 -> V2 has a corresponding value 1 at the matrix entry [v1, v2]. Matrix column and row names are in the order
#' of bnlearn::nodes(dag).

dag2matrix = function(dag) {
  
  vars = bnlearn::nodes(dag)
  nvars = length(vars)
  
  mtx = matrix(0, nrow = nvars, ncol = nvars, dimnames = list(vars, vars))
  
  for (i in 1:nvars) {
    
    children = dag$nodes[[i]]$children # set of children of node i
    
    if (length(children) > 0) {
      
      for (j in 1:length(children)) {
        
        mtx[i, which(vars == children[j])] = 1
        
      } # end for j
      
    } # end if 
    
  } # end for i
  
  return(mtx)
  
}