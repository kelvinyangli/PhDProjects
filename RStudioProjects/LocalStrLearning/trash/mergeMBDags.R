# this function merges mbdags into an entire dag 
# it could contain cycles or bi-directed arcs
mergeMBDags = function(mbpts, vars) {
  
  adjmtx_global = matrix(0, length(vars), length(vars), dimnames = list(vars, vars))
  
  for (k in 1:length(vars)) {
    
    if (nrow(mbpts[[k]]) > 1) { # when a var's mb is not empty 
      
      for (i in 1:nrow(mbpts[[k]])) {
        
        for (j in 1:ncol(mbpts[[k]])) {
          
          if (mbpts[[k]][i, j] == 1) adjmtx_global[rownames(mbpts[[k]])[i], colnames(mbpts[[k]])[j]] = 1
          
        } # end for j
        
      } # end for i
      
    } # end if 
    
  } # end for each var k 
  
  #if (isDag(adjmtx_global)) {
  
  return(adjmtx_global)
  
  #} else print("The resulting graph is not a dag!")
  
}