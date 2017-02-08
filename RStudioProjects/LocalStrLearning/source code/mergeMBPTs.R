# this function merges mbdags into an entire dag 
# it could contain cycles or bi-directed arcs
mergeMBDags = function(mbdagList, vars) {
  
  temp = matrix(0, length(vars), length(vars), dimnames = list(vars, vars))
  
  for (k in 1:length(vars)) {
    
    if (nrow(mbdagList[[k]]) > 1) {
      
      for (i in 1:nrow(mbdagList[[k]])) {
        
        for (j in 1:ncol(mbdagList[[k]])) {
          
          if (mbdagList[[k]][i, j] == 1) temp[rownames(mbdagList[[k]])[i], colnames(mbdagList[[k]])[j]] = 1
          
        } # end for j
        
      } # end for i
      
    } # end if 
    
  } # end for each var k 
  
  #if (isDag(temp)) {
    
    return(temp)
    
  #} else print("The resulting graph is not a dag!")
  
}