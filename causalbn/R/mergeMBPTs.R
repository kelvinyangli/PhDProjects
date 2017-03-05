# this function merge local structures into a global structure 
#
mergeMBPTs = function(strList, vars) {
  
  adjmtx_global = matrix(0, nrow = length(vars), ncol = length(vars), dimnames = list(vars, vars))
  
  for (k in 1:length(strList)) {
    
    for (i in 1:nrow(strList[[k]])) {
      
      for (j in 1:ncol(strList[[k]])) {
        
        if (i != j) {
          
          adjmtx_global[rownames(strList[[k]])[i], colnames(strList[[k]])[j]] = 
            adjmtx_global[rownames(strList[[k]])[i], colnames(strList[[k]])[j]] + 
            strList[[k]][i, j]
          
        } # end if  
        
      } # end for j 
      
    } # end for i  
    
  } # end for k 
  
  return(adjmtx_global)
  
}










