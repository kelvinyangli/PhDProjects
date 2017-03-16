# this function refine the initial merged global mbpt matrix 
# ij + ji >= 3 means there is a v-str for certain, the common effect is decided based on the 
# majority vote
# ij + ji = 2 means the edge exist, but the direction is also decided based on 
# the majority vote, either directed or bidirected, in which case we assign no
# direction, note that there is also a chance that a v-str exists, but not 100% certain
# ij + ji = 1 means the existence of the edge is not certain, in this case, we 
# choose to drop this edge
# ij + ji = 0 means there is no edge b/w vars 
refineMergedMBPT = function(mbpt_global) {
  
  for (i in 1:(nrow(mbpt_global) - 1)) {
    
    for (j in (i + 1):ncol(mbpt_global)) { # upper triangular matrix
      
      if (mbpt_global[i, j] + mbpt_global[j, i] == 1) {
        
        mbpt_global[i, j] = mbpt_global[j, i] = 0 # remove edge 
        
      } else if (mbpt_global[i, j] + mbpt_global[j, i] > 1) {
        
        if (mbpt_global[i, j] > mbpt_global[j, i]) {
          
          mbpt_global[j, i] = 0
          
        } else if (mbpt_global[i, j] < mbpt_global[j, i]) {
          
          mbpt_global[i, j] = 0
          
        } # end else 
        
      } # end if ij+ji>1
      
    } # end for j
    
  } # end for i
  
  return(ceiling(mbpt_global/max(mbpt_global)))

}



