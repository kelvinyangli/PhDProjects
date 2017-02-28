# this function refine the initial merged global mbpt matrix 
# if x[i,j]=x[j,i]=1 then the corresponded edge cannot be directed
# if x[i,j]=3 then this var is contained in a v-str
# if x[i,j]=1 but x[j,i]=0 then need to consider the existence of the edge, let drop the edge for now in this case 
# maybe better alternatives will later 
refineMergedMBPT = function(mbpt_global) {
  
  for (i in 1:nrow(mbpt_global)) {
    
    for (j in 1:ncol(mbpt_global)) {
      
      if ((mbpt_global[i, j] > 1) && mbpt_global[j, i] == 0) {
        
        mbpt_global[i, j] = 1
        
      } else if ((mbpt_global[i, j] == 1) && (mbpt_global[j, i] == 0)) {
        
        mbpt_global[i, j] = 0
        
      }
      
    } # end for j
    
  } # end for i
 
  return(mbpt_global)
  
}