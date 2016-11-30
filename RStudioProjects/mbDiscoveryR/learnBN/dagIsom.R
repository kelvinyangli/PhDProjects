dagIsom = function(dagList) {
  
  dup = c() # store duplicated collider dag indices
  for (i in 1:(length(dagList) - 1)) {
    
    for (j in (i + 1):length(dagList)) {
      
      if (all.equal(dagList[[i]], dagList[[j]]) == TRUE) {
        
        dup = c(dup, j)
        
      } # end if 
      
    } # end for j
    
  } # end for i
  
  return(dup)
  
}
