dagIsom = function(dagList) {
  
  dup = matrix(0, nrow = length(dagList) - 1, ncol = 2) # store duplicated collider dag indices
  count = 1
  
  for (i in 1:(length(dagList) - 1)) {
    
    for (j in (i + 1):length(dagList)) {
      
      if (!i %in% dup[, 2]) {
        
        if (matrixIdentical(dagList[[i]], dagList[[j]])) {
          
          dup[count, ] = c(i, j)
          count = count + 1
          
        } # end if 
        
      } # end if i not in dup[, 2]
      
    } # end for j
    
  } # end for i
  
  if (all(dup == 0)) {
    
    return(NULL)
    
  } else {
    
    dup = dup[-which(dup[, 1] == 0), ]
    return(dup)
    
  }
  
}

matrixIdentical = function(x, y) {
  
  var = colnames(x)
  return(all(x == y[var, var]))
  
}




