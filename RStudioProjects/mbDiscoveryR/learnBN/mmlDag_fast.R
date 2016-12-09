# this function computes the mml score for an entire dag 
# this is a faster version of computing mml scores, since the score for each node is pre-computed and saved into 
# a matrix called mmlMatrix
mmlDag_fast = function(dagMatrix, vars, dataInfo, mmlMatrix, n) {
  
  mml = 0 
  
  for (j in 1:ncol(dagMatrix)) {
    
    var = colnames(dagMatrix)[j]
    pa = colnames(dagMatrix)[which(dagMatrix[, j] == 1)]
    
    if (length(pa) == 0) {# when there are no paretns
      
      mml = mml + mmlMatrix[var, "NULL"]
      
    } else {
      
      pa = pa[order(pa)]
      pa = paste(pa, collapse = "_")
      mml = mml + mmlMatrix[var, pa]
      
    } # end else 
    
  } # end for j
  
  return(mml)
  
}