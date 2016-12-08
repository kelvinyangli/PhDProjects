mmlDag = function(dagMatrix, vars, dataInfo, n, mmlMatrix = NULL) {
  
  #vars = colnames(dagMatrix)
  mml = 0 
  
  for (j in 1:ncol(dagMatrix)) {
    
    pa = which(dagMatrix[, j] == 1)
    varIndex = which(vars == colnames(dagMatrix)[j])
    
    if (length(pa) > 0) {
      
      paIndices = which(vars %in% colnames(dagMatrix)[pa])
    
    } else {
      
      paIndices = c()
      
    }
    
    mml = mml + mmlCPT(varIndex, paIndices, dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
    
  }
  
  return(mml)
  
}

