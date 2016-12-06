mmlDag = function(dagMatrix, dataInfo, vars, n) {
  
  #vars = colnames(dagMatrix)
  mml = 0 
  
  for (j in 1:ncol(dagMatrix)) {
    
    pa = which(dagMatrix[, j] == 1)
    nodeIndex = which(vars == colnames(dagMatrix)[j])
    if (length(pa) > 0) {
      
      parentsIndices = which(vars %in% colnames(dagMatrix)[pa])
    
    } else {
      
      parentsIndices = c()
      
    }
    
    mml = mml + mmlCPT(nodeIndex, parentsIndices, dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
    
  }
  
  return(mml)
  
}

