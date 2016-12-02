mmlDag = function(dagMatrix, dataInfo, n) {
  
  vars = colnames(dagMatrix)
  mml = 0 
  
  for (j in 1:length(vars)) {
    
    pa = which(dagMatrix[, j] == 1)
    mml = mml + mmlCPT(j, pa, dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
    
  }
  
  return(mml)
  
}

