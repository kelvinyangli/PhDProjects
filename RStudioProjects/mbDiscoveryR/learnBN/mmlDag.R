# this version of computing mml score for an entire dag does not require mml scores for each var to be pre-computed
# hence this function is slower when score for the same var has to be computed many times
mmlDag = function(dagMatrix, vars, dataInfo, n) {
  
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

