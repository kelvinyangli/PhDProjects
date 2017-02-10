# this function computes mmlcpt for an entire dag based on the adjacency matrix and data
# it's relatively slower because several dags may share the same adjacencies that shouldn't be re-computed 
mmlDag = function(adjmtx, vars, dataInfo, n) {
  
  mml = 0 
  
  for (j in 1:ncol(adjmtx)) {
    
    pa = which(adjmtx[, j] == 1)
    varIndex = which(vars == colnames(adjmtx)[j])
    
    if (length(pa) > 0) {
      
      paIndices = which(vars %in% colnames(adjmtx)[pa])
    
    } else {
      
      paIndices = c()
      
    }
    
    mml = mml + mmlCPT(varIndex, paIndices, dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
    
  }
  
  return(mml)
  
}

