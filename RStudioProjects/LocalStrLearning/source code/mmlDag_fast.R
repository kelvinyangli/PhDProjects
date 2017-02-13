# this function returns the same result as mmlDag, but much faster when computing mpts 
# since mmlcpt for each node in mbVars given its possible parents are pre-computed and taken as an input
mmlDag_fast = function(adjmtx, vars, dataInfo, mmlmtx, n) {
  
  mml = 0 
  
  for (j in 1:ncol(adjmtx)) {
    
    var = colnames(adjmtx)[j]
    pa = colnames(adjmtx)[which(adjmtx[, j] == 1)]
    
    if (length(pa) == 0) {# when there are no paretns
      
      mml = mml + mmlmtx[var, "NULL"]
      
    } else {
      
      pa = pa[order(pa)]
      pa = paste(pa, collapse = "_")
      mml = mml + mmlmtx[var, pa]
      
    } # end else 
    
  } # end for j
  
  return(mml)
  
}