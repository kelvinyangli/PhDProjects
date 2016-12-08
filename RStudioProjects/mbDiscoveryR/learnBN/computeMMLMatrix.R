# compute mml score for valid entries
# notice that not all entries in mmlMatrix are valid, for example
# if v1, v2 \in mb(T), then v1 cannot have v2 as its parent alone. 
computeMMLMatrix = function(x, y, vars, dataInfo, n) {
  
  # generate a matrix to store mml score for each node given eligible parents 
  mmlMatrix = matrix(0, nrow = length(x) + 1, ncol = 2 ^ (length(x) + 1) - 1)
  dimnames(mmlMatrix) = list(c(y, x), powerset(c(y, x)))

  for (i in 1:nrow(mmlMatrix)) {
    
    var = rownames(mmlMatrix)[i]
    varIndex = which(vars == var)
    
    mmlMatrix[i, 1] = mmlCPT(varIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
    
    for (j in 2:ncol(mmlMatrix)) {
      
      pa = colnames(mmlMatrix)[j]
      pa = strsplit(pa, "_")[[1]]
      paIndices = which(vars %in% pa)
      
      if (!var %in% pa) {# when pa(var) doesn't involve itself
        
        if ((var == y) || (y %in% pa)) {
          
          mmlMatrix[i, j] = mmlCPT(varIndex, paIndices, dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
          
        } # end if 
        
      } # end else if 
      
    } # end for j 
    
  } # end for each var i 
  
  return(mmlMatrix)
  
}
