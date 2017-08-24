#' A function to make a cach of MML scores 
#'
#' Given a node x and its learned Markov blanket mb(x), this function calculates the mml score of each node 
#' y in mb(x) given its potential parents. There are certain nodes that can't be parents of y, such as any 
#' subset that contains y, or any subset that doesn't contain the target node x, because if x is not a 
#' parent of y, then any parents of y must not exist in mb(x).
#' @param vars A vector of all variables. 
#' @param mbVars The learned mb of the target node 
#' @param target A taregt node. 
#' @export
# this function takes all vars, mb candidates, target var, dataInfo that pre-computed by getDataInfo(), and sample size 
# as inputs, it computes mmlcpt for each var in c(mbVars, target) given its parents
# there are certain vars or var combinations that cannot be parents of a var
# for example, the var itself, or any combination includes itself, other vars without the target, because this will become
# grandparents of the target
# this function helps speed up computing mmlcpt for an entire mbpt
make_mml_cach = function(arities, vars, mbVars, target, dataInfo, n) {
  
  # generate a matrix to store mml score for each node given eligible parents 
  mmlMatrix = matrix(0, nrow = length(mbVars) + 1, ncol = 2 ^ (length(mbVars) + 1) - 1)
  if (length(mbVars) == 0) {
    
    dimnames(mmlMatrix) = list(target, "NULL")
    
  } else {
    
    dimnames(mmlMatrix) = list(c(target, mbVars), powerset(c(target, mbVars)))
    
  }

  for (i in 1:nrow(mmlMatrix)) {
    
    var = rownames(mmlMatrix)[i]
    varIndex = which(vars == var)
    
    # when there is no parent
    mmlMatrix[i, 1] = mmlCPT(varIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
    
    if (length(mbVars) > 0) {
      
      for (j in 2:ncol(mmlMatrix)) {
        
        pa = colnames(mmlMatrix)[j]
        pa = strsplit(pa, "_")[[1]]
        paIndices = which(vars %in% pa)
        
        if (!var %in% pa) {# when pa(var) doesn't involve itself
          
          if ((var == target) || (target %in% pa)) {
            
            mmlMatrix[i, j] = mmlCPT(varIndex, paIndices, dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
            
          } # end if 
          
        } # end if 
        
      } # end for j 
      
    } # end if 
    
  } # end for each var i 
  
  return(mmlMatrix)
  
}
