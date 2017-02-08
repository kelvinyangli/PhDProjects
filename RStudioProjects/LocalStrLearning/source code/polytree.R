# this function returns the acestor of a var
# mtx is an adjacency matrix, var is the index of a var in the order of column names (or row names)
ancestor = function(mtx, var) {
  
  parents = which(mtx[, var] == 1)
  ancst = parents
  
  repeat {
    
    temp = c()
    
    for (i in 1:length(parents)) {
      
      temp = c(temp, which(mtx[, parents[i]] == 1))
      
    } # end for i 
      
    if (length(temp) == 0) break
    
    parents = temp
    ancst = c(ancst, temp)
      
  } # end repeat
  
  names(ancst) = c()
  
  return(ancst)
  
}

# this function takes the number of variables and the maximum number of parents as inputs 
# and generate a random polytree 
polytree = function(nVars, maxNPa) {
  
  vars = paste0("V", 1:nVars)
  mtx = matrix(0, nVars, nVars)
  dimnames(mtx) = list(vars, vars)
  
  
  for (i in 2:nVars) {
    
    nPa = min(maxNPa, sample(0:(i - 1), 1)) # sample the number of parents from [0, n vars preceding]
    if (nPa > 0) pa = sample(1:(i - 1), nPa) # when nPa is non-zeor, sample nPa parents from preceding vars
    
    if (nPa > 0) {
      
      for (j in 1:nPa) {
        
        ancst_i = ancestor(mtx, i)
        ancst_j = ancestor(mtx, pa[j])
        commonAncst = intersect(ancst_i, ancst_j) 
        if ((length(commonAncst) < 1) && (!pa[j] %in% ancst_i)) mtx[pa[j], i] = 1
        
      } # end for j 
      
    } # end if 
    
  } # end for i 
  
  return(mtx)
  
}














