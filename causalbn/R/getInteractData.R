# compute all pairs of variable interactions
# since indicatorMatric contains 0 and 1, where 0 means absence of a state and 1 means
# occurance of a state, an interaction b/w 0 an 1 will result in 0, which means absence
# of variable interaction
# this only makes sense for binary variables, if non-binary then we need to work out a 
# way of interpreting interaction terms
getInteractData = function(indicatorMatrix) {
  
  interact = matrix(0, nrow = nrow(indicatorMatrix), ncol = choose(ncol(indicatorMatrix), 2))
  
  interact = as.data.frame(interact)
  
  k = 1
  
  for (i in 1:(ncol(indicatorMatrix) - 1)) {
    
    for (j in (i + 1):ncol(indicatorMatrix)) {
      
      interact[, k] = indicatorMatrix[, i] * indicatorMatrix[, j]
      
      colnames(interact)[k] = paste0(colnames(indicatorMatrix)[i], colnames(indicatorMatrix)[j], collapse = "")
      
      k = k + 1
      
    } # end for j
    
  } # end for i
  
  return(interact)
  
}