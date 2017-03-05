####################################################################################################
# convert discrete variable values from "A", "B", "C", ... to 0, 1, 2, ... 
# for the use of mml logit method 
####################################################################################################
getIndicator = function(data) {
  
  indicatorMatrix = matrix(nrow = nrow(data), ncol = ncol(data), dimnames = list(NULL, colnames(data)))
  
  for (i in 1:ncol(data)) {
    
    # as.numeric convert categrical data values to 1, 2, 3, ...
    # -1 transfers them to 0, 1, 2, ...
    indicatorMatrix[, i] = as.numeric(data[, i]) - 1 
    
  }
  
  # add an extra column of 1s in front for the intercept of a logit model
  #indicatorMatrix = cbind(1, indicatorMatrix)
  
  return(indicatorMatrix)
  
}
