varLevels = function(cpts) {
  
  nl = rep(2, length(cpts))
  
  for (i in 1:length(cpts)) {
    
    if (length(dim(cpts[[i]]$prob)) == 1) {
      
      nl[i] = length(cpts[[i]]$prob)
      
    } else {
      
      nl[i] = nrow(cpts[[i]]$prob)
      
    }
    
  }
  
  return(nl)
  
}