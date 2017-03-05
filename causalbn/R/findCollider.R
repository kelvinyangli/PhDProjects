findCollider = function(mtx, data) {
  
  toDelete = c()
  for (i in 1:nrow(mtx)) {
    
    interactionInfo = ci.test(allNodes[mtx[i, 1]], allNodes[mtx[i, 2]], 
                              allNodes[mtx[i, 3]], data = data)$statistic[[1]] - 
      ci.test(allNodes[mtx[i, 1]], allNodes[mtx[i, 2]], data = data)$statistic[[1]]
    
    if (interactionInfo < 0) toDelete = c(toDelete, i)
    
  }
  
  if (length(toDelete) > 0) mtx = mtx[-toDelete, ]
  
  return(mtx)
  
}

