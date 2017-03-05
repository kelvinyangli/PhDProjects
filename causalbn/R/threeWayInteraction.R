# returns matrix of indices, where each row is a potential three way interaction
threeWayInteraction = function(allNodes, mbList) {
  
  mtx = matrix(ncol = 3)
  
  for (i in 1:length(mbList)) {
    
    if (length(mbList[[i]]) > 1) {# only to have possible collider if mb has at least 2 variables
      
      target = allNodes[i]
      mblanket = mbList[[i]]
      
      for (j in 1:(length(mblanket) - 1)) {
        
        for (k in (j + 1):length(mblanket)) {
          
          indexJ = which(allNodes == mblanket[j])
          indexK = which(allNodes == mblanket[k])
          if (prod(c(target, allNodes[indexJ]) %in% mbList[[indexK]]) && prod(c(target, allNodes[indexK]) %in% mbList[[indexJ]])) {
            
            collider = c(i, indexJ, indexK)
            mtx = rbind(mtx, collider[order(collider)])  
            
          } # end if 
          
        } # end for k
        
      } # end for j
      
    } # end if
    
  } # end for i
  
  return(unique(mtx[-1, ]))
  
}


