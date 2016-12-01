enumWithNoSp = function(x, y) {
  
  dagList = list()
  dag = matrix(0, nrow = length(x) + 1, ncol = length(x) + 1)
  dimnames(dag) = list(c(x, y), c(x, y))
  
  if (length(x) > 0) {
    
    dag[x, y] = 1 # add x as parents of y first
    dagList[[1]] = dag
    count = 2
    
    if (length(x) > 1) {
      
      for (numChild in 1:(length(x) - 1)) {# for each parent set size starting from 1 till |x|-1
        
        childComb = combn(x, numChild)
        
        for (i in 1:ncol(childComb)) {# for each possible children combination
          
          dagList[[count]] = dag
          dagList[[count]][y, childComb[, i]] = 1 
          dagList[[count]][childComb[, i], y] = 0 
          count = count + 1
          
        } # end for i
        
      } # end for numChild
      
    } # end if length(x) > 1
    
    dagList[[count]] = matrix(0, nrow = length(x) + 1, ncol = length(x) + 1)
    dimnames(dagList[[count]]) = list(c(x, y), c(x, y))
    dagList[[count]][y, x] = 1
    
  } else {
    
    dagList[[1]] = dag
    
  } # end else 
  
  return(dagList)
  
} 

