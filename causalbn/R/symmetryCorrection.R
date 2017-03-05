# this function apply the symmetry check for the mb results that returned by mmlCPT
# given that mmlCPT has high precision and low recall, there is a high chance that if x \in mb(y) then y \in mb(x)
# hence we use symmetry check to add more nodes into the learned mb by mmlCPT 

symmetryCorrection = function(vars, mbList) {
  
  for (i in 1:length(vars)) {
    
    # if node x is in mb(y), then y is also in mb(x)
    for (j in 1:length(vars)) { 
      
      # for all other vars, if vars[i] \in mb(vars[j]) but vars[j] \notin mb(vars[i])
      if ((j != i) && (vars[i] %in% mbList[[j]]) && (!vars[j] %in% mbList[[i]])) {
        
        mbList[[i]] = c(mbList[[i]], vars[j]) # then add vars[j] into mb(vars[i])
          
      } # end if
      
    } # end for j
    
  } # end for i
  
  return(mbList)
  
}




