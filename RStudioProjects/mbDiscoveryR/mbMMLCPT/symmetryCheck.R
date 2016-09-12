# this function apply the symmetry check for the mb results that returned by mmlCPT
# given that mmlCPT has high precision and low recall, there is a high chance that if x \in mb(y) then y \in mb(x)
# hence we use symmetry check to add more nodes into the learned mb by mmlCPT 

symmetryCheck = function(allNodes, mbList) {
  
  for (i in 1:length(allNodes)) {
    
    node = allNodes[i] 
    
    # if node x is in mb(y), then y is also in mb(x)
    for (j in 1:length(allNodes)) {
      
      if ((j != i) && (node %in% mbList[[j]])) {# if node exists in the mb of another node
        
        if (!allNodes[j] %in% mbList[[i]]) {# if this other node is not in mb(node)
          
          # then add it into mb(node)
          mbList[[i]] = c(mbList[[i]], allNodes[j])
          
        } # end if
        
      } # end if
      
    } # end for j
    
  } # end for i
  
  return(mbList)
  
}