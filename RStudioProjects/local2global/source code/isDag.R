# use catnet::isDAG
# input dag is in bnlearn format
isDag = function(x) {
  
  if (!is.matrix(x)) {
    
    lnodes = bnlearn::nodes(x)
    lpars = list()
    for (i in 1:length(lnodes)) {
      
      lpars[[i]] = x$nodes[[i]]$parents  
      
    }
    
  } else {
    
    lnodes = colnames(x)
    lpars = list()
    for (i in 1:length(lnodes)) {
      
      lpars[[i]] = which(x[, i] == 1)
      
    }  
    
  } # end else 
  
  return(isDAG(lnodes, lpars))
  
}

