# resultList is a list returned by netica2bnlearn, it contains nodes and parentsList
parentsList2BN = function(resultList) {
  
  dag = empty.graph(resultList$node)
  
  for (i in 1:length(resultList$parent)) {# add directed arc from its parents to each node i
    
    if (length(resultList$parent[[i]]) > 0) {# only add arc if there is at least 1 parent for node i
      
      for (j in 1:length(resultList$parent[[i]])) {# for each parent j 
        
        dag = set.arc(dag, resultList$parent[[i]][j], resultList$node[i])
        
      }# end for j 
      
    }# end if 
    
  }# end for i
  
  return(dag)
  
}