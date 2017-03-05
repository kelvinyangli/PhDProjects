# this function generate a random dag based on the number of variables and the maximum number of parents given 

randDag = function(numNodes, maxNumParents) {
  
  allNodes = paste0("V", 1:numNodes)
  
  dag = empty.graph(allNodes)
  
  if (maxNumParents > 0) { # if nodes have parents, sample parents from preceding nodes
    
    for (i in 2:length(allNodes)) {
      
      numParents = sample(0:min(maxNumParents, i - 1), 1) 
      
      parents = sample(allNodes[1:(i - 1)], numParents)
      
      if (length(parents) > 0) { # add arc only if a node has at least 1 parents
        
        for (j in 1:numParents) dag = set.arc(dag, parents[j], allNodes[i])
        
      }
      
    } # end for i 
    
  } # else return empty dag
  
  # re-order allNodes to keep the ordering consistent when generating cpts and data
  nodes(dag) = node.ordering(dag)
  return(dag)
  
}