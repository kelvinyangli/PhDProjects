learnBN = function(mbList, allNodes) {
  
  dagLearned = empty.graph(allNodes) #empty dag
  
  # connect each variable with the 1st found variable in its learned mb
  # because the 1st found variable has a very high chance of being in the pc(target)
  # but no direction can be inferred due to statistical equivalence b/w x -> y and x <- y
  for (i in 1:length(allNodes)) {
    
    if(length(mbList[[i]]) > 0) dagLearned = set.edge(dagLearned, allNodes[i], mbList[[i]][1])
    
  }
  
  return(dagLearned)
  
}