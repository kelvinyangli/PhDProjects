# create initial skeleton from data using mi

initialSkeleton = function(data) {
  
  allNodes = colnames(data)
  
  edges = c()
  
  for (i in 1:length(allNodes)) {
    # find the node that has the max mi to each node
    x = allNodes[i]
    y = allNodes[-i]
    
    MIs = rep(0, length(y)) 
    for (j in 1:length(y)) MIs[j] = ci.test(x, y[j], data = data)$statistic
    
    edges = rbind(edges, c(x, y[which.max(MIs)]))
  }
  
  
  # check if an edge appears twice
  edgesTrue = c()
  for (ii in 1:nrow(edges)) {
    (apply(rev(edges[ii,]) == edges[-ii,], 1, prod))
    if (sum(apply(rev(edges[ii,]) == edges[-ii,], 1, prod))) edgesTrue = rbind(edgesTrue, edges[ii,])
  }
  sklt = empty.graph(allNodes) # empty graph 
  
  # add edges
  for (k in 1:nrow(edges)) sklt = set.edge(sklt, edges[k, 1], edges[k, 2])
  
  return(sklt)
  
}

