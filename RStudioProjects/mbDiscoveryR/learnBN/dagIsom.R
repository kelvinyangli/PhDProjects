# dag isomorphism check
# x and y are dags in the format of bnlearn
dagIsom = function(x, y) {
  
  allNodes = nodes(x)
  for (nodeIndex in 1:length(allNodes)) {
    
    parents(x, allNodes[nodeIndex]) == parents(y, allNodes[nodeIndex])
    
  } # end for each node nodeIndex
  
}


ls = readRDS("all mb dags/mbDags6.rds")
count = 0
dup = matrix(nrow = 180, ncol = 2)
for (i in 1:(length(ls) - 1)) {
  for (j in (i+1):length(ls)) {
    if (all.equal(ls[[i]], ls[[j]]) == TRUE) {
      count = count + 1
      dup[count, ] = c(i, j)
    }
  }
}