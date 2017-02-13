# this function extract the local str within mb(target) from the true model
# it takes the adjacency matrix, all vars, target var, and the mb vars as inputs
mbLocalStr = function(adjmtx, vars, target, mbVars) {
  
  indices = c(which(vars == target), which(vars %in% mbVars))
  
  return(adjmtx[indices, indices])
  
}