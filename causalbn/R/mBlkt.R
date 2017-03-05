# this function takes the adjacency matrix and a var as inputs 
# it extracts the mb(x)
mBlkt = function(adjmtx, x) {
  
  xIndex = which(colnames(adjmtx) == x)
  pa = which(adjmtx[, xIndex] == 1)
  chd = which(adjmtx[xIndex, ] == 1)
  mb = c(pa, chd)
  
  # get children's other parents
  for (i in 1:length(chd)) {
    
    pa = which(adjmtx[, chd[i]] == 1)  
    mb = c(mb, pa)
    
  }
  
  mb = mb[mb != xIndex] # remove x from mb 
  mbIndices = unique(mb) # remove duplicates due to the above concatenations
  
  return(colnames(adjmtx)[mbIndices]) # return mb in terms of vars
  
}