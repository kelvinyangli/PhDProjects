# this function takes adjmtx and a var as inputs
# it extracts mb(x) and its local str
# it returns the adjmtx of the local str
mbLocalStr = function(adjmtx, x) {
  
  xIndex = which(colnames(adjmtx) == x)
  pa = which(adjmtx[, xIndex] == 1)
  chd = which(adjmtx[xIndex, ] == 1)
  mb = c(pa, chd)
  
  # get children's other parents
  for (i in 1:length(chd)) {
    
    pa = which(adjmtx[, chd[i]] == 1)  
    mb = c(mb, pa)
    
  }
  
  mb = unique(mb)
  adjmtx[-c(mb, xIndex), ] = 0
  adjmtx[, -c(mb, xIndex)] = 0
  
  return(adjmtx)
  
}

