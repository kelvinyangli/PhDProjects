# this function exhaustively searches for the best polytree within a mb
# dagList is the list of all possible polytrees that pre-generated and saved into disk
# vars are all variables in the given dataset
# x = mb(y), where y is a variable
# dataInfo is pre-computed by getDataInfo() for the use of computing mml scores
# n is the sample size
# the function returns the best polytree and its mml score in a list form
# the mml score for each node given its all possible parents sets are pre-computed 
# and saved into the matrix mmlmtx for faster computing the entire score of each polytree
# because several polytrees could share the same substructures so mml scores don't need
# to be re-computed 
polytree_exhaustive = function(mbpts, vars, dataInfo, mmlmtx, n) {
  
  scores = rep(0, length(mbpts))
  
  for (i in 1:length(mbpts)) {
    
    scores[i] = mmlDag_fast(mbpts[[i]], vars, dataInfo, mmlmtx, n)
    
  }
  
  minIndex = which.min(scores)
  
  #ls = list("dag" = dagList[[minIndex]], "mml" = scores[minIndex])
  
  return(mbpts[[minIndex]])
  
}

#mtx = dagList[[4]]
#dag = matrix2dag(mtx)
#cpts = generateCPTs(dag, 2, 1)
#data = rbn(cpts, 1000)
#table(data)
