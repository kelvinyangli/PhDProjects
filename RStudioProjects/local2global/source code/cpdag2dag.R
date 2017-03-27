cpdag2dag = function(cpdag, adjmtx, dataInfo, vars, n) {
  
  undirected = bnlearn::undirected.arcs(cpdag)
  for (i in 1:nrow(undirected)) {
    
    if (i %% 2 == 1) {# since each undirected arc is listed twice, we only take the odd row #
      
      index1 = which(vars == undirected[i, 1])
      index2 = which(vars == undirected[i, 2])
      
      adjmtx[index2, index1] = 0
      cpdag = set.arc(cpdag, undirected[i, 1], undirected[i, 2], check.cycles = FALSE)
      if (acyclic(cpdag)) {# if the output contains no cycle
        
        mml_12 = mmlCPT(index1, which(adjmtx[, index1] == 1), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, exp(1)) + 
          mmlCPT(index2, which(adjmtx[, index2] == 1), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, exp(1))
        
      } else {# drop direction
        
        adjmtx[index2, index1] = 1
        cpdag = drop.arc(cpdag, undirected[i, 1], undirected[i, 2])
        
      }
      
      
      adjmtx[index1, index2] = 0
      cpdag = drop.arc(cpdag, undirected[i, 1], undirected[i, 2])
      adjmtx[index2, index1] = 1
      cpdag = set.arc(cpdag, undirected[i, 2], undirected[i, 1], check.cycles = FALSE)
      
      if (acyclic(cpdag)) {
        
        mml_21 = mmlCPT(index1, which(adjmtx[, index1] == 1), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, exp(1)) + 
          mmlCPT(index2, which(adjmtx[, index2] == 1), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, exp(1))
        
      } else {# drop direction
        
        adjmtx[index1, index2] = 1
        cpdag = drop.arc(cpdag, undirected[i, 1], undirected[i, 2])
        
      }
      
      if (mml_21 > mml_12) {# if ij has lower mml score, then reverse ji back to ij
        
        adjmtx[index1, index2] = 1
        adjmtx[index2, index1] = 0
        cpdag = set.arc(cpdag, undirected[i, 1], undirected[i, 2], check.cycles = FALSE)
        
      } # end reverse back to ij
      
    } # end for even row #
    
  } # end for all undirected arcs
  
  return(list(dag = cpdag, adjmtx = adjmtx))
  
}