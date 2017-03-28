# simulated annealing
# dag must be fully directed, if partially directed, then random add direction using mml w/o introducing cycles
sa = function(adjmtx, vars, dataInfo, n, step = 0.02, temp = 1, maxItr = 100, debug = FALSE) {
  
  for (nStep in 1:maxItr) {
    
    if (debug) cat("Step -", nStep, " - temperate", temp, "\n")
    
    temp <- (1 - step) ^ nStep # update temperate 
    i = sample(1:length(vars), 1)
    nbr = c(which(adjmtx[i, ] == 1), which(adjmtx[, i] == 1))
    #names(nbr) = c()
    
    if (length(nbr) > 0) {# if there are nbr, then sample 1 index from nbr
      
      if (length(nbr) == 1) {
        
        j = nbr
        
      } else {
        
        j = sample(nbr, 1)
        
      }
      
      # compute mml for this arc
      current_mml = mmlCPT(i, which(adjmtx[, i] == 1), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, exp(1)) + 
        mmlCPT(j, which(adjmtx[, j] == 1), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, exp(1))
      
      # reverse arc direction
      adjmtx[i, j] = abs(adjmtx[i, j] - 1)
      adjmtx[j, i] = abs(adjmtx[j, i] - 1)
      
      if (debug) {
        cat("reverse arc", i, j, "\n")
        cat("current mml", current_mml, "\n")
      }
      if (acyclic(matrix2dag(adjmtx))) {# if contains no cycle 
        
        # compute mml score change, if mml is decreased then keep the reversion, otherwise reverse it back 
        next_mml = mmlCPT(i, which(adjmtx[, i] == 1), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, exp(1)) + 
          mmlCPT(j, which(adjmtx[, j] == 1), dataInfo$indexListPerNodePerValue, dataInfo$arities, n, exp(1))
        
        if (debug) {
          cat("reversion doesn't create cycle \n")
          cat("new mml", next_mml, "\n")
        }
        if ((next_mml >= current_mml) && runif(1, 0, 1) > exp(-(next_mml - current_mml) / temp)) {
          # if mml doesn't improve then reverse the arc back to original
          if (debug) {
            cat("new mml > current mml & sa suggests to reverse back \n")
          }
          adjmtx[i, j] = abs(adjmtx[i, j] - 1)
          adjmtx[j, i] = abs(adjmtx[j, i] - 1)
          
        } # end checking mml improvement 
        
      } else {# if there is a cycle, then reverse back
        
        if (debug) cat("reversion creates a cycle, reverse back \n")
        adjmtx[i, j] = abs(adjmtx[i, j] - 1)
        adjmtx[j, i] = abs(adjmtx[j, i] - 1)
        
      } # end of checking cycle
      
    } # end if are nbr  
    
  } # end maxItr
  
  return(adjmtx)
    
}

