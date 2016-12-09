addArcs_greedy = function(adjmtx, mmlmtx, dataInfo, vars, debug = FALSE) {
  
  adjmtx_new = matrix(0, nrow = nrow(adjmtx), ncol = ncol(adjmtx)) # another adj mtx to store newly added arcs
  dimnames(adjmtx_new) = dimnames(adjmtx)
  
  for (i in 1:ncol(adjmtx)) {
    
    v_i = colnames(adjmtx)[i]
    v_i_index = which(vars == v_i) # index of i in vars for computing mmlCPT
    pa_index = which(adjmtx[, i] == 1) # existing parents of v_i
    if (length(pa_index) == 0) {
      
      pa = "NULL"
      
    } else {
      
      pa = colnames(adjmtx)[pa_index]
      pa_index = which(vars %in% pa) # update pa_index in terms of vars
      pa = pa[order(pa)]
      pa = paste0(pa, collapse = "_")
      
      
    } # end else 
    
    mml_min = mmlmtx[v_i, pa]
    toAdd = c()
    
    if (debug) cat("mml_min (", v_i, "):", mml_min, "\n")
    
    for (j in 1:ncol(adjmtx)) {
      
      if (j != i) {
        
        if (adjmtx_new[i, j] == 1) {# if arc v_i -> v_j exists exists in adjmtx_new then try to reverse it
          
          # reverse arc 
          
        } else if (!adjmtx[i, j] && !adjmtx[j, i]) {# if there is no arc b/w v_i and v_j
          
          # assume an arc v_j -> v_i is added
          adjmtx[j, i] = 1
          
          # check once v_j -> v_i is added, if the resulting graph is still a dag
          if (isDag(adjmtx)) {
            
            v_j = colnames(adjmtx)[j]
            v_j_index = which(vars == v_j) # index of j in vars for computing mmlCPT
            mml_current = mmlCPT(v_i_index, c(pa_index, v_j_index), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
            
            if (debug) cat("arc", v_j, "->", v_i, "--- mml:", mml_current, "\n")
            
            # compare mml(v_i | pa) and mml(v_i | c(pa, v_j))
            if (mml_current < mml_min) {# when adding v_j as a parent of v_i the mml score is improved
              
              toAdd = j # then keep the arc v_j -> v_i
              mml_min = mml_current
              
            } # end if mml_current < mml_min
            
          } # end if adjmtx is still a dag
          
          adjmtx[j, i] = 0 # remove the arc for now, and if this arc is the best option, it will be added later
          
        } 
        
      } # end if j != i
      
      
      
      
      
    } # end for j
    
    if (length(toAdd) > 0) {
      
      adjmtx[toAdd, i] = 1 # add the best arc
      mmlmtx[v_i, colnames(adjmtx)[toAdd]] = mml_min
      arcs_new = rbind(arcs_new, c(v_j, v_i)) # store newly added arcs for furture reference 
      if (debug) cat("add arc", colnames(adjmtx)[toAdd], "->", v_i, "\n")
    
    }
    
  } # end for i 
  
  return(adjmtx)
  
}












