addArcs_greedy = function(adjmtx, mmlmtx, dataInfo, vars, debug = FALSE) {
  
  for (i in 1:ncol(adjmtx)) {
    
    v_i = colnames(adjmtx)[i]
    vi_index = which(vars == v_i) # index of v_i in global vars
    vi_pa = colnames(adjmtx)[which(adjmtx[, i] == 1)] # existing parents of v_i
    vi_pa_indices = which(vars %in% vi_pa) # indices of existing parents of v_i in vars
    vi_paStr = makePaString(vi_pa) # make consistency parent string with colnames(mmlmtx)
    
    mml_min = mmlmtx[v_i, vi_paStr] # store current score as min mml score
    toAdd = c()
    
    if (debug) cat("mml_min (", v_i, "):", mml_min, "\n")
    
    for (j in 1:ncol(adjmtx)) {
      
      if (j != i && !adjmtx[i, j] && !adjmtx[j, i]) {# if v_i and v_j are not adjcent
      
        # assume an arc v_j -> v_i is added
        adjmtx[j, i] = 1
        
        # check once v_j -> v_i is added, if the resulting graph is still a dag
        if (isDag(adjmtx)) {
          
          v_j = colnames(adjmtx)[j]
          vj_index = which(vars == v_j) # index of j in vars 
          # mml_current = mml(v_i|c(pa, v_j))
          mml_current = mmlCPT(vi_index, c(vi_pa_indices, vj_index), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
          
          if (debug) cat("arc", v_j, "->", v_i, "--- mml:", mml_current, "\n")
          
          # compare mml(v_i|pa) and mml(v_i|c(pa, v_j))
          if (mml_current < mml_min) {# when adding v_j as a parent of v_i the mml score is improved
            
            toAdd = colnames(adjmtx)[j] # then keep the arc v_j -> v_i
            mml_min = mml_current
            
          } # end if mml_current < mml_min
          
        } # end if adjmtx is still a dag
        
        adjmtx[j, i] = 0 # remove the arc for now, and if this arc is the best option, it will be added later
        
      } # end if v_i - v_j are not adjacent
      
    } # end for j
    
    if (length(toAdd) > 0) {
      
      if (debug) cat("check for reversal \n")
      # var toAdd has been selected to add as a parent of v_i
      # now test if reverse the arc, what happens to the mml score
      toAdd_pa = colnames(adjmtx)[which(adjmtx[, toAdd] == 1)] # existing parents of toAdd
      toAdd_pa_indices = which(vars %in% toAdd_pa)
      toAdd_paStr = makePaString(toAdd_pa)
      # mml(toAdd -> v_i) = mml(toAdd|toAdd_pa) + mml(v_i|c(vi_pa, toAdd))
      mml_toAdd2vi = mmlmtx[toAdd, toAdd_paStr] + mml_min
      if (debug) cat("mml(", toAdd, "->", v_i, "):", mml_toAdd2vi, "\n")
      
      # mml(v_i -> toAdd) = mml(v_i|vi_pa) + mml(toAdd|c(toAdd_pa, v_i))
      mml_toAdd = mmlCPT(which(vars == toAdd), c(toAdd_pa_indices, vi_index), 
             dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
      mml_vi2toAdd = mmlmtx[v_i, vi_paStr] + mml_toAdd
      if (debug) cat("mml(", v_i, "->", toAdd, "):", mml_vi2toAdd, "\n")
      
      if (mml_toAdd2vi < mml_vi2toAdd) {
        
        adjmtx[toAdd, i] = 1 # add toAdd -> v_i
        if (mmlmtx[v_i, makePaString(c(toAdd, vi_pa))] == 0) {# update mml score in mmlmtx
          
          mmlmtx[v_i, makePaString(c(toAdd, vi_pa))] = mml_min
          
        }
        
        if (debug) cat("no reversal -- add arc", toAdd, "->", v_i, "\n")
        
      } else {# reverse arc
        
        adjmtx[i, toAdd] = 1 # add v_i -> toAdd
        
        if (isDag(adjmtx)) {
          
          if (mmlmtx[toAdd, makePaString(c(toAdd_pa, v_i))] == 0) {# update mml score in mmlmtx
            
            mmlmtx[toAdd, makePaString(c(toAdd_pa, v_i))] = mml_toAdd
            
          } # end if 
          
          if (debug) cat("reverse arc -- add arc", v_i, "->", toAdd, "\n")
          
        } else {# if reverse arc doesn't form a dag
          
          adjmtx[i, toAdd] = 0 # remove arc
          if (debug) cat("cannot add arc", v_i, "->", toAdd, "because it forms a cycle \n")
          
        } # end else 
        
      } # end else reverse arc
      
    } # end if length(toAdd) > 0
    
    if (debug) cat("------------------------------------ \n")
    
  } # end for i 
  
  return(adjmtx)
  
}












