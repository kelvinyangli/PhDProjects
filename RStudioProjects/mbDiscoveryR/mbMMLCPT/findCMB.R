# this function finds the pc(T) and pc(x), for all x \in pc(T)

findCMB = function(data, node, dataInfo, base = 2, debug = FALSE) {
  
  # cmb = c()
  
  cmb = mmlPC(data, node, dataInfo, debug = debug)
  
  if (length(cmb) > 0) {
    
    for (j in 1:length(cmb)) {
      
      cmb = c(cmb, mmlPC(data, cmb[j], dataInfo, base = base, debug = debug))
      
    } # end for j
    
    cmb = unique(cmb)
    cmb = cmb[cmb != node] # remove target node
    
  } # end if 
  
  return(cmb)
  
}

