findCMB = function(data, node, dataInfo, debug = FALSE) {
  
  # cmb = c()
  
  cmb = mmlPC(data, node, dataInfo, debug = debug)
  
  if (length(cmb) > 0) {
    
    for (j in 1:length(cmb)) {
      
      cmb = c(cmb, mmlPC(data, cmb[j], dataInfo, debug = debug))
      
    } # end for j
    
    cmb = cmb[cmb != node] # remove target node
    
  } # end if 
  
  return(cmb)
  
}
