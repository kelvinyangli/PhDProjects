findMB = function(data, node, dataInfo, base = 2, debug = FALSE) {
  
  cmb = findCMB(data, node, dataInfo, base = base, debug = debug)
  
  if (length(cmb) > 0) {
    
    data = data[, c(cmb, node)]
    
    dataInfo = getDataInfo(data)
    
    mb = mbForwardSelection.fast(data, node, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue, base = base, debug = debug)
    
  } else {
    
    mb = cmb
    
  }
  
  return(mb)
  
}