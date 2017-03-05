# this function takes vars and pa_indices (in terms of vars) as inputs 
# and construct the a string output in the form of "V1_V2_V3", etc
# the pa vars are ordered in an ascending order, so that results are consistent with
# column names of mmlmtx 
makePaString = function(pa) {
  
  paStr = "NULL"
  
  if (length(pa) > 0) {
    
    paStr = pa[order(pa)]
    paStr = paste0(paStr, collapse = "_")
    
  }
  
  return(paStr)
  
}