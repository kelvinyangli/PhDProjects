# 7. using bootstrapping to measure the uncertainty of a feature in a local str
# be aware of isolated nodes
featureUncertainty = function(x, y, mbList, localStrs, mbptsList, vars, data, nResamples) {
  
  n = nrow(data)
  x2y = y2x = xy = none = 0 
  
  for (k in 1:length(localStrs)) {# identify the local str that contains both x and y
    
    if (prod(c(x, y) %in% colnames(localStrs[[k]]))) {
      
      for (i in 1:nResamples) {
        
        #indices = sample(1:n, n, replace = TRUE)
        data_resampled = data[sample(1:n, n, replace = TRUE), ]
        dataInfo_resampled = getDataInfo(data_resampled)
        mmlmtx = computeMMLMatrix(vars, mbList[[k]], vars[k], dataInfo_resampled, n)
        
        mbpts = substituteVar(mbptsList[[length(mbList[[k]]) + 1]], vars[k], mbList[[k]])
        scores = rep(0, length(mbpts))
        
        for (j in 1:length(scores)) {
          
          scores[j] = mmlDag_fast(mbpts[[j]], vars, dataInfo_resampled, mmlmtx, n)
          
        } # end for j 
        
        index = which.min(scores)
        if ((mbpts[[index]][x, y] == 1) && (mbpts[[index]][y, x] == 0)) {
          
          x2y = x2y + 1
          
        } else if ((mbpts[[index]][y, x] == 1) && (mbpts[[index]][x, y] == 0)) {
          
          y2x = y2x + 1
          
        } else if ((mbpts[[index]][x, y] == 1) && (mbpts[[index]][y, x] == 1)) {
          
          xy = xy + 1
          
        } else {
          
          none = none + 1
          
        } # end else 
        
      } # end for i
      
    } # end if 
    
    
  } # end for k 
  
  return(data.frame(x2y, y2x, xy, none))
  
}