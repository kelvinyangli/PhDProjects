# nrew version that replaces arcPrior and featureUncertainty 
arcCount = function(arcs, mbList, mbptsList, vars, data, n, nResamples) {
  
  if (length(arcs) > 0) {
    
    count = matrix(0, nrow(arcs), 3) 
    
    for (k in 1:length(vars)) {
      cat("Start var", k, "bootstrapping \n")
      # add an extra condition to check if c(target, mb) contains existing arc variables
      #if (c(vars[k], mbList[[k]]) %in% arcs)
      
      if (length(mbList[[k]]) > 0) {
        
        mbpts = substituteVar(mbptsList[[length(mbList[[k]]) + 1]], vars[k], mbList[[k]])
        
        for (h in 1:nResamples) {
          cat(h, "\n")
          indices = sample(1:n, n, replace = TRUE)
          dataInfo_resampled = getDataInfo(data[indices, ])
          mmlmtx = computeMMLMatrix(colnames(data), mbList[[k]], vars[k], dataInfo_resampled, n)
          scores = rep(0, length(mbpts))
          for (j in 1:length(scores)) {
            
            scores[j] = mmlDag_fast(mbpts[[j]], mbList[[k]], dataInfo_resampled, mmlmtx, n)
            
          } # end for j 
          
          index = which.min(scores)
          
          for (i in 1:nrow(mbpts[[index]])) {
            
            for (j in 1:ncol(mbpts[[index]])) {
              
              if (mbpts[[index]][i, j] == 1) { # update count if cell == 1
                
                for (m in 1:nrow(arcs)) { # for each arc
                  
                  if (prod(arcs[m, ] == c(rownames(mbpts[[index]])[i], colnames(mbpts[[index]])[j]))) {
                    
                    count[m, 1] = count[m, 1] + mbpts[[index]][i, j] # update i -> j count
                    
                  } else if (prod(rev(arcs[m, ]) == c(rownames(mbpts[[index]])[i], colnames(mbpts[[index]])[j]))) {
                    
                    count[m, 2] = count[m, 2] + mbpts[[index]][j, i] # update j -> i count
                    
                  } # end else if 
                  
                } # end for each arc
                
              } # end if cell == 1

            } # end for j
            
          } # end for i
          
        } # end for resample
        
      } # end if no mb for var[k]
      
    } # end for k
      
  } else {
    
    count = 0
    
  } 
  
  return(count)
  
}

