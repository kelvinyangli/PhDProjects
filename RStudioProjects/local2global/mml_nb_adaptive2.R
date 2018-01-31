mml_nb_adaptive2 = function(data, arities, sampleSize, targetIndex, targetProbsAdpt, cachPTs, 
                            cachInd, chIndices) {
  
  lp = log_prob_adaptive(data, sampleSize, targetIndex, targetProbsAdpt) # log probability
  # a matrix to store the normalizting constant in p(T|Xs)
  margProbs = targetProbsAdpt
  for (x in chIndices) {# go through each node in a given str
    
    # get the adaptive count of it given its parent set
    ind = which(names(cachPTs) == paste(c(x, targetIndex), collapse = ""))
    if (length(ind) > 0) {
      
      #cat(paste(c(x, targetIndex), collapse = ""), " using existing PT \n")
      condProbsAdpt = cachPTs[[ind]]
      
    } else {# cach condProbsAdpt if it hasn't been cached
      
      #cat("cal new PT, name", paste(c(x, targetIndex), collapse = ""), "\n")
      condProbsAdpt = cond_probs_adaptive(data, arities, sampleSize, targetIndex, x, targetIndex)
      cachPTs[[cachInd]] = condProbsAdpt 
      # assign an unique name as primary key to look up existing cached PTs
      names(cachPTs)[cachInd] = paste(c(x, targetIndex), collapse = "")
      cachInd = cachInd + 1 
      
    }
    
    lp = lp + log_prob_adaptive(data, sampleSize, targetIndex, condProbsAdpt)
    margProbs = margProbs * condProbsAdpt
      
  }
  
  llh = -(lp - sum(log(apply(margProbs, 2, sum)))) # log(p(T|Xs))
  lst = list(llh = llh, cachPTs = cachPTs, cachInd = cachInd)
  
  return(lst)
  
}







