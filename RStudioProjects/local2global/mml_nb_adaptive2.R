mml_nb_adaptive = function(data, arities, sampleSize, targetIndex, logProbTarget, cachPTs, chIndices) {
  
  lp = logProbTarget
  # a matrix to store the normalizting constant in p(T|Xs)
  margProbs = cachPTs[[targetIndex]]
  for (x in chIndices) {# go through each node in a given str
    
    condProbsAdpt = cachPTs[[x]]
    lp = lp + log_prob_adaptive(data, sampleSize, targetIndex, condProbsAdpt)
    margProbs = margProbs * condProbsAdpt
      
  }
  
  llh = -(lp - sum(log(apply(margProbs, 2, sum)))) # log(p(T|Xs))

  return(llh)
  
}







