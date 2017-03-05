# edit distance between dags
# this is the same function as the hammingDags function used in autoEditDistance in bn evaluations
editDistDags = function(learned, true, debug = FALSE) {
  
  arcsLearned = directed.arcs(learned) 
  
  arcsTrue = directed.arcs(true)
  
  addition = 0
  deletion = 0
  reversion = 0
  
  # checkTrue = 0 means arc in true not in learned
  # hence addition add 1
  # checkTrue = -1 means arc in both but wrong direction
  # hence reversion add 1
  
  if ((nrow(arcsTrue) == 0) && (nrow(arcsLearned) > 0)) {
    
    # if dagTrue is empty but not dagLearned 
    deletion = nrow(arcsLearned)
    if (debug) cat("All arcs in the learned dag need to be deleted. \n")
    
  } else if ((nrow(arcsTrue) > 0) && (nrow(arcsLearned) == 0)) {
    
    # if dagLearned is empty but not dagTrue
    addition = nrow(arcsTrue)
    if (debug) cat("All arcs in the true dag need to be added to the learned dag. \n")
    
  } else if ((nrow(arcsTrue) > 0) && (nrow(arcsLearned) > 0)) {
    
    # if both dags are not empty
    for (i in 1:nrow(arcsTrue)) {
      
      # check if each arc in true appears in learned with the correct or reversed direction
      # as there can be only one 1 or -1, then sum up all results
      checkTrue = sum(apply(arcsLearned, 1, checkArc, x = arcsTrue[i,]))
      
      if (checkTrue == 0) {
        
        addition = addition + 1
        if (debug) {
          
          cat("* arcs between", arcsTrue[i,][[1]], "and", arcsTrue[i,][[2]], "do not match (addition). \n")
          cat("  > the learned network contains no arc between", arcsTrue[i,][[1]], "and", arcsTrue[i,][[2]], ". \n")
          cat("  > the true network contains", arcsTrue[i,][[1]], "->", arcsTrue[i,][[2]], ". \n")
          
        } # end debug
        
      } else if (checkTrue == -1) {
        
        reversion = reversion + 1
        if (debug) {
          
          cat("* arcs between", arcsTrue[i,][[1]], "and", arcsTrue[i,][[2]], "do not match (reversion). \n")
          cat("  > the learned network contains", arcsTrue[i,][[2]], "->", arcsTrue[i,][[1]], ". \n")
          cat("  > the true network contains", arcsTrue[i,][[1]], "->", arcsTrue[i,][[2]], ". \n")
          
        } # end debug
        
      } # end else if
      
    } # end for i
    
    for (j in 1:nrow(arcsLearned)) {
      
      # check if each arc in learned appears in true
      # as there can be only one 1 or -1, sum up all results
      checkLearned = sum(apply(arcsTrue, 1, checkArc, x = arcsLearned[j,]))
      
      if (checkLearned == 0) {
        
        deletion = deletion + 1
        if (debug) {
          
          cat("* arcs between", arcsTrue[j,][[1]], "and", arcsTrue[j,][[2]], "do not match (deletion). \n")
          cat("  > the learned network contains", arcsTrue[j,][[1]], "->", arcsTrue[j,][[2]], ". \n")
          cat("  > the true network contains no arc between", arcsTrue[j,][[1]], "and", arcsTrue[j,][[2]], ". \n")
          
        } # end debug
      
      } # end if 
      
    } # end for j
    
  }
  
  return(sum(addition, deletion, reversion))
  
}



