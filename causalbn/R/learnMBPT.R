# vars contains all variables
# mbList is the list contains mb(x), for each x in vars
# mbptsList is the list of all possible mbpts for each mb size n \in [0, 7]
# dataInfo contains detailed information of data that are pre-computed using getDataInfo()
# n is the sample size 
learnMBPT = function(vars, mbList, mbptsList, dataInfo, n) {
  
  strList = list()
  for (i in 1:length(vars)) {
    
    # compute mmlcpt for a var x given its possible parents, which are chosen from mb(x)
    mmlmtx = computeMMLMatrix(vars, mbList[[i]], vars[i], dataInfo, n)     
    mbpts = mbptsList[[length(mbList[[i]]) + 1]]
    mbpts = substituteVar(mbpts, vars[i], mbList[[i]]) # replace default vars with mb vars
    # compute the mml score for the 1st mbpt and set its score to the minimum for now
    mml_min = Inf
    index = 0 
    for (j in 1:length(mbpts)) {
      
      mml_current = mmlDag_fast(mbpts[[j]], vars, dataInfo, mmlmtx, n)
      if (mml_current < mml_min) { # if mbpt[j] is better then replace mml_min and its index
        
        mml_min = mml_current
        index = j
        
      } # end if 
      
    } # end for j
    
    strList[[i]] = mbpts[[index]] # list of the optimal local str for each var
    
  } # end for i
  
  # merging local structures into global structure
  mbpt_global = mergeMBPTs(strList, vars)
  mbpt_global = refineMergedMBPT(mbpt_global)  
  
  return(mbpt_global)
  
}

