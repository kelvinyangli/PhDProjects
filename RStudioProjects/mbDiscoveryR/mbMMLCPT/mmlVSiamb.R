mmlVSiamb = function(numNodes, maxNumParents, maxNumValues, concentrationParameter, 
                   sampleSize, numTests, debug = FALSE) {
  
  precisions1 = rep(0, numTests)
  recalls1 = rep(0, numTests)
  
  precisions2 = rep(0, numTests)
  recalls2 = rep(0, numTests)
    
  precisions3 = rep(0, numTests)
  recalls3 = rep(0, numTests)
  
  meanMBSize = rep(0, numTests)
  
  for (i in 1:numTests) {
    
    dag = generateDag(numNodes, maxNumParents)
    
    cpts = generateCPTs(dag, maxNumValues, concentrationParameter)
    
    data = rbn(cpts, sampleSize)
    
    allNodes = names(data)
    
    numNodes = length(allNodes)
    
    precision1 = 0
    recall1 = 0 
    
    precision2 = 0
    recall2 = 0
    
    precision3 = 0
    recall3 = 0
    
    totalMBSize = 0 
    
    for (j in 1:numNodes) {
      
      mbTrue = bnlearn::mb(cpts, allNodes[j])
      
      totalMBSize = totalMBSize + length(mbTrue)
      
      mbLearned1 = mbMMLCPT(data, allNodes[j])  
      mbLearned2 = mbMMLCPT.revised(data, allNodes[j])
      mbLearned3 = learn.mb(data, allNodes[j], method = "iamb")
      
      accuracies1 = mbAccuracy(mbTrue, mbLearned1, allNodes[j], allNodes)
      accuracies2 = mbAccuracy(mbTrue, mbLearned2, allNodes[j], allNodes)
      accuracies3 = mbAccuracy(mbTrue, mbLearned3, allNodes[j], allNodes)
      
      if (debug) cat("gs+mml:", accuracies1, "\n")
      if (debug) cat("gsLookAhead+mml:", accuracies2, "\n")
      if (debug) cat("iamb:", accuracies3, "\n")
      if (debug) cat("-------------------------- \n")
      
      precision1 = precision1 + accuracies1[5]
      recall1 = recall1 + accuracies1[6]
      
      precision2 = precision2 + accuracies2[5]
      recall2 = recall2 + accuracies2[6]
      
      precision3 = precision3 + accuracies3[5]
      recall3 = recall3 + accuracies3[6]
      
    } # end for j
    
    precisions1[i] = precision1/numNodes
    recalls1[i] = recall1/numNodes
    
    precisions2[i] = precision2/numNodes
    recalls2[i] = recall2/numNodes
    
    precisions3[i] = precision3/numNodes
    recalls3[i] = recall3/numNodes
    
    meanMBSize[i] = totalMBSize/numNodes
    
  }
  
  f1 = 2*precisions1*recalls1/(precisions1 + recalls1)
  f2 = 2*precisions2*recalls2/(precisions2 + recalls2)
  f3 = 2*precisions3*recalls3/(precisions3 + recalls3)
  
  df = data.frame(pre.gsmml = precisions1, pre.lookaheadmml = precisions2, 
                  pre.iamb = precisions3, 
                  re.gsmml = recalls1, re.lookaheadmml = recalls2, 
                  re.iamb = recalls3, f1 = f1, f2 = f2, f3 = f3)
  
  
  return(df)
  
}