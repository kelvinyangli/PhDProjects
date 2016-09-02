libraries = c("bnlearn", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2", "Rgraphviz")
lapply(libraries, require, character.only = TRUE)

sourceDir <- function(path, fileName = NULL, trace = TRUE, ...) {
  allFiles = list.files(path, pattern = fileName)
  for (file in allFiles) {
    if(trace) cat(file,":")
    source(file.path(path, file), ...)
    if(trace) cat("\n")
  }
}

# source from local repository
sourceDir("mbMMLCPT/")
sourceDir("createBN/")

# apply mbMMLCPT on the alarm network
# each node will be considered as a target and the average of precision and recall over all nodes are reported
# euclidean distance to the perfect precision and recall is also reported, that is, 
# sqrt((1 - precision)^2 + (1 - recall)^2)


##############################################################################################################
# re-sample cpts for alarm using uniform prior i.e dirichlet beta = 1
dag = readRDS("alarmDag.rds") # read dag
arities = readRDS("alarmArity.rds") # read arities
n = 16000
beta = 1
nIter = 10

res.std = matrix(0, nrow = nIter ^ 2, ncol = 4)
res.sym = res.std

for (ii in 1:nIter) {
  
  seed1 = generateSeed()
  set.seed(seed1)
  
  cpts = generateCPTs2(dag, arities, beta) # sample cpts using specified arities
  
  for (jj in 1:nIter) {
    
    seed2 = generateSeed()
    set.seed(seed2)
    
    data = rbn(cpts, n) # sample data
    write.table(data, paste0("pcmb/alarm_", n, "_", seed1, "_", seed2, ".data"), row.names = FALSE, col.names = FALSE) # save data
    
    dataInfo = getDataInfo(data) 
    mbList = list()
    allNodes = colnames(data)
    resultsMatrix = matrix(0, nrow = length(allNodes), ncol = 4, dimnames = list(allNodes, c("precision", "recall", "distance", "fmeasure")))
    
    # compute mb of each node using standard forward selection
    for (i in 1:length(allNodes)) {
      
      targetNode = allNodes[i]
      
      mbLearned = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue)
      
      mbList[[i]] = mbLearned
      
      mbTrue = bnlearn::mb(cpts, targetNode)

      results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
      
      resultsMatrix[i, "precision"] = results$precision
      resultsMatrix[i, "recall"] = results$recall
      resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
      if (results$precision + results$recall == 0) {
        
        resultsMatrix[i, "fmeasure"] = 0
        
      } else {
        
        resultsMatrix[i, "fmeasure"] = 2 * results$precision * results$recall / (results$precision + results$recall)
        
      }
      
    } 
    
    res.std[(ii - 1) * nIter + jj, ] = colMeans(resultsMatrix)
    
    # use symmetry condition to re-check for mb candidate for each node
    names(mbList) = allNodes
    for (i in 1:length(allNodes)) {
      
      node = allNodes[i] 
      
      # if node x is in mb(y), then y is also in mb(x)
      for (j in 1:length(allNodes)) {
        
        if ((j != i) && (node %in% mbList[[j]])) {# if node exists in the mb of another node
          
          if (!allNodes[j] %in% mbList[[i]]) {# if this other node is not in mb(node)
            
            # then add it into mb(node)
            mbList[[i]] = c(mbList[[i]], allNodes[j])
            
          } # end if
          
        } # end if
        
      } # end for j
      
    } # end for i
    
    # evaluation
    for (i in 1:length(allNodes)) {
      
      targetNode = allNodes[i]
      
      mbLearned = mbList[[i]]
      mbTrue = bnlearn::mb(cpts, targetNode)
      
      results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
      
      resultsMatrix[i, "precision"] = results$precision
      resultsMatrix[i, "recall"] = results$recall
      resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
      if (results$precision + results$recall == 0) {
        
        resultsMatrix[i, "fmeasure"] = 0
        
      } else {
        
        resultsMatrix[i, "fmeasure"] = 2 * results$precision * results$recall / (results$precision + results$recall)
        
      } # end else
      
    }
    res.sym[(ii - 1) * nIter + jj, ] = colMeans(resultsMatrix)
    
  }
  
}


##############################################################################################################


##############################################################################################################
#indicatorMatrix = getIndicator(data)
#interactData = getInteractData(indicatorMatrix)
#completeIndicatorMatrix = cbind(indicatorMatrix, interactData)

# compute mb of each node using mmlCPT with 2 stages
for (i in 1:length(allNodes)) {
  
  targetNode = allNodes[i]
  
  mbLearned = findMB(data, targetNode, dataInfo)
    
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
  
} 

colMeans(resultsMatrix)

##############################################################################################################
# pcmb from c++
results = system(paste0("kmb4 alarm.data ", n, " 37 -1 1.0 1 1 0.01"), intern = TRUE)

mbList = getPCMBsyn(results, 1)

for (i in 1:length(mbList)) {
  
  targetNode = allNodes[i]
  
  mbLearned = allNodes[mbList[[i]]]
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
  
}

colMeans(resultsMatrix)

# iamb from c++
results = system(paste0("kmb4 alarm.data ", n, " 37 -1 1.0 1 0 0.01"), intern = TRUE)

mbList = getPCMBsyn(results, 0)

for (i in 1:length(mbList)) {
  
  targetNode = allNodes[i]
  
  mbLearned = allNodes[mbList[[i]]]
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
  
}

colMeans(resultsMatrix)




