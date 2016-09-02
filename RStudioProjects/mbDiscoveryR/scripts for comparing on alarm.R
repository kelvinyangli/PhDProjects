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
cpts = generateCPTs2(dag, arities, 1) # sample cpts using specified arities
dataPool = rbn(cpts, 20000) # sample data
write.table(data, "alarm.data", row.names = FALSE, col.names = FALSE) # save data

##############################################################################################################
m = 500
data = dataPool[1:m, ]
dataInfo = getDataInfo(data) 
mbList = list()
allNodes = colnames(data)
resultsMatrix = matrix(0, nrow = length(allNodes), ncol = 3, dimnames = list(allNodes, c("precision", "recall", "distance")))

# compute mb of each node using standard forward selection
for (i in 1:length(allNodes)) {
  
  targetNode = allNodes[i]
  
  mbLearned = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue)
  
  mbList[[i]] = mbLearned
  
  mbTrue = bnlearn::mb(cpts, targetNode)
  #mbTrue = bnlearn::nbr(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$precision) ^ 2 + (1 - results$recall) ^ 2)
  
} 
colMeans(resultsMatrix)

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
  
}
colMeans(resultsMatrix)


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
results = system(paste0("kmb4 alarm.data ", m, " 37 -1 1.0 1 1 0.01"), intern = TRUE)

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
results = system(paste0("kmb4 alarm.data ", m, " 37 -1 1.0 1 0 0.01"), intern = TRUE)

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




