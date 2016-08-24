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

# pc 
# let's try this out
# source from dropbox
#sourceDir("C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/")
#sourceDir("C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/MB discovery/mbMMLCPT/")

# source from local repository
sourceDir("mbMMLCPT/")
sourceDir("createBN/")

# apply mbMMLCPT on the alarm network
# each node will be considered as a target and the average of precision and recall over all nodes are reported
# euclidean distance to the perfect precision and recall is also reported, that is, 
# sqrt((1 - precision)^2 + (1 - recall)^2)

getStatistic = function(resultsMatrix) {
  
  sDeviations = apply(resultsMatrix, 2, sd)
  
  return(1.96 * sDeviations)
  
}

cpts = read.dsc("Known BNs/alarm.dsc")
data = rbn(cpts, 4000)

dataInfo = getDataInfo(data) 
allNodes = nodes(cpts)

resultsMatrix = matrix(0, nrow = length(allNodes), ncol = 3, dimnames = list(allNodes, c("precision", "recall", "distance")))

# compute mb of each node using standard forward selection
for (i in 1:length(allNodes)) {
  
  targetNode = allNodes[i]
  
  mbLearned = mbForwardSelection.fast(data, targetNode, mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue)
  
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$recall) ^ 2 + (1 - results$recall) ^ 2)
  
} 

colMeans(resultsMatrix)

##############################################################################################################
#indicatorMatrix = getIndicator(data)
#interactData = getInteractData(indicatorMatrix)
#completeIndicatorMatrix = cbind(indicatorMatrix, interactData)

# compute mb of each node using mmlLogit
for (i in 1:length(allNodes)) {
  
  targetNode = allNodes[i]
  
  mbLearned = learn.mb(data, targetNode, method = "iamb")
  
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$recall) ^ 2 + (1 - results$recall) ^ 2)
  
} 

colMeans(resultsMatrix)


