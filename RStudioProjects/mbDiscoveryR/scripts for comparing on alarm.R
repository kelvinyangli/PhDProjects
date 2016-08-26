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

cpts = read.dsc("Known BNs/alarm.dsc")

##############################################################################################################
# re-sample cpts for alarm using uniform prior 
dag = model2network(modelstring(cpts))
data = rbn(cpts, 20000)

#dag = generateDag(37, 4)
#cpts = generateCPTs(dag, 4, 1)

data = rbn(cpts, 4000)

data = read.csv("alarm.csv") # use data provided by Jena for PCMB 20000 samples
data = data[1:4000,]
dataInfo = getDataInfo(data) 
allNodes = nodes(cpts)

##############################################################################################################
mbList = list()
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
  resultsMatrix[i, "distance"] = sqrt((1 - results$recall) ^ 2 + (1 - results$recall) ^ 2)
  
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
  resultsMatrix[i, "distance"] = sqrt((1 - results$recall) ^ 2 + (1 - results$recall) ^ 2)
  
}
colMeans(resultsMatrix)


##############################################################################################################
#indicatorMatrix = getIndicator(data)
#interactData = getInteractData(indicatorMatrix)
#completeIndicatorMatrix = cbind(indicatorMatrix, interactData)

# compute mb of each node using mmlCPT with 2 stages
for (i in 1:length(allNodes)) {
  
  targetNode = allNodes[i]
  
  mbLearned = mb2stage(data, targetNode, mmlCPT.fast, dataInfo)
    
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$recall) ^ 2 + (1 - results$recall) ^ 2)
  
} 

colMeans(resultsMatrix)

##############################################################################################################
#evaluate pcmb results from c++
ord = c(3, 1, 2, 17, 25, 18, 26, 28, 7, 8, 30, 9, 20, 19, 4, 14, 23, 15, 12, 32, 11, 10, 21, 31, 22, 13, 24, 16, 37, 36, 35, 34, 33, 27, 29, 6, 5)
allNodes.short = colnames(alarm)
allNodes.full = bnlearn::nodes(cpts)

allNodes.short[ord] = allNodes.full

mbList = getPCMB("pcmb.csv")[ord]

for (i in 1:length(mbList)) {
  
  targetNode = allNodes.full[i]
  
  mbLearned = allNodes.short[mbList[[i]]]
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes.full)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$recall) ^ 2 + (1 - results$recall) ^ 2)
  
}

colMeans(resultsMatrix)





