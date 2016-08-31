# automate testing for mb discovery algorithm
# tested algorithms: iamb (c++), pcbm (c++), mmlcpt (R)
# evaluation measures: precision, recall, distance, f-measure
# details: both iamb and pcmb are written in c++ by jena, they require dataset without headers, but the true dag matrix is required. 
# since all methods will be tested under various sample sizes, we'll generate a large dataset at once. 
# 95% confidence intervals are reported together with the computed statistics

nNodes = 12
maxNParents = 3
maxArity = 2
beta = 1 # concentration parameter
n = 1000
seed = generateSeed()
set.seed(seed)

dag = generateDag(nNodes, maxNParents)
cpts = generateCPTs(dag, maxArity, beta)
data = rbn(cpts, n)
dagMatrix = dag2matrix(dag)

fileName = paste(nNodes, maxNParents, maxArity, beta, n, seed, sep = "_")
dir = "../../../Users/Administrator/Desktop/LearningMBs/IJAR/code/web/"
write.table(data, paste0(fileName, ".data"), row.names = FALSE, col.names = FALSE)
write.table(dagMatrix, paste0(fileName, ".data.net"), row.names = FALSE, col.names = FALSE)

#################################################### apply mbMMLCPT #################################################### 
dataInfo = getDataInfo(data[1:1000,]) 
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

################################################### evaluate iamb and pcmb results ###################################### 
# access c++ from R
results = system(paste0("kmb4 ", fileName, ".data ", 100, " ", nNodes, " -1 1.0 1 1 0.01"), intern = TRUE)

mbList = getPCMBsyn(results, 1)
#mbList = getPCMB("pcmb.csv")

for (i in 1:length(mbList)) {
  
  targetNode = allNodes[i]
  
  mbLearned = allNodes[mbList[[i]]]
  mbTrue = bnlearn::mb(cpts, targetNode)
  
  results = mbAccuracy(mbTrue, mbLearned, targetNode, allNodes)
  
  resultsMatrix[i, "precision"] = results$precision
  resultsMatrix[i, "recall"] = results$recall
  resultsMatrix[i, "distance"] = sqrt((1 - results$recall) ^ 2 + (1 - results$recall) ^ 2)
  
}

colMeans(resultsMatrix)




