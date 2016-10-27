indicatorMatrix = dataToIndicator(data)

# list all potential BMs of a target
allNodes = colnames(data)
allNodesIndexes = 1:length(allNodes)
y = "V1"
indexY = which(allNodes == y)
potentialNodesIndexes = allNodesIndexes[-indexY] 
mbList = list()
for (i in 1:4) mbList[[i]] = potentialNodesIndexes[i]
for (i in 5:8) mbList[[i]] = potentialNodesIndexes[-(i-4)]
mbList[[9]] = potentialNodesIndexes
mbList[[10]] = potentialNodesIndexes[c(1,2)]
mbList[[11]] = potentialNodesIndexes[c(1,3)]
mbList[[12]] = potentialNodesIndexes[c(1,4)]
mbList[[13]] = potentialNodesIndexes[c(2,3)]
mbList[[14]] = potentialNodesIndexes[c(2,4)]
mbList[[15]] = potentialNodesIndexes[c(3,4)]
cardinalities = rep(2,5)
mml = rep(0,15)
for (i in 1:15) {
  res = msgLen(data, indicatorMatrix, indexY, mbList[[i]], cardinalities, allNodes, sigma = 3)
  mml[i] = res$mml
}
mbList[[which.min(mml)]]
mbList[order(mml)]



