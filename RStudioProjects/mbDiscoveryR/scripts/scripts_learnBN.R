####################################################################
# generate random dag, cpts, and data
par(mfrow = c(2, 2))
seed = generateSeed()
set.seed(seed)
dag = generateDag(20, 3)
graphviz.plot(dag, main = "dag")
cpts = generateCPTs(dag, 4, 1)
data = rbn(cpts, 10000)

graphviz.plot(mmhc(data), main = "mmhc")
####################################################################
# learn mbs for each varaible using mmlCPT
dataInfo = getDataInfo(data)
allNodes = names(data)
mbList = list()
debug = F
for (i in 1:length(allNodes)) {
  
  if (debug) cat("For", allNodes[i], ": \n")
  mbList[[i]] = mbForwardSelection.fast(data, allNodes[i], dataInfo$arities, 
                                                                    dataInfo$indexListPerNodePerValue, base = exp(1), debug)
  if (debug) cat("################################################################# \n")
  
}

# apply symmetry check for each learned mb
# i.e if a node x is in mb of y, and y is not in the mb of x, then add y into mb of x
# because mmlCPT has higher precision than recall
mbList = symmetryCheck(allNodes, mbList)
names(mbList) = allNodes
accuracies = matrix(nrow = length(allNodes), ncol = 2)
for (i in 1:length(allNodes)) {
  res = mbAccuracy(mb(dag, allNodes[i]), mbList[[i]], allNodes[i], allNodes)
  accuracies[i, 1] = res$precision
  accuracies[i, 2] = res$recall
}
colMeans(accuracies)
####################################################################
# 1st step is to connect each variable with the first found variable in its mb
# since the 1st found variable has a very high chance of being either parent or child of the target variable
dagLearned = empty.graph(allNodes) #empty dag

# connect each variable with the 1st found variable in its learned mb
# because the 1st found variable has a very high chance of being in the pc(target)
# but no direction can be inferred due to statistical equivalence b/w x -> y and x <- y
for (i in 1:length(allNodes)) if (length(mbList[[i]]) > 0) dagLearned = set.edge(dagLearned, allNodes[i], mbList[[i]][1])

graphviz.plot(dagLearned, main = "initial skeleton")
####################################################################
# find collider in each mb using interaction information (i.e. the chance of conditional mutual information)
potentialInteractions = threeWayInteraction(allNodes, mbList)
colliderIndices = findCollider(potentialInteractions, data)
colliders = matrix(nrow = nrow(colliderIndices), ncol = 3)
for (i in 1:nrow(colliderIndices)) colliders[i, ] = allNodes[colliderIndices[i, ]]
colliders

####################################################################
# assess three possible collider structures using mmlCPT
for (i in 1:nrow(colliders)) {
  
  x = colliders[i, 1]
  y = colliders[i, 2]
  z = colliders[i, 3]
  
  xIndex = which(allNodes == x)
  yIndex = which(allNodes == y)
  zIndex = which(allNodes == z)
  
  n = nrow(data)
  mmlCPT(xIndex, c(zIndex, yIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)-
  mmlCPT(xIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
  
  mmlCPT(yIndex, c(xIndex, zIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)-
  mmlCPT(yIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
  
  mmlCPT(zIndex, c(xIndex, yIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)-
  mmlCPT(zIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
  
  
  
  # x->z<-y
  len1 = mmlCPT(xIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(yIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(zIndex, c(xIndex, yIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
  
  # x->y<-z
  len2 = mmlCPT(xIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(zIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(yIndex, c(xIndex, zIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
  
  # z->x<-y
  len3 = mmlCPT(zIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(yIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(xIndex, c(zIndex, yIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
  
  index = which.min(c(len1, len2, len3))
  
  if (index == 1) {
    
    dagLearned = set.arc(dagLearned, x, z)
    dagLearned = set.arc(dagLearned, y, z)
    
  } else if (index == 2) {
    
    dagLearned = set.arc(dagLearned, x, y)
    dagLearned = set.arc(dagLearned, z, y)
    
  } else if (index == 3) {
    
    dagLearned = set.arc(dagLearned, z, x)
    dagLearned = set.arc(dagLearned, y, x)
    
  } # end else if
  
}

graphviz.plot(dagLearned)

####################################################################

  





