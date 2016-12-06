dag3 = set.arc(dag1,"V2", "T")
#dag4 = set.arc(dag2, "T", "V1")
dag3 = empty.graph(c("V1","V2","V3"))
dag3 = set.arc(dag3, "V1","V2")
n = 1000
count = 0
for (i in 1:100) {
  
  cpts3 = generateCPTs(dag3, 2, 1)
  #cpts4 = generateCPTs(dag4, 3, 1)
  
  data3 = rbn(cpts3, n)
  #data4 = rbn(cpts4, n)
  
  dataInfo3 = getDataInfo(data3)
  #dataInfo4 = getDataInfo(data4)
  
    mmlCPT(1, c(2), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n) 
    mmlCPT(2, c(), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n) 
    mmlCPT(3, c(), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n)
  
    mmlCPT(1, c(), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n)
    mmlCPT(2, c(1), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n) 
    mmlCPT(3, c(), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n)
  
  if (mml3 < mml4) count = count + 1
  
}
cat(count)

nFiles = c(1, 1, 2, 3, 5, 7, 10, 13) # n files for each value n \in [0, 7]
#dag = generateDag(7,3)
#graphviz.plot(dag)

# for polytrees
mtx = polytree(7, 3)
graphviz.plot(matrix2dag(mtx))

dag = matrix2dag(mtx)
cpts = generateCPTs(dag, 3, 1)
n = 1000
data = rbn(cpts, n)
graphviz.plot(mmhc(data))
dataInfo = getDataInfo(data)
par(mfrow = c(1, 2))

y = "V7"
graphviz.plot(dag, main = "true dag", highlight = list(nodes = y))
(x = mb(dag, y))
(files = list.files("mbDags/", paste0(length(x), "_"))[1:nFiles[length(x) + 1]])
dagList = list()
for (i in 1:length(files)) {
  dagList = c(dagList, readRDS(paste0("mbDags/", files[i])))
}
length(dagList)
#dagList[[1]]
dagList = substituteVar(dagList, y, x)
#dagList[[1]]
scores = rep(0, length(dagList))
for (i in 1:length(dagList)) {
  scores[i] = mmlDag(dagList[[i]], dataInfo, colnames(data), n)
}
minIndex = which.min(scores)
graphviz.plot(matrix2dag(dagList[[minIndex]]), main = "learned mb", highlight = list(nodes = y))
mtx = dag2matrix(dag)
dimnames(mtx) = list(nodes(dag), nodes(dag))
mbMatrix = mtx[c(x, y), c(x, y)]
index = c()
for (i in 1:length(dagList)) {
  if (matrixIdentical(mbMatrix, dagList[[i]])) index = c(index, i)
}
index # the true dag may not be found in our dagList, because we only generated polytrees, not the entire mbdag space
scores[c(minIndex, index)]
scores[order(scores)][1:5]




