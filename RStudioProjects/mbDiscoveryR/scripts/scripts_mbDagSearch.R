nFiles = c(1, 1, 2, 3, 5, 7, 10, 13) # n files for each value n \in [0, 7]
dag = generateDag(7,3)
graphviz.plot(dag, main = "true dag")

# for polytrees
#mtx = polytree(7, 3)
#graphviz.plot(matrix2dag(mtx))
#dag = matrix2dag(mtx)

cpts = generateCPTs(dag, 2, 1)
n = 10000
data = rbn(cpts, n)
vars = colnames(data)
graphviz.plot(mmhc(data), main = "mmch")
dataInfo = getDataInfo(data)
par(mfrow = c(1, 3))

y = "V2"
graphviz.plot(dag, main = "true dag", highlight = list(nodes = y))
(x = mb(dag, y))
(files = list.files("mbDags/", paste0(length(x), "_"))[1:nFiles[length(x) + 1]])
dagList = list()
for (i in 1:length(files)) {
  dagList = c(dagList, readRDS(paste0("mbDags/", files[i])))
}
length(dagList)
dagList = substituteVar(dagList, y, x)
mmlmtx = computeMMLMatrix(x, y, vars, dataInfo, n)
scores = rep(0, length(dagList))
for (i in 1:length(dagList)) {
  scores[i] = mmlDag_fast(dagList[[i]], vars, dataInfo, mmlmtx, n)
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

adjmtx = addArcs_greedy(dagList[[minIndex]], mmlmtx, dataInfo, vars)
graphviz.plot(matrix2dag(adjmtx), highlight = list(nodes = y))


