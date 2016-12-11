nFiles = c(1, 1, 2, 3, 5, 7, 10, 13) # n files for each value n \in [0, 7]
dir = "mbDags/"
dag = generateDag(12,3)
graphviz.plot(dag, main = "true dag")

cpts = generateCPTs(dag, 2, 1)
n = 10000
data = rbn(cpts, n)
vars = colnames(data)
graphviz.plot(mmhc(data), main = "mmch")
dataInfo = getDataInfo(data)
par(mfrow = c(1, 3))

y = "V4"
graphviz.plot(dag, main = "true dag", highlight = list(nodes = y))

#################################### learn mb(y) #################################### 
(x = mb(dag, y))
#(x = mbForwardSelection.fast(data, y, dataInfo$arities, dataInfo$indexListPerNodePerValue))

################# exhaustive search for best polytree in terms of mml ###############
files = list.files(dir, paste0(length(x), "_"))[1:nFiles[length(x) + 1]]
dagList = list()
for (i in 1:length(files)) dagList = c(dagList, readRDS(paste0(dir, files[i])))
dagList = substituteVar(dagList, y, x)
mmlmtx = computeMMLMatrix(vars, x, y, dataInfo, n)
polytree = polytree_exhaustive(dagList, vars, x, y, dataInfo, mmlmtx, n)
graphviz.plot(matrix2dag(polytree), main = "learned mb", highlight = list(nodes = y))

######################### greedy search for additional arcs #########################
adjmtx = addArcs_greedy(polytree, mmlmtx, dataInfo, vars, debug = T)
graphviz.plot(matrix2dag(adjmtx), highlight = list(nodes = y))

################ compare edit distance b/w true mbdag and learned ###################
editDistDags(learned, true) subgraph(dag, c(x, y))
