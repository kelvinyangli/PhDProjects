nFiles = c(1, 1, 2, 3, 5, 7, 10, 13) # n files for each value n \in [0, 7]
dir = "mbDags/"
par(mfrow = c(1, 3))
dag = generateDag(9, 2)
graphviz.plot(dag, main = "true dag")

cpts = generateCPTs(dag, 2, 1)
n = 1000
data = rbn(cpts, n)
data = data[, sample(1:ncol(data))]
vars = colnames(data)
learned.mmhc = mmhc(data)
#graphviz.plot(learned.mmhc, main = "mmch")
dataInfo = getDataInfo(data)

#y = "V4"
#graphviz.plot(dag, main = "true dag", highlight = list(nodes = y))

#################################### learn mb(y) #################################### 
mbList_true = list()
mbList_learned = list()
for (i in 1:length(vars)) {
  
  mbList_true[[i]] = mb(dag, vars[i])
  mbList_learned[[i]] = mbForwardSelection.fast(data, vars[i], dataInfo$arities, dataInfo$indexListPerNodePerValue)
  
}

#names(mbList) = vars
#(x = mb(dag, y))
#(x = mbForwardSelection.fast(data, y, dataInfo$arities, dataInfo$indexListPerNodePerValue))

################# exhaustive search for best polytree in terms of mml ###############
# only learn structure within mb if |mb| > 0
# and perhaps start with vars with medium size mb
mbdagList_true = list()
for (i in 1:length(vars)) {
  
  if (length(mbList_true[[i]]) > 0) {
    
    files = list.files(dir, paste0(length(mbList_true[[i]]), "_"))[1:nFiles[length(mbList_true[[i]]) + 1]]
    dagList = list()
    for (j in 1:length(files)) dagList = c(dagList, readRDS(paste0(dir, files[j])))
    dagList = substituteVar(dagList, vars[i], mbList_true[[i]])
    mmlmtx = computeMMLMatrix(vars, mbList_true[[i]], vars[i], dataInfo, n)
    polytree = polytree_exhaustive(dagList, vars, mbList_true[[i]], vars[i], dataInfo, mmlmtx, n)
    mbdagList_true[[i]] = addArcs_greedy(polytree, mmlmtx, dataInfo, vars, debug = F)
    
  } else {
    
    mbdagList_true[[i]] = matrix(0, 1, 1, dimnames = list(vars[i], vars[i]))
    
  } # end else 
  
} 

mbdagList_learned = list()
for (i in 1:length(vars)) {
  
  if (length(mbList_learned[[i]]) > 0) {
    
    files = list.files(dir, paste0(length(mbList_learned[[i]]), "_"))[1:nFiles[length(mbList_learned[[i]]) + 1]]
    dagList = list()
    for (j in 1:length(files)) dagList = c(dagList, readRDS(paste0(dir, files[j])))
    dagList = substituteVar(dagList, vars[i], mbList_learned[[i]])
    mmlmtx = computeMMLMatrix(vars, mbList_learned[[i]], vars[i], dataInfo, n)
    polytree = polytree_exhaustive(dagList, vars, mbList_learned[[i]], vars[i], dataInfo, mmlmtx, n)
    mbdagList_learned[[i]] = addArcs_greedy(polytree, mmlmtx, dataInfo, vars, debug = F)
    
  } else {
    
    mbdagList_learned[[i]] = matrix(0, 1, 1, dimnames = list(vars[i], vars[i]))
    
  } # end else 
  
} 
#names(mbdagList) = vars

learned_true = matrix2dag(mergeMBDags(mbdagList_true, vars))
learned_learned = matrix2dag(mergeMBDags(mbdagList_learned, vars))
#graphviz.plot(learned.mml, main = "mml")

#graphviz.plot(matrix2dag(polytree), main = "learned mb", highlight = list(nodes = y))

######################### greedy search for additional arcs #########################
#adjmtx = addArcs_greedy(polytree, mmlmtx, dataInfo, vars, debug = T)
#graphviz.plot(matrix2dag(adjmtx), highlight = list(nodes = y))

################ compare edit distance b/w true mbdag and learned ###################
cat("mmhc: dag --", editDistDags(learned.mmhc, dag), "; skeleton --", hamming(learned.mmhc, dag), "\n")
cat("mml_true: dag --", editDistDags(learned_true, dag), "; skeleton --", hamming(learned_true, dag), "\n")
cat("mml_learned: dag --", editDistDags(learned_learned, dag), "; skeleton --", hamming(learned_learned, dag), "\n")

ed_true = rep(0, length(vars))
ed_learned = rep(0, length(vars))
for (i in 1:length(vars)) {
  
  ed_true[i] = editDistDags(matrix2dag(mbdagList_true[[i]]), subgraph(dag, c(mbList_true[[i]], vars[i])))
  ed_learned[i] = editDistDags(matrix2dag(mbdagList_learned[[i]]), subgraph(dag, c(mbList_true[[i]], vars[i])))
  
}

cat(mean(ed_true), "\n")
cat(mean(ed_learned))









