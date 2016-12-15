nFiles = c(1, 1, 2, 3, 5, 7, 10, 13) # n files for each value n \in [0, 7]
dir = "mbDags/"
par(mfrow = c(1, 3))
dag = generateDag(9, 2)
graphviz.plot(dag, main = "true dag")

cpts = generateCPTs(dag, 2, 1)
n = 10000
data = rbn(cpts, n)
data = data[, sample(1:ncol(data))]
vars = colnames(data)
#learned.mmhc = mmhc(data)
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

################# exhaustive search for best polytree in terms of mml ###############
# only learn structure within mb if |mb| > 0
# and perhaps start with vars with medium size mb
# when |mb| = 1, it is the hardest problem, and perhaps no method could distinguish the two directions using 
# only observational data
ml_true = ml_true_ext = ml_learned = ml_learned_ext = list()

for (i in 1:length(vars)) {
  
  if (length(mbList_true[[i]]) > 0) {
    
    files = list.files(dir, paste0(length(mbList_true[[i]]), "_"))[1:nFiles[length(mbList_true[[i]]) + 1]]
    dagList = list()
    for (j in 1:length(files)) dagList = c(dagList, readRDS(paste0(dir, files[j])))
    dagList = substituteVar(dagList, vars[i], mbList_true[[i]])
    mmlmtx = computeMMLMatrix(vars, mbList_true[[i]], vars[i], dataInfo, n)
    ml_true[[i]] = polytree_exhaustive(dagList, vars, mbList_true[[i]], vars[i], dataInfo, mmlmtx, n)
    ml_true_ext[[i]] = addArcs_greedy(ml_true[[i]], mmlmtx, dataInfo, vars, debug = F)
    
  } else {
    
    ml_true[[i]] = matrix(0, 1, 1, dimnames = list(vars[i], vars[i]))
    ml_true_ext[[i]] = matrix(0, 1, 1, dimnames = list(vars[i], vars[i]))
    
  } # end else 
  
} 

for (i in 1:length(vars)) {
  
  if (length(mbList_learned[[i]]) > 0) {
    
    files = list.files(dir, paste0(length(mbList_learned[[i]]), "_"))[1:nFiles[length(mbList_learned[[i]]) + 1]]
    dagList = list()
    for (j in 1:length(files)) dagList = c(dagList, readRDS(paste0(dir, files[j])))
    dagList = substituteVar(dagList, vars[i], mbList_learned[[i]])
    mmlmtx = computeMMLMatrix(vars, mbList_learned[[i]], vars[i], dataInfo, n)
    ml_learned[[i]] = polytree_exhaustive(dagList, vars, mbList_learned[[i]], vars[i], dataInfo, mmlmtx, n)
    ml_learned_ext[[i]] = addArcs_greedy(ml_learned[[i]], mmlmtx, dataInfo, vars, debug = F)
    
  } else {
    
    ml_learned[[i]] = matrix(0, 1, 1, dimnames = list(vars[i], vars[i]))
    ml_learned_ext[[i]] = matrix(0, 1, 1, dimnames = list(vars[i], vars[i]))
    
  } # end else 
  
} 

#names(mbdagList) = vars

ed_true = ed_true_ext = ed_learned = ed_learned_ext = rep(0, length(vars))
for (i in 1:length(vars)) {
  
  ed_true[i] = editDistDags(matrix2dag(ml_true[[i]]), subgraph(dag, c(mbList_true[[i]], vars[i])))
  ed_true_ext[i] = editDistDags(matrix2dag(ml_true_ext[[i]]), subgraph(dag, c(mbList_true[[i]], vars[i])))
  ed_learned[i] = editDistDags(matrix2dag(ml_learned[[i]]), subgraph(dag, c(mbList_true[[i]], vars[i])))
  ed_learned_ext[i] = editDistDags(matrix2dag(ml_learned_ext[[i]]), subgraph(dag, c(mbList_true[[i]], vars[i])))
  
}

cat(ed_true, "\n")
cat(ed_true_ext, "\n")
cat(ed_learned, "\n")
cat(ed_learned_ext)

par(mfrow = c(1, 3))

i = 2
graphviz.plot(subgraph(dag, c(mbList_true[[i]], vars[i])), highlight = list(nodes = vars[i]), main = "true")
graphviz.plot(matrix2dag(ml_true[[i]]), highlight = list(nodes = vars[i]), main = "learned_true")
graphviz.plot(matrix2dag(ml_true_ext[[i]]), highlight = list(nodes = vars[i]), main = "learned_true_ext")

#learned_true = matrix2dag(mergeMBDags(mbdagList_true, vars))
#learned_learned = matrix2dag(mergeMBDags(mbdagList_learned, vars))
#graphviz.plot(learned.mml, main = "mml")

#graphviz.plot(matrix2dag(polytree), main = "learned mb", highlight = list(nodes = y))

######################### greedy search for additional arcs #########################
#adjmtx = addArcs_greedy(polytree, mmlmtx, dataInfo, vars, debug = T)
#graphviz.plot(matrix2dag(adjmtx), highlight = list(nodes = y))

################ compare edit distance b/w true mbdag and learned ###################
#cat("mmhc: dag --", editDistDags(learned.mmhc, dag), "; skeleton --", hamming(learned.mmhc, dag), "\n")
#cat("mml_true: dag --", editDistDags(learned_true, dag), "; skeleton --", hamming(learned_true, dag), "\n")
#cat("mml_learned: dag --", editDistDags(learned_learned, dag), "; skeleton --", hamming(learned_learned, dag), "\n")





