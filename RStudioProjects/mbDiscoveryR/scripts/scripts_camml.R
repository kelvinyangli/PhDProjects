cptsTrue = read.dsc("asia.dsc")
dagTrue = cpts2dag(cptsTrue)

#dagTrue = readRDS("wShapeDag.rds")
#cptsTrue = generateCPTs(dagTrue, 3, 1)
n = c(100, 500, 2500)
for (i in 1:length(n)) {
  seed = generateSeed()
  set.seed(seed)
  data = rbn(cptsTrue, n[i])
  #write.arff(data, paste0("../../../Documents/CaMML-master/Camml/asia_", n[i], ".arff")) # save data in .arff for camml
  write.arff(data, paste0("camml data/asia_", n[i], "_", seed, ".arff"))
}

# learn mb using data
data = read.arff("camml data/asia_100_832307.arff")
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
  res = mbAccuracy(mb(dagTrue, allNodes[i]), mbList[[i]], allNodes[i], allNodes)
  accuracies[i, 1] = res$precision
  accuracies[i, 2] = res$recall
}
colMeans(accuracies)


# apply mb results to camml
res = netica2bnlearn("../../../Documents/CaMML-master/Camml/asia_100_0.dne")
dag_camml = parentsList2BN(res)
# graphviz.plot(dag_camml)

# shd for cpdags
bnlearn::shd(learned = dag_camml, true = dagTrue)
# shd for skeletons
hamming(learned = dag_camml, true = dagTrue)
# shd for dags
editDistDags(learned = dag_camml, true = dagTrue, debug = T)

# build initial skeleton based on the 1st found variable in each mb
initialSkeleton = empty.graph(allNodes) #empty dag
for (i in 1:length(allNodes)) if (length(mbList[[i]]) > 0) initialSkeleton = set.edge(initialSkeleton, allNodes[i], mbList[[i]][1])
graphviz.plot(initialSkeleton, main = "initial skeleton")

# create prior for camml
prob = 1
path = "camml prior/prior.txt"
text = "arcs {"
for (i in seq(1, nrow(initialSkeleton$arcs), 2)) {
  
  text = paste(text, "\n", initialSkeleton$arcs[i, 1], "--", initialSkeleton$arcs[i, 2], prob, ";")
  
}
text = paste(text, "\n }")
write_file(text, path)

# evaluate
# apply mb results to camml
res_withPrior = netica2bnlearn("../../../Documents/CaMML-master/Camml/asia_100_prior0.9_0.dne")
dag_camml_withPrior = parentsList2BN(res_withPrior)
# graphviz.plot(dag_camml)

# shd for cpdags
bnlearn::shd(learned = dag_camml_withPrior, true = dagTrue)
# shd for skeletons
hamming(learned = dag_camml_withPrior, true = dagTrue)
# shd for dags
editDistDags(learned = dag_camml_withPrior, true = dagTrue, debug = T)






