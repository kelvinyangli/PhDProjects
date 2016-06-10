numNodes = 15
maxNumParents = 2
maxNumValues = 2
concentration = 1
sampleSize = 1000
seed = generateSeed()
set.seed(seed)

# generate BNs and data 
dag = generateDag(numNodes, maxNumParents)
graphviz.plot(dag)
cpts = generateCPTs(dag, maxNumValues, concentration)
data = rbn(cpts, sampleSize)

allNodes = names(data)

datainfo = dataInfo(data)

indicatorMatrix = getIndicator(data)

for (i in 1:length(allNodes)) {
  
  cat("target:  ", allNodes[i], "\n")
  cat("mb.true: ", bnlearn::mb(dag, allNodes[i]), "\n")
  
  mb.forward = mbForwardSelection(data, allNodes[i], mmlCPT, datainfo$arities, datainfo$indexListPerNodePerValue)
  
  cat("mb.forward:  ", mb.forward, "\n")
  
  mb.backward = mbBackwardElemination(data, allNodes[i], mmlCPT)
  
  cat("mb.backward:  ", mb.backward, "\n")
  
  mb.logit = mbForwardSelection(data, allNodes[i], mmlLogit, datainfo$arities, datainfo$indexListPerNodePerValue, 2, indicatorMatrix)
  
  cat("mb.logit:", mb.logit, "\n")
  
  mb.iamb = learn.mb(data, allNodes[i], "iamb")
  cat("mb.iamb:", mb.iamb, "\n")
  
  cat("--------------------------- \n")
  
}




for (i in 1:length(allNodes)) {
  
  cat("target:  ", allNodes[i], "\n")
  cat("mb.true: ", bnlearn::mb(dag, allNodes[i]), "\n")
  
  mb.cpt = mbGreedySearchWithLookAhead(data, allNodes[i], mmlCPT)
  
  cat("mb.cpt:  ", mb.cpt, "\n")
  
  mb.iamb = learn.mb(data, allNodes[i], "iamb")
  
  cat("mb.iamb: ", mb.iamb, "\n")
  cat("--------------------------- \n")
  
}







