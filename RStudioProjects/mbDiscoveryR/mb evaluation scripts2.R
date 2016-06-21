numNodes = 21
maxNumParents = 3
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
  
  mb.cpt = mbForwardSelection(data,allNodes[i], mmlCPT, datainfo$arities, datainfo$indexListPerNodePerValue,
                              indicatorMatrix = NULL, debug = F)
  
  cat("mb.cpt:  ", mb.cpt, "\n")
  
  #mb.backward = mbBackwardElemination(data, allNodes[i], mmlCPT)
  
  #cat("mb.backward:  ", mb.backward, "\n")
  
  mb.logit = mbForwardSelection(data,allNodes[i], mmlLogit, datainfo$arities, datainfo$indexListPerNodePerValue,
                                indicatorMatrix = indicatorMatrix, debug = F)
  cat("mb.logit:", mb.logit, "\n")
  
  mb.logit2 = mbForwardSelection(data,allNodes[i], mmlLogit2ndOrder, datainfo$arities, datainfo$indexListPerNodePerValue,
                                indicatorMatrix = indicatorMatrix, interaction = T, debug = F)
  
  cat("mb.logit2:", mb.logit2, "\n")
  
  mb.iamb = learn.mb(data, allNodes[i], "iamb")
  cat("mb.iamb:", mb.iamb, "\n")
  
  cat("--------------------------- \n")
  
}











