dag = generateDag(5, 2)
cpts = generateCPTs(dag, 2, 1)
data = rbn(cpts, 1000)
graphviz.plot(dag)

#data = asia

dataInfo = getDataInfo(data)
arities = dataInfo$arities
indexListPerNodePerValue = dataInfo$indexListPerNodePerValue
base = exp(1)
indicatorMatrix = getIndicator(data)
interactData = getInteractData(indicatorMatrix)
completeIndicatorMatrix = cbind(indicatorMatrix, interactData)

debug = T
allNodes = names(cpts)

ls0 = ls1 = ls2 = ls3 = list()
for (i in 1:length(cpts)) {
  
  node = allNodes[i]
  
  ls0[[i]] = bnlearn::mb(cpts, node)
  
  ls1[[i]] = mbForwardSelection(data, node, mmlLogit, arities, indexListPerNodePerValue, 
                     base = exp(1), indicatorMatrix = indicatorMatrix, mbSize = 1000, 
                     interaction = FALSE, debug)
  
  ls2[[i]] = mbForwardSelectionForMML2ndOrderLogit(data, node, arities, indexListPerNodePerValue,
                                        base = exp(1), indicatorMatrix, 
                                        interactData, completeIndicatorMatrix, debug)
  
  ls3[[i]] = mbForwardSelection.fast(data, node, arities, indexListPerNodePerValue, base = exp(1), debug)
}

# compute accuracy 
mtx = matrix(0, nrow = length(cpts) + 1, ncol = 6)
rownames(mtx) = c(allNodes, "mean")
colnames(mtx) = c("pre_1st", "pre_2nd", "pre_cpt", "rec_1st", "rec_2nd", "rec_cpt")
for (i in 1:length(cpts)) {
  
  res = mbAccuracy(ls0[[i]], ls1[[i]], allNodes[i], allNodes)
  mtx[i, "pre_1st"] = res$precision
  mtx[i, "rec_1st"] = res$recall
  
  res = mbAccuracy(ls0[[i]], ls2[[i]], allNodes[i], allNodes)
  mtx[i, "pre_2nd"] = res$precision
  mtx[i, "rec_2nd"] = res$recall
  
  res = mbAccuracy(ls0[[i]], ls3[[i]], allNodes[i], allNodes)
  mtx[i, "pre_cpt"] = res$precision
  mtx[i, "rec_cpt"] = res$recall
  
}

mtx[length(cpts) + 1, ] = apply(mtx, 2, mean)
round(mtx, 2)












