dag = generateDag(17, 2)
cpts = generateCPTs(dag, 2, 1)
data = rbn(cpts, 1000)
graphviz.plot(dag)


dataInfo = getDataInfo(data)
arities = dataInfo$arities
indexListPerNodePerValue = dataInfo$indexListPerNodePerValue
base = exp(1)
indicatorMatrix = getIndicator(data)
interactData = getInteractData(indicatorMatrix)
completeIndicatorMatrix = cbind(indicatorMatrix, interactData)

node = "V1"
allNodes = names(cpts)
ls0 = ls1 = ls2 = list()
for (i in 1:length(cpts)) {
  
  node = allNodes[i]
  
  ls0[[i]] = bnlearn::mb(cpts, node)
  
  ls1[[i]] = mbForwardSelection(data, node, mmlLogit, arities, indexListPerNodePerValue, 
                     base = exp(1), indicatorMatrix = indicatorMatrix, mbSize = 1000, 
                     interaction = FALSE, debug = T)
  
  ls2[[i]] = mbForwardSelectionForMML2ndOrderLogit(data, node, arities, indexListPerNodePerValue,
                                        base = exp(1), indicatorMatrix, 
                                        interactData, completeIndicatorMatrix, debug = T)
}

formula = "V1~V2+V3+V4"
fit1 = glm(formula, family = binomial(link = "logit"), data = data)
fit2 = bayesglm(formula, family = binomial(link = "logit"), data = data)
fit1$coefficients
fit2$coefficients










