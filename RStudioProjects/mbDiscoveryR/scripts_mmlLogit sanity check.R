# sample data from logistic regression 
dag = empty.graph(c("V1", "V2", "V3"))
dag = set.arc(dag, "V1", "V3")
dag = set.arc(dag, "V2", "V3")
graphviz.plot(dag)

n = 500
v1 = rbinom(n, 1, 0.3)
v2 = rbinom(n, 1, 0.56)
y = 1 + 0.2 * v1 + 0.5 * v2 + 12 * v1 * v2
pr = 1 / (1 + exp(-y))
v3 = rbinom(n, 1, pr)

indicatorMatrix = data.frame(v1, v2, v3)
colnames(indicatorMatrix) = c("V1", "V2", "V3")
class(indicatorMatrix)

data = indicatorMatrix
for (i in 1:3) {
  data[, i] = as.factor(LETTERS[indicatorMatrix[, i] + 1])
}


# pre-process data
dataInfo = getDataInfo(data)
arities = dataInfo$arities
indexListPerNodePerValue = dataInfo$indexListPerNodePerValue
base = exp(1)
indicatorMatrix = getIndicator(data)
interactData = getInteractData(indicatorMatrix)
completeIndicatorMatrix = cbind(indicatorMatrix, interactData)

debug = T
node = "V3"

mbForwardSelection(data, node, mmlLogit, arities, indexListPerNodePerValue, 
                   base = exp(1), indicatorMatrix = indicatorMatrix, mbSize = 1000, 
                   interaction = FALSE, debug)

cat("############ \n")
mbForwardSelectionForMML2ndOrderLogit(data, node, arities, indexListPerNodePerValue,
                                      base = exp(1), indicatorMatrix, 
                                      interactData, completeIndicatorMatrix, debug)

cat("############ \n")
mbForwardSelection.fast(data, node, arities, indexListPerNodePerValue, base = exp(1), debug)