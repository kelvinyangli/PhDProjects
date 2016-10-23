# sample data from logistic regression 
dag = empty.graph(c("V1", "V2", "V3", "V4", "V5", "V6"))
dag = set.arc(dag, "V1", "V3")
dag = set.arc(dag, "V2", "V3")
dag = set.arc(dag, "V4", "V5")
#graphviz.plot(dag)

n = 1000
v1 = rbinom(n, 1, runif(1))
v2 = rbinom(n, 1, runif(1))
y = 1 + 0.2 * v1 + 0.5 * v2 + 1.4 * v1 * v2
pr = 1 / (1 + exp(-y))
v3 = rbinom(n, 1, pr)

v4 = rbinom(n, 1, runif(1))
z = 0.3 + 1.7 * v4
pr = 1 / (1 + exp(-z))
v5 = rbinom(n, 1, pr)

v6 = rbinom(n, 1, runif(1))

indicatorMatrix = data.frame(v1, v2, v3, v4, v5, v6)
colnames(indicatorMatrix) = c("V1", "V2", "V3", "V4", "V5", "V6")

data = indicatorMatrix
for (i in 1:6) {
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

cat("#################################### \n")
mbForwardSelectionForMML2ndOrderLogit(data, node, arities, indexListPerNodePerValue,
                                      base = exp(1), indicatorMatrix, 
                                      interactData, completeIndicatorMatrix, debug)

cat("#################################### \n")
mbForwardSelection.fast(data, node, arities, indexListPerNodePerValue, base = exp(1), debug)







