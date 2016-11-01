dag = generateDag(20, 3)
cpts = generateCPTs(dag, 3, 1)
data = rbn(cpts, 1000)
#par(mfrow=c(1, 2))
graphviz.plot(dag)

#data = numeric2Nominal(data)
dataInfo = getDataInfo(data)
debug = F
allNodes = names(data)

mbList = list()
for (i in 1:length(allNodes)) mbList[[i]] = mbForwardSelection.fast(data, allNodes[i], dataInfo$arities, dataInfo$indexListPerNodePerValue, base = exp(1), debug)
mbList = symmetryCheck(allNodes, mbList)
names(mbList) = allNodes

dagLearned = learnBN(mbList, allNodes)

graphviz.plot(dagLearned)

hamming(dagLearned, dag, debug = T)

#ci.test("V1", "V2", data = data)$statistic
#ci.test("V1", "V2", "V7", data = data)$statistic

for (i in 1:length(allNodes)) {
  
  cat(allNodes[i], "\n")
  if (length(mbList[[i]]) > 1) computeRatio(allNodes[i], mbList[[i]], data)
  
}

cpts = generateCPTs(dag, 2, 1)
data = rbn(cpts, 1000)
ci.test("V1", "V2", "V8", data=data[101:200,])$statistic / ci.test("V1", "V2", data=data[101:200,])$statistic
ci.test("V1", "V3", "V2", data=data)$statistic - ci.test("V1", "V3", data=data)$statistic
ci.test("V2", "V3", "V1", data=data)$statistic - ci.test("V3", "V2", data=data)$statistic

missed = list()
for (i in 1:length(allNodes)) {
  
  mbTrue = mb(dag, allNodes[i])
  missed[[i]] = mbTrue[which(!mbTrue %in% mbList[[i]])]
  
}
names(missed) = allNodes

temp = data[, c("V3", "V4", "V6")]
ci.test("V3", "V4", "V2", data=data)$statistic / ci.test("V3", "V4", data=data)$statistic
ci.test("V3", "V4", "V6", data=temp)$statistic / ci.test("V3", "V4", data=temp)$statistic

values = levels(data[, "V6"])
cmi = 0
for (i in 1:length(values)) {
  
  indices = which(data[,"V6"] == values[i])
  temp = data[indices, c("V3", "V4")]
  dataInfo = getDataInfo(temp)
  mbForwardSelection.fast(temp, "V3", dataInfo$arities, 
                          dataInfo$indexListPerNodePerValue, base = exp(1), T)
  #cmi = cmi + ci.test("V3", "V4", data=temp[, c("V3","V4")])$statistic  
  
}

temp = data[, c("V3", "V4")]
dataInfo = getDataInfo(temp)
mbForwardSelection.fast(temp, "V3", dataInfo$arities, dataInfo$indexListPerNodePerValue, base = exp(1), T)

dag = empty.graph(c("V1", "V2", "V3"))
dag1 = set.arc(dag, "V1", "V2")
dag1 = set.arc(dag1, "V3", "V2")
graphviz.plot(dag1)

dag2 = dag
graphviz.plot(dag2)

dag3 = set.arc(dag, "V2", "V1")
dag3 = set.arc(dag3, "V3", "V1")
graphviz.plot(dag3)

countv2 = countv3 = 0
for (i in 1:100) {
  
  cpts1 = generateCPTs(dag1, 2, 1)
  data1 = rbn(cpts1, 100)
  
  allNodes = names(cpts1)
  dataInfo = getDataInfo(data1)
  res = mbForwardSelection.fast(data1, "V1", dataInfo$arities, dataInfo$indexListPerNodePerValue, debug = F)
  
  if ((length(res) != 0) && (res[1] == "V2")) countv2 = countv2 + 1
  if ((length(res) != 0) && (res[1] == "V3")) countv3 = countv3 + 1
  
}
countv2
countv3

countv2 = countv3 = 0
for (i in 1:100) {
  
  cpts3 = generateCPTs(dag3, 2, 1)
  data3 = rbn(cpts3, 1000)
  
  allNodes = names(cpts3)
  dataInfo = getDataInfo(data3)
  res = mbForwardSelection.fast(data3, "V1", dataInfo$arities, dataInfo$indexListPerNodePerValue, debug = T)
  cat("+++++++++++++++++++++++++++++++++++++++++++++++ \n")
  if ((length(res) != 0) && (res[1] == "V2")) countv2 = countv2 + 1
  if ((length(res) != 0) && (res[1] == "V3")) countv3 = countv3 + 1
  
}
countv2
countv3














