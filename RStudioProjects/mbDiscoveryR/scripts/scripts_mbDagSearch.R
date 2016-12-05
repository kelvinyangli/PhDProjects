dag3 = set.arc(dag1,"V2", "T")
#dag4 = set.arc(dag2, "T", "V1")
dag3 = empty.graph(c("V1","V2","V3"))
dag3 = set.arc(dag3, "V1","V2")
n = 1000
count = 0
for (i in 1:100) {
  
  cpts3 = generateCPTs(dag3, 2, 1)
  #cpts4 = generateCPTs(dag4, 3, 1)
  
  data3 = rbn(cpts3, n)
  #data4 = rbn(cpts4, n)
  
  dataInfo3 = getDataInfo(data3)
  #dataInfo4 = getDataInfo(data4)
  
    mmlCPT(1, c(2), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n) 
    mmlCPT(2, c(), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n) 
    mmlCPT(3, c(), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n)
  
    mmlCPT(1, c(), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n)
    mmlCPT(2, c(1), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n) 
    mmlCPT(3, c(), dataInfo3$indexListPerNodePerValue, dataInfo3$arities, n)
  
  if (mml3 < mml4) count = count + 1
  
}
cat(count)


dag = generateDag(12,3)
graphviz.plot(dag)
cpts = generateCPTs(dag, 2, 1)
n = 10000
data = rbn(cpts, n)
dataInfo = getDataInfo(data)

y = "V11"
x = mb(dag, y)
length(x)
x
files = list.files("mbDags/", paste0(length(x), "_"))
(files = files[1:3])
dagList = list()
for (i in 1:length(files)) {
  dagList = c(dagList, readRDS(paste0("mbDags/", files[i])))
}
length(dagList)
dagList[[1]]
dagList = substituteVar(dagList, y, x)
scores = rep(0, length(dagList))
for (i in 1:length(dagList)) {
  scores[i] = mmlDag(dagList[[i]], dataInfo, n)
}
scores

graphviz.plot(matrix2dag(dagList[[which.min(scores)]]))
graphviz.plot(dag)






