####################################################################
# test for collider identify accuracy
dag = empty.graph(c("X", "Y", "Z"))
dag = set.arc(dag, "X", "Z")
dag = set.arc(dag, "Y", "Z")

dag = generateDag(7, 2)
graphviz.plot(dag)

allNodes = bnlearn::nodes(dag)
X = "V2"
Y = "V4"
Z = "V6"
xIndex = which(allNodes == X)
yIndex = which(allNodes == Y)
zIndex = which(allNodes == Z)

n = 1000
cmicount = mmlcount = rep(0, 3) 
for (i in 1:100) {
  cpts = generateCPTs(dag, 2, 1)
  data = rbn(cpts, n)
  dataInfo = getDataInfo(data)
  
  # x ->z<- y
  len1 = mmlCPT(xIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(yIndex, c(xIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(zIndex, c(xIndex, yIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
  
  cmi1 = (ci.test(X, Y, Z, data = data)$statistic - ci.test(X, Y, data = data)$statistic)/ci.test(X, Y, data = data)$statistic
  
  # x->y<-z
  len2 = mmlCPT(xIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(zIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(yIndex, c(xIndex, zIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
  
  cmi2 = (ci.test(X, Z, Y, data = data)$statistic - ci.test(X, Z, data = data)$statistic)/ci.test(X, Z, data = data)$statistic
  
  # z->x<-y
  len3 = mmlCPT(zIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(yIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(xIndex, c(zIndex, yIndex), dataInfo$indexListPerNodePerValue, dataInfo$arities, n)
  
  cmi3 = (ci.test(Y, Z, X, data = data)$statistic - ci.test(Y, Z, data = data)$statistic)/ci.test(Y, Z, data = data)$statistic
  
  #cat("z collider:", len1, "-- y collider:", len2, "-- z collider", len3, "\n")
  
  indexmml = which.min(c(len1, len2, len3))
  indexcmi = which.max(c(cmi1, cmi2, cmi3))
  mmlcount[indexmml] = mmlcount[indexmml] + 1
  cmicount[indexcmi] = cmicount[indexcmi] + 1

}

(mmlcount)
(cmicount)


for (n in 1:10) cat("n =", n, ": #DAGs =", nDags(n), "-- #MBDAGs =", nMBDags(n - 1), "\n")











