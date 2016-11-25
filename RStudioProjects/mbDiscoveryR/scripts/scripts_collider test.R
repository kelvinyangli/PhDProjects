####################################################################
# test for collider identify accuracy
dag = empty.graph(c("X", "Y", "Z"))
dag = set.arc(dag, "X", "Z")
dag = set.arc(dag, "Y", "Z")

dag = generateDag(7, 2)
graphviz.plot(dag)

allNodes = bnlearn::nodes(dag)
X = "V2"
Y = "V3"
Z = "V4"
xIndex = which(allNodes == X)
yIndex = which(allNodes == Y)
zIndex = which(allNodes == Z)

n = 100
cmicount = mmlcount = 0 
for (i in 1:1000) {
  cpts = generateCPTs(dag, 2, 1)
  data = rbn(cpts, n)
  dataInfo = getDataInfo(data)
  
  # x ->z<- y
  len1 = mmlCPT(xIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
    mmlCPT(yIndex, c(), dataInfo$indexListPerNodePerValue, dataInfo$arities, n) + 
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
  
  if (which.min(c(len1, len2, len3)) == 1) mmlcount = mmlcount + 1
  if (which.max(c(cmi1, cmi2, cmi3)) == 1) cmicount = cmicount + 1
  
}

print(mmlcount)
print(cmicount)