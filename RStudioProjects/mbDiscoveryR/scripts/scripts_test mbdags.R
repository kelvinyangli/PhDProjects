#(vars = names(data))
#(y = vars[3])
#(candidates = vars[-3])
dag = generateDag(9,2)
graphviz.plot(dag)
cpts=generateCPTs(dag,2,1)
n=10000
data=rbn(cpts,n)
dataInfo=getDataInfo(data)
vars=names(data)
y = "V9"
x = c("V6")
candidates = vars[!vars %in% c(x, y)]
v = rep(0, length(candidates))
files = list.files(dir, paste0(length(x)+1, "_"))[1:nFiles[length(x)+1 + 1]]
dagList = list()
for (j in 1:(length(files))) dagList = c(dagList, readRDS(paste0(dir, files[j])))
for (i in 1:length(candidates)) {
  
  len = 0 
  z = c(x, candidates[i])
  dagList = substituteVar(dagList, y, z)
  mmlmtx = computeMMLMatrix(vars, z, y, dataInfo, n)
  polytree = polytree_exhaustive(dagList, vars, z, y, dataInfo, mmlmtx, n)
  len = mmlDag(polytree, vars, dataInfo, n)
  #for (j in 1:length(dagList)) {
   # len = len + mmlDag(dagList[[j]], vars, dataInfo, n)
  #}
  v[i] = len
 
}
candidates[order(v)]
w = rep(0, length(candidates))
for (i in 1:length(candidates)) {
  
  j = which(vars == candidates[i])
  w[i] = 
    6975.09 + 
    #mmlCPT(which(vars == y),c(),dataInfo$indexListPerNodePerValue,dataInfo$arities,n)+
    mmlCPT(j,c(),dataInfo$indexListPerNodePerValue,dataInfo$arities,n)
  
}

indices = which(v < w)
cat(candidates[indices], "\n")
v[indices]
#w[indices]
cat(v[indices] - w[indices], "\n")
