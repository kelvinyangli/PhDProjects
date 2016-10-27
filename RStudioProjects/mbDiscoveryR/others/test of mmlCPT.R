
# set.seed(494246) 
dag = generateDag(12, 2)
graphviz.plot(dag)

cpts = generateCPTs(dag, 2, 5)
data = rbn(cpts, 5000)

mmlCPT("V6","V5",data)

mmlCPT("V5","V6",data)

sapply(names(data), mmlCPT, childNode = "V2", data = data, base = 2)

mmlCPT("V6",c("V7","V1"),data)


dag2Nodes = empty.graph(c("V1","V2"))
dag2Nodes = set.arc(dag2Nodes, "V1","V2")


times = 100
res = rep(0, times)
resa = res
resb = res
for (i in 1:times) {
  
  cpts2Nodes = generateCPTs(dag2Nodes, 2, 1)
  data2Nodes = rbn(cpts2Nodes, 100)
  
  a = mmlCPT("V1","V2",data=data2Nodes,base=2) + mmlSingleNode("V2", data = data2Nodes,base=2)
  b = mmlCPT("V2","V1",data=data2Nodes,base=2) + mmlSingleNode("V1", data = data2Nodes,base=2)
  
  c = a - b
  #if (!is.numeric(c)) {dd = data2Nodes}
  res[i] = c
  resa[i] = a
  resb[i] = b
}

sum(res < 0)
sum(res > 0)
sum(res == 0)

#######

times = 20
res = rep(0, times)
for (i in 1:times) {
  
  cpts = generateCPTs(dag, 2, 1)
  data = rbn(cpts, 1000)
  
  a = mmlCPT("V5","V1",data=data,base=2)
  b = mmlCPT("V5",c("V1","V7"),data=data,base=2) 
  c = mmlCPT("V5",c("V1","V7","V3"),data=data,base=2) 
  
  cat("parent", a, "-- parent+children", b, "-- mb", c, "\n")
  
  d = c(a,b,c)
  
  res[i] = which.min(d)
  
}

sum(res == 1)
sum(res == 2)
sum(res == 3)


### 
dag = empty.graph(c("V1","V2","V3"))
dag = set.arc(dag, "V1","V2")
dag = set.arc(dag, "V3","V2")
dag = set.arc(dag, "V1","V3")

allNodes = colnames(data)
t = "V7"
v = allNodes[allNodes != t]
res = sapply(v, mmlCPT, childNode = t, data = data, base = 2)
res = res[order(res)]
res

res2=sapply(v, ci.test, x = t, data = data)[1,]
res2=unlist(res2)
res2=res2[order(res2, decreasing = TRUE)]
res2

unlist(sapply(v[v!="V2"], ci.test, x = "V7", data = data)[1,])
unlist(sapply(v[v!="V2"], ci.test, x = "V7", z = "V2", data = data)[1,])

for (i in 1:10) {
  #cpts = generateCPTs(dag, 2, 1)
  data = rbn(cpts, 5000)
  a=ci.test("V2","V4","V7",data=data)$statistic - ci.test("V2","V4",data=data)$statistic
  b=ci.test("V7","V4","V2",data=data)$statistic - ci.test("V7","V4",data=data)$statistic
  c=ci.test("V2","V7","V4",data=data)$statistic - ci.test("V2","V7",data=data)$statistic
  
  a1 = mmlCPT("V7",c("V2","V4"),data)
  b1=mmlCPT("V2",c("V7","V4"),data)
  c1=mmlCPT("V4",c("V7","V2"),data)
  
  v1 = c(a,b,c)
  v2 = c(a1,b1,c1)
  
  cat("mi:", which.max(v1), "& mml:", which.min(v2), "\n")
}


for (i in 1:10) {
  cpts = generateCPTs(dag, 2, 5)
  data=rbn(cpts,5000)
  a=mmlSingleNode("V1",data)+mmlCPT("V2",c("V1","V3"),data)+mmlCPT("V3","V1",data)
  
  b=mmlSingleNode("V1",data)+mmlCPT("V2",c("V1","V3"),data)+mmlSingleNode("V3",data)
  
  cat("true:", a, "non-true:", b, "\n")
}














