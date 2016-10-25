#dag = generateDag(2, 1)
#graphviz.plot(dag)

dag = empty.graph(c("V1","V2"))
dag1 = set.arc(dag, "V1", "V2")
dag2 = set.arc(dag, "V2", "V1")

s = rep(0, 100)
ss = s
sss = s
ssss = s
for (i in 1:100) {
  
  N = 1000
  cpts1 = generateCPTs(dag1, 2, 1)
  data1 = rbn(cpts1, N)
  
  
  L1 = mmlSingleNode2("V1", data1) # true
  L2 = mmlCPT2("V1", "V2", data1)
  
  L3 = mmlSingleNode2("V2", data1)
  L4 = mmlCPT2("V2","V1",data1) # true
  
  L5 = L1+L4 # true
  L6 = L2+L3
  
  error1 = L2 - L1
  error2 = L3 - L4
  error3 = L6 - L5
  error4 = L2 - L4
  
  if (error1 > 0) {
    s[i] = 1
  } 
   
  if (error2 > 0) {
    ss[i] = 1
  } 
  
  if (error3 > 0) sss[i] = 1
  
  if (error4 > 0) ssss[i] = 1
}

sum(s)
sum(ss)
sum(sss)
sum(ssss)


dag3 = empty.graph(c("V1","V2","V3"))
dag3 = set.arc(dag3, "V1","V2")
dag3 = set.arc(dag3, "V3","V2")
dag4 = set.arc(dag3, "V1", "V3")

cpts3 = generateCPTs(dag3, 2, 1)
data3 = rbn(cpts3, 1000)
cpts4 = generateCPTs(dag4, 2, 1)
data4 = rbn(cpts4, 1000)

mbMMLCPT(data3,"V2",debug=T)

mbMMLCPT(data4,"V2",debug=T)





