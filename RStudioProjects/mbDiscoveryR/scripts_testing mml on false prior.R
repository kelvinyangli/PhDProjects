# test on mml using different prior

libraries = c("bnlearn", "pcalg", "gRain", "gtools", "entropy", "reshape2", "ggplot2", "Rgraphviz")
lapply(libraries, require, character.only = TRUE)

sourceDir <- function(path, fileName = NULL, trace = TRUE, ...) {
  allFiles = list.files(path, pattern = fileName)
  for (file in allFiles) {
    if(trace) cat(file,":")
    source(file.path(path, file), ...)
    if(trace) cat("\n")
  }
}

# source from local repository
sourceDir("mbMMLCPT/")
sourceDir("createBN/")

dag = empty.graph(c("T", paste0("V", 1:5)))
for (i in 1:5) dag = set.arc(dag, paste0("V", i), "T")
graphviz.plot(dag)

n = 10 ^ (2:4)
f = function(beta) {
  
  for (i in 1:length(n)) {
    
    ss = 0 
    for (k in 1:5) {
      
      cpts = generateCPTs(dag, 2, beta) 
      
      for (j in 1:5) {
        
        data = rbn(cpts, n[i])
        dataInfo = getDataInfo(data)
        markovBlanket = mbForwardSelection.fast(data, "T", mmlCPT.fast, dataInfo$arities, dataInfo$indexListPerNodePerValue)
        #cat(length(markovBlanket) / 5, "   ")
        ss = ss + length(markovBlanket) / 5
        
      }
      
    }
    
    print(ss / 25)
    
  }
  
}

> f(1)
[1] 0.432
[1] 0.952
[1] 1
> f(2)
[1] 0.272
[1] 0.8
[1] 1
> f(3)
[1] 0.24
[1] 0.848
[1] 1
> f(4)
[1] 0.128
[1] 0.752
[1] 0.96
> f(5)
[1] 0.112
[1] 0.456
[1] 1
6
[1] 0.136
[1] 0.192
[1] 1
7
[1] 0.136
[1] 0.288
[1] 0.944
8
[1] 0.136
[1] 0.184
[1] 0.992
9
[1] 0.144
[1] 0.232
[1] 0.888
10
[1] 0.152
[1] 0.12
[1] 0.856









