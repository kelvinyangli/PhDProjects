# scripts for mb discovery
# 
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

# pc 
# let's try this out
# source from dropbox
#sourceDir("C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/Experiments/source code/")
#sourceDir("C:/Users/Administrator/Dropbox/PhD@Monash/R/Code/MB discovery/mbMMLCPT/")

# source from local repository
sourceDir("mbMMLCPT/")
sourceDir("../causal-discovery-experiment-R/source code/")

# mac
#sourceDir("~/Dropbox/PhD@Monash/R/Code/MB discovery/mbMMLCPT/")
#sourceDir("~/Dropbox/PhD@Monash/R/Code/Experiments/source code/")

datasets = list.files("Experiments_mbDiscovery/Datasets/")

# asia network

cpts = read.dsc("Desktop/CPTs/alarm.dsc")
data = rbn(cpts, 5000)
allNodes = names(data)
ls.mml = list()
ls.iamb = list()
ls.true = list()

for (i in 1:ncol(data)) {
  
  ls.true[[i]] = bnlearn::mb(cpts, allNodes[i])
  ls.mml[[i]] = mbMMLCPT(data, allNodes[i])
  ls.iamb[[i]] = learn.mb(data, allNodes[i], method = "iamb")
  
  accuracy1 = mbAccuracy(ls.true[[i]], ls.mml[[i]], allNodes[i], allNodes)
  accuracy2 = mbAccuracy(ls.true[[i]], ls.iamb[[i]], allNodes[i], allNodes)
  cat("mml:", accuracy1, "\n")
  cat("iamb:", accuracy2, "\n")
  cat("------------------ \n")
  
}

ls.mml
ls.iamb

