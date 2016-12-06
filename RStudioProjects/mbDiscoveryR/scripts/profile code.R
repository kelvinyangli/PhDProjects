# profile R code
Rprof("mml.out")
for (i in 1:length(allNodes)) {
  
  if (debug) cat("For", allNodes[i], ": \n")
  mbList[[i]] = mbForwardSelection.fast(data, allNodes[i], dataInfo$arities, 
                                        dataInfo$indexListPerNodePerValue, base = exp(1), debug)
  if (debug) cat("################################################################# \n")
  
}
Rprof(NULL)
proftable("mml.out")
